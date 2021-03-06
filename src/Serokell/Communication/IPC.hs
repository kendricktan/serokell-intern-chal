{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TupleSections              #-}

module Serokell.Communication.IPC
       ( NodeId (..)
       , Recv
       , Send
       , Conversation (..)
       , SocketDirectory

       , connectToUnixSocket
       , listenUnixSocket
       ) where

import           Control.Concurrent        (threadDelay)
import           Control.Concurrent.QSem   (QSem, newQSem, signalQSem, waitQSem)
import           Control.Exception.Safe    (bracket, bracket_)
import           Control.Monad             (when)
import           Data.Binary               (Binary)
import           Data.ByteString           (ByteString)
import           GHC.Generics              (Generic)
import           System.Environment        (lookupEnv)
import           System.FilePath           ((<.>), (</>))
import           System.IO.Unsafe          (unsafePerformIO)
import           Text.Read                 (readMaybe)

import qualified Network.Socket            as Net
import qualified Network.Socket.ByteString as NetByte

-- | Type alias for file path, representing directory with sockets.
type SocketDirectory = FilePath

-- | Id of node.
newtype NodeId = NodeId { unNodeId :: Int }
    deriving (Eq, Ord, Binary, Generic, Show)

type Send = ByteString -> IO ()
type Recv = IO ByteString

-- | Newtype wrapper for sending action to socket.
data Conversation = Conversation
    { send :: ByteString -> IO ()  -- ^ sends given 'ByteString' to socket
    , recv :: IO ByteString  -- ^ recieves 'ByteString' with length up to 4096 bytes from socket
    }

readEnv :: Read a => String -> IO (Maybe a)
readEnv name = (>>= readMaybe) <$> lookupEnv name

-- blocking QSem for multithreaded sending environment
-- reads env variable SRK_SOCK_CONC, concurrency parameter (capacity of semaphore)
sendSem :: Maybe QSem
sendSem = unsafePerformIO $ readEnv "SRK_SOCK_CONC" >>= traverse newQSem

{-# NOINLINE sendSem #-}

-- delay on outbound send (in milliseconds)
-- reads env variable SRK_SOCK_DELAY
sendDelay :: Maybe Int
sendDelay = unsafePerformIO $ readEnv "SRK_SOCK_DELAY"

{-# NOINLINE sendDelay #-}

withQSem :: QSem -> IO a -> IO a
withQSem qsem = bracket_ (waitQSem qsem) (signalQSem qsem)

outboundSendRecv :: Net.Socket -> Conversation
outboundSendRecv socket = Conversation outSend (recvSock socket)
  where
    outSend :: Send
    outSend data_ =
        (maybe id withQSem) sendSem $ do
            _ <- traverse msDelay sendDelay
            sendDo data_

    sendDo = sendSock socket

    msDelay :: Int -> IO ()
    msDelay milliseconds = threadDelay $ milliseconds * 10^(3 :: Int)

sendSock :: Net.Socket -> Send
sendSock conn = NetByte.sendAll conn

recvSock :: Net.Socket -> Recv
recvSock conn = NetByte.recv conn 4096

acceptServerSock :: Net.Socket -> IO Conversation
acceptServerSock socket = do
    (conn, _) <- Net.accept socket
    return $ Conversation (sendSock conn) (recvSock conn)

-- Creates socket using provided allocator and handles all exceptions using bracket function.
initializeSocket :: IO Net.Socket -> (Net.Socket -> IO ()) -> IO ()
initializeSocket before = bracket before Net.close

-- | Create new UNIX socket for node 'NodeId', listen to it and use supplied
-- handler to process incoming connections.
listenUnixSocket :: SocketDirectory
                 -> NodeId
                 -> (Conversation -> IO Bool)
                 -> IO ()
listenUnixSocket socketDirectory (NodeId nodeId) handler =
    initializeSocket allocator acceptLoop
  where
    acceptLoop socket = acceptServerSock socket >>= handler >>= flip when (acceptLoop socket)
    allocator :: IO Net.Socket
    allocator = do
        socket <- Net.socket Net.AF_UNIX Net.Stream Net.defaultProtocol
        let socketPath = socketDirectory </> show nodeId <.> "sock"
        Net.bind socket $ Net.SockAddrUnix socketPath
        Net.listen socket 5
        return socket

-- | Connect and communicate to unix socket for node 'NodeId', handler is supplied.
connectToUnixSocket :: SocketDirectory
                    -> NodeId
                    -> (Conversation -> IO ())
                    -> IO ()
connectToUnixSocket socketDirectory (NodeId nodeId) handler =
    initializeSocket allocator (handler . outboundSendRecv)
  where
    allocator :: IO Net.Socket
    allocator = do
        socket <- Net.socket Net.AF_UNIX Net.Stream Net.defaultProtocol
        let socketPath = socketDirectory </> show nodeId <.> "sock"
        Net.connect socket $ Net.SockAddrUnix socketPath
        return socket
