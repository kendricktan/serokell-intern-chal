{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Serokell.Node where

import           Control.Concurrent         (MVar, forkIO, modifyMVar,
                                             modifyMVar_, newEmptyMVar, newMVar,
                                             putMVar, readMVar, takeMVar,
                                             threadDelay)
import           Control.Exception.Base     (IOException, catch, try)
import           Control.Monad              (forever)
import           Control.Monad.Trans        (lift, liftIO)
import           Data.Semigroup             ((<>))
import           Serokell.Communication.IPC (Conversation (..), NodeId (..),
                                             connectToUnixSocket,
                                             listenUnixSocket)
import           System.Directory           (createDirectory, doesFileExist,
                                             removeFile)
import           System.IO                  (hPrint, hPutStrLn, stderr)

import qualified Data.Binary                as Binary
import qualified Data.ByteString            as BString
import qualified Data.ByteString.Char8      as C8
import qualified Data.ByteString.Lazy       as LBString
import qualified Data.ByteString.Lazy.Char8 as LC8
import qualified Data.String                as String
import qualified Data.Text                  as Text
import qualified Data.Text.IO               as TextIO

import           Serokell.Helpers
import           Serokell.Types


runClient :: MTxState -> NodeEnvironment -> IO ()
runClient ref env = forever $ connectToUnixSocket (nodeSocketFolder env) (nodeId env) (clientHandler ref)

clientHandler :: MTxState -> Conversation -> IO ()
clientHandler ref c = loop
    where loop :: IO ()
          loop = do
            input <- TextIO.getLine
            txstate1 <- readMVar ref
            case String.words $ Text.unpack input of
              -- SUBMIT handles signing
              -- Don't wanna send the pk over sockets
              ("SUBMIT" : sk : pk : amnt : _) -> do
                  let tx = createTx sk pk amnt txstate1
                  let txbinary = Binary.encode tx
                  send c $ LBString.toStrict $ LC8.pack "SUBMIT " <> txbinary
              -- Every other case just pust it to the server
              (_ : _)       -> send c $ LBString.toStrict $ LC8.pack $ Text.unpack input
              []            -> send c $ LBString.toStrict $ LC8.pack " "
            resp <- recv c
            putStrLn $ C8.unpack resp

sends c s = send c $ LBString.toStrict $ LC8.pack s

runServer :: MTxState -> NodeEnvironment -> IO ()
runServer ref env =
    forever $ listenUnixSocket (nodeSocketFolder env) (nodeId env) ((True <$) . forkIO . serverHandler ref)
        where
            serverHandler :: MTxState -> Conversation -> IO ()
            serverHandler ref c = loop
                    where
                        loop :: IO ()
                        loop = do
                            input <- recv c
                            txstate1 <- readMVar ref

                            case String.words $ C8.unpack input of
                              ("SUBMIT" : _)             -> do
                                  let tx = Binary.decode (LBString.fromStrict $ BString.drop 7 input) :: Tx
                                  if verifyTx tx txstate1
                                     then do
                                         modifyMVar_ ref (return . applyTx tx)
                                         broadcastTx tx env
                                         sends c $ "1 " ++ txHash tx
                                     else hPutStrLn stderr "0"
                              ("QUERY" : txid : _)             ->
                                  case memberTx txid txstate1 of
                                    Just x  -> sends c $ "1 " <> txHash x
                                    Nothing -> sends c "0"

                              ("GETTX" : txno : _)            ->
                                  case getTxNo (read txno) (txs txstate1) of
                                    Just x  -> send c $ LBString.toStrict $ "1 " <> Binary.encode x
                                    Nothing -> sends c "0"

                              ("LATESTTX" : _)                -> sends c $ show $ getTxLength txstate1

                              ("BALANCE" : pubkey : _)        -> sends c $ show $ getUtxo pubkey txstate1

                              ("RECEIVE" : _)         -> do
                                  let tx = Binary.decode (LBString.fromStrict $ BString.drop 8 input) :: Tx
                                  if verifyTx tx txstate1
                                     then do modifyMVar_ ref (return . applyTx tx)
                                             putStrLn $ "1 " ++ txHash tx
                                     else sends c "0"

                              _                               -> sends c $ C8.unpack $ "Invalid command: " <> input

-- Syncs with other nodes
--
syncNode :: MVar () -> MTxState -> NodeId -> NodeEnvironment -> IO ()
syncNode refSync refState did env =
    -- Checks if the socket path exists
    doesFileExist (getSocketFilePath env) >>= (\x ->
        case x of
          -- If it exists then connect to it
          True  -> connectToUnixSocket (nodeSocketFolder env) (nodeId env) (syncHandler refState)
          -- Else traverses through the list of sockets and keeps
          -- trying to reconnect until it succeeds
          False -> case nodeCount env == unNodeId (nodeId env) of
                        True -> do putMVar refSync ()
                                   threadDelay (resyncTimeout env)
                        False -> reconnectToNewPeer
    )
    where reconnectToNewPeer :: IO ()
          reconnectToNewPeer = case newPeer env did of
                                   -- Tries to get a new peer (always starts from the lowest number)
                                   -- Updates the nodeenv state with the new successfully connected node
                                   Just e  -> syncNode refSync refState did e
                                   -- If it has traversed through every node and can't connect with them
                                   -- Then just assume there's no peers. Unlock the main thread
                                   -- And try to resync later
                                   Nothing -> do putMVar refSync ()
                                                 threadDelay (resyncTimeout env)

          -- Logic used to sync up existing transactions with a fresh slate
          syncHandler :: MTxState -> Conversation -> IO ()
          syncHandler refState c = loop
              where loop :: IO ()
                    loop = do
                        txstate0 <- readMVar refState

                        -- Gets TX no
                        sends c $ "GETTX " ++ (show . (+1) . getTxLength) txstate0
                        resp <- recv c

                        case String.words $ C8.unpack resp of
                          -- Once we finish syncing then we unblock the main thread
                          ["0"] -> do putMVar refSync ()
                                      threadDelay (resyncTimeout env)
                          -- Keep syncing
                          ("1" : _)  -> do
                              let tx = Binary.decode (LBString.fromStrict (BString.drop 2 resp)) :: Tx
                              if verifyTx tx txstate0
                                 then do modifyMVar_ refState (return . applyTx tx)
                                         putStrLn $ "SYCNED: " ++ txHash tx
                                 else reconnectToNewPeer
                          _ -> reconnectToNewPeer

                        syncNode refSync refState did env

runNode :: NodeEnvironment -> TxState -> IO ()
runNode env txstate = do
    -- References
    syncStatus <- newEmptyMVar
    ref <- newMVar txstate

    -- Start syncing
    putStrLn "[P] Syncing with nearby nodes..."
    forkIO (syncNode syncStatus ref (nodeId env) (initSyncEnv env))

    -- Wait for syncing thread to complete
    takeMVar syncStatus
    putStrLn "[C] Done syncing :-)"

    -- If there is another file remove it with the same
    -- socket, remove it
    createDirectory (nodeSocketFolder env) `catch` (const $ return () :: IOException -> IO ())
    removeFile (getSocketFilePath env) `catch` (const $ return () :: IOException -> IO ())

    -- Run daemon
    forkIO (runServer ref env)
    putStrLn "[C] Daemon started :-)"

    -- Mitigate the race condition of making the socket file
    -- sleep for 500 milliseconds
    threadDelay 500000

    -- Run CLI Interface
    putStrLn "=====> Quickstart <====="
    putStrLn ">> SUBMIT [priv key] [pub addr] [amount]"
    putStrLn ">> BALANCE [pub addr]"
    putStrLn ">> QUERY [txid]"
    runClient ref env
