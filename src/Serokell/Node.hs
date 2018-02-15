{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Serokell.Node where

import           Control.Concurrent          (forkIO, killThread, threadDelay)
import           Control.Concurrent.Async    (mapConcurrently)
import           Control.Monad               (void)
import           Control.Monad.Trans         (lift, liftIO)
import           Data.IORef                  (IORef, modifyIORef, newIORef,
                                              readIORef)
import           Data.Semigroup              ((<>))
import           GHC.Generics                (Generic)
import           Numeric.Natural             (Natural)
import           Serokell.Communication.IPC  (Conversation (..), NodeId (..),
                                              connectToUnixSocket,
                                              listenUnixSocket)
import           System.IO                   (hPrint, hPutStrLn, stderr)

import qualified Control.Concurrent.MVar     as MV
import qualified Crypto.Hash.SHA256          as SHA256
import qualified Crypto.Sign.Ed25519         as ECC
import qualified Data.Aeson                  as Aeson
import qualified Data.Binary                 as Binary
import qualified Data.ByteString             as BString
import qualified Data.ByteString.Base16      as B16
import qualified Data.ByteString.Base16.Lazy as B16L
import qualified Data.ByteString.Char8       as C8
import qualified Data.ByteString.Lazy        as LBString
import qualified Data.ByteString.Lazy.Char8  as LC8
import qualified Data.HashMap                as HM
import qualified Data.String                 as String
import qualified Data.Text                   as Text
import qualified Data.Text.IO                as TextIO

-- Blockchain Primitives
type Hash    = String
type Address = String

data Tx = Tx { txHash     :: Hash     -- Hash of the tx
             , txPrevHash :: Hash -- Hash of previous tx
             , txSig      :: Hash     -- Signature of the tx
             , txFrom     :: Address  -- Who are we sending it to
             , txTo       :: Address  -- Who is receiving it
             , txAmount   :: Natural  -- How much
             } deriving (Eq, Ord, Generic, Show)

-- Error Handling
data TxError = TxSubmitError
             | TxQueryError deriving Show

-- Node Environment, Immutable
data NodeEnvironment = NodeEnvironment
    { nodeEnvId         :: NodeId
    , nodeSocketFolder  :: String
    , nodeCount         :: Int
    , disconnectTimeout :: Int
    , stabilityTimeout  :: Int
    , resyncTimeout     :: Int
    }

-- TxState (keep track of the transactions and connected nodes)
data TxState = TxState { txs   :: [Tx]
                       , txshm :: HM.Map Hash Tx
                       , utxos :: HM.Map Address Natural
                       } deriving Show


-- Tx -> Binary
instance Binary.Binary Tx

instance Aeson.ToJSON NodeId

instance Aeson.ToJSON Tx

instance Aeson.FromJSON NodeId

instance Aeson.FromJSON Tx
-- Helper functions

-- Base16 encode
b16e x = C8.unpack $ B16.encode x

-- Base16 decode
b16d x = B16.decode $ C8.pack x

-- Base16 encode public keys
b16pk :: ECC.PublicKey -> String
b16pk k = b16e $ ECC.unPublicKey k

-- Base16 encode secret keys
b16sk :: ECC.SecretKey -> String
b16sk k = b16e $ ECC.unSecretKey k

-- Base16 decode public key
b16dpk :: String -> ECC.PublicKey
b16dpk s = ECC.PublicKey $ (fst . b16d) s

-- Base16 decode secret key
b16dsk :: String -> ECC.SecretKey
b16dsk s = ECC.SecretKey $ (fst . b16d) s

-- Get address (public key) from secret key
getSKAddress :: ECC.SecretKey -> String
getSKAddress = b16pk . ECC.toPublicKey

-- Get hash of a tx
getTxHashBString :: Tx -> BString.ByteString
getTxHashBString (Tx _ p _ f t a) = SHA256.hash $ C8.pack $ concat $ show <$> [p, f, t, show a]

getTxHash :: Tx -> Hash
getTxHash = b16e . getTxHashBString

getUtxo :: Address -> TxState -> Natural
getUtxo pubkey txstate = case HM.lookup pubkey (utxos txstate) of
                           Nothing -> 0
                           Just x  -> x

getTxNo :: Int -> [Tx] -> Maybe Tx
getTxNo _ []          = Nothing
getTxNo i x@(xh : xt) = if length x == i
                           then Just xh
                           else getTxNo i xt

-- Sign a tx
signTxHash :: Hash -> ECC.SecretKey -> String
signTxHash h sk = b16e $ ECC.sign sk (C8.pack h)

-- Is the tx valid?
-- Checks if we have enough , then checks the signature
verifyTx :: Tx -> TxState -> Bool
verifyTx tx txstate = case HM.lookup (txFrom tx) (utxos txstate) of
                        Nothing -> False
                        Just x  -> (x >= txAmount tx) && ECC.verify (b16dpk (txFrom tx)) (fst . b16d $ txSig tx)

createTx :: String -> String -> String -> TxState -> Tx
createTx sk txto amnt txstate = Tx txhash txprevhash txsig txfr txto txamnt
    where txamnt = read amnt :: Natural
          txfr = getSKAddress (b16dsk sk)
          txprevhash = txHash . head $ txs txstate
          txhash = getTxHash (Tx "" txprevhash "" txfr txto txamnt)
          txsig = signTxHash txhash (b16dsk sk)

applyTx :: Tx -> TxState -> TxState
applyTx tx txstate = TxState txs1 txshm1 utxos2
    where txs1 = tx : txs txstate
          -- Calculate new utxo values
          txfromutxo = getUtxo (txFrom tx) txstate - txAmount tx
          txtoutxo = getUtxo (txTo tx) txstate + txAmount tx
          -- Update local state
          txshm1 = HM.insert (txHash tx) tx (txshm txstate)
          utxos1 = HM.insert (txFrom tx) txfromutxo (utxos txstate)
          utxos2 = HM.insert (txTo tx) txtoutxo utxos1

memberTx :: Hash -> TxState -> Maybe Tx
memberTx h txstate0 = HM.lookup h (txshm txstate0)

broadcastTx :: Tx -> NodeEnvironment -> IO ()
broadcastTx tx env = void $ mapConcurrently (broadcastSocket cmd (nodeSocketFolder env)) (filter ((/=) curNode) [0..maxNode])
    where maxNode = nodeCount env
          curNode = unNodeId $ nodeEnvId env
          txbinary = Binary.encode tx
          cmd = LC8.pack "RECEIVE " <> txbinary

broadcastSocket :: LBString.ByteString -> String -> Int -> IO ()
broadcastSocket bs f i = connectToUnixSocket f (NodeId i) bHandler
    where bHandler :: Conversation -> IO ()
          bHandler c = void $ send c $ LBString.toStrict $ bs


-- Node functions
runClient :: NodeEnvironment -> IO ()
runClient env = connectToUnixSocket (nodeSocketFolder env) (nodeEnvId env) clientHandler

clientHandler :: Conversation -> IO ()
clientHandler c = loop
    where loop :: IO ()
          loop = do
            input <- TextIO.getLine
            void . forkIO $ do send c $ LBString.toStrict $ LC8.pack $ Text.unpack input
                               resp <- recv c
                               putStrLn $ C8.unpack resp
            loop

sends c s = send c $ LBString.toStrict $ LC8.pack $ s

runServer :: NodeEnvironment -> TxState -> IO ()
runServer env txstate0 = do
    ref <- newIORef txstate0
    listenUnixSocket (nodeSocketFolder env) (nodeEnvId env) ((True <$) . forkIO . serverHandler ref)
        where
            serverHandler :: IORef TxState -> Conversation -> IO ()
            serverHandler ref c = loop
                    where
                        loop :: IO ()
                        loop = do
                            input <- recv c
                            txstate1 <- readIORef ref

                            case String.words $ C8.unpack input of
                              ("SUBMIT" : sk : pk : amnt : _) -> do
                                  let tx = createTx sk pk amnt txstate1
                                  if verifyTx tx txstate1
                                     then do
                                         modifyIORef ref (applyTx tx)
                                         broadcastTx tx env
                                         sends c $ "1 " ++ txHash tx
                                     else hPutStrLn stderr "0"

                              ("QUERY": txid : _)             -> do
                                  case memberTx txid txstate1 of
                                    Just x  -> sends c $ "1 " <> txHash x
                                    Nothing -> sends c "0"

                              ("BALANCE" : pubkey : _)        -> do
                                  sends c $ show $ getUtxo pubkey txstate1

                              ("RECEIVE" : txbinary : _)         -> do
                                  let tx = Binary.decode (LBString.fromStrict $ C8.pack txbinary) :: Tx
                                  if verifyTx tx txstate1
                                     then do modifyIORef ref (applyTx tx)
                                             sends c $ "1 " ++ txHash tx
                                     else sends c $ "0"

                              _                               -> do
                                  sends c $ C8.unpack $ "Invalid command: " <> input

                            loop

runNode :: NodeEnvironment -> TxState -> IO ()
runNode env txstate = do
    threadId <- forkIO (runServer env txstate)
    -- Mitigate the race condition of making the socket file
    -- sleep for 500 milliseconds
    threadDelay 500000
    runClient env


initialEnv = NodeEnvironment (NodeId 0) "sockets" 1 0 0 0

initialEnv2 = NodeEnvironment (NodeId 1) "sockets" 1 0 0 0

gh = "0000000000000000000000000000000000000000000000000000000000000000"

genesisTx = Tx gh gh gh gh gh 0

initialState = TxState [genesisTx] (HM.empty :: HM.Map Hash Tx) (HM.empty :: HM.Map Address Natural)
