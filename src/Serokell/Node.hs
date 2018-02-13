{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Serokell.Node where

import           Control.Concurrent          (forkIO)
import           Control.Monad               (void)
import           Control.Monad.Reader        (ReaderT, ask, runReaderT)
import           Control.Monad.State         (StateT, get, put, runStateT)
import           Control.Monad.Trans         (lift, liftIO)
import           Control.Monad.Trans.Except  (ExceptT, runExceptT)
import           Data.Either                 (Either, either)
import           Data.IORef                  (IORef, modifyIORef, newIORef,
                                              readIORef)
import           Data.Semigroup              ((<>))
import           GHC.Generics                (Generic)
import           Numeric.Natural             (Natural)
import           Serokell.Communication.IPC  (Conversation (..), NodeId (..),
                                              connectToUnixSocket,
                                              listenUnixSocket)
import           System.IO                   (hPrint, hPutStrLn, stderr)

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

-- TxAction Monad
-- e = Environment
-- s = State
type TxAction e s = ExceptT TxError (ReaderT e (StateT s IO))

runTxAction :: TxAction r s a -> r -> s -> IO (Either TxError a, s)
runTxAction action env0 = runStateT (runReaderT (runExceptT action) env0)

evalTxAction :: TxAction r s a -> r -> s -> IO (Maybe a)
evalTxAction action env0 state0 = do
    (result, state1) <- runTxAction action env0 state0
    either (\x -> hPrint stderr x >> return Nothing) (return . Just) result


-- Blockchain Primitives
type Hash    = String
type Address = String

data Tx = Tx { txHash   :: Hash     -- Hash of the tx
             , txSig    :: Hash     -- Signature of the tx
             , txFrom   :: Address  -- Who are we sending it to
             , txTo     :: Address  -- Who is receiving it
             , txAmount :: Natural  -- How much
             } deriving (Eq, Ord, Generic, Show)

-- Error Handling
data TxError = TxSubmitError
             | TxQueryError deriving Show

-- Tx -> Binary
instance Binary.Binary Tx

instance Aeson.ToJSON NodeId

instance Aeson.ToJSON Tx

instance Aeson.FromJSON NodeId

instance Aeson.FromJSON Tx

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
                       , utxos :: HM.Map Address Natural
                       } deriving Show

-- DRY
type TxMonad a = TxAction NodeEnvironment TxState a

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
getTxHashBString (Tx _ _ f t a) = SHA256.hash $ C8.pack $ concat $ show <$> [f, t, show a]

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
signTx :: Tx -> ECC.SecretKey -> String
signTx tx sk = b16e $ ECC.sign sk (getTxHashBString tx)

-- Is the tx valid?
-- Checks if we have enough , then checks the signature
verifyTx :: Tx -> TxState -> Bool
verifyTx tx txstate = case HM.lookup (txFrom tx) (utxos txstate) of
                        Nothing -> False
                        Just x  -> (x >= txAmount tx) && ECC.verify (b16dpk (txFrom tx)) (getTxHashBString tx)

createTx :: String -> String -> String -> Tx
createTx sk txto amnt = Tx txhash txsig txfr txto txamnt
    where txamnt = read amnt :: Natural
          txfr = getSKAddress (b16dsk sk)
          txtmp = Tx "" "" txfr txto txamnt
          txhash = getTxHash txtmp
          txsig = signTx txtmp (b16dsk sk)

submitTx :: Tx -> TxMonad ()
submitTx tx = do
    txstate0 <- lift2 get
    let txs1 = tx : txs txstate0
        -- Calculate new utxo values
    let txfromutxo = getUtxo (txFrom tx) txstate0 - txAmount tx
    let txtoutxo = getUtxo (txTo tx) txstate0 + txAmount tx
        -- Update
    let utxos1 = HM.insert (txFrom tx) txfromutxo (utxos txstate0)
    let utxos2 = HM.insert (txTo tx) txtoutxo utxos1
    put (TxState txs1 utxos2)

-- Lens maybe?
lift2 a = lift $ lift a
lift3 a = lift $ lift $ lift a

-- Runs client
runClientNode :: NodeEnvironment -> TxState -> IO ()
runClientNode env txstate = connectToUnixSocket (nodeSocketFolder env) (nodeEnvId env) (clientHandler env txstate)

clientHandler :: NodeEnvironment -> TxState -> Conversation -> IO ()
clientHandler r s c = loop
    where loop :: IO void
          loop = do
            input <- TextIO.getLine
            void . forkIO $ send c $ LBString.toStrict $ LC8.pack $ Text.unpack input
            loop

runServerNode :: NodeEnvironment -> TxState -> IO ()
runServerNode env txstate = do
    ref <- newIORef 0
    listenUnixSocket (nodeSocketFolder env) (nodeEnvId env) ((True <$) . forkIO . serverHandler env txstate ref)

serverHandler :: NodeEnvironment -> TxState -> IORef Int -> Conversation -> IO ()
serverHandler env txstate ref c = void $ runTxAction loop env txstate
    where loop :: TxMonad void
          loop = do
              input <- lift3 $ recv c

              case String.words $ C8.unpack input of
                ("SUBMIT" : sk : pk : amnt : _) -> do
                    let tx = createTx sk pk amnt
                    txstate0 <- lift2 get
                    if verifyTx tx txstate0
                       then do
                           submitTx tx -- TODO: broadcast this to all known nodes
                           lift3 $ void . forkIO $ do
                               send c $ LBString.toStrict $ Binary.encode tx
                               putStrLn $ "1 " ++ txHash tx
                       else lift3 $ hPutStrLn stderr "0"
                    loop

                ("QUERY": txno : _)             -> do
                    txstate0 <- lift2 get
                    case getTxNo (read txno :: Int) (txs txstate0) of
                      Just x  -> lift3 $ putStrLn $ "1 " <> txHash x
                      Nothing -> lift3 $ putStrLn "0"
                    loop

                ("BALANCE" : pubkey : _)        -> do
                    txstate0 <- lift2 get
                    lift3 $ putStrLn $ show $ getUtxo pubkey txstate0
                    loop

                _                               -> do
                    lift3 $ hPrint stderr $ "Invalid command: " <> input
                    loop

runNode :: NodeEnvironment -> TxState -> IO ()
runNode env txstate = do
    void $ forkIO (runServerNode env txstate)
    runClientNode env txstate


initialEnv :: NodeEnvironment
initialEnv = NodeEnvironment (NodeId 0) "sockets" 0 0 0 0

initialState :: TxState
initialState = TxState [] (HM.empty :: HM.Map Address Natural)
