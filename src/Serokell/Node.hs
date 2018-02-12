{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Serokell.Node where

import           Control.Concurrent          (forkIO)
import           Control.Monad               (void)
import           Control.Monad.Reader        (ReaderT, runReaderT)
import           Control.Monad.State         (StateT, get, put, runStateT)
import           Control.Monad.Trans         (lift)
import           Control.Monad.Trans.Except  (ExceptT, runExceptT)
import           Data.Either                 (Either, either)
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
import qualified Data.HashMap                as HM
import qualified Data.Set                    as Set
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
    { nodeEnvId     :: NodeId
    , nodeEnvSecKey :: ECC.SecretKey
    , nodeEnvPubKey :: ECC.PublicKey
    }

-- Cache State (Keeping track of all the UTXOs)
data TxState = TxState { txs   :: Set.Set Tx
                       , txsm  :: HM.Map Hash Tx
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
getTxHash = C8.unpack . getTxHashBString

-- Sign a tx
signTx :: Tx -> ECC.SecretKey -> String
signTx tx sk = b16e $ ECC.sign sk (getTxHashBString tx)

-- Is the tx valid?
-- Checks if we have enough , then checks the signature
verifyTx :: Tx -> TxState -> Bool
verifyTx tx txstate = case HM.lookup (txFrom tx) (utxos txstate) of
                        Nothing -> False
                        Just x  -> case x < (txAmount tx) of
                                     False -> False
                                     True  -> ECC.verify (b16dpk (txFrom tx)) (getTxHashBString tx)

createTx :: String -> String -> String -> Tx
createTx sk txto amnt = Tx txhash txsig txfr txto (read amnt :: Natural)
    where txfr = getSKAddress (b16dsk sk)
          txtmp = Tx "" "" txfr txto (read amnt :: Natural)
          txhash = getTxHash txtmp
          txsig = signTx txtmp (b16dsk sk)

-- Lens maybe?
lift2 a = lift $ lift a
lift3 a = lift $ lift $ lift a

-- Runs client
runNode :: TxAction e s ()
runNode = undefined

nodeHandler :: Conversation -> TxMonad void
nodeHandler c = do
    input <- lift3 TextIO.getLine
    case Text.words input of
      ("SUBMIT" : privKey : pubKey : amnt) -> undefined
      ("QUERY": txhash)                    -> undefined
      ("BALANCE": pubKey)                  -> undefined
      _                                    -> do
          lift3 $ hPrint stderr $ "Invalid command: " <> input
          nodeHandler c
