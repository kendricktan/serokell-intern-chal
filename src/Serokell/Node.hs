{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Serokell.Node where

import           Control.Concurrent         (forkIO)
import           Control.Monad              (void)
import           Control.Monad.Reader       (ReaderT, runReaderT)
import           Control.Monad.State        (StateT, get, put, runStateT)
import           Control.Monad.Trans        (lift)
import           Control.Monad.Trans.Except (ExceptT, runExceptT)
import           Data.ByteString.Char8      (pack)
import           Data.ByteString.Lazy       (ByteString, fromStrict, toStrict)
import           Data.ByteString.Lazy.UTF8  (fromString, toString)
import           Data.Either                (Either, either)
import           Data.Semigroup             ((<>))
import           Data.Set                   (Set, insert)
import           GHC.Generics               (Generic)
import           Numeric.Natural            (Natural)
import           Serokell.Communication.IPC (Conversation (..), NodeId (..),
                                             connectToUnixSocket,
                                             listenUnixSocket)
import           System.IO                  (hPrint, hPutStrLn, stderr)

import qualified Crypto.Hash.SHA256         as SHA256
import qualified Crypto.Sign.Ed25519        as ECC
import qualified Data.Aeson                 as Aeson
import qualified Data.Binary                as Binary
import qualified Data.HashMap               as HM
import qualified Data.Text                  as Text
import qualified Data.Text.IO               as TextIO

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
data Tx = Tx { txHash   :: Maybe String
             , txIn     :: String
             , txOut    :: String
             , txAmount :: Natural
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
    { nodeEnvId      :: NodeId
    , nodeEnvPrivKey :: ECC.SecretKey
    , nodeEnvPubKey  :: ECC.PublicKey
    }

-- Cache State (Keeping track of all the UTXOs)
data TxState = TxState { txs    :: Set Tx
                       , txsMap :: HM.Map String Tx
                       , utxos  :: HM.Map String Natural
                       } deriving Show

-- DRY
type TxMonad a = TxAction NodeEnvironment TxState a

-- Tx helper functions
createTx :: String -> String -> String -> Either TxError Tx
createTx privKey pubKey amnt = undefined

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
      ("QUERY": _)   -> undefined
      _              -> do
          lift3 $ hPrint stderr $ "Invalid command: " <> input
          nodeHandler c
