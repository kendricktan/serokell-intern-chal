{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Serokell.Types where

import           Control.Concurrent         (MVar)
import           GHC.Generics               (Generic)
import           Numeric.Natural            (Natural)
import           Serokell.Communication.IPC (Conversation (..), NodeId (..))

import qualified Control.Concurrent.MVar    as MV
import qualified Data.Aeson                 as Aeson
import qualified Data.Binary                as Binary
import qualified Data.HashMap               as HM
import qualified Data.String                as String

---- Blockchain Primitives
---------------------------------

-- Aliases
type Hash    = String
type Address = String
type MTxState = MVar TxState

-- Data structure for a basic transaction
data Tx = Tx { txHash     :: Hash     -- Hash of the tx
             , txPrevHash :: Hash -- Hash of previous tx
             , txSig      :: Hash     -- Signature of the tx
             , txFrom     :: Address  -- Who are we sending it to
             , txTo       :: Address  -- Who is receiving it
             , txAmount   :: Natural  -- How much
             } deriving (Eq, Ord, Generic, Show)

-- Node Environment, Immutable
data NodeEnvironment = NodeEnvironment
    { nodeId            :: NodeId
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
