{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables        #-}

module Serokell.Node () where

import           Data.Binary                (Binary (..), decode, encode)
import           Data.Set                   as Set
import           Numeric.Natural            (Natural)
import           Serokell.Communication.IPC (Conversation (..), NodeId (..),
                                             connectToUnixSocket,
                                             listenUnixSocket)

import qualified Crypto.Sign.Ed25519        as ECC
import qualified Data.ByteString.Lazy       as LByteString
import           Data.Semigroup             ((<>))


-- Simple transaction data structure
data Tx = Tx { txFrom   :: NodeId
             , txTo     :: NodeId
             , txAmount :: Natural
             , txSig    :: LByteString.ByteString
             } deriving (Eq, Ord)

-- Instructions on how to convert it to bytestring
instance Binary Tx where
    put t = put (txFrom t) <> put (txTo t) <> put (txAmount t) <> put (txSig t)

    get = Tx <$> get <*> get <*> get <*> get

-- Node Environment, Immutable
data NodeEnvironment = NodeEnvironment
    { nodeEnvId      :: NodeId
    , nodeEnvPrivKey :: ECC.SecretKey
    , nodeEnvPubKey  :: ECC.PublicKey
    }

-- Mutable part of the node state
data NodeStorage = NodeStorage { nodeStorageTxs :: SetTx }
