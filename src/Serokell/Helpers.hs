{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Serokell.Helpers where

import           Control.Concurrent.Async    (mapConcurrently)
import           Control.Monad               (void)
import           Data.Maybe                  (fromMaybe)
import           Data.Semigroup              ((<>))
import           Numeric.Natural             (Natural)
import           Serokell.Communication.IPC  (Conversation (..), NodeId (..),
                                              connectToUnixSocket)
import           System.Directory            (doesFileExist)

import qualified Crypto.Hash.SHA256          as SHA256
import qualified Crypto.Sign.Ed25519         as ECC
import qualified Data.Binary                 as Binary
import qualified Data.ByteString             as BString
import qualified Data.ByteString.Base16      as B16
import qualified Data.ByteString.Base16.Lazy as B16L
import qualified Data.ByteString.Char8       as C8
import qualified Data.ByteString.Lazy        as LBString
import qualified Data.ByteString.Lazy.Char8  as LC8
import qualified Data.HashMap                as HM

import           Serokell.Types


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

-- Get hash of a tx (bytestring)
getTxHashBString :: Tx -> BString.ByteString
getTxHashBString (Tx _ p _ f t a) = SHA256.hash $ C8.pack $ concat $ show <$> [p, f, t, show a]

-- Get amount of Tx that has occured
getTxLength :: TxState -> Int
getTxLength = length . txs

-- Get hash of a tx string)
getTxHash :: Tx -> Hash
getTxHash = b16e . getTxHashBString

-- Get unspent monies
getUtxo :: Address -> TxState -> Natural
getUtxo pubkey txstate = fromMaybe 0 (HM.lookup pubkey (utxos txstate))

-- Get tx number [x]
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

-- Creates a tx given the secret key, the receiver, the amount, and the blockchain state
createTx :: String -> String -> String -> TxState -> Tx
createTx sk txto amnt txstate = Tx txhash txprevhash txsig txfr txto txamnt
    where txamnt = read amnt :: Natural
          txfr = getSKAddress (b16dsk sk)
          txprevhash = txHash . head $ txs txstate
          txhash = getTxHash (Tx "" txprevhash "" txfr txto txamnt)
          txsig = signTxHash txhash (b16dsk sk)

-- Applies the tx to the current state
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

-- Have we seen this newly received tx?
memberTx :: Hash -> TxState -> Maybe Tx
memberTx h txstate0 = HM.lookup h (txshm txstate0)

-- Tells every connected node about our tx
broadcastTx :: Tx -> NodeEnvironment -> IO ()
broadcastTx tx env = void $ mapConcurrently (broadcastSocket env cmd) (filter (curNode /=) [0..maxNode])
    where maxNode = nodeCount env
          curNode = unNodeId $ nodeId env
          txbinary = Binary.encode tx
          cmd = LC8.pack "RECEIVE " <> txbinary

broadcastSocket :: NodeEnvironment -> LBString.ByteString -> Int -> IO ()
broadcastSocket env bs i =
    doesFileExist (getSocketFilePath_ env i) >>= (\x ->
        case x of
          True  -> connectToUnixSocket (nodeSocketFolder env) (NodeId i) bHandler
          False -> return ()
    )
    where bHandler :: Conversation -> IO ()
          bHandler c = void $ send c $ LBString.toStrict bs

getSocketFilePath :: NodeEnvironment -> String
getSocketFilePath env = nodeSocketFolder env ++ "/" ++ (show . unNodeId . nodeId) env ++ ".sock"

getSocketFilePath_ :: NodeEnvironment -> Int -> String
getSocketFilePath_ env i = nodeSocketFolder env ++ "/" ++ show i ++ ".sock"

-- Tries to reconnect to another peer
newPeer :: NodeEnvironment -> NodeId -> Maybe NodeEnvironment
newPeer (NodeEnvironment (NodeId i) f c d s r) (NodeId did) = case fid of
                                          []      -> Nothing
                                          (x : _) -> Just $ NodeEnvironment (NodeId x) f c d s r
    where fid = filter (\a -> a > i && did /= a) [0..c]

initSyncEnv (NodeEnvironment (NodeId i) f c d s r) = NodeEnvironment (NodeId i') f c d s r
    where i' = if i == 0 then 1 else 0
