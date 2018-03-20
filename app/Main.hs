module Main where

import           Numeric.Natural
import           Serokell.Communication.IPC
import           Serokell.Node
import           Serokell.Types
import           System.Environment

import qualified Data.HashMap               as HM



main :: IO ()
main = do
    [nodeid, nodecount, socketdir, dtimeout, stimeout, rtimeout, dfile] <- getArgs

    -- Initial Env
    let initialEnv = NodeEnvironment (NodeId (read nodeid)) socketdir (read nodecount) (read dtimeout) (read stimeout) (read rtimeout)

    -- Initial State
    -- Set initial utxo state via distribution file
    distUtxo <- readFile dfile
    let initialUtxos = foldr (\b m -> HM.insert (head b) (read (b !! 1)) m) (HM.empty :: HM.Map Address Natural) (words <$> lines distUtxo)
    let gh = "0000000000000000000000000000000000000000000000000000000000000000"
    let genesisTx = Tx gh gh gh gh gh 0
    let initialState = TxState [genesisTx] mempty initialUtxos

    runNode initialEnv initialState
