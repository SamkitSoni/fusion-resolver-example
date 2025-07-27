module Main where

import Control.Monad.Freer.Extras.Log
import Data.Default (Default (..))
import FusionResolverCardano
import Plutus.Contract
import Plutus.PAB.Effects.Contract.Builtin (Builtin, SomeBuiltin (..), type (.\\))
import Plutus.PAB.Effects.Contract.Builtin qualified as Builtin
import Plutus.PAB.Simulator (SimulatorEffectHandlers)
import Plutus.PAB.Simulator qualified as Simulator
import Plutus.PAB.Webserver.Server qualified as PAB.Server

-- | Default escrow parameters for testing
defaultEscrowParams :: EscrowParams
defaultEscrowParams = EscrowParams
    { epResolver = "resolver_pubkey_hash"  -- This should be set to actual resolver pubkey hash
    , epMinDeposit = 1000000  -- 1 ADA minimum
    }

-- | Contract handlers for PAB
data ContractExample = FusionResolver
    deriving (Eq, Ord, Show, Generic)
    deriving anyclass (FromJSON, ToJSON)

instance Pretty ContractExample where
    pretty = viaShow

instance HasDefinitions ContractExample where
    getDefinitions = [FusionResolver]
    getSchema _ = Builtin.endpointsToSchemas @EscrowSchema
    getContract _ = SomeBuiltin $ endpoints defaultEscrowParams

handlers :: SimulatorEffectHandlers (Builtin ContractExample)
handlers = Simulator.mkSimulatorHandlers def def

-- | Main entry point
main :: IO ()
main = void $ Simulator.runSimulationWith handlers $ do
    Simulator.logString @(Builtin ContractExample) "Starting Fusion Resolver Cardano service"
    
    -- Start the PAB webserver
    shutdown <- PAB.Server.startServerDebug
    
    -- Keep the service running
    Simulator.waitUntilFinished
    shutdown
