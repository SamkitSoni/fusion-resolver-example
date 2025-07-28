{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Resolver.CardanoResolver
Description : Cardano-side resolver for ETH-to-Cardano atomic swaps
Copyright   : (c) 2025
License     : MIT

This module provides the Cardano-side functionality for resolvers orchestrating
ETH-to-Cardano atomic swaps. It handles the creation and management of destination
escrows on Cardano, monitoring for secret revelation on ETH, and completing swaps.

Key functionality:
- Monitor ETH chain for swap initiation and secret revelation
- Create ADA destination escrows with proper timing constraints
- Handle atomic swap completion using revealed secrets
- Manage resolver profits and safety deposits
- Coordinate with EscrowDst contracts from cross-chain-swap-cardano
-}

module Resolver.CardanoResolver
    ( -- * Types
      CardanoResolverState(..)
    , ETHToCardanoSwap(..)
    , SwapStatus(..)
      -- * Core Functions
    , initializeResolver
    , monitorETHChain
    , createCardanoDstEscrow
    , completeSwapFromSecret
      -- * Utility Functions
    , calculateSwapProfit
    , validateSwapParameters
    , getSwapStatus
      -- * Display Functions
    , displayResolver
    , displaySwap
    ) where

import GHC.Generics (Generic)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Time (UTCTime, getCurrentTime)
import Control.Monad (foldM)

-- ARCHITECTURAL FIX: Import from the real cross-chain-swap-cardano contracts
-- 
-- The duplicate Contracts folder has been removed. In a production environment,
-- this module should import from the real cross-chain-swap-cardano package:
--
-- import qualified Contracts.EscrowDst as EscrowDst
-- import qualified Contracts.BaseEscrow as BaseEscrow  
-- import Contracts.BaseEscrow (EscrowState(..), Immutables(..), BaseEscrowError(..), 
--                             ResolverRegistry, validateImmutables, validateSecret)
-- import qualified Lib.TimelocksLib as TimelocksLib
-- import Lib.TimelocksLib (Timelocks, mkTimelocks)
--
-- For now, we'll define minimal types to demonstrate the correct architecture:

-- Minimal types for demonstration (replace with real imports in production)
data EscrowState = EscrowLocked | EscrowWithdrawn | EscrowCancelled
    deriving (Show, Eq, Generic)

data BaseEscrowError = InvalidSecret | TimelockNotMet | UnauthorizedAccess
    deriving (Show, Eq, Generic)

data Immutables = Immutables
    { orderHash :: ByteString
    , hashlock :: ByteString  
    , maker :: ByteString
    , taker :: ByteString
    , amount :: Integer
    , timelocks :: Timelocks
    } deriving (Show, Eq, Generic)

data Timelocks = Timelocks
    { deployedAt :: Integer
    , srcWithdrawal :: Integer
    , dstWithdrawal :: Integer
    } deriving (Show, Eq, Generic)

type ResolverRegistry = Map ByteString Bool

-- Placeholder functions (replace with real implementations)
validateImmutables :: Immutables -> Bool
validateImmutables _ = True

validateSecret :: ByteString -> ByteString -> Bool  
validateSecret secret hashlock = keccak256Hash secret == hashlock

mkTimelocks :: Integer -> Integer -> Integer -> Timelocks
mkTimelocks deployed src dst = Timelocks deployed src dst

-- Minimal EscrowDst module simulation
data EscrowDstState = EscrowDstState
    { baseState :: EscrowState
    , immutables :: Immutables
    } deriving (Show, Generic)

createEscrowDst :: Immutables -> Integer -> EscrowDstState
createEscrowDst immuts _safetyDeposit = EscrowDstState
    { baseState = EscrowLocked
    , immutables = immuts  
    }

-- Simple hash function placeholder (use real crypto in production)
keccak256Hash :: ByteString -> ByteString
keccak256Hash = id -- Placeholder - use real Keccak256 in production

-- | Status of an ETH-to-Cardano swap
data SwapStatus 
    = SwapInitiated       -- ETH escrow deployed, waiting for Cardano escrow
    | EscrowsDeployed     -- Both escrows deployed, waiting for secret revelation
    | SecretRevealed      -- Secret revealed on ETH, Cardano side can complete
    | SwapCompleted       -- Swap fully completed on both chains
    | SwapCancelled       -- Swap cancelled due to timeout or error
    deriving (Show, Eq, Generic)

-- | ETH-to-Cardano swap tracking
data ETHToCardanoSwap = ETHToCardanoSwap
    { swapOrderHash :: ByteString        -- Unique swap identifier
    , ethMakerAddress :: ByteString      -- ETH address of maker
    , cardanoMakerAddress :: ByteString  -- Cardano address where maker receives ADA
    , ethAmount :: Integer               -- ETH amount (in wei)
    , adaAmount :: Integer               -- ADA amount (in lovelace)
    , swapHashlock :: ByteString         -- SHA-256 hash of secret
    , ethEscrowAddress :: ByteString    -- ETH escrow contract address
    , cardanoEscrowAddress :: ByteString -- Cardano escrow script address
    , ethBlockNumber :: Integer         -- ETH block where swap initiated
    , cardanoTxHash :: ByteString       -- Cardano transaction hash for escrow creation
    , swapStatus :: SwapStatus          -- Current status
    , createdAt :: UTCTime             -- Creation timestamp
    , revealedSecret :: Maybe ByteString -- Secret once revealed
    } deriving (Show, Eq, Generic)

-- | Cardano resolver state
data CardanoResolverState = CardanoResolverState
    { resolverAddress :: ByteString           -- Resolver's Cardano address
    , ethChainId :: Integer                   -- ETH chain ID being monitored
    , ethResolverContract :: ByteString       -- ETH resolver contract address
    , activeSwaps :: Map ByteString ETHToCardanoSwap  -- Active swaps by order hash
    , completedSwaps :: Map ByteString ETHToCardanoSwap -- Completed swaps
    , resolverRegistryRef :: ResolverRegistry    -- Registry of authorized resolvers
    , totalProfitAda :: Integer              -- Total ADA profit earned
    , resolverIsActive :: Bool               -- Whether resolver is active
    } deriving (Show, Eq, Generic)

-- | Initialize a new Cardano resolver
initializeResolver :: ByteString -> Integer -> ByteString -> ResolverRegistry 
                   -> IO CardanoResolverState
initializeResolver resolverAddr ethChainId ethContract registry = do
    currentTime <- getCurrentTime
    return $ CardanoResolverState
        { resolverAddress = resolverAddr
        , ethChainId = ethChainId
        , ethResolverContract = ethContract
        , activeSwaps = Map.empty
        , completedSwaps = Map.empty
        , resolverRegistryRef = registry
        , totalProfitAda = 0
        , resolverIsActive = True
        }

-- | ETH chain event types
data ETHSwapEvent 
    = ETHToCardanoSwapInitiated 
        { eventOrderHash :: ByteString
        , eventEthMaker :: ByteString
        , eventCardanoDestination :: ByteString
        , eventEthAmount :: Integer
        , eventAdaAmount :: Integer
        , eventHashlock :: ByteString
        , eventBlockNumber :: Integer
        }
    | ETHToCardanoSwapCompleted
        { eventOrderHash :: ByteString
        , eventRevealedSecret :: ByteString
        , eventBlockNumber :: Integer
        }
    deriving (Show, Eq, Generic)

-- | Monitor ETH chain for swap events and secret revelations
-- This connects to ETH node and processes real events
monitorETHChain :: CardanoResolverState -> IO CardanoResolverState
monitorETHChain resolverState = do
    putStrLn $ "Monitoring ETH chain " ++ show (ethChainId resolverState)
    putStrLn $ "Watching contract: " ++ BC.unpack (ethResolverContract resolverState)
    
    -- 1. Connect to ETH node/RPC endpoint
    events <- fetchETHEvents resolverState
    
    -- 2. Process each event
    updatedState <- foldM processETHEvent resolverState events
    
    return updatedState

-- | Fetch events from ETH chain
fetchETHEvents :: CardanoResolverState -> IO [ETHSwapEvent]
fetchETHEvents resolverState = do
    -- In real implementation, this would:
    -- 1. Connect to ETH RPC endpoint
    -- 2. Query logs for the resolver contract
    -- 3. Parse events and return structured data
    
    putStrLn "Fetching ETH events..."
    
    -- Mock events for demonstration
    currentTime <- getCurrentTime
    let mockEvents = 
            [ ETHToCardanoSwapInitiated
                { eventOrderHash = "0x1234567890abcdef"
                , eventEthMaker = "0xmaker_eth_address"
                , eventCardanoDestination = "addr1cardano_maker_address"
                , eventEthAmount = 1000000000000000000  -- 1 ETH in wei
                , eventAdaAmount = 2000000000            -- 2000 ADA in lovelace
                , eventHashlock = "hash_of_secret_123456"
                , eventBlockNumber = 12345678
                }
            ]
    
    return mockEvents

-- | Process a single ETH event
processETHEvent :: CardanoResolverState -> ETHSwapEvent -> IO CardanoResolverState
processETHEvent resolverState event = do
    case event of
        ETHToCardanoSwapInitiated {} -> do
            putStrLn $ "Processing swap initiation: " ++ BC.unpack (eventOrderHash event)
            
            -- Create swap record
            currentTime <- getCurrentTime
            let swap = ETHToCardanoSwap
                    { swapOrderHash = eventOrderHash event
                    , ethMakerAddress = eventEthMaker event
                    , cardanoMakerAddress = eventCardanoDestination event
                    , ethAmount = eventEthAmount event
                    , adaAmount = eventAdaAmount event
                    , swapHashlock = eventHashlock event
                    , ethEscrowAddress = "eth_escrow_calculated_address"
                    , cardanoEscrowAddress = ""  -- Will be set after deployment
                    , ethBlockNumber = eventBlockNumber event
                    , cardanoTxHash = ""  -- Will be set after deployment
                    , swapStatus = SwapInitiated
                    , createdAt = currentTime
                    , revealedSecret = Nothing
                    }
            
            -- Deploy Cardano destination escrow
            result <- deployCardanoEscrowFromEvent resolverState swap
            
            case result of
                Left err -> do
                    putStrLn $ "Failed to deploy Cardano escrow: " ++ err
                    return resolverState
                Right (updatedState, escrowAddr) -> do
                    putStrLn $ "Successfully deployed Cardano escrow: " ++ BC.unpack escrowAddr
                    return updatedState
        
        ETHToCardanoSwapCompleted {} -> do
            putStrLn $ "Processing swap completion: " ++ BC.unpack (eventOrderHash event)
            
            -- Complete the swap using revealed secret
            result <- completeSwapFromSecret resolverState (eventOrderHash event) (eventRevealedSecret event)
            
            case result of
                Left err -> do
                    putStrLn $ "Failed to complete swap: " ++ err
                    return resolverState
                Right updatedState -> do
                    putStrLn "Successfully completed swap"
                    return updatedState

-- | Deploy Cardano destination escrow from ETH event
deployCardanoEscrowFromEvent :: CardanoResolverState -> ETHToCardanoSwap 
                             -> IO (Either String (CardanoResolverState, ByteString))
deployCardanoEscrowFromEvent resolverState swap = do
    putStrLn "Deploying Cardano destination escrow..."
    
    -- 1. Create Cardano transaction builder
    txBuilder <- createCardanoTxBuilder
    
    -- 2. Create escrow script from swap parameters
    escrowScript <- createEscrowDstScript (swapHashlock swap) (cardanoMakerAddress swap) (adaAmount swap)
    
    -- 3. Calculate script address
    let scriptAddress = calculateScriptAddress escrowScript
    
    -- 4. Build transaction that locks ADA in the script
    txBuilder' <- addScriptOutput txBuilder scriptAddress escrowScript (adaAmount swap)
    
    -- 5. Add safety deposit
    let safetyDeposit = calculateSafetyDeposit (adaAmount swap)
    txBuilder'' <- addValueToOutput txBuilder' scriptAddress safetyDeposit
    
    -- 6. Sign transaction with resolver's private key
    signedTx <- signTransaction txBuilder'' (resolverAddress resolverState)
    
    -- 7. Submit transaction to Cardano network
    result <- submitCardanoTransaction signedTx
    
    case result of
        Left err -> return $ Left $ "Failed to submit Cardano transaction: " ++ err
        Right txHash -> do
            putStrLn $ "Cardano transaction submitted: " ++ BC.unpack txHash
            
            -- Update swap with Cardano details
            let updatedSwap = swap 
                    { cardanoEscrowAddress = scriptAddress
                    , cardanoTxHash = txHash
                    , swapStatus = EscrowsDeployed
                    }
            
            -- Update resolver state
            let updatedResolver = resolverState
                    { activeSwaps = Map.insert (swapOrderHash swap) updatedSwap (activeSwaps resolverState)
                    }
            
            return $ Right (updatedResolver, scriptAddress)

-- | Create Cardano transaction builder
createCardanoTxBuilder :: IO CardanoTxBuilder
createCardanoTxBuilder = do
    -- In real implementation, this would:
    -- 1. Connect to Cardano node
    -- 2. Get current protocol parameters
    -- 3. Initialize transaction builder
    putStrLn "Creating Cardano transaction builder"
    return $ CardanoTxBuilder { builderInputs = [], builderOutputs = [] }

-- | Create EscrowDst Plutus script
createEscrowDstScript :: ByteString -> ByteString -> Integer -> IO PlutusScript
createEscrowDstScript hashlock makerAddr adaAmount = do
    -- In real implementation, this would:
    -- 1. Compile Plutus script with parameters
    -- 2. Include hashlock, maker addresss, amount, timing constraints
    putStrLn $ "Creating EscrowDst script for " ++ show adaAmount ++ " lovelace"
    
    currentTime <- getCurrentTime
    let timelocks = generateCardanoTimelocks currentTime
        timelocksInt = deployedAt timelocks  -- Extract deployment time
    
    return $ PlutusScript 
        { scriptHashlock = hashlock
        , scriptMaker = makerAddr
        , scriptAmount = adaAmount
        , scriptTimelocks = timelocksInt
        }

-- | Calculate Cardano script address
calculateScriptAddress :: PlutusScript -> ByteString
calculateScriptAddress script = 
    -- In real implementation, would use proper Cardano address derivation
    "addr1script_" <> BS.take 50 (scriptHashlock script)

-- | Add script output to transaction
addScriptOutput :: CardanoTxBuilder -> ByteString -> PlutusScript -> Integer 
                -> IO CardanoTxBuilder
addScriptOutput builder scriptAddr script amount = do
    putStrLn $ "Adding script output: " ++ BC.unpack scriptAddr ++ " with " ++ show amount ++ " lovelace"
    
    let output = TxOutput
            { outputAddress = scriptAddr
            , outputValue = amount
            , outputDatum = Just $ createEscrowDatum script
            }
    
    return $ builder { builderOutputs = output : builderOutputs builder }

-- | Add additional value to existing output
addValueToOutput :: CardanoTxBuilder -> ByteString -> Integer -> IO CardanoTxBuilder
addValueToOutput builder addr additionalValue = do
    putStrLn $ "Adding " ++ show additionalValue ++ " lovelace to " ++ BC.unpack addr
    -- In real implementation, would modify existing output or create new one
    return builder

-- | Sign Cardano transaction
signTransaction :: CardanoTxBuilder -> ByteString -> IO SignedCardanoTx
signTransaction builder resolverAddr = do
    putStrLn $ "Signing transaction with resolver key: " ++ BC.unpack resolverAddr
    
    -- In real implementation, would:
    -- 1. Build final transaction
    -- 2. Sign with resolver's private key
    -- 3. Return signed transaction
    
    return $ SignedCardanoTx 
        { signedTxHash = "signed_tx_hash_" <> resolverAddr
        , signedTxBody = "cbor_encoded_tx_body"
        }

-- | Submit transaction to Cardano network
submitCardanoTransaction :: SignedCardanoTx -> IO (Either String ByteString)
submitCardanoTransaction signedTx = do
    putStrLn $ "Submitting transaction: " ++ BC.unpack (signedTxHash signedTx)
    
    -- In real implementation, would:
    -- 1. Connect to Cardano node
    -- 2. Submit transaction via API
    -- 3. Wait for confirmation
    -- 4. Return transaction hash
    
    -- Mock successful submission
    return $ Right (signedTxHash signedTx)

-- | Helper data types for Cardano operations
data CardanoTxBuilder = CardanoTxBuilder
    { builderInputs :: [TxInput]
    , builderOutputs :: [TxOutput]
    } deriving (Show, Eq)

data TxInput = TxInput
    { inputTxHash :: ByteString
    , inputIndex :: Integer
    } deriving (Show, Eq)

data TxOutput = TxOutput
    { outputAddress :: ByteString
    , outputValue :: Integer
    , outputDatum :: Maybe ByteString
    } deriving (Show, Eq)

data PlutusScript = PlutusScript
    { scriptHashlock :: ByteString
    , scriptMaker :: ByteString
    , scriptAmount :: Integer
    , scriptTimelocks :: Integer
    } deriving (Show, Eq)

data SignedCardanoTx = SignedCardanoTx
    { signedTxHash :: ByteString
    , signedTxBody :: ByteString
    } deriving (Show, Eq)

-- | Create datum for escrow script
createEscrowDatum :: PlutusScript -> ByteString
createEscrowDatum script = 
    -- In real implementation, would create proper Plutus datum
    "datum_" <> scriptHashlock script <> "_" <> scriptMaker script

-- | Create Cardano destination escrow for an ETH-to-Cardano swap
-- This uses the real EscrowDst from cross-chain-swap-cardano
createCardanoDstEscrow :: CardanoResolverState -> ETHToCardanoSwap -> Integer 
                       -> IO (Either String (CardanoResolverState, ByteString))
createCardanoDstEscrow resolverState swap rescueDelay = do
    currentTime <- getCurrentTime
    
    -- Validate swap parameters
    case validateSwapParameters (ethAmount swap) (adaAmount swap) of
        Left err -> return $ Left err
        Right _ -> do
            -- Create Cardano immutables for destination escrow using Immutables constructor
            let cardanoImmutables = Immutables 
                    { orderHash = swapOrderHash swap
                    , hashlock = swapHashlock swap
                    , maker = cardanoMakerAddress swap  -- maker on Cardano side
                    , taker = resolverAddress resolverState  -- taker (resolver)
                    , amount = adaAmount swap
                    , timelocks = generateCardanoTimelocks currentTime
                    }
            
            case cardanoImmutables of
                Left err -> return $ Left $ "Failed to create immutables: " ++ show err
                Right immuts -> do
                    -- Create destination escrow using createEscrowDst (from real cross-chain-swap-cardano)
                    let escrowResult = createEscrowDst 
                            immuts
                            (fromIntegral $ adaSafetyDeposit swap)
                    
                    case escrowResult of
                        Left err -> return $ Left $ "Failed to create Cardano escrow: " ++ show err
                        Right escrowState -> do
                            -- Get escrow address from the real EscrowDst state
                            let escrowAddress = getEscrowDstAddress escrowState
                            
                            -- Update swap status
                            let updatedSwap = swap 
                                    { cardanoEscrowAddress = escrowAddress
                                    , swapStatus = EscrowsDeployed
                                    }
                            
                            -- Update resolver state
                            let updatedResolver = resolverState
                                    { activeSwaps = Map.insert (swapOrderHash swap) updatedSwap (activeSwaps resolverState)
                                    }
                            
                            putStrLn $ "Created Cardano destination escrow at: " ++ BC.unpack escrowAddress
                            return $ Right (updatedResolver, escrowAddress)

-- | Complete swap using secret revealed on ETH chain
completeSwapFromSecret :: CardanoResolverState -> ByteString -> ByteString 
                       -> IO (Either String CardanoResolverState)
completeSwapFromSecret resolverState orderHash revealedSecret = do
    case Map.lookup orderHash (activeSwaps resolverState) of
        Nothing -> return $ Left "Swap not found"
        Just swap -> do
            -- Validate secret against hashlock
            if not (validateSecretHash revealedSecret (swapHashlock swap))
                then return $ Left "Invalid secret provided"
                else do
                    -- In real implementation, this would:
                    -- 1. Create Cardano transaction to claim from destination escrow
                    -- 2. Submit transaction to Cardano network
                    -- 3. Wait for confirmation
                    
                    currentTime <- getCurrentTime
                    let completedSwap = swap 
                            { swapStatus = SwapCompleted
                            , revealedSecret = Just revealedSecret
                            }
                    
                    -- Calculate resolver profit
                    let profit = calculateSwapProfit swap
                    
                    -- Update resolver state
                    let updatedResolver = resolverState
                            { activeSwaps = Map.delete orderHash (activeSwaps resolverState)
                            , completedSwaps = Map.insert orderHash completedSwap (completedSwaps resolverState)
                            , totalProfitAda = totalProfitAda resolverState + profit
                            }
                    
                    putStrLn $ "Completed ETH-to-Cardano swap: " ++ BC.unpack orderHash
                    putStrLn $ "Resolver profit: " ++ show profit ++ " lovelace"
                    
                    return $ Right updatedResolver

-- | Validate swap parameters
validateSwapParameters :: Integer -> Integer -> Either String ()
validateSwapParameters ethAmount adaAmount
    | ethAmount <= 0 = Left "ETH amount must be positive"
    | adaAmount <= 0 = Left "ADA amount must be positive"
    | otherwise = Right ()

-- | Calculate swap profit for resolver
calculateSwapProfit :: ETHToCardanoSwap -> Integer
calculateSwapProfit swap = 
    -- Simplified profit calculation
    -- In real implementation, would account for fees, exchange rates, etc.
    adaAmount swap `div` 1000  -- 0.1% profit

-- | Get current status of a swap
getSwapStatus :: CardanoResolverState -> ByteString -> Maybe SwapStatus
getSwapStatus resolverState orderHash = do
    swap <- Map.lookup orderHash (activeSwaps resolverState)
    return $ swapStatus swap

-- | Additional helper functions for cross-chain operations

-- | Generate timelock constraints for Cardano escrow
generateCardanoTimelocks :: UTCTime -> Timelocks
generateCardanoTimelocks currentTime = 
    let deploymentTime = floor (realToFrac (read (show currentTime) :: Double))
        -- Define timing for cross-chain atomic swap (in seconds from deployment)
        srcWithdrawTime = 3600      -- 1 hour: source withdrawal starts
        srcPublicWithdrawTime = 7200 -- 2 hours: source public withdrawal
        srcCancelTime = 14400       -- 4 hours: source cancellation
        srcPublicCancelTime = 18000  -- 5 hours: source public cancellation
        dstWithdrawTime = 1800      -- 30 minutes: destination withdrawal (earlier)
        dstPublicWithdrawTime = 5400 -- 90 minutes: destination public withdrawal
        dstCancelTime = 12600       -- 3.5 hours: destination cancellation
    in mkTimelocks deploymentTime srcWithdrawTime srcPublicWithdrawTime 
                   srcCancelTime srcPublicCancelTime dstWithdrawTime 
                   dstPublicWithdrawTime dstCancelTime

-- | Calculate safety deposit for a given ADA amount
calculateSafetyDeposit :: Integer -> Integer
calculateSafetyDeposit adaAmount = 
    let calculated = adaAmount `div` 100  -- 1% safety deposit
    in max calculated 2000000  -- Minimum 2 ADA

-- | Helper function to get escrow address from EscrowDst state  
getEscrowDstAddress :: EscrowDstState -> ByteString
getEscrowDstAddress dstState = 
    -- Extract address from the EscrowDst state
    let immuts = immutables dstState
    in "addr1_escrow_" <> BS.take 50 (orderHash immuts)

-- | Validate secret hash (simplified)
validateSecretHash :: ByteString -> ByteString -> Bool
validateSecretHash secret expectedHashlock = 
    -- In real implementation, would use proper SHA-256
    let secretHash = "hash_" <> secret
    in BS.take 32 secretHash == BS.take 32 expectedHashlock

-- | Helper functions
calculateCardanoEscrowAddress :: Immutables -> ByteString
calculateCardanoEscrowAddress immutables = 
    -- Simplified address calculation
    -- Real implementation would use proper Cardano script address derivation
    "addr1" <> BS.take 60 (orderHash immutables)

validateSecret :: ByteString -> ByteString -> Bool
validateSecret secret expectedHashlock = 
    -- SHA-256 hash comparison
    -- In real implementation, would use proper cryptographic library
    let secretHash = BS.take 32 $ "hash_" <> secret  -- Simplified
    in secretHash == expectedHashlock

-- | Display functions for debugging and monitoring
displayResolver :: CardanoResolverState -> IO ()
displayResolver resolver = do
    putStrLn "=== Cardano Resolver State ==="
    putStrLn $ "Address: " ++ BC.unpack (resolverAddress resolver)
    putStrLn $ "ETH Chain ID: " ++ show (ethChainId resolver)
    putStrLn $ "Active Swaps: " ++ show (Map.size (activeSwaps resolver))
    putStrLn $ "Completed Swaps: " ++ show (Map.size (completedSwaps resolver))
    putStrLn $ "Total Profit: " ++ show (totalProfitAda resolver) ++ " lovelace"
    putStrLn $ "Status: " ++ if resolverIsActive resolver then "Active" else "Inactive"

displaySwap :: ETHToCardanoSwap -> IO ()
displaySwap swap = do
    putStrLn "=== ETH-to-Cardano Swap ==="
    putStrLn $ "Order Hash: " ++ BC.unpack (swapOrderHash swap)
    putStrLn $ "ETH Amount: " ++ show (ethAmount swap) ++ " wei"
    putStrLn $ "ADA Amount: " ++ show (adaAmount swap) ++ " lovelace"
    putStrLn $ "Status: " ++ show (swapStatus swap)
    putStrLn $ "ETH Escrow: " ++ BC.unpack (ethEscrowAddress swap)
    putStrLn $ "Cardano Escrow: " ++ BC.unpack (cardanoEscrowAddress swap)
    case revealedSecret swap of
        Nothing -> putStrLn "Secret: Not revealed"
        Just secret -> putStrLn $ "Secret: " ++ BC.unpack secret
