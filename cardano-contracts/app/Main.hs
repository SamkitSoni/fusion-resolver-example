{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Main
Description : Main executable for Cardano resolver
Copyright   : (c) 2025
License     : MIT
-}

module Main (main) where

import System.Environment (getArgs)
import Data.ByteString.Char8 as BC

import Resolver.SwapCoordinator (runCoordinator)
import Resolver.CardanoResolver (initializeResolver, displayResolver)
import Resolver.ETHMonitor (ETHChainConfig(..), initializeMonitor)

main :: IO ()
main = do
    args <- getArgs
    case args of
        ["run"] -> do
            putStrLn "Starting Cardano Resolver for ETH-to-Cardano atomic swaps..."
            runCoordinator
            
        ["test"] -> do
            putStrLn "Running Cardano Resolver tests..."
            runTests
            
        ["monitor"] -> do
            putStrLn "Starting ETH chain monitor only..."
            runMonitorOnly
            
        _ -> do
            putStrLn "Usage:"
            putStrLn "  cardano-resolver-monitor run     - Start full resolver"
            putStrLn "  cardano-resolver-monitor test    - Run tests"
            putStrLn "  cardano-resolver-monitor monitor - Monitor ETH only"

-- | Run resolver tests
runTests :: IO ()
runTests = do
    putStrLn "=== Cardano Resolver Tests ==="
    
    -- Test resolver initialization
    let mockRegistry = mempty
    resolver <- initializeResolver 
        "addr1qx2fxv2umyhttkxyxp8x0dlpdt3k6cwng5pxj3jhsydzer3n0d3vllmyqwsx5wktcd8cc3sq835lu7drv2xwl2wywfgs2x2l"
        1  -- ETH mainnet
        "0x1234567890123456789012345678901234567890"  -- ETH resolver contract
        mockRegistry
    
    displayResolver resolver
    
    putStrLn "\n=== Tests Completed ==="

-- | Run ETH monitor only
runMonitorOnly :: IO ()
runMonitorOnly = do
    let ethConfig = ETHChainConfig
            { ethRpcUrl = "https://eth-mainnet.g.alchemy.com/v2/your-api-key"
            , ethChainId = 1
            , resolverContractAddress = "0x1234567890123456789012345678901234567890"
            , startBlockNumber = 18500000
            , confirmationBlocks = 12
            }
    
    let monitor = initializeMonitor ethConfig
    putStrLn "ETH monitor initialized"
    
    -- In a real implementation, this would run the monitoring loop
    putStrLn "Monitor would run here..."
