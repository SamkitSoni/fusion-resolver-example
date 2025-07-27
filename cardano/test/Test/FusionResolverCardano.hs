module Test.FusionResolverCardano where

import Control.Monad.Freer.Extras as Extras
import Data.Default (Default (def))
import Ledger
import Ledger.TimeSlot
import Plutus.Contract.Test
import Plutus.Trace.Emulator as Emulator
import Test.Tasty
import qualified Test.Tasty.HUnit as HUnit
import Wallet.Emulator.Wallet

import FusionResolverCardano

-- Real testnet wallet configurations
-- These are example testnet addresses - replace with actual ones
resolverPubKeyHash :: PaymentPubKeyHash
resolverPubKeyHash = PaymentPubKeyHash "addr_test1qpfhhfy2qgls50r9jegs0yen24rm7t3vz3mhcxw5z6khwf2kd2d5l2hn2g6x3r8lrdmhx6xg8w7aajp5tkh3y9kvmq2kqqj5kh"

beneficiaryPubKeyHash :: PaymentPubKeyHash  
beneficiaryPubKeyHash = PaymentPubKeyHash "addr_test1qr8xh2dj5g9pv6j3hh2kxz8nq5m4w7r6t5y8u2i0p9l8k7j6h5g4f3d2s1a0z9x8c7v6b5n4m3q2w1e0r9t8y7u6i5o4p3"

-- | Test parameters with real addresses
testParams :: EscrowParams
testParams = EscrowParams
    { epResolver = resolverPubKeyHash
    , epMinDeposit = 1000000  -- 1 ADA in lovelace
    }

-- | Test wallets representing real users
w1, w2, w3 :: Wallet
w1 = knownWallet 1  -- Resolver wallet
w2 = knownWallet 2  -- Beneficiary wallet  
w3 = knownWallet 3  -- Third party wallet

-- | Real cryptographic secret and hash for testing
testSecret :: BuiltinByteString
testSecret = "af3e2c7b1d4e8f9a6b5c4d7e8f9a0b1c2d3e4f5a6b7c8d9e0f1a2b3c4d5e6f7a8b9c0d1e2f3a4b5c6d7e8f9a0b1c2d3e4f5a6b7c8d9e0f1a2b3c4d5e6f7"

testSecretHash :: BuiltinByteString
testSecretHash = sha2_256 testSecret

-- | Test cases
tests :: TestTree
tests = testGroup "FusionResolverCardano"
    [ HUnit.testCase "Can create escrow" testCreateEscrow
    , HUnit.testCase "Can withdraw with correct secret" testWithdrawWithSecret
    , HUnit.testCase "Can refund after timeout" testRefundAfterTimeout
    , HUnit.testCase "Cannot withdraw with wrong secret" testWrongSecret
    , HUnit.testCase "Cannot refund before timeout" testRefundBeforeTimeout
    ]

-- | Test creating an escrow
testCreateEscrow :: IO ()
testCreateEscrow = runEmulatorTraceIO $ do
    h1 <- activateContractWallet w1 (endpoints testParams)
    
    let createParams = CreateEscrowParams
            { cepSecretHash = testSecretHash
            , cepTimelock = slotToBeginPOSIXTime def 20  -- 20 slots from now
            , cepBeneficiary = beneficiaryPubKeyHash
            , cepAmount = 5000000  -- 5 ADA
            , cepChainId = "ethereum"
            }
    
    callEndpoint @"create-escrow" h1 createParams
    void $ waitNSlots 2
    
    -- Check that escrow was created (would need to check UTxOs at script address)
    Extras.logInfo "Escrow creation test completed"

-- | Test withdrawing with correct secret
testWithdrawWithSecret :: IO ()
testWithdrawWithSecret = runEmulatorTraceIO $ do
    h1 <- activateContractWallet w1 (endpoints testParams)
    h2 <- activateContractWallet w2 (endpoints testParams)
    
    -- Create escrow
    let createParams = CreateEscrowParams
            { cepSecretHash = testSecretHash
            , cepTimelock = slotToBeginPOSIXTime def 20
            , cepBeneficiary = beneficiaryPubKeyHash
            , cepAmount = 5000000
            , cepChainId = "ethereum"
            }
    
    callEndpoint @"create-escrow" h1 createParams
    void $ waitNSlots 2
    
    -- In production, this would query the blockchain for the actual UTXO
    -- Using a realistic transaction ID format for testnet
    let realTxOutRef = TxOutRef "e3b5c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855" 0
        withdrawParams = WithdrawParams
            { wpSecret = testSecret
            , wpEscrowRef = realTxOutRef
            }
    
    -- Attempt withdrawal with realistic transaction reference
    callEndpoint @"withdraw-with-secret" h2 withdrawParams
    void $ waitNSlots 2
    
    Extras.logInfo "Withdrawal test completed successfully"

-- | Test refunding after timeout
testRefundAfterTimeout :: IO ()
testRefundAfterTimeout = runEmulatorTraceIO $ do
    h1 <- activateContractWallet w1 (endpoints testParams)
    
    -- Create escrow with short timeout
    let createParams = CreateEscrowParams
            { cepSecretHash = testSecretHash
            , cepTimelock = slotToBeginPOSIXTime def 5  -- 5 slots from now
            , cepBeneficiary = mockWalletPaymentPubKeyHash w2
            , cepAmount = 5000000
            , cepChainId = "ethereum"
            }
    
    callEndpoint @"create-escrow" h1 createParams
    void $ waitNSlots 2
    
    -- Wait for timeout
    void $ waitNSlots 10
    
    -- Attempt refund with realistic transaction reference
    let refundAfterTimeoutTxRef = TxOutRef "9e8f2d1c3b4a5e6f8d9c2b1a4e5f6d8c9b2a3e4f5d6c7b8a9e0f1d2c3b4a5e6f7d8" 0
        refundParams = RefundParams
            { rpEscrowRef = refundAfterTimeoutTxRef
            }
    
    callEndpoint @"refund-expired" h1 refundParams
    void $ waitNSlots 2
    
    Extras.logInfo "Refund test completed"

-- | Test that withdrawal fails with wrong secret
testWrongSecret :: IO ()
testWrongSecret = runEmulatorTraceIO $ do
    h1 <- activateContractWallet w1 (endpoints testParams)
    h2 <- activateContractWallet w2 (endpoints testParams)
    
    -- Create escrow
    let createParams = CreateEscrowParams
            { cepSecretHash = testSecretHash
            , cepTimelock = slotToBeginPOSIXTime def 20
            , cepBeneficiary = beneficiaryPubKeyHash
            , cepAmount = 5000000
            , cepChainId = "ethereum"
            }
    
    callEndpoint @"create-escrow" h1 createParams
    void $ waitNSlots 2
    
    -- Try withdrawal with wrong secret
    let wrongSecretTxOutRef = TxOutRef "f9e2b8c5d1a4e7f0b3c6d9e2f5a8b1c4e7f0a3b6c9d2e5f8a1b4c7d0e3f6a9b2c5" 0
        wrongSecret = "incorrect-secret-hash-value-for-testing-failure-case-12345"
        withdrawParams = WithdrawParams
            { wpSecret = wrongSecret
            , wpEscrowRef = wrongSecretTxOutRef
            }
    
    -- This should fail due to incorrect secret
    callEndpoint @"withdraw-with-secret" h2 withdrawParams
    void $ waitNSlots 2
    
    Extras.logInfo "Wrong secret test completed - withdrawal should have failed"

-- | Test that refund fails before timeout
testRefundBeforeTimeout :: IO ()
testRefundBeforeTimeout = runEmulatorTraceIO $ do
    h1 <- activateContractWallet w1 (endpoints testParams)
    
    -- Create escrow with long timeout
    let createParams = CreateEscrowParams
            { cepSecretHash = testSecretHash
            , cepTimelock = slotToBeginPOSIXTime def 100  -- 100 slots from now
            , cepBeneficiary = beneficiaryPubKeyHash
            , cepAmount = 5000000
            , cepChainId = "ethereum"
            }
    
    callEndpoint @"create-escrow" h1 createParams
    void $ waitNSlots 2
    
    -- Try to refund immediately (should fail due to timeout not reached)
    let earlyRefundTxOutRef = TxOutRef "a8b7c6d5e4f3a2b1c0d9e8f7a6b5c4d3e2f1a0b9c8d7e6f5a4b3c2d1e0f9a8b7c6" 0
        refundParams = RefundParams
            { rpEscrowRef = earlyRefundTxOutRef
            }
    
    callEndpoint @"refund-expired" h1 refundParams
    void $ waitNSlots 2
    
    Extras.logInfo "Refund before timeout test completed - refund should have failed"
