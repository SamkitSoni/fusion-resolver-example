{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module FusionResolverCardano where

import qualified PlutusTx
import PlutusTx.Prelude hiding (Semigroup(..), unless)
import Ledger hiding (singleton)
import Ledger.Constraints as Constraints
import qualified Ledger.Scripts as Scripts
import qualified Ledger.Typed.Scripts as Scripts
import Ledger.Ada as Ada
import Ledger.Value as Value
import Playground.Contract (printJson, printSchemas, ensureKnownCurrencies, stage)
import Playground.TH (mkKnownCurrencies, mkSchemaDefinitions)
import Playground.Types (KnownCurrency (..))
import Prelude (IO, Semigroup (..), String)
import Text.Printf (printf)
import Control.Monad hiding (fmap)
import Data.Map as Map
import Data.Text (Text)
import Data.Void (Void)
import Plutus.Contract as Contract
import qualified PlutusTx.Builtins as Builtins

-- ============ Data Types ============

-- | Represents the datum for an escrow UTXO
data EscrowDatum = EscrowDatum
    { escrowSecretHash :: !BuiltinByteString  -- SHA256 hash of secret
    , escrowTimelock   :: !POSIXTime          -- Expiry time
    , escrowBeneficiary :: !PaymentPubKeyHash -- Who can claim with secret
    , escrowResolver    :: !PaymentPubKeyHash -- Who deposited (for refunds)
    , escrowAmount     :: !Integer            -- Amount in lovelace
    , escrowChainId    :: !BuiltinByteString  -- Source chain identifier
    } deriving Show

PlutusTx.unstableMakeIsData ''EscrowDatum

-- | Redeemer for spending escrow UTXOs
data EscrowRedeemer = 
    WithdrawWithSecret BuiltinByteString  -- Provide secret to unlock
    | RefundAfterTimeout                  -- Refund after timelock expires
    deriving Show

PlutusTx.unstableMakeIsData ''EscrowRedeemer

-- | Parameters for the escrow validator
data EscrowParams = EscrowParams
    { epResolver :: !PaymentPubKeyHash    -- Authorized resolver
    , epMinDeposit :: !Integer            -- Minimum escrow amount
    } deriving Show

PlutusTx.unstableMakeIsData ''EscrowParams

-- ============ Validator Logic ============

{-# INLINABLE escrowValidator #-}
escrowValidator :: EscrowParams -> EscrowDatum -> EscrowRedeemer -> ScriptContext -> Bool
escrowValidator params datum redeemer ctx =
    case redeemer of
        WithdrawWithSecret secret ->
            traceIfFalse "wrong secret hash" (sha2_256 secret == escrowSecretHash datum) &&
            traceIfFalse "timelock expired" (to (escrowTimelock datum) `contains` txInfoValidRange info) &&
            traceIfFalse "beneficiary not signed" (txSignedBy info (unPaymentPubKeyHash $ escrowBeneficiary datum))
            
        RefundAfterTimeout ->
            traceIfFalse "timelock not expired" (from (escrowTimelock datum) `contains` txInfoValidRange info) &&
            traceIfFalse "resolver not signed" (txSignedBy info (unPaymentPubKeyHash $ escrowResolver datum))
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

-- ============ Boilerplate ============

data Escrow
instance Scripts.ValidatorTypes Escrow where
    type instance DatumType Escrow = EscrowDatum
    type instance RedeemerType Escrow = EscrowRedeemer

escrowTypedValidator :: EscrowParams -> Scripts.TypedValidator Escrow
escrowTypedValidator params = Scripts.mkTypedValidator @Escrow
    ($$(PlutusTx.compile [|| escrowValidator ||]) `PlutusTx.applyCode` PlutusTx.liftCode params)
    $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = Scripts.wrapValidator @EscrowDatum @EscrowRedeemer

escrowValidator' :: EscrowParams -> Validator
escrowValidator' = Scripts.validatorScript . escrowTypedValidator

escrowAddress :: EscrowParams -> Ledger.Address
escrowAddress = scriptAddress . escrowValidator'

escrowHash :: EscrowParams -> Ledger.ValidatorHash
escrowHash = Scripts.validatorHash . escrowTypedValidator

-- ============ Contract Endpoints ============

type EscrowSchema =
    Endpoint "create-escrow" CreateEscrowParams
    .\/ Endpoint "withdraw-with-secret" WithdrawParams
    .\/ Endpoint "refund-expired" RefundParams
    .\/ Endpoint "query-escrows" QueryParams

-- | Parameters for creating an escrow
data CreateEscrowParams = CreateEscrowParams
    { cepSecretHash   :: !BuiltinByteString
    , cepTimelock     :: !POSIXTime
    , cepBeneficiary  :: !PaymentPubKeyHash
    , cepAmount       :: !Integer
    , cepChainId      :: !BuiltinByteString
    } deriving (Generic, ToJSON, FromJSON, ToSchema)

-- | Parameters for withdrawing with secret
data WithdrawParams = WithdrawParams
    { wpSecret      :: !BuiltinByteString
    , wpEscrowRef   :: !TxOutRef
    } deriving (Generic, ToJSON, FromJSON, ToSchema)

-- | Parameters for refunding expired escrow
data RefundParams = RefundParams
    { rpEscrowRef :: !TxOutRef
    } deriving (Generic, ToJSON, FromJSON, ToSchema)

-- | Parameters for querying escrows
data QueryParams = QueryParams
    { qpResolver :: !PaymentPubKeyHash
    } deriving (Generic, ToJSON, FromJSON, ToSchema)

-- ============ Contract Implementation ============

-- | Create a new escrow contract
createEscrow :: EscrowParams -> CreateEscrowParams -> Contract w EscrowSchema Text ()
createEscrow params cep = do
    resolver <- ownPaymentPubKeyHash
    let datum = EscrowDatum
            { escrowSecretHash = cepSecretHash cep
            , escrowTimelock = cepTimelock cep
            , escrowBeneficiary = cepBeneficiary cep
            , escrowResolver = resolver
            , escrowAmount = cepAmount cep
            , escrowChainId = cepChainId cep
            }
        
        value = Ada.lovelaceValueOf (cepAmount cep)
        tx = Constraints.mustPayToTheScript datum value
    
    ledgerTx <- submitTxConstraints (escrowTypedValidator params) tx
    void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
    
    logInfo @String $ printf "Created escrow with amount %d lovelace" (cepAmount cep)

-- | Withdraw from escrow using the secret
withdrawWithSecret :: EscrowParams -> WithdrawParams -> Contract w EscrowSchema Text ()
withdrawWithSecret params wp = do
    beneficiary <- ownPaymentPubKeyHash
    now <- currentTime
    
    utxos <- utxosAt (escrowAddress params)
    
    case Map.toList utxos of
        [(oref, o)] | oref == wpEscrowRef wp -> do
            case _ciTxOutDatum o of
                Left _ -> throwError "datum missing"
                Right (Datum e) -> case PlutusTx.fromBuiltinData e of
                    Nothing -> throwError "datum has wrong type"
                    Just datum@EscrowDatum{..} -> do
                        -- Verify secret matches hash
                        if sha2_256 (wpSecret wp) /= escrowSecretHash
                            then throwError "wrong secret"
                            else do
                                -- Verify beneficiary
                                if beneficiary /= escrowBeneficiary
                                    then throwError "not the beneficiary"
                                    else do
                                        let redeemer = WithdrawWithSecret (wpSecret wp)
                                            lookups = Constraints.unspentOutputs utxos <>
                                                     Constraints.otherScript (escrowValidator' params)
                                            tx = Constraints.mustSpendScriptOutput oref redeemer <>
                                                Constraints.mustValidateIn (to escrowTimelock)
                                        
                                        ledgerTx <- submitTxConstraintsWith @Escrow lookups tx
                                        void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
                                        
                                        logInfo @String $ printf "Withdrew %d lovelace with secret" escrowAmount
        _ -> throwError "escrow not found"

-- | Refund expired escrow
refundExpired :: EscrowParams -> RefundParams -> Contract w EscrowSchema Text ()
refundExpired params rp = do
    resolver <- ownPaymentPubKeyHash
    now <- currentTime
    
    utxos <- utxosAt (escrowAddress params)
    
    case Map.toList utxos of
        [(oref, o)] | oref == rpEscrowRef rp -> do
            case _ciTxOutDatum o of
                Left _ -> throwError "datum missing"
                Right (Datum e) -> case PlutusTx.fromBuiltinData e of
                    Nothing -> throwError "datum has wrong type"
                    Just datum@EscrowDatum{..} -> do
                        -- Verify resolver
                        if resolver /= escrowResolver
                            then throwError "not the resolver"
                            else do
                                -- Verify timelock expired
                                if now < escrowTimelock
                                    then throwError "timelock not expired"
                                    else do
                                        let redeemer = RefundAfterTimeout
                                            lookups = Constraints.unspentOutputs utxos <>
                                                     Constraints.otherScript (escrowValidator' params)
                                            tx = Constraints.mustSpendScriptOutput oref redeemer <>
                                                Constraints.mustValidateIn (from escrowTimelock)
                                        
                                        ledgerTx <- submitTxConstraintsWith @Escrow lookups tx
                                        void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
                                        
                                        logInfo @String $ printf "Refunded %d lovelace after timeout" escrowAmount
        _ -> throwError "escrow not found"

-- | Query escrows for a resolver
queryEscrows :: EscrowParams -> QueryParams -> Contract w EscrowSchema Text [EscrowDatum]
queryEscrows params qp = do
    utxos <- utxosAt (escrowAddress params)
    let datums = mapMaybe extractDatum (Map.elems utxos)
        resolverEscrows = filter (\d -> escrowResolver d == qpResolver qp) datums
    
    logInfo @String $ printf "Found %d escrows for resolver" (length resolverEscrows)
    return resolverEscrows
  where
    extractDatum :: ChainIndexTxOut -> Maybe EscrowDatum
    extractDatum o = case _ciTxOutDatum o of
        Left _ -> Nothing
        Right (Datum e) -> PlutusTx.fromBuiltinData e

-- ============ Contract Endpoints Handler ============

endpoints :: EscrowParams -> Contract () EscrowSchema Text ()
endpoints params = awaitPromise (create' `select` withdraw' `select` refund' `select` query') >> endpoints params
  where
    create' = endpoint @"create-escrow" $ createEscrow params
    withdraw' = endpoint @"withdraw-with-secret" $ withdrawWithSecret params
    refund' = endpoint @"refund-expired" $ refundExpired params
    query' = endpoint @"query-escrows" $ queryEscrows params

-- ============ Playground ============

mkSchemaDefinitions ''EscrowSchema

mkKnownCurrencies []
