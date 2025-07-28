// SPDX-License-Identifier: MIT

pragma solidity ^0.8.20;

/**
 * @title ICardanoEscrowFactory
 * @dev Interface for creating Cardano-side escrows in cross-chain atomic swaps
 */
interface ICardanoEscrowFactory {
    /**
     * @dev Cardano-specific immutable parameters for escrow creation
     */
    struct CardanoImmutables {
        bytes32 orderHash;           // Unique identifier for the swap order
        bytes32 hashlock;            // Hash of the secret for atomic swap
        bytes29 cardanoMaker;        // Cardano address that will receive funds
        bytes29 cardanoTaker;        // Cardano address that locks funds (resolver)
        bytes32 policyId;            // Token policy ID (0x0 for ADA)
        bytes32 assetName;           // Token asset name (0x0 for ADA)
        uint256 amount;              // Amount to be locked (in lovelace for ADA)
        uint256 adaSafetyDeposit;    // Safety deposit in ADA (minimum 2 ADA)
        uint256 timelocks;           // Time constraints for the escrow (simplified)
    }

    /**
     * @dev Cardano transaction data for escrow operations
     */
    struct CardanoTxData {
        bytes serializedTx;          // Serialized Cardano transaction
        bytes32 txHash;              // Transaction hash
        uint256 outputIndex;         // Output index in the transaction
        bytes signature;             // Transaction signature
    }

    /**
     * @notice Create a destination escrow on Cardano blockchain
     * @param cardanoImmutables The immutable parameters for the Cardano escrow
     * @param srcCancellationTimestamp When the source escrow can be cancelled
     * @return txHash The hash of the Cardano transaction that created the escrow
     */
    function createCardanoDstEscrow(
        CardanoImmutables calldata cardanoImmutables,
        uint256 srcCancellationTimestamp
    ) external payable returns (bytes32 txHash);

    /**
     * @notice Calculate the deterministic address of a Cardano escrow
     * @param cardanoImmutables The immutable parameters for the escrow
     * @return The Cardano address where the escrow will be created
     */
    function addressOfCardanoEscrow(
        CardanoImmutables calldata cardanoImmutables
    ) external pure returns (bytes29);

    /**
     * @notice Convert an EVM address to Cardano address format
     * @param evmAddr The EVM address to convert
     * @return The corresponding Cardano address representation
     */
    function evmToCardanoAddress(address evmAddr) external pure returns (bytes29);

    /**
     * @notice Submit a transaction to the Cardano blockchain
     * @dev This function interfaces with Cardano node to submit transactions
     * @param txData The transaction data to submit
     */
    function submitCardanoTransaction(CardanoTxData calldata txData) external;

    /**
     * @notice Get the current Cardano network parameters
     * @return protocolMagic The network protocol magic number
     * @return minUtxo The minimum UTXO value
     * @return minFee The minimum transaction fee
     */
    function getCardanoNetworkParams() external view returns (
        uint32 protocolMagic,
        uint256 minUtxo,
        uint256 minFee
    );

    // Events
    event CardanoEscrowCreated(
        bytes32 indexed orderHash,
        bytes29 indexed cardanoAddress,
        bytes32 indexed txHash,
        uint256 amount
    );

    event CardanoTransactionSubmitted(
        bytes32 indexed txHash,
        address indexed submitter
    );
}
