// SPDX-License-Identifier: MIT
pragma solidity ^0.8.20;

import "../interfaces/ICardanoEscrowFactory.sol";

/**
 * @title MockCardanoEscrowFactory
 * @dev Mock implementation of ICardanoEscrowFactory for testing
 */
contract MockCardanoEscrowFactory is ICardanoEscrowFactory {
    // State variables to track calls
    bool public createCardanoDstEscrowCalled = false;
    bool public submitTransactionCalled = false;
    bool public shouldFail = false;
    
    // Return values for mocking
    bytes32 public expectedTxHash;
    bytes29 public expectedCardanoAddress;
    
    // Storage for the last call parameters
    CardanoImmutables public lastCardanoImmutables;
    uint256 public lastSrcCancellationTimestamp;
    CardanoTxData public lastTxData;

    function setExpectedTxHash(bytes32 _txHash) external {
        expectedTxHash = _txHash;
    }

    function setExpectedCardanoAddress(bytes29 _address) external {
        expectedCardanoAddress = _address;
    }

    function setShouldFail(bool _shouldFail) external {
        shouldFail = _shouldFail;
    }

    function createCardanoDstEscrow(
        CardanoImmutables calldata cardanoImmutables,
        uint256 srcCancellationTimestamp
    ) external payable override returns (bytes32 txHash) {
        createCardanoDstEscrowCalled = true;
        lastCardanoImmutables = cardanoImmutables;
        lastSrcCancellationTimestamp = srcCancellationTimestamp;

        if (shouldFail) {
            revert("Cardano escrow creation failed");
        }

        // Emit the expected event
        emit CardanoEscrowCreated(
            cardanoImmutables.orderHash,
            cardanoImmutables.cardanoTaker,
            expectedTxHash,
            expectedCardanoAddress
        );

        return expectedTxHash;
    }

    function addressOfCardanoEscrow(
        CardanoImmutables calldata /* cardanoImmutables */
    ) external pure override returns (bytes29 scriptAddress) {
        return bytes29(0x0101010101010101010101010101010101010101010101010101010101);
    }

    function evmToCardanoAddress(
        address evmAddress
    ) external pure override returns (bytes29 cardanoAddress) {
        // Simple mock conversion - just use the address as part of the bytes29
        bytes32 addressBytes = bytes32(uint256(uint160(evmAddress)));
        // Take first 29 bytes
        return bytes29(addressBytes);
    }

    function submitCardanoTransaction(
        CardanoTxData calldata txData
    ) external override {
        submitTransactionCalled = true;
        lastTxData = txData;

        emit CardanoTransactionSubmitted(
            txData.txHash,
            expectedCardanoAddress,
            0 // Mock amount
        );
    }

    // Helper functions for testing
    function resetCallFlags() external {
        createCardanoDstEscrowCalled = false;
        submitTransactionCalled = false;
        shouldFail = false;
    }

    function getLastCardanoImmutables() external view returns (CardanoImmutables memory) {
        return lastCardanoImmutables;
    }

    function getLastSrcCancellationTimestamp() external view returns (uint256) {
        return lastSrcCancellationTimestamp;
    }

    function getLastTxData() external view returns (CardanoTxData memory) {
        return lastTxData;
    }
}
