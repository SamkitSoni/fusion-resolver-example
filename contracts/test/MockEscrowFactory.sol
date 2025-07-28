// SPDX-License-Identifier: MIT
pragma solidity ^0.8.20;

/**
 * @title Mock Escrow Factory for testing
 * @dev Simulates the behavior of the real EscrowFactory contract
 */
contract MockEscrowFactory {
    address private _mockEscrowAddress;
    bytes private _lastCallData;
    uint256 private _lastValue;
    
    // Store the last call parameters for verification
    struct LastCall {
        address caller;
        uint256 value;
        bytes data;
    }
    
    LastCall public lastCall;
    
    // Mock immutables struct for testing
    struct Immutables {
        bytes32 orderHash;
        bytes32 hashlock;
        address maker;
        address taker;
        address token;
        uint256 amount;
        uint256 safetyDeposit;
        uint256 timelocks;
    }
    
    Immutables public lastImmutables;
    
    event MockEscrowCreated(address escrow, uint256 value);
    
    // Allow the contract to receive ETH
    receive() external payable {}
    
    /**
     * @notice Set the mock escrow address that will be returned
     */
    function setMockEscrowAddress(address escrowAddress) external {
        _mockEscrowAddress = escrowAddress;
    }
    
    /**
     * @notice Mock implementation of addressOfEscrowSrc
     */
    function addressOfEscrowSrc(Immutables calldata immutables) external view returns (address) {
        return _mockEscrowAddress;
    }
    
    /**
     * @notice Mock implementation of addressOfEscrowDst
     */
    function addressOfEscrowDst(Immutables calldata immutables) external view returns (address) {
        return _mockEscrowAddress;
    }
    
    /**
     * @notice Mock implementation of createDstEscrow
     */
    function createDstEscrow(
        Immutables calldata dstImmutables, 
        uint256 srcCancellationTimestamp
    ) external payable {
        lastCall = LastCall({
            caller: msg.sender,
            value: msg.value,
            data: msg.data
        });
        
        lastImmutables = dstImmutables;
        
        emit MockEscrowCreated(_mockEscrowAddress, msg.value);
    }
    
    /**
     * @notice Get the last call data for verification
     */
    function getLastCallData() external view returns (bytes memory) {
        return lastCall.data;
    }
    
    /**
     * @notice Get the last immutables for verification
     */
    function getLastImmutables() external view returns (Immutables memory) {
        return lastImmutables;
    }
    
    /**
     * @notice Get info about what this contract was last called with
     */
    function lastCalledWith() external view returns (address caller, uint256 value, bytes memory data) {
        return (lastCall.caller, lastCall.value, lastCall.data);
    }
}
