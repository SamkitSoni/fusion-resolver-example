// SPDX-License-Identifier: MIT
pragma solidity ^0.8.20;

import { IOrderMixin } from "@1inch/limit-order-protocol-contract/contracts/interfaces/IOrderMixin.sol";
import { TakerTraits } from "@1inch/limit-order-protocol-contract/contracts/libraries/TakerTraitsLib.sol";
import { Address, AddressLib } from "@1inch/solidity-utils/contracts/libraries/AddressLib.sol";

/**
 * @title Mock Limit Order Protocol for testing
 * @dev Simulates the behavior of the real LOP contract
 */
contract MockLimitOrderProtocol {
    bytes private _lastCallData;
    address private _lastCaller;
    uint256 private _lastValue;
    
    IOrderMixin.Order public lastOrder;
    bytes32 public lastR;
    bytes32 public lastVs;
    uint256 public lastAmount;
    TakerTraits public lastTakerTraits;
    bytes public lastArgs;
    
    event MockOrderFilled(
        address indexed maker,
        uint256 makingAmount,
        uint256 takingAmount
    );
    
    // Allow the contract to receive ETH
    receive() external payable {}
    
    /**
     * @notice Mock implementation of fillOrderArgs
     */
    function fillOrderArgs(
        IOrderMixin.Order calldata order,
        bytes32 r,
        bytes32 vs,
        uint256 amount,
        TakerTraits takerTraits,
        bytes calldata args
    ) external payable {
        _lastCallData = msg.data;
        _lastCaller = msg.sender;
        _lastValue = msg.value;
        
        lastOrder = order;
        lastR = r;
        lastVs = vs;
        lastAmount = amount;
        lastTakerTraits = takerTraits;
        lastArgs = args;
        
        emit MockOrderFilled(AddressLib.get(order.maker), order.makingAmount, order.takingAmount);
    }
    
    /**
     * @notice Get the last call data for verification
     */
    function getLastCallData() external view returns (bytes memory) {
        return _lastCallData;
    }
    
    /**
     * @notice Get the last caller
     */
    function getLastCaller() external view returns (address) {
        return _lastCaller;
    }
    
    /**
     * @notice Get the last value sent
     */
    function getLastValue() external view returns (uint256) {
        return _lastValue;
    }
    
    /**
     * @notice Verify taker traits have the target flag set
     */
    function hasTargetFlag(TakerTraits takerTraits) external pure returns (bool) {
        return (TakerTraits.unwrap(takerTraits) & (1 << 251)) != 0;
    }
    
    /**
     * @notice Get last taker traits as uint256
     */
    function getLastTakerTraitsUint() external view returns (uint256) {
        return TakerTraits.unwrap(lastTakerTraits);
    }
}
