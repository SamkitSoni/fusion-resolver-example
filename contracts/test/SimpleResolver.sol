// SPDX-License-Identifier: MIT
pragma solidity ^0.8.20;

import { IERC20 } from "@openzeppelin/contracts/token/ERC20/IERC20.sol";
import { Ownable } from "@openzeppelin/contracts/access/Ownable.sol";
import { Pausable } from "@openzeppelin/contracts/utils/Pausable.sol";

/**
 * @title Simple test contract to isolate the issue
 */
contract SimpleResolver is Ownable, Pausable {
    address private _escrowFactory;
    address private _lopv4;
    
    constructor(
        address lopv4,
        address escrowFactory,
        address owner
    ) Ownable(owner) {
        _lopv4 = lopv4;
        _escrowFactory = escrowFactory;
    }
    
    function getEscrowFactory() external view returns (address) {
        return _escrowFactory;
    }
    
    function getLimitOrderProtocol() external view returns (address) {
        return _lopv4;
    }
    
    function pause() external onlyOwner {
        _pause();
    }
    
    function unpause() external onlyOwner {
        _unpause();
    }
    
    // Very simple test function
    function testBasicCall() external onlyOwner whenNotPaused {
        // Just do something basic that requires state change
        emit TestCall(msg.sender);
    }
    
    event TestCall(address indexed caller);
}
