// SPDX-License-Identifier: MIT
pragma solidity ^0.8.20;

/**
 * @title MockEscrowReceiver
 * @dev Simple contract that can receive ETH for testing escrow functionality
 */
contract MockEscrowReceiver {
    event ETHReceived(address sender, uint256 amount);
    
    receive() external payable {
        emit ETHReceived(msg.sender, msg.value);
    }
    
    function withdraw(address payable to, uint256 amount) external {
        to.transfer(amount);
    }
    
    function getBalance() external view returns (uint256) {
        return address(this).balance;
    }
}
