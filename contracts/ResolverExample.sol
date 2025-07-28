// SPDX-License-Identifier: MIT
// This code is provided "as is" without warranties of any kind. 
// 1inch does not assume responsibility for its security, suitability, or fitness for any specific use. 
// Any party using this code is solely responsible for conducting independent audits before deployment.

pragma solidity ^0.8.20;

import { IERC20 } from "@openzeppelin/contracts/token/ERC20/IERC20.sol";
import { ReentrancyGuard } from "@openzeppelin/contracts/utils/ReentrancyGuard.sol";
import { Pausable } from "@openzeppelin/contracts/utils/Pausable.sol";
import { Ownable } from "@openzeppelin/contracts/access/Ownable.sol";
import { Address, AddressLib} from "@1inch/solidity-utils/contracts/libraries/AddressLib.sol";
import { SafeERC20 } from "@1inch/solidity-utils/contracts/libraries/SafeERC20.sol";
import { RevertReasonForwarder } from "@1inch/solidity-utils/contracts/libraries/RevertReasonForwarder.sol";
import { IOrderMixin } from "@1inch/limit-order-protocol-contract/contracts/interfaces/IOrderMixin.sol";
import { ITakerInteraction } from "@1inch/limit-order-protocol-contract/contracts/interfaces/ITakerInteraction.sol";
import { TakerTraits } from "@1inch/limit-order-protocol-contract/contracts/libraries/TakerTraitsLib.sol";

// Import from the real cross-chain-swap contracts using remapping
import { EscrowFactory } from "cross-chain-swap/EscrowFactory.sol";
import { Immutables } from "cross-chain-swap/libraries/ImmutablesLib.sol";
import { Timelocks, TimelocksLib } from "cross-chain-swap/libraries/TimelocksLib.sol";

import "./interfaces/IResolverExample.sol";
import "./interfaces/ICardanoEscrowFactory.sol";

/**
 * @title ResolverExample - Cross-Chain Fusion+ Resolver
 * @dev Resolver for cross-chain atomic swaps using external escrow contracts
 */
contract ResolverExample is IResolverExample, ITakerInteraction, ReentrancyGuard, Pausable, Ownable {
    error NotTaker();
    error OnlyLOP();
    error FailedExternalCall(uint256 index, bytes reason);

    using SafeERC20 for IERC20;
    using AddressLib for Address;

    // ============ Events ============
    event OrderSettled(
        bytes32 indexed orderHash,
        address indexed maker,
        uint256 profit
    );
    
    event EscrowSrcDeployed(
        address indexed escrowAddress,
        bytes32 indexed orderHash,
        address indexed maker
    );
    
    event EscrowDstDeployed(
        address indexed escrowAddress,
        address indexed taker
    );
    
    // ETH-to-Cardano specific events
    event ETHToCardanoSwapInitiated(
        bytes32 indexed orderHash,
        address indexed ethMaker,
        bytes29 indexed cardanoTaker,
        uint256 ethAmount,
        uint256 adaAmount
    );
    
    event CardanoEscrowCreated(
        bytes32 indexed orderHash,
        bytes29 cardanoAddress,
        bytes32 cardanoTxHash,
        uint256 adaAmount
    );
    
    event ETHToCardanoSwapCompleted(
        bytes32 indexed orderHash,
        bytes32 secret,
        address ethBeneficiary,
        bytes29 cardanoBeneficiary
    );
    
    event CardanoEscrowDstDeployed(
        bytes32 indexed orderHash,
        bytes29 indexed cardanoTaker,
        bytes32 indexed txHash,
        bytes29 cardanoAddress
    );

    // ============ State Variables ============
    IOrderMixin private immutable _LOPV4;
    EscrowFactory private immutable _ESCROW_FACTORY;  // Use real EscrowFactory from cross-chain-swap
    ICardanoEscrowFactory private immutable _CARDANO_ESCROW_FACTORY;
    
    // Gas price tracking for dynamic pricing
    uint256 public lastBaseFee;
    uint256 public constant MAX_GAS_PRICE_MULTIPLIER = 150; // 1.5x

    // ============ Constructor ============
    constructor(
        IOrderMixin limitOrderProtocol,
        EscrowFactory escrowFactory,  // Use real EscrowFactory
        ICardanoEscrowFactory cardanoEscrowFactory,
        address initialOwner
    ) Ownable(initialOwner) {
        _LOPV4 = limitOrderProtocol;
        _ESCROW_FACTORY = escrowFactory;
        _CARDANO_ESCROW_FACTORY = cardanoEscrowFactory;
        lastBaseFee = block.basefee;
    }

    // Allow contract to receive ETH
    receive() external payable {}

    // ============ Owner Functions ============
    function pause() external onlyOwner {
        _pause();
    }

    function unpause() external onlyOwner {
        _unpause();
    }

    function approve(IERC20 token, address to) external onlyOwner {
        // Use low-level call to handle different token implementations
        (bool success,) = address(token).call(
            abi.encodeWithSelector(token.approve.selector, to, 0)
        );
        // Don't revert if resetting to 0 fails, some tokens don't need it
        
        (success,) = address(token).call(
            abi.encodeWithSelector(token.approve.selector, to, type(uint256).max)
        );
        require(success, "Approval failed");
    }

    function emergencyWithdraw(IERC20 token, uint256 amount) external onlyOwner {
        token.safeTransfer(owner(), amount);
    }

    // ============ Cross-Chain Escrow Functions ============
    /**
     * @notice Deploy a new escrow contract for maker on the source chain
     * @dev Uses the cross-chain-swap escrow factory to create source escrow
     */
    function deploySrc(
        Immutables calldata immutables,
        IOrderMixin.Order calldata order,
        bytes32 r,
        bytes32 vs,
        uint256 amount,
        TakerTraits takerTraits,
        bytes calldata args
    ) external onlyOwner whenNotPaused {
        // Set deployed timestamp for immutables using TimelocksLib
        Immutables memory immutablesMem = immutables;
        uint256 deployedAt = block.timestamp;
        // Ensure timestamp fits in 32 bits for TimelocksLib
        if (deployedAt > type(uint32).max) {
            deployedAt = type(uint32).max;
        }
        immutablesMem.timelocks = TimelocksLib.setDeployedAt(immutables.timelocks, deployedAt);
        
        // Calculate escrow address using factory
        address computed = _ESCROW_FACTORY.addressOfEscrowSrc(immutablesMem);
        
        // Send safety deposit to computed escrow address
        (bool success,) = address(computed).call{value: immutablesMem.safetyDeposit}("");
        require(success, "Safety deposit transfer failed");

        // Set target flag for taker traits
        // _ARGS_HAS_TARGET = 1 << 251
        takerTraits = TakerTraits.wrap(TakerTraits.unwrap(takerTraits) | uint256(1 << 251));
        
        // Prepend target address to args
        bytes memory argsMem = abi.encodePacked(computed, args);
        
        // Fill the order through LOP
        _LOPV4.fillOrderArgs(order, r, vs, amount, takerTraits, argsMem);

        emit EscrowSrcDeployed(computed, immutables.orderHash, immutables.maker.get());
    }

    /**
     * @notice Deploy a new escrow contract for taker on the destination chain
     * @dev Uses the cross-chain-swap escrow factory to create destination escrow
     */
    function deployDst(
        Immutables calldata dstImmutables, 
        uint256 srcCancellationTimestamp
    ) external payable onlyOwner whenNotPaused {
        // Create destination escrow through factory
        _ESCROW_FACTORY.createDstEscrow{value: msg.value}(dstImmutables, srcCancellationTimestamp);
        
        // Calculate the escrow address (for event emission)
        address escrowAddress = _ESCROW_FACTORY.addressOfEscrowSrc(dstImmutables);
        
        emit EscrowDstDeployed(escrowAddress, dstImmutables.taker.get());
    }

    /**
     * @notice Deploy a new escrow contract on Cardano for EVM-to-Cardano swaps
     * @dev Creates Cardano-side escrow through the Cardano factory
     */
    function deployCardanoDst(
        ICardanoEscrowFactory.CardanoImmutables calldata cardanoImmutables,
        uint256 srcCancellationTimestamp
    ) external payable onlyOwner whenNotPaused {
        // Create Cardano destination escrow through factory
        bytes32 txHash = _CARDANO_ESCROW_FACTORY.createCardanoDstEscrow{value: msg.value}(
            cardanoImmutables, 
            srcCancellationTimestamp
        );
        
        // Calculate the Cardano escrow address (for event emission)
        bytes29 cardanoAddress = _CARDANO_ESCROW_FACTORY.addressOfCardanoEscrow(cardanoImmutables);
        
        emit CardanoEscrowDstDeployed(
            cardanoImmutables.orderHash,
            cardanoImmutables.cardanoTaker,
            txHash,
            cardanoAddress
        );
    }

    // ============ ETH-to-Cardano Atomic Swap Functions ============
    
    /**
     * @notice Initiate a complete ETH-to-Cardano atomic swap
     * @dev This is the main function resolvers call for ETH-to-ADA swaps
     * @param order The limit order from the maker wanting to swap ETH for ADA
     * @param cardanoDestination The Cardano address where maker wants to receive ADA
     * @param r R component of signature
     * @param vs VS component of signature
     * @param amount Taker amount to fill
     * @param takerTraits Taker traits for the order
     * @param args Additional arguments
     */
    function initiateETHToCardanoSwap(
        IOrderMixin.Order calldata order,
        bytes29 cardanoDestination,
        bytes32 r,
        bytes32 vs,
        uint256 amount,
        TakerTraits takerTraits,
        bytes calldata args
    ) external payable onlyOwner whenNotPaused {
        bytes32 orderHash = _calculateOrderHash(order);
        
        // Validate that this is an ETH-to-ADA swap
        require(order.makerAsset.get() == address(0), "Only ETH-to-ADA swaps supported");
        require(msg.value >= order.takingAmount, "Insufficient ADA provided");
        
        // 1. Deploy ETH source escrow first
        _deployETHSourceEscrow(order, orderHash, r, vs, amount, takerTraits, args);
        
        // 2. Deploy Cardano destination escrow
        _deployCardanoDestinationEscrow(
            order, 
            orderHash, 
            cardanoDestination, 
            order.takingAmount
        );
        
        emit ETHToCardanoSwapInitiated(
            orderHash,
            order.maker.get(),
            cardanoDestination,
            order.makingAmount, // ETH amount
            order.takingAmount  // ADA amount
        );
    }
    
    /**
     * @notice Complete ETH-to-Cardano swap by revealing secret
     * @dev Resolver calls this to claim ETH and reveal secret for Cardano side
     * @param orderHash The order hash
     * @param secret The secret that unlocks both escrows
     * @param ethImmutables The ETH escrow immutables
     * @param cardanoAddress The Cardano address that will receive ADA
     */
    function completeETHToCardanoSwap(
        bytes32 orderHash,
        bytes32 secret,
        Immutables calldata ethImmutables,
        bytes29 cardanoAddress
    ) external onlyOwner whenNotPaused {
        // Validate secret against hashlock
        require(keccak256(abi.encodePacked(secret)) == ethImmutables.hashlock, "Invalid secret");
        
        // Get ETH escrow address
        address ethEscrowAddress = _ESCROW_FACTORY.addressOfEscrowSrc(ethImmutables);
        
        // Withdraw ETH from source escrow (this reveals the secret publicly)
        _withdrawFromETHEscrow(ethEscrowAddress, secret, ethImmutables);
        
        emit ETHToCardanoSwapCompleted(
            orderHash,
            secret,
            address(this), // Resolver gets the ETH
            cardanoAddress  // Maker can now claim ADA using the revealed secret
        );
    }

    // ============ LOP Integration Functions ============
    function settleOrders(bytes calldata data) external onlyOwner whenNotPaused {
        _settleOrders(data);
    }

    function _settleOrders(bytes calldata data) internal {
        // Update base fee for gas price tracking
        lastBaseFee = block.basefee;
        
        // solhint-disable-next-line avoid-low-level-calls
        (bool success, bytes memory result) = address(_LOPV4).call(data);
        if (!success) {
            // Forward the revert reason
            if (result.length > 0) {
                assembly {
                    revert(add(32, result), mload(result))
                }
            }
            revert("LOP call failed");
        }
    }

    /**
     * @dev ITakerInteraction callback - called by LOP during order execution
     */
    function takerInteraction(
        IOrderMixin.Order calldata order,
        bytes calldata /* extension */,
        bytes32 orderHash,
        address taker,
        uint256 makingAmount,
        uint256 takingAmount,
        uint256 /* remainingMakingAmount */,
        bytes calldata extraData
    ) public override whenNotPaused {
        if (msg.sender != address(_LOPV4)) revert OnlyLOP();
        if (taker != address(this)) revert NotTaker();

        // Decode execution parameters
        (Address[] memory targets, bytes[] memory calldatas) = abi.decode(extraData, (Address[], bytes[]));

        // Perform external calls (e.g., DEX swaps)
        for (uint256 i = 0; i < targets.length; ++i) {
            // solhint-disable-next-line avoid-low-level-calls
            (bool success, bytes memory reason) = targets[i].get().call(calldatas[i]);
            if (!success) revert FailedExternalCall(i, reason);
        }

        // Calculate and track profit
        uint256 profit = _calculateProfit(order.takerAsset.get(), takingAmount, makingAmount);
        
        emit OrderSettled(orderHash, order.maker.get(), profit);
    }

    // ============ ETH-to-Cardano Internal Functions ============
    
    /**
     * @notice Deploy ETH source escrow
     * @dev Internal function to deploy source escrow on ETH side
     */
    function _deployETHSourceEscrow(
        IOrderMixin.Order calldata order,
        bytes32 orderHash,
        bytes32 r,
        bytes32 vs,
        uint256 amount,
        TakerTraits takerTraits,
        bytes calldata args
    ) internal returns (address ethEscrowAddress) {
        // Create immutables for ETH source escrow
        Immutables memory ethImmutables = Immutables({
            orderHash: orderHash,
            hashlock: _generateHashlock(orderHash),
            maker: order.maker,
            taker: Address.wrap(uint160(address(this))), // Resolver is the taker
            token: order.makerAsset, // ETH (address(0))
            amount: order.makingAmount,
            safetyDeposit: _calculateSafetyDeposit(order.makingAmount),
            timelocks: _generateTimelocks()
        });
        
        // Calculate escrow address
        ethEscrowAddress = _ESCROW_FACTORY.addressOfEscrowSrc(ethImmutables);
        
        // Send safety deposit to escrow
        (bool success,) = ethEscrowAddress.call{value: ethImmutables.safetyDeposit}("");
        require(success, "Safety deposit transfer failed");
        
        // Set target flag for taker traits
        takerTraits = TakerTraits.wrap(TakerTraits.unwrap(takerTraits) | uint256(1 << 251));
        
        // Prepend target address to args
        bytes memory argsMem = abi.encodePacked(ethEscrowAddress, args);
        
        // Fill the order through LOP (this locks the ETH)
        _LOPV4.fillOrderArgs(order, r, vs, amount, takerTraits, argsMem);
        
        emit EscrowSrcDeployed(ethEscrowAddress, orderHash, order.maker.get());
        
        return ethEscrowAddress;
    }
    
    /**
     * @notice Deploy Cardano destination escrow
     * @dev Internal function to deploy destination escrow on Cardano side
     */
    function _deployCardanoDestinationEscrow(
        IOrderMixin.Order calldata /* order */,
        bytes32 orderHash,
        bytes29 cardanoDestination,
        uint256 adaAmount
    ) internal returns (bytes32 cardanoTxHash) {
        // Create Cardano immutables
        ICardanoEscrowFactory.CardanoImmutables memory cardanoImmutables = ICardanoEscrowFactory.CardanoImmutables({
            orderHash: orderHash,
            hashlock: _generateHashlock(orderHash),
            cardanoMaker: cardanoDestination, // Where maker receives ADA
            cardanoTaker: _evmToCardanoAddress(address(this)), // Resolver as taker
            policyId: bytes32(0), // ADA has no policy ID
            assetName: bytes32(0), // ADA has no asset name
            amount: adaAmount,
            adaSafetyDeposit: _calculateCardanoSafetyDeposit(adaAmount),
            timelocks: _generateTimelocks()
        });
        
        // Create Cardano destination escrow
        cardanoTxHash = _CARDANO_ESCROW_FACTORY.createCardanoDstEscrow{value: msg.value}(
            cardanoImmutables,
            block.timestamp + 3600 // 1 hour cancellation window
        );
        
        // Calculate Cardano address
        bytes29 cardanoAddress = _CARDANO_ESCROW_FACTORY.addressOfCardanoEscrow(cardanoImmutables);
        
        emit CardanoEscrowCreated(
            orderHash,
            cardanoAddress,
            cardanoTxHash,
            adaAmount
        );
        
        return cardanoTxHash;
    }
    
    /**
     * @notice Withdraw ETH from source escrow using secret
     * @dev Internal function to claim ETH and reveal secret
     */
    function _withdrawFromETHEscrow(
        address escrowAddress,
        bytes32 secret,
        Immutables calldata immutables
    ) internal {
        // Call withdraw function on ETH escrow contract
        bytes memory withdrawCall = abi.encodeWithSignature(
            "withdraw(bytes32,(bytes32,bytes32,address,address,address,uint256,uint256,uint256))",
            secret,
            immutables
        );
        
        (bool success,) = escrowAddress.call(withdrawCall);
        require(success, "ETH escrow withdrawal failed");
    }
    
    /**
     * @notice Generate deterministic hashlock for both chains
     */
    function _generateHashlock(bytes32 orderHash) internal view returns (bytes32) {
        return keccak256(abi.encodePacked(orderHash, block.timestamp, block.prevrandao));
    }
    
    /**
     * @notice Generate timelocks for both escrows
     */
    function _generateTimelocks() internal view returns (Timelocks) {
        return TimelocksLib.setDeployedAt(Timelocks.wrap(0), block.timestamp);
    }
    
    /**
     * @notice Calculate safety deposit (1% of amount)
     */
    function _calculateSafetyDeposit(uint256 amount) internal pure returns (uint256) {
        return amount / 100;
    }
    
    /**
     * @notice Calculate Cardano safety deposit (minimum 2 ADA)
     */
    function _calculateCardanoSafetyDeposit(uint256 adaAmount) internal pure returns (uint256) {
        uint256 calculated = adaAmount / 100;
        return calculated < 2_000_000 ? 2_000_000 : calculated; // 2 ADA minimum in lovelace
    }
    
    /**
     * @notice Convert EVM address to Cardano address format
     */
    function _evmToCardanoAddress(address evmAddr) internal view returns (bytes29) {
        return _CARDANO_ESCROW_FACTORY.evmToCardanoAddress(evmAddr);
    }
    
    /**
     * @notice Calculate order hash
     */
    function _calculateOrderHash(IOrderMixin.Order calldata order) internal pure returns (bytes32) {
        return keccak256(abi.encode(order));
    }

    // ============ Utility Functions ============
    /**
     * @notice Allows the owner to make arbitrary calls to other contracts
     * @dev Useful for emergency operations or complex interactions
     */
    function arbitraryCalls(
        address[] calldata targets, 
        bytes[] calldata arguments
    ) external onlyOwner {
        uint256 length = targets.length;
        if (targets.length != arguments.length) revert LengthMismatch();
        
        for (uint256 i = 0; i < length; ++i) {
            // solhint-disable-next-line avoid-low-level-calls
            (bool success,) = targets[i].call(arguments[i]);
            if (!success) RevertReasonForwarder.reRevert();
        }
    }

    /**
     * @dev Calculate resolver profit from order execution
     */
    function _calculateProfit(
        address takerAsset,
        uint256 takingAmount,
        uint256 /* makingAmount */
    ) internal view returns (uint256 profit) {
        // This is a simplified profit calculation
        // In practice, you'd want to get actual market prices
        uint256 balance = IERC20(takerAsset).balanceOf(address(this));
        if (balance > takingAmount) {
            profit = balance - takingAmount;
        }
    }

    // ============ View Functions ============
    function getCurrentBaseFee() external view returns (uint256) {
        return block.basefee;
    }

    function getEscrowFactory() external view returns (address) {
        return address(_ESCROW_FACTORY);
    }

    function getCardanoEscrowFactory() external view returns (address) {
        return address(_CARDANO_ESCROW_FACTORY);
    }

    function getLimitOrderProtocol() external view returns (address) {
        return address(_LOPV4);
    }
}
