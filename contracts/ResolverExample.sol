// SPDX-License-Identifier: MIT
// This code is provided “as is” without warranties of any kind. 
// 1inch does not assume responsibility for its security, suitability, or fitness for any specific use. 
// Any party using this code is solely responsible for conducting independent audits before deployment.

pragma solidity ^0.8.20;

import { IERC20 } from "@openzeppelin/contracts/token/ERC20/IERC20.sol";
import { ReentrancyGuard } from "@openzeppelin/contracts/utils/ReentrancyGuard.sol";
import { Pausable } from "@openzeppelin/contracts/utils/Pausable.sol";
import { Address, AddressLib} from "@1inch/solidity-utils/contracts/libraries/AddressLib.sol";
import { SafeERC20 } from "@1inch/solidity-utils/contracts/libraries/SafeERC20.sol";
import { IOrderMixin } from "@1inch/limit-order-protocol-contract/contracts/interfaces/IOrderMixin.sol";
import { ITakerInteraction } from "@1inch/limit-order-protocol-contract/contracts/interfaces/ITakerInteraction.sol";

/**
 * @title ResolverExample - Enhanced Cross-Chain Fusion+ Resolver
 * @dev Extended resolver with Ethereum-Cardano cross-chain atomic swap capabilities
 */
contract ResolverExample is ITakerInteraction, ReentrancyGuard, Pausable {
    error OnlyOwner();
    error NotTaker();
    error OnlyLOP();
    error FailedExternalCall(uint256 index, bytes reason);
    error EscrowNotFound();
    error EscrowAlreadyCompleted();
    error InvalidSecret();
    error TimelockNotExpired();
    error TimelockExpired();
    error InsufficientBalance();
    error InvalidParameters();
    error UnauthorizedResolver();

    using SafeERC20 for IERC20;
    using AddressLib for Address;

    // ============ Events ============
    event EscrowCreated(
        uint256 indexed escrowId,
        bytes32 indexed secretHash,
        address indexed token,
        uint256 amount,
        address beneficiary,
        uint256 timelock,
        string destinationChain,
        string cardanoAddress
    );
    
    event EscrowCompleted(
        uint256 indexed escrowId,
        bytes32 secret,
        address indexed completer
    );
    
    event EscrowCancelled(
        uint256 indexed escrowId,
        address indexed canceller,
        uint256 safetyDepositClaimed
    );
    
    event OrderSettled(
        bytes32 indexed orderHash,
        address indexed maker,
        uint256 profit
    );
    
    event SafetyDepositDeposited(
        address indexed depositor,
        uint256 amount
    );

    // ============ Structs ============
    struct Escrow {
        bytes32 secretHash;
        uint256 timelock;
        uint256 amount;
        uint256 safetyDeposit;
        address token;
        address beneficiary;
        address resolver;
        bool completed;
        bool cancelled;
        string destinationChain;
        string cardanoAddress;
    }

    struct AuctionParams {
        uint256 startTime;
        uint256 duration;
        uint256 initialPrice;
        uint256 minimumPrice;
        uint256 baseFee;
    }

    // ============ State Variables ============
    IOrderMixin private immutable _LOPV4;
    address private immutable _OWNER;
    
    uint256 private _escrowCounter;
    mapping(uint256 => Escrow) public escrows;
    mapping(bytes32 => uint256) public secretHashToEscrow;
    mapping(address => uint256) public safetyDeposits;
    mapping(bytes32 => bool) public usedSecrets;
    
    // Emergency pause functionality
    mapping(address => bool) public authorizedResolvers;
    
    // Gas price tracking for dynamic pricing
    uint256 public lastBaseFee;
    uint256 public constant MAX_GAS_PRICE_MULTIPLIER = 150; // 1.5x

    // ============ Modifiers ============
    modifier onlyOwner () {
        if (msg.sender != _OWNER) revert OnlyOwner();
        _;
    }

    modifier onlyAuthorizedResolver() {
        if (!authorizedResolvers[msg.sender] && msg.sender != _OWNER) {
            revert UnauthorizedResolver();
        }
        _;
    }

    modifier validEscrow(uint256 escrowId) {
        if (escrowId >= _escrowCounter) revert EscrowNotFound();
        if (escrows[escrowId].completed || escrows[escrowId].cancelled) {
            revert EscrowAlreadyCompleted();
        }
        _;
    }

    // ============ Constructor ============
    constructor(IOrderMixin limitOrderProtocol) {
        _LOPV4 = limitOrderProtocol;
        _OWNER = msg.sender;
        authorizedResolvers[msg.sender] = true;
        lastBaseFee = block.basefee;
    }

    // ============ Owner Functions ============
    function addAuthorizedResolver(address resolver) external onlyOwner {
        authorizedResolvers[resolver] = true;
    }

    function removeAuthorizedResolver(address resolver) external onlyOwner {
        authorizedResolvers[resolver] = false;
    }

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
        token.safeTransfer(_OWNER, amount);
    }

    // ============ Escrow Functions ============
    /**
     * @dev Create a new escrow for Ethereum-Cardano atomic swap
     */
    function createEscrow(
        bytes32 secretHash,
        uint256 timelock,
        address token,
        uint256 amount,
        address beneficiary,
        uint256 safetyDepositAmount,
        string calldata destinationChain,
        string calldata cardanoAddress
    ) external onlyAuthorizedResolver whenNotPaused nonReentrant returns (uint256 escrowId) {
        if (timelock <= block.timestamp) revert InvalidParameters();
        if (amount == 0 || beneficiary == address(0)) revert InvalidParameters();
        if (secretHashToEscrow[secretHash] != 0) revert InvalidParameters();
        if (bytes(destinationChain).length == 0) revert InvalidParameters();
        
        // Validate destination chain is supported (only "cardano" for now)
        if (keccak256(bytes(destinationChain)) != keccak256(bytes("cardano"))) {
            revert InvalidParameters();
        }

        // Transfer tokens to escrow
        IERC20(token).safeTransferFrom(msg.sender, address(this), amount);
        
        // Handle safety deposit
        if (safetyDepositAmount > 0) {
            if (safetyDeposits[msg.sender] < safetyDepositAmount) {
                revert InsufficientBalance();
            }
            safetyDeposits[msg.sender] -= safetyDepositAmount;
        }

        escrowId = _escrowCounter++;
        
        escrows[escrowId] = Escrow({
            secretHash: secretHash,
            timelock: timelock,
            amount: amount,
            safetyDeposit: safetyDepositAmount,
            token: token,
            beneficiary: beneficiary,
            resolver: msg.sender,
            completed: false,
            cancelled: false,
            destinationChain: destinationChain,
            cardanoAddress: cardanoAddress
        });

        secretHashToEscrow[secretHash] = escrowId;

        emit EscrowCreated(escrowId, secretHash, token, amount, beneficiary, timelock, destinationChain, cardanoAddress);
    }

    /**
     * @dev Withdraw from escrow using the secret
     */
    function withdrawWithSecret(
        uint256 escrowId,
        bytes32 secret
    ) external validEscrow(escrowId) whenNotPaused nonReentrant {
        Escrow storage escrow = escrows[escrowId];
        
        if (block.timestamp >= escrow.timelock) revert TimelockExpired();
        if (keccak256(abi.encodePacked(secret)) != escrow.secretHash) revert InvalidSecret();
        if (usedSecrets[secret]) revert InvalidSecret();

        escrow.completed = true;
        usedSecrets[secret] = true;

        // Transfer tokens to beneficiary
        IERC20(escrow.token).safeTransfer(escrow.beneficiary, escrow.amount);

        // Return safety deposit to resolver
        if (escrow.safetyDeposit > 0) {
            safetyDeposits[escrow.resolver] += escrow.safetyDeposit;
        }

        emit EscrowCompleted(escrowId, secret, msg.sender);
    }

    /**
     * @dev Cancel expired escrow and claim safety deposit
     */
    function cancelExpiredEscrow(
        uint256 escrowId
    ) external validEscrow(escrowId) whenNotPaused nonReentrant {
        Escrow storage escrow = escrows[escrowId];
        
        if (block.timestamp < escrow.timelock) revert TimelockNotExpired();

        escrow.cancelled = true;

        // Return tokens to resolver (original depositor)
        IERC20(escrow.token).safeTransfer(escrow.resolver, escrow.amount);

        // Award safety deposit to canceller (incentive for cleanup)
        uint256 safetyDepositReward = escrow.safetyDeposit;
        if (safetyDepositReward > 0) {
            safetyDeposits[msg.sender] += safetyDepositReward;
        }

        emit EscrowCancelled(escrowId, msg.sender, safetyDepositReward);
    }

    /**
     * @dev Deposit safety deposit for future escrows
     */
    function depositSafetyDeposit() external payable whenNotPaused {
        safetyDeposits[msg.sender] += msg.value;
        emit SafetyDepositDeposited(msg.sender, msg.value);
    }

    /**
     * @dev Withdraw safety deposit
     */
    function withdrawSafetyDeposit(uint256 amount) external nonReentrant {
        if (safetyDeposits[msg.sender] < amount) revert InsufficientBalance();
        
        safetyDeposits[msg.sender] -= amount;
        payable(msg.sender).transfer(amount);
    }

    // ============ LOP Integration Functions ============
    function settleOrders(bytes calldata data) external onlyAuthorizedResolver whenNotPaused {
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

    // ============ Auction & Pricing Functions ============
    /**
     * @dev Calculate Dutch auction price considering gas costs
     */
    function calculateAuctionPrice(
        AuctionParams memory params
    ) public view returns (uint256 currentPrice) {
        if (block.timestamp < params.startTime) {
            return params.initialPrice;
        }

        uint256 elapsed = block.timestamp - params.startTime;
        if (elapsed >= params.duration) {
            return _adjustPriceForGas(params.minimumPrice, params.baseFee);
        }

        // Linear price decay
        uint256 priceDecay = ((params.initialPrice - params.minimumPrice) * elapsed) / params.duration;
        uint256 basePrice = params.initialPrice - priceDecay;
        
        return _adjustPriceForGas(basePrice, params.baseFee);
    }

    /**
     * @dev Adjust price based on current gas conditions
     */
    function _adjustPriceForGas(uint256 basePrice, uint256 expectedBaseFee) internal view returns (uint256) {
        if (block.basefee <= expectedBaseFee) {
            return basePrice;
        }

        // Adjust price upward if gas is more expensive than expected
        uint256 gasMultiplier = (block.basefee * 100) / expectedBaseFee;
        if (gasMultiplier > MAX_GAS_PRICE_MULTIPLIER) {
            gasMultiplier = MAX_GAS_PRICE_MULTIPLIER;
        }

        return (basePrice * gasMultiplier) / 100;
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
    function getEscrow(uint256 escrowId) external view returns (Escrow memory) {
        return escrows[escrowId];
    }

    function getEscrowBySecretHash(bytes32 secretHash) external view returns (Escrow memory) {
        uint256 escrowId = secretHashToEscrow[secretHash];
        return escrows[escrowId];
    }

    function isEscrowActive(uint256 escrowId) external view returns (bool) {
        if (escrowId >= _escrowCounter) return false;
        Escrow memory escrow = escrows[escrowId];
        return !escrow.completed && !escrow.cancelled && block.timestamp < escrow.timelock;
    }

    function getResolverSafetyDeposit(address resolver) external view returns (uint256) {
        return safetyDeposits[resolver];
    }

    function getCurrentBaseFee() external view returns (uint256) {
        return block.basefee;
    }

    // ============ Emergency Functions ============
    receive() external payable {
        safetyDeposits[msg.sender] += msg.value;
        emit SafetyDepositDeposited(msg.sender, msg.value);
    }
}
