// SPDX-License-Identifier: MIT

pragma solidity ^0.8.20;

import { IOrderMixin } from "@1inch/limit-order-protocol-contract/contracts/interfaces/IOrderMixin.sol";
import { TakerTraits } from "@1inch/limit-order-protocol-contract/contracts/libraries/TakerTraitsLib.sol";
import { Address } from "@1inch/solidity-utils/contracts/libraries/AddressLib.sol";
import { Timelocks } from "../libraries/TimelocksLib.sol";

// Interface for BaseEscrow Immutables - matches cross-chain-swap exactly
struct Immutables {
    bytes32 orderHash;
    bytes32 hashlock;
    Address maker;
    Address taker;
    Address token;
    uint256 amount;
    uint256 safetyDeposit;
    Timelocks timelocks;
}

// Interface for escrow factory - matches IEscrowFactory from cross-chain-swap
interface IEscrowFactory {
    /**
     * @notice Creates a new escrow contract for taker on the destination chain.
     * @dev The caller must send the safety deposit in the native token along with the function call
     * and approve the destination token to be transferred to the created escrow.
     * @param dstImmutables The immutables of the escrow contract that are used in deployment.
     * @param srcCancellationTimestamp The start of the cancellation period for the source chain.
     */
    function createDstEscrow(Immutables calldata dstImmutables, uint256 srcCancellationTimestamp) external payable;

    /**
     * @notice Returns the deterministic address of the source escrow based on the salt.
     * @param immutables The immutable arguments used to compute salt for escrow deployment.
     * @return The computed address of the escrow.
     */
    function addressOfEscrowSrc(Immutables calldata immutables) external view returns (address);

    /**
     * @notice Returns the deterministic address of the destination escrow based on the salt.
     * @param immutables The immutable arguments used to compute salt for escrow deployment.
     * @return The computed address of the escrow.
     */
    function addressOfEscrowDst(Immutables calldata immutables) external view returns (address);
}

/**
 * @title Interface for the sample implementation of a Resolver contract for cross-chain swap.
 */
interface IResolverExample {
    error InvalidLength();
    error LengthMismatch();

    /**
     * @notice Deploys a new escrow contract for maker on the source chain.
     * @param immutables The immutables of the escrow contract that are used in deployment.
     * @param order Order quote to fill.
     * @param r R component of signature.
     * @param vs VS component of signature.
     * @param amount Taker amount to fill
     * @param takerTraits Specifies threshold as maximum allowed takingAmount when takingAmount is zero, otherwise specifies
     * minimum allowed makingAmount. The 2nd (0 based index) highest bit specifies whether taker wants to skip maker's permit.
     * @param args Arguments that are used by the taker (target, extension, interaction, permit).
     */
    function deploySrc(
        Immutables calldata immutables,
        IOrderMixin.Order calldata order,
        bytes32 r,
        bytes32 vs,
        uint256 amount,
        TakerTraits takerTraits,
        bytes calldata args
    ) external;

    /**
     * @notice Deploys a new escrow contract for taker on the destination chain.
     * @param dstImmutables The immutables of the escrow contract that are used in deployment.
     * @param srcCancellationTimestamp The start of the cancellation period for the source chain.
     */
    function deployDst(Immutables calldata dstImmutables, uint256 srcCancellationTimestamp) external payable;

    /**
     * @notice Allows the owner to make arbitrary calls to other contracts on behalf of this contract.
     * @param targets The addresses of the contracts to call.
     * @param arguments The arguments to pass to the contract calls.
     */
    function arbitraryCalls(address[] calldata targets, bytes[] calldata arguments) external;
}
