# Enhanced Cross-Chain Fusion+ Resolver

This project demonstrates an enhanced implementation of a 1inch Fusion+ resolver with **cross-chain atomic swap capabilities** between Ethereum and Cardano networks. The resolver extends the standard 1inch Limit Order Protocol (LOP) with additional features for cross-chain coordination, safety deposits, and Dutch auction mechanisms.

## 🚀 Features

### Core Functionality
- **1inch Fusion+ Integration**: Full compatibility with 1inch LOP V4
- **Cross-Chain Atomic Swaps**: Ethereum ↔ Cardano using Hash Time-Locked Contracts (HTLCs)
- **Safety Deposit System**: Collateral management for resolvers
- **Dutch Auction Pricing**: Dynamic pricing mechanism for orders
- **Emergency Controls**: Pause/unpause functionality with admin controls

### Cross-Chain Capabilities
- **Escrow Management**: Secure cross-chain order coordination
- **Timelock Support**: Configurable time-based conditions
- **Secret Hash Validation**: Cryptographic proof verification
- **Multi-Chain State Tracking**: Comprehensive cross-chain state management

## 📋 Architecture

### Smart Contracts

#### ResolverExample.sol
Enhanced resolver contract with cross-chain capabilities:
- **Base**: OpenZeppelin ReentrancyGuard, Pausable
- **1inch Imports**: Uses original @1inch/solidity-utils and @1inch/limit-order-protocol-contract packages
- **Interfaces**: ITakerInteraction, IOrderMixin from 1inch packages
- **Features**: Cross-chain escrows, safety deposits, auction pricing

#### Dependencies
- `@1inch/solidity-utils`: Core utilities (AddressLib, SafeERC20, RevertReasonForwarder)
- `@1inch/limit-order-protocol-contract`: LOP interfaces (IOrderMixin, ITakerInteraction)
- `@openzeppelin/contracts`: Standard security and utility contracts

#### Cardano Integration
- `FusionResolverCardano.hs`: Complete Plutus smart contract
- Full PAB (Plutus Application Backend) integration
- HTLC implementation for atomic swaps

## 🛠️ Installation & Setup

### Prerequisites
- Node.js >= 16.x
- npm or yarn
- Hardhat development environment
- (Optional) Haskell/Plutus for Cardano development

### Installation

```bash
# Clone and install dependencies
cd resolver/fusion-resolver-example
npm install

# Compile contracts
npx hardhat compile

# Run tests
npm test

# Run only cross-chain tests
npm run test:fusion
```

## 📊 Testing

### Test Coverage
The project includes comprehensive test suites:

#### Cross-Chain Tests (`FusionResolverEthereum.test.ts`)
- ✅ Basic functionality tests
- ✅ Access control verification
- ✅ Safety deposit management
- ✅ Auction price calculations
- ✅ Emergency function testing

```bash
# Run cross-chain specific tests
npm run test:fusion

# All tests
npm test
```

**Test Results**: 9/9 tests passing ✅

### Test Categories
1. **Basic Functionality**: Deployment, resolver authorization, deposits
2. **Access Control**: Owner-only functions, unauthorized access prevention
3. **Emergency Functions**: Pause/unpause, ETH handling

## 🚀 Deployment

### Local Deployment

```bash
# Deploy to local hardhat network
npm run deploy

# Deploy to specific network
npm run deploy:localhost
```

### Production Deployment

The deployment script automatically configures the correct 1inch LOP addresses for different networks:
- **Mainnet**: `0x111111125421ca6dc452d289314280a0f8842a65`
- **Polygon**: `0x111111125421ca6dc452d289314280a0f8842a65`
- **Arbitrum**: `0x111111125421ca6dc452d289314280a0f8842a65`
- **And more...**

## 📖 Usage Examples

### Basic Resolver Operations

```typescript
// Deploy resolver
const resolver = await ResolverFactory.deploy(lopAddress);

// Add authorized resolver
await resolver.addAuthorizedResolver(resolverAddress);

// Deposit safety deposit
await resolver.depositSafetyDeposit({ value: ethers.parseEther("1.0") });

// Calculate auction price
const price = await resolver.calculateAuctionPrice({
    startTime: startTime,
    duration: 3600,
    initialPrice: ethers.parseEther("1.0"),
    minimumPrice: ethers.parseEther("0.5"),
    baseFee: ethers.parseUnits("20", "gwei")
});
```

### Cross-Chain Operations

```typescript
// Create cross-chain escrow
await resolver.createEscrow(
    secretHash,
    tokenAddress,
    amount,
    beneficiaryAddress,
    timelock,
    "cardano",
    cardanoAddress
);

// Complete escrow with secret
await resolver.withdrawWithSecret(escrowId, secret);
```

## 🔧 Configuration

### Network Configuration
The resolver supports multiple networks with automatic LOP address configuration:

```typescript
const LOP_ADDRESSES = {
    mainnet: '0x111111125421ca6dc452d289314280a0f8842a65',
    polygon: '0x111111125421ca6dc452d289314280a0f8842a65',
    // ... other networks
};
```

### Environment Variables
- Network-specific configurations in `hardhat.config.ts`
- Gas optimization settings
- Compiler configurations

## 🛡️ Security Features

### Access Control
- **Owner-only functions**: Critical operations restricted to contract owner
- **Authorized resolvers**: Whitelist system for trusted resolvers
- **Pause mechanism**: Emergency stop functionality

### Cross-Chain Security
- **Timelock protection**: Time-based security for cross-chain operations
- **Secret validation**: Cryptographic proof requirements
- **Escrow management**: Secure fund handling during cross-chain swaps

### Audit Considerations
- ReentrancyGuard protection on critical functions
- SafeERC20 for token operations
- Input validation and error handling
- Emergency withdrawal capabilities

## 📚 Additional Resources

### Documentation
- [1inch Fusion+ Documentation](https://docs.1inch.io/docs/fusion-plus/introduction)
- [Limit Order Protocol](https://docs.1inch.io/docs/limit-order-protocol/introduction)
- [Hardhat Documentation](https://hardhat.org/docs)

### Related Projects
- [1inch Fusion SDK](https://github.com/1inch/fusion-sdk)
- [OpenZeppelin Contracts](https://github.com/OpenZeppelin/openzeppelin-contracts)

## 🤝 Contributing

1. Fork the repository
2. Create your feature branch (`git checkout -b feature/amazing-feature`)
3. Commit your changes (`git commit -m 'Add amazing feature'`)
4. Push to the branch (`git push origin feature/amazing-feature`)
5. Open a Pull Request

## ⚖️ License

This project is licensed under the MIT License - see the [LICENSE](LICENSE.md) file for details.

## ⚠️ Disclaimer

This code is provided "as is" without warranties of any kind. 1inch does not assume responsibility for its security, suitability, or fitness for any specific use. Any party using this code is solely responsible for conducting independent audits before deployment.

## 🔗 Links

- **1inch**: https://1inch.io/
- **Fusion+**: https://1inch.io/fusion/
- **Documentation**: https://docs.1inch.io/

---

**Built with ❤️ for the DeFi community**