import hre from 'hardhat'
import { network } from 'hardhat'
import { ResolverExample } from '../typechain-types'

async function main() {
    console.log(`Deploying ResolverExample with cross-chain capabilities to ${network.name}...`)

    // 1inch LOP V4 addresses on different networks
    const LOP_ADDRESSES: { [key: string]: string } = {
        mainnet: '0x111111125421ca6dc452d289314280a0f8842a65',
        polygon: '0x111111125421ca6dc452d289314280a0f8842a65',
        arbitrum: '0x111111125421ca6dc452d289314280a0f8842a65',
        optimism: '0x111111125421ca6dc452d289314280a0f8842a65',
        bsc: '0x111111125421ca6dc452d289314280a0f8842a65',
        avalanche: '0x111111125421ca6dc452d289314280a0f8842a65',
        hardhat: '0x111111125421ca6dc452d289314280a0f8842a65', // Use mainnet address for testing
        localhost: '0x111111125421ca6dc452d289314280a0f8842a65'
    }

    // Cross-chain swap escrow factory addresses (you'll need to update these with actual deployed addresses)
    const ESCROW_FACTORY_ADDRESSES: { [key: string]: string } = {
        mainnet: '0x0000000000000000000000000000000000000000', // Replace with actual address
        polygon: '0x0000000000000000000000000000000000000000', // Replace with actual address
        arbitrum: '0x0000000000000000000000000000000000000000', // Replace with actual address
        optimism: '0x0000000000000000000000000000000000000000', // Replace with actual address
        bsc: '0x0000000000000000000000000000000000000000', // Replace with actual address
        avalanche: '0x0000000000000000000000000000000000000000', // Replace with actual address
        hardhat: '0x0000000000000000000000000000000000000001', // Mock address for testing
        localhost: '0x0000000000000000000000000000000000000001' // Mock address for testing
    }

    const lopAddress = LOP_ADDRESSES[network.name]
    const escrowFactoryAddress = ESCROW_FACTORY_ADDRESSES[network.name]
    
    if (!lopAddress) {
        throw new Error(`LOP address not configured for network: ${network.name}`)
    }
    
    if (!escrowFactoryAddress) {
        throw new Error(`Escrow factory address not configured for network: ${network.name}`)
    }

    console.log(`Using LOP address: ${lopAddress}`)
    console.log(`Using Escrow Factory address: ${escrowFactoryAddress}`)

    const [deployer] = await hre.ethers.getSigners()
    console.log(`Deploying with account: ${deployer.address}`)
    console.log(`Account balance: ${hre.ethers.formatEther(await hre.ethers.provider.getBalance(deployer.address))} ETH`)

    // Deploy ResolverExample with cross-chain capabilities
    const ResolverFactory = await hre.ethers.getContractFactory('ResolverExample')
    const resolver = await ResolverFactory.deploy(lopAddress, escrowFactoryAddress, deployer.address) as ResolverExample

    await resolver.waitForDeployment()
    const resolverAddress = await resolver.getAddress()

    console.log(`ResolverExample deployed to: ${resolverAddress}`)

    // Verify deployment
    console.log('Verifying deployment...')
    const currentBaseFee = await resolver.getCurrentBaseFee()
    console.log(`Contract deployed successfully, current base fee: ${currentBaseFee}`)
    console.log(`Deployer address: ${deployer.address}`)

    // Send some ETH to the contract for gas and safety deposits if on testnet
    if (network.name !== 'mainnet') {
        const fundAmount = hre.ethers.parseEther('1.0')
        console.log(`Funding contract with ${hre.ethers.formatEther(fundAmount)} ETH...`)
        
        const tx = await deployer.sendTransaction({
            to: resolverAddress,
            value: fundAmount
        })
        await tx.wait()
        
        console.log(`Contract funded with ${hre.ethers.formatEther(fundAmount)} ETH`)
    }

    // Log deployment info
    console.log('\n=== DEPLOYMENT SUMMARY ===')
    console.log(`Network: ${network.name}`)
    console.log(`ResolverExample: ${resolverAddress}`)
    console.log(`LOP Address: ${lopAddress}`)
    console.log(`Escrow Factory: ${escrowFactoryAddress}`)
    console.log(`Deployer: ${deployer.address}`)
    console.log(`Gas used: Check transaction receipt`)
    
    if (network.name !== 'hardhat' && network.name !== 'localhost') {
        console.log('\nTo verify on Etherscan, run:')
        console.log(`npx hardhat verify --network ${network.name} ${resolverAddress} "${lopAddress}" "${escrowFactoryAddress}" "${deployer.address}"`)
    }

    return {
        resolverAddress,
        lopAddress,
        escrowFactoryAddress,
        deployer: deployer.address
    }
}

// Only run if this script is called directly
if (require.main === module) {
    main()
        .then(() => process.exit(0))
        .catch((error) => {
            console.error(error)
            process.exit(1)
        })
}

export { main as deployFusionResolver }
