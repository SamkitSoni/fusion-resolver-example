import hre from 'hardhat'
import { FusionResolverEthereum } from '../typechain-types'

async function main() {
    const resolverAddress = process.env.RESOLVER_ADDRESS
    if (!resolverAddress) {
        throw new Error('Please set RESOLVER_ADDRESS environment variable')
    }

    console.log(`Interacting with FusionResolverEthereum at: ${resolverAddress}`)

    const [user] = await hre.ethers.getSigners()
    console.log(`Using account: ${user.address}`)

    // Connect to the deployed contract
    const resolver = await hre.ethers.getContractAt('FusionResolverEthereum', resolverAddress) as FusionResolverEthereum

    // Test 1: Deposit safety deposit
    console.log('\n=== Test 1: Deposit Safety Deposit ===')
    const depositAmount = hre.ethers.parseEther('0.1')
    console.log(`Depositing ${hre.ethers.formatEther(depositAmount)} ETH as safety deposit...`)
    
    const depositTx = await resolver.depositSafetyDeposit({ value: depositAmount })
    await depositTx.wait()
    console.log(`Deposit transaction: ${depositTx.hash}`)

    const safetyDeposit = await resolver.getResolverSafetyDeposit(user.address)
    console.log(`Current safety deposit balance: ${hre.ethers.formatEther(safetyDeposit)} ETH`)

    // Test 2: Check auction pricing
    console.log('\n=== Test 2: Calculate Auction Price ===')
    const currentTime = Math.floor(Date.now() / 1000)
    const auctionParams = {
        startTime: currentTime,
        duration: 3600, // 1 hour
        initialPrice: hre.ethers.parseEther('1.0'),
        minimumPrice: hre.ethers.parseEther('0.5'),
        baseFee: hre.ethers.parseUnits('20', 'gwei')
    }

    const currentPrice = await resolver.calculateAuctionPrice(auctionParams)
    console.log(`Current auction price: ${hre.ethers.formatEther(currentPrice)} ETH`)

    // Test 3: Check if user is authorized resolver
    console.log('\n=== Test 3: Check Authorization ===')
    const isAuthorized = await resolver.authorizedResolvers(user.address)
    console.log(`User ${user.address} is authorized resolver: ${isAuthorized}`)

    // Test 4: Get contract state
    console.log('\n=== Test 4: Contract State ===')
    const baseFee = await resolver.getCurrentBaseFee()
    console.log(`Current base fee: ${baseFee} wei`)
    console.log(`Current base fee: ${hre.ethers.formatUnits(baseFee, 'gwei')} gwei`)

    // Test 5: Try to authorize user as resolver (will fail if not owner)
    console.log('\n=== Test 5: Try to Authorize User (may fail if not owner) ===')
    try {
        const authTx = await resolver.addAuthorizedResolver(user.address)
        await authTx.wait()
        console.log(`Successfully authorized ${user.address} as resolver`)
        console.log(`Authorization transaction: ${authTx.hash}`)
    } catch (error: any) {
        console.log(`Failed to authorize user (expected if not owner): ${error.message}`)
    }

    // Test 6: Partial withdrawal of safety deposit
    console.log('\n=== Test 6: Partial Withdrawal ===')
    const withdrawAmount = hre.ethers.parseEther('0.05')
    try {
        const withdrawTx = await resolver.withdrawSafetyDeposit(withdrawAmount)
        await withdrawTx.wait()
        console.log(`Successfully withdrew ${hre.ethers.formatEther(withdrawAmount)} ETH`)
        console.log(`Withdrawal transaction: ${withdrawTx.hash}`)

        const remainingDeposit = await resolver.getResolverSafetyDeposit(user.address)
        console.log(`Remaining safety deposit: ${hre.ethers.formatEther(remainingDeposit)} ETH`)
    } catch (error: any) {
        console.log(`Withdrawal failed: ${error.message}`)
    }

    console.log('\n=== Interaction Complete ===')
}

if (require.main === module) {
    main()
        .then(() => process.exit(0))
        .catch((error) => {
            console.error(error)
            process.exit(1)
        })
}

export { main as interactWithResolver }
