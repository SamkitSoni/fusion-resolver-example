import { ethers } from 'hardhat'
import { ResolverExample } from '../typechain-types'

async function main() {
    const resolverAddress = process.env.RESOLVER_ADDRESS
    if (!resolverAddress) {
        throw new Error('Please set RESOLVER_ADDRESS environment variable')
    }

    console.log(`Interacting with ResolverExample at: ${resolverAddress}`)

    const [user] = await ethers.getSigners()
    console.log(`Using account: ${user.address}`)

    // Connect to the deployed contract
    const resolver = await ethers.getContractAt('ResolverExample', resolverAddress) as ResolverExample

    // Test 1: Deposit safety deposit
    console.log('\n=== Test 1: Deposit Safety Deposit ===')
    const depositAmount = ethers.parseEther('0.1')
    console.log(`Depositing ${ethers.formatEther(depositAmount)} ETH as safety deposit...`)
    
    const depositTx = await resolver.depositSafetyDeposit({ value: depositAmount })
    await depositTx.wait()
    console.log(`Deposit transaction: ${depositTx.hash}`)

    const safetyDeposit = await resolver.getResolverSafetyDeposit(user.address)
    console.log(`Current safety deposit balance: ${ethers.formatEther(safetyDeposit)} ETH`)

    // Test 2: Check auction pricing
    console.log('\n=== Test 2: Calculate Auction Price ===')
    const currentTime = Math.floor(Date.now() / 1000)
    const auctionParams = {
        startTime: currentTime,
        duration: 3600, // 1 hour
        initialPrice: ethers.parseEther('1.0'),
        minimumPrice: ethers.parseEther('0.5'),
        baseFee: ethers.parseUnits('20', 'gwei')
    }

    const currentPrice = await resolver.calculateAuctionPrice(auctionParams)
    console.log(`Current auction price: ${ethers.formatEther(currentPrice)} ETH`)

    // Test 3: Check if user is authorized resolver
    console.log('\n=== Test 3: Check Authorization ===')
    const isAuthorized = await resolver.authorizedResolvers(user.address)
    console.log(`User ${user.address} is authorized resolver: ${isAuthorized}`)

    // Test 4: Get contract state
    console.log('\n=== Test 4: Contract State ===')
    const baseFee = await resolver.getCurrentBaseFee()
    console.log(`Current base fee: ${baseFee} wei`)
    console.log(`Current base fee: ${ethers.formatUnits(baseFee, 'gwei')} gwei`)

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
    const withdrawAmount = ethers.parseEther('0.05')
    try {
        const withdrawTx = await resolver.withdrawSafetyDeposit(withdrawAmount)
        await withdrawTx.wait()
        console.log(`Successfully withdrew ${ethers.formatEther(withdrawAmount)} ETH`)
        console.log(`Withdrawal transaction: ${withdrawTx.hash}`)

        const remainingDeposit = await resolver.getResolverSafetyDeposit(user.address)
        console.log(`Remaining safety deposit: ${ethers.formatEther(remainingDeposit)} ETH`)
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
