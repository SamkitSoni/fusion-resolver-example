import { ethers } from 'hardhat'
import { ResolverExample } from '../typechain-types'
import { expect } from 'chai'
import { Signer } from 'ethers'
import { ONE_INCH_LOP_V4 } from './helpers/constants'

describe('ResolverExample Cross-Chain Tests', function () {
    let resolverContract: ResolverExample
    let owner: Signer
    let user1: Signer
    let user2: Signer

    beforeEach(async function () {
        [owner, user1, user2] = await ethers.getSigners()

        // Deploy ResolverExample with actual 1inch LOP V4 address
        const ResolverExample = await ethers.getContractFactory('ResolverExample')
        resolverContract = await ResolverExample.deploy(ONE_INCH_LOP_V4) as unknown as ResolverExample
    })

    describe('Basic Functionality', function () {
        it('should deploy successfully', async function () {
            expect(await resolverContract.getAddress()).to.not.equal(ethers.ZeroAddress)
        })

        it('should allow owner to add authorized resolvers', async function () {
            const user1Address = await user1.getAddress()
            
            await resolverContract.addAuthorizedResolver(user1Address)
            expect(await resolverContract.authorizedResolvers(user1Address)).to.be.true
        })

        it('should allow depositing safety deposits', async function () {
            const depositAmount = ethers.parseEther('1')
            const user1Address = await user1.getAddress()
            
            await resolverContract.connect(user1).depositSafetyDeposit({
                value: depositAmount
            })
            
            expect(await resolverContract.getResolverSafetyDeposit(user1Address))
                .to.equal(depositAmount)
        })

        it('should allow withdrawing safety deposits', async function () {
            const depositAmount = ethers.parseEther('1')
            const user1Address = await user1.getAddress()
            
            // Deposit first
            await resolverContract.connect(user1).depositSafetyDeposit({
                value: depositAmount
            })
            
            // Then withdraw
            await resolverContract.connect(user1).withdrawSafetyDeposit(depositAmount)
            
            expect(await resolverContract.getResolverSafetyDeposit(user1Address))
                .to.equal(0)
        })

        it('should pause and unpause correctly', async function () {
            await resolverContract.pause()
            
            await expect(
                resolverContract.connect(user1).depositSafetyDeposit({value: ethers.parseEther('1')})
            ).to.be.revertedWithCustomError(resolverContract, 'EnforcedPause')
            
            await resolverContract.unpause()
            
            await expect(
                resolverContract.connect(user1).depositSafetyDeposit({value: ethers.parseEther('1')})
            ).to.not.be.reverted
        })

        it('should calculate auction prices correctly', async function () {
            const currentTime = Math.floor(Date.now() / 1000)
            const auctionParams = {
                startTime: currentTime,
                duration: 3600,
                initialPrice: ethers.parseEther('1'),
                minimumPrice: ethers.parseEther('0.5'),
                baseFee: ethers.parseUnits('20', 'gwei')
            }
            
            const price = await resolverContract.calculateAuctionPrice(auctionParams)
            expect(price).to.be.gte(auctionParams.minimumPrice)
            expect(price).to.be.lte(auctionParams.initialPrice)
        })
    })

    describe('Access Control', function () {
        it('should revert when non-owner tries to add resolver', async function () {
            const user2Address = await user2.getAddress()
            
            await expect(
                resolverContract.connect(user1).addAuthorizedResolver(user2Address)
            ).to.be.revertedWithCustomError(resolverContract, 'OnlyOwner')
        })

        it('should revert when non-owner tries to pause', async function () {
            await expect(
                resolverContract.connect(user1).pause()
            ).to.be.revertedWithCustomError(resolverContract, 'OnlyOwner')
        })
    })

    describe('Emergency Functions', function () {
        it('should handle direct ETH transfers', async function () {
            const user1Address = await user1.getAddress()
            const amount = ethers.parseEther('0.5')
            
            await user1.sendTransaction({
                to: await resolverContract.getAddress(),
                value: amount
            })
            
            expect(await resolverContract.getResolverSafetyDeposit(user1Address))
                .to.equal(amount)
        })
    })
})
