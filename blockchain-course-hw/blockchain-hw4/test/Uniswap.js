const { expect } = require("chai");
const { Contract } = require("ethers");

describe ("Uniswap", function() {

  async function deployByName(name) {
      const [deployer] = await hre.ethers.getSigners();
      console.log(
        "         Deploying " , name,  "\n         The deployer is:",
        deployer.address
        );

      const Factory = await hre.ethers.getContractFactory(name);
      const factory = await Factory.deploy();
      console.log("         Contract ", name, "deployed to address:", factory.address);
      return [factory, deployer];
    }

    async function deployFirst() {
      const [second, deployer2] = await deployByName("First");
      return [second, deployer2];
    }

    async function deploySecond(name) {
      const [second, deployer2] = await deployByName("Second");
      return [second, deployer2];
    }

  describe("Deploy tokens: ", function() {
      it ("Should have right owners", async function() {
        const [first,deployer1] = await deployFirst();
        const [second, deployer2] = await deploySecond();
        expect(await first.owner()).to.equal(deployer1.address);
        expect(await second.owner()).to.equal(deployer2.address);
      });

      it ("Check balances", async function() {
        const [first,deployer1] = await deployFirst();
        const [second, deployer2] = await deploySecond();
        expect(await first.balanceOf(deployer1.address)).to.equal(await first.totalSupply());
        expect(await first.balanceOf(deployer2.address)).to.equal(await second.totalSupply());
      });
  });

  describe("Swap tokens: ", function() {
    it ("Swap should be completed", async function() {
      const [first,deployer1] = await deployFirst();
      const [second, deployer2] = await deploySecond();
      expect(deployer1.address).to.equal(deployer2.address);

      const IUniswapV2Factory = require('@uniswap/v2-core/build/IUniswapV2Factory.json');
      const uniswapAddress = require('../data/uniswap_addr.json');
      const UniswapV2Pair = require('@uniswap/v2-core/build/IUniswapV2Pair.json');

      const IUniswapV2FactoryContract = new Contract(uniswapAddress, IUniswapV2Factory.abi, deployer1);

      const transaction = await IUniswapV2FactoryContract.createPair(first.address, second.address);
      const transactionReceipt = await transaction.wait();
      expect(transactionReceipt.events[0].event == "PairCreated", 'PairCreated event had to be called');
      //console.log("pair addr: ", res);
      const args = transactionReceipt.events[0].args;
      const pairAddress = args.pair;
      expect(args.token0 == first.address);
      expect(args.token1 == second.address)
      

      const uniswapPair = new Contract(pairAddress, UniswapV2Pair.abi, deployer1);

      const firstBalance = 1000;
      const secondBalance = 2000;
      await first.transfer(pairAddress, firstBalance);
      await uniswapPair.sync();
      await second.transfer(pairAddress, secondBalance);
      await uniswapPair.sync();

      const reserves = await uniswapPair.getReserves();
      expect(reserves.reserve0 == firstBalance);
      expect(reserves.reserve1 == secondBalance);

      const [_, signerNext,] = await ethers.getSigners();
      const swap = 100;
      console.log('         ** Transfer swap', swap);
      await first.transfer(pairAddress, swap);
      console.log('         ** Check balance after swap');
      expect(await first.balanceOf(signerNext.address) == 0);
      expect(await second.balanceOf(signerNext.address) == swap);
    });
});
});