const hre = require("hardhat");


async function deployFirst() {
  const [deployer] = await hre.ethers.getSigners();
  console.log(
    "Deploying contracts with the account:",
    deployer.address
    );

  const currentTimestampInSeconds = Math.round(Date.now() / 1000);
  const ONE_YEAR_IN_SECS = 365 * 24 * 60 * 60;
  const unlockTime = currentTimestampInSeconds + ONE_YEAR_IN_SECS;
  const First = await hre.ethers.getContractFactory("First");
  const first = await First.deploy(unlockTime);
  console.log("Contract deployed to address:", first.address);
  return first.address;
}

async function deploySecond() {
  const [deployer] = await hre.ethers.getSigners();
  console.log(
    "Deploying contracts with the account:",
    deployer.address
    );

  const currentTimestampInSeconds = Math.round(Date.now() / 1000);
  const ONE_YEAR_IN_SECS = 365 * 24 * 60 * 60;
  const unlockTime = currentTimestampInSeconds + ONE_YEAR_IN_SECS;
  const Second = await hre.ethers.getContractFactory("Second");
  const second = await Second.deploy(unlockTime);
  console.log("Contract deployed to address:", second.address);
  return second.address;
}

async function deployBoth() {
  const firstAddr = deployFirst();
  const secondAddr = deploySecond();
  return [firstAddr, secondAddr];
}

async function main() {

}

main().catch((error) => {
  console.error(error);
  process.exitCode = 1;
});
