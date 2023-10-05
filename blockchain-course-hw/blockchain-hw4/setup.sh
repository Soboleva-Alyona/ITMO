#!/bin/bash   

npm install --save @openzeppelin/contracts@v4.8.0 # to be able to use ERC20 contract interface
npm install --save @uniswap/v2-core

API_KEY=$(cat ./data/api_key.json)
npx hardhat node --fork https://eth-mainnet.g.alchemy.com/v2/API_KEY # This command will execute the Hardhat package and create a fork of Mainnet
