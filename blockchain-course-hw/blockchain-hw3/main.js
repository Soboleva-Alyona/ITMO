const ethers = require('ethers')
const abi = require('./data/abi.json')
const apiKey = require('./data/api_key.json')
const oraclesInfo = require('./data/oracles.json')
const Web3 = require('web3')

const settings = {
    apiKey: apiKey
};

'use strict'

const main = async () => {

  const web3 = new Web3('wss://eth-mainnet.ws.alchemyapi.io/ws/' + settings.apiKey)
  
  oraclesInfo.oracles.forEach(({ address, tokenName, precision }) => {
    const contract = new web3.eth.Contract(abi, address)

    contract.events.AnswerUpdated()
    .on("connected", function(subscriptionId){
        console.log(`[${tokenName}] Subscribed on events "AnswerUpdated", address = ${address}`)
      })
      .on('data', function(event) {
          const formattedCurrent = ethers.utils.formatUnits(event.returnValues.current, precision)
          const formattedUpdatedAt = new Date(event.returnValues.updatedAt * 1000)
          console.log(`[${tokenName}] At ${formattedUpdatedAt} exchange rate was ${formattedCurrent} in round #${event.returnValues.roundId}`)
      })
      .on('error', function(error, receipt) { 
         console.log("error")
      })
  })
}

main()