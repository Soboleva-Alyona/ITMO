# HW №4 К занятию “Solidity + EVM, low-level patterns”

In [this file](https://github.com/Soboleva-Alyona/blockchaing-course-hw/blob/main/blockchain-hw4/test/Uniswap.js) we create and deploy two tokens ([First/FST](https://github.com/Soboleva-Alyona/blockchaing-course-hw/blob/main/blockchain-hw4/contracts/First.sol) and [Second/SND](https://github.com/Soboleva-Alyona/blockchaing-course-hw/blob/main/blockchain-hw4/contracts/Second.sol)) and then swap them using UniswapV2Factory and UniswapV2Pair

To setup the projest run [setup.sh](https://github.com/Soboleva-Alyona/blockchaing-course-hw/blob/main/blockchain-hw4/setup.sh) script

Then run 
```
npx hardhat test --network localhost
```

Output example:
```
Uniswap
    Deploy tokens: 
         Deploying  First 
         The deployer is: 0xf39Fd6e51aad88F6F4ce6aB8827279cffFb92266
         Contract  First deployed to address: 0x4Af9f320fE64C09a59572B6F687B308278367D61
         Deploying  Second 
         The deployer is: 0xf39Fd6e51aad88F6F4ce6aB8827279cffFb92266
         Contract  Second deployed to address: 0x1F8B0Ab82C79bDBB02AbB87F6681a464CF24D50A
      ✔ Should have right owners (937ms)
         Deploying  First 
         The deployer is: 0xf39Fd6e51aad88F6F4ce6aB8827279cffFb92266
         Contract  First deployed to address: 0x7AaBb13928E9382eE4b4148b9618039aBBcA7945
         Deploying  Second 
         The deployer is: 0xf39Fd6e51aad88F6F4ce6aB8827279cffFb92266
         Contract  Second deployed to address: 0xc993301287f7E7f7C0EB28c4616534CcAbA348BA
      ✔ Check balances (459ms)
    Swap tokens: 
         Deploying  First 
         The deployer is: 0xf39Fd6e51aad88F6F4ce6aB8827279cffFb92266
         Contract  First deployed to address: 0xeAd4C2cc3c9c44be601373460BEe3c331FaFfe96
         Deploying  Second 
         The deployer is: 0xf39Fd6e51aad88F6F4ce6aB8827279cffFb92266
         Contract  Second deployed to address: 0xB7b8FF73bc1b41feF618f43dfe063f42753470f1
         ** Transfer swap 100
         ** Check balance after swap
      ✔ Swap should be completed (1432ms)


  3 passing (3s)
 ```

