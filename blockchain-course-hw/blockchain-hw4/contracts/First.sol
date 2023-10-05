// SPDX-License-Identifier: UNLICENSED
pragma solidity ^0.8.0;

import "./../node_modules/@openzeppelin/contracts/token/ERC20/ERC20.sol";
import "./../node_modules/@openzeppelin/contracts/access/Ownable.sol";

contract First is ERC20, Ownable  {
    constructor() ERC20("First", "FST") {
        _mint(msg.sender, 1e9);
    }
}