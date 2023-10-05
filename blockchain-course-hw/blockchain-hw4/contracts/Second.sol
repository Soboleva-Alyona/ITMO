// SPDX-License-Identifier: UNLICENSED
pragma solidity ^0.8.0;

import "./../node_modules/@openzeppelin/contracts/token/ERC20/ERC20.sol";
import "./../node_modules/@openzeppelin/contracts/access/Ownable.sol";

contract Second is ERC20, Ownable {
    constructor() ERC20("Second", "SND") {
        _mint(msg.sender, 1e9);
    }
}