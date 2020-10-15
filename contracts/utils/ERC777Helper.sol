// SPDX-License-Identifier: MIT
pragma solidity 0.7.3;

import { IERC1820Registry } from "@openzeppelin/contracts/introspection/IERC1820Registry.sol";


library ERC777Helper {

    IERC1820Registry constant internal _ERC1820_REGISTRY =
        IERC1820Registry(0x1820a4B7618BdE71Dce8cdc73aAB6C95905faD24);

    // keccak256("ERC777TokensSender")
    bytes32 constant internal _TOKENS_SENDER_INTERFACE_HASH =
        0x29ddb589b1fb5fc7cf394961c1adf5f8c6454761adf795e67fe149f658abe895;

    // keccak256("ERC777TokensRecipient")
    bytes32 constant internal _TOKENS_RECIPIENT_INTERFACE_HASH =
        0xb281fc8c12954d22544db45de3159a39272895b169a852b314f9cc762e44c53b;


    /// @dev ERC777 operators support self structure
    struct Operators {
        address[] defaultOperatorsArray;
        mapping(address => bool) defaultOperators;
        mapping(address => mapping(address => bool)) operators;
        mapping(address => mapping(address => bool)) revokedDefaultOperators;
    }

    function isOperatorFor(Operators storage self, address operator, address tokenHolder) internal view returns (bool) {
        return operator == tokenHolder ||
            (
                self.defaultOperators[operator] &&
                !self.revokedDefaultOperators[tokenHolder][operator]
            ) ||
            self.operators[tokenHolder][operator];
    }

    function authorizeOperator(Operators storage self, address holder, address operator) internal {
        require(holder != operator, "ERC777Operators: authorizing self");

        if (self.defaultOperators[operator]) {
            delete self.revokedDefaultOperators[holder][operator];
        } else {
            self.operators[holder][operator] = true;
        }
    }

    function revokeOperator(Operators storage self, address holder, address operator) internal {
        if (self.defaultOperators[operator]) {
            self.revokedDefaultOperators[holder][operator] = true;
        } else {
            delete self.operators[holder][operator];
        }
    }

    function defaultOperators(Operators storage self) internal view returns (address[] memory) {
        return self.defaultOperatorsArray;
    }

}