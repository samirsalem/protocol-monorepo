require("@typechain/hardhat");
require("@nomiclabs/hardhat-ethers");
require("@nomiclabs/hardhat-waffle");
const {config} = require("dotenv");
config();

/**
 * This Hardhat config is only used for testing the subgraph.
 * Note: For tests to work, 0xf39fd6e51aad88f6f4ce6ab8827279cfffb92266
 * must be the deployer of the contracts. (add to readme.md).
 */
module.exports = {
    solidity: {
        version: "0.7.6",
        settings: {
            optimizer: {
                enabled: true,
                runs: 200,
            },
        },
    },
    networks: {
        localhost: {
            url: "http://0.0.0.0:8545/",
            chainId: 1337,
        },
        matic: {
            accounts: [process.env.TEST_ACCOUNT || ""],
            url: process.env.MATIC_PROVIDER_URL || "",
            chainId: 137,
        },
        opkovan: {
            accounts: [process.env.TEST_ACCOUNT || ""],
            url: process.env.OPKOVAN_PROVIDER_URL || "",
            chainId: 69,
        },
        arbrinkeby: {
            accounts: [process.env.TEST_ACCOUNT || ""],
            url: process.env.ARBRINKEBY_PROVIDER_URL || "",
            chainId: 421611,
        },
        avafuji: {
            accounts: [process.env.TEST_ACCOUNT || ""],
            url: process.env.AVAFUJI_PROVIDER_URL || "",
            chainId: 43113,
        },
    },
    mocha: {
        timeout: 250000,
    },
};
