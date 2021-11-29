require("dotenv");
const SuperfluidSDK = require("@superfluid-finance/js-sdk");
const Superfluid_ABI = require("@superfluid-finance/js-sdk/src/abi");
const Web3 = require("web3");
//using fDAIx on Goerli
const fDAIx = "0xF2d68898557cCb2Cf4C10c3Ef2B034b2a69DAD00";
const TradeableCashflow = artifacts.require("TradeableCashflow");

// const deployedTradeableCashflow = require("../artifacts/goerli/TradeableCashflow.json");
// const tradeableCashflowAddress = deployedTradeableCashflow.address;

//create a flow
module.exports = async function main(callback) {
    try {
        const tradeableCashflow = await TradeableCashflow.deployed();
        const tradeableCashflowAddress = tradeableCashflow.address;

        const sf = new SuperfluidSDK.Framework({
            web3
        })
        await sf.initialize();
        console.log("TCF address: " + tradeableCashflowAddress)
        const accounts = await web3.eth.getAccounts();
        
        const carol = sf.user({
            address: accounts[0],
            token: fDAIx
        });

        await carol.flow({recipient: tradeableCashflowAddress, flowRate: "3000000"})
         
        callback(0)
    } catch (error) {
        console.error(error);
        callback(1)
    }
  }

// We recommend this pattern to be able to use async/await everywhere
// and properly handle errors.
// main();
//   .then(() => process.exit(0))
//   .catch((error) => {
//     console.error(error);
//     process.exit(1);
//   });