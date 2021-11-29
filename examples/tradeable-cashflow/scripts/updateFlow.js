require("dotenv");
const SuperfluidSDK = require("@superfluid-finance/js-sdk");
const Superfluid_ABI = require("@superfluid-finance/js-sdk/src/abi");
//using fDAIx on Goerli
const fDAIx = "0xF2d68898557cCb2Cf4C10c3Ef2B034b2a69DAD00";
const TradeableCashflow = artifacts.require("TradeableCashflow");

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

        await carol.flow({recipient: tradeableCashflowAddress, flowRate: "4000000"})
         
        callback(0)
    } catch (error) {
        console.error(error);
        callback(1)
    }
  }