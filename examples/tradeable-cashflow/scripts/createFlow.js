const SuperfluidSDK = require("@superfluid-finance/js-sdk");
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

        await carol.flow({recipient: tradeableCashflowAddress, flowRate: "3000000"})
         
        callback(0)
    } catch (error) {
        console.error(error);
        callback(1)
    }
  }

  // //ONLY use migrate for goerli
// require("dotenv");
// //migration hardcoded here for goerli
// const name = "Tradeable Cashflow NFT";
// const symbol = "TCF";
// const host = "0x22ff293e14F1EC3A09B137e9e06084AFd63adDF9";
// const cfa = "0xEd6BcbF6907D4feEEe8a8875543249bEa9D308E8";
// //goerli fDAIx address
// const acceptedToken = "0xF2d68898557cCb2Cf4C10c3Ef2B034b2a69DAD00"
// const TradeableCashflow = artifacts.require("TradeableCashflow");


// module.exports = function(deployer, networks, accounts) {
//   deployer.deploy(
//     TradeableCashflow, 
//     accounts[1], 
//     name,
//     symbol,
//     host,
//     cfa,
//     acceptedToken
//     );
// };
