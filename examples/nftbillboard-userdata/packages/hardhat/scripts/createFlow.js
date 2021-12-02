const SuperfluidSDK = require("@superfluid-finance/js-sdk");
require("@nomiclabs/hardhat-ganache");

//using fDAIx on Goerli
const fDAIx = "0xF2d68898557cCb2Cf4C10c3Ef2B034b2a69DAD00";
const Web3 = require("web3");
const deployedTradeableCashflow = require("../deployments/goerli/TradeableCashflow.json");
const tradeableCashflowAddress = deployedTradeableCashflow.address;


//create a flow
async function main() {
    try {
        console.log("TCF address: " + tradeableCashflowAddress);
       
        const sf = new SuperfluidSDK.Framework({
          web3
        });
        await sf.initialize();
        const accounts = await web3.eth.getAccounts();
  
        const carol = sf.user({
            address: accounts[0],
            token: fDAIx
        });

        await carol.flow({recipient: tradeableCashflowAddress, flowRate: "300000", userData: web3.eth.abi.encodeParameter('string', 'Hello world!')})
         
    } catch (error) {
        console.error(error);
    }
  }

  // We recommend this pattern to be able to use async/await everywhere
// and properly handle errors.
main()
.then(() => process.exit(0))
.catch((error) => {
  console.error(error);
  process.exit(1);
});

