require("dotenv");

const deployFramework = require("@superfluid-finance/ethereum-contracts/scripts/deploy-framework");
const deployTestToken = require("@superfluid-finance/ethereum-contracts/scripts/deploy-test-token");
const deploySuperToken = require("@superfluid-finance/ethereum-contracts/scripts/deploy-super-token");
//test goerli fDAIx
const fDAIx = "0xF2d68898557cCb2Cf4C10c3Ef2B034b2a69DAD00";
const SuperfluidSDK = require("@superfluid-finance/js-sdk");
const { defaultNetwork } = require("../hardhat.config");
const Web3 = require("web3");
// const { web3 } = require("hardhat");



module.exports = async ({ getNamedAccounts, deployments }) => {
  
  const accounts = await web3.eth.getAccounts();
  //your address here...
  const owner = accounts[1];
  const { deployer } = await getNamedAccounts();
 
  const { deploy } = deployments;
  let sf;

  if (defaultNetwork == "ganache" || defaultNetwork == "localhost") {
    
    console.log(defaultNetwork)

      const errorHandler = (err) => {
        if (err) throw err;
      };

      await deployFramework(errorHandler, {
        web3,
        from: deployer,
      });

      await deployTestToken(errorHandler, [":", "fDAI"], {
        web3,
        from: deployer,
      });
      await deploySuperToken(errorHandler, [":", "fDAI"], {
        web3,
        from: deployer,
      });

      sf = new SuperfluidSDK.Framework({
        web3: (new Web3("http://127.0.0.1:8545")),
        tokens: ["fDAI"],
      });

      await sf.initialize();

      console.log('deploying on local network')
      await deploy("TradeableCashflow", {
        from: deployer,
        args: [owner, 'nifty_billboard', 'NFTBoard', sf.host.address, sf.agreements.cfa.address, sf.tokens.fDAIx.address],
        log: true,
      });
  } else {

  sf = new SuperfluidSDK.Framework({
    web3,
  })

  await sf.initialize();

  console.log(owner);
  console.log(sf.host.address);
  console.log(sf.agreements.cfa.address)
  console.log('deploying on live testnet or network')
  await deploy("TradeableCashflow", {
    from: deployer,
    args: [owner, 'nifty_billboard', 'NFTBoard', sf.host.address, sf.agreements.cfa.address, fDAIx],
    log: true,
  });
  }
};
module.exports.tags = ["TradeableCashflow"];
