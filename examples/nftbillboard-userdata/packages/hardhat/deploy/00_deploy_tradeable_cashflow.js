require("dotenv");

const deployFramework = require("@superfluid-finance/ethereum-contracts/scripts/deploy-framework");
const deployTestToken = require("@superfluid-finance/ethereum-contracts/scripts/deploy-test-token");
const deploySuperToken = require("@superfluid-finance/ethereum-contracts/scripts/deploy-super-token");
//test goerli fDAIx
const fDAIx = "0xF2d68898557cCb2Cf4C10c3Ef2B034b2a69DAD00";
const SuperfluidSDK = require("@superfluid-finance/js-sdk");
const { defaultNetwork } = require("../hardhat.config");

//your address here...
const owner = `${process.env.NFT_OWNER_ADDRESS}`;

module.exports = async ({ getNamedAccounts, deployments }) => {

  const { deployer } = await getNamedAccounts();
  console.log(deployer);
  const { deploy } = deployments;
  let sf;

  if (defaultNetwork == "ganache" || defaultNetwork == "localhost") {
  

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
        web3,
        tokens: ["fDAI"],
      });

      await sf.initialize();

      console.log(owner)
      console.log('deploying on local network')
      await deploy("TradeableCashflow", {
        from: deployer,
        args: [owner, 'nifty_billboard', 'NFTBoard', sf.host.address, sf.agreements.cfa.address, sf.tokens.fDAIx.address],
        log: true,
      });
  }

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

};
module.exports.tags = ["TradeableCashflow"];
