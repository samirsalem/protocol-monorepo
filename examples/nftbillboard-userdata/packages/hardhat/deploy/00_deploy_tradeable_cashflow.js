require("dotenv");

const deployFramework = require("@superfluid-finance/ethereum-contracts/scripts/deploy-framework");
const deployTestToken = require("@superfluid-finance/ethereum-contracts/scripts/deploy-test-token");
const deploySuperToken = require("@superfluid-finance/ethereum-contracts/scripts/deploy-super-token");
const SuperfluidSDK = require("@superfluid-finance/js-sdk");

//your address here...
const owner = `${process.env.NFT_OWNER_ADDRESS}`;

module.exports = async ({ getNamedAccounts, deployments }) => {
  const { deploy } = deployments;

  const { deployer } = await getNamedAccounts();
  console.log(deployer);


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

  let sf = new SuperfluidSDK.Framework({
    web3,
    version: "test",
    tokens: ["fDAI"],
  });

  await sf.initialize();

  console.log(owner)
  await deploy("TradeableCashflow", {
    from: deployer,
    args: [owner, 'nifty_billboard', 'NFTBoard', sf.host.address, sf.agreements.cfa.address, sf.tokens.fDAIx.address],
    log: true,
  });

};
module.exports.tags = ["TradeableCashflow"];
