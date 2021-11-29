require("dotenv");
//migration hardcoded here for goerli
const owner = process.env.NFT_OWNER_ADDRESS;
const name = "Tradeable Cashflow NFT";
const symbol = "TCF";
const host = "0x22ff293e14F1EC3A09B137e9e06084AFd63adDF9";
const cfa = "0xEd6BcbF6907D4feEEe8a8875543249bEa9D308E8";
//goerli fDAIx address
const acceptedToken = "0xF2d68898557cCb2Cf4C10c3Ef2B034b2a69DAD00"
const TradeableCashflow = artifacts.require("TradeableCashflow");


module.exports = function(deployer) {
  deployer.deploy(
    TradeableCashflow, 
    owner, 
    name,
    symbol,
    host,
    cfa,
    acceptedToken
    );
};