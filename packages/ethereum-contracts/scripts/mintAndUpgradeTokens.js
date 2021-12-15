const hardhat = require("hardhat");

const networkIDToTokensMap = new Map([
    [
        69,
        {
            // fDAI: "0xbe49ac1EadAc65dccf204D4Df81d650B50122aB2",
            fDAIx: "0x04d4f73e9DE52a8fEC544087a66BBbA660A35957",
            // fTUSD: "0x642332562BC60a4Bd9681E7bb1588f7456A497aC",
            fTUSDx: "0xeacC35377202982EFCB01B4a56fcA9F5eFF8A5f1",
            // fUSDC: "0xA794C9ee519FD31BbCE643e8D8138f735E97D1DB",
            fUSDCx: "0xaA1EA30cEe569fA70B8561c0F52F10Da249Aecb5",
        },
    ],
    [
        421611,
        {
            // fDAI: "0xf8d8f02b788DE5191Ecd20f7BDb07D80963410B5",
            fDAIx: "0x4B746F88fb25516731D54cEfB1c2d00eadEFf366",
            // fTUSD: "0x67270eB3a30c1a787f216Cf711cF1dBD8326266d",
            fTUSDx: "0x8E64a3b0A7997D1E5Fb77Ad914A753B7dFe28729",
            // fUSDC: "0xb8F12031d79615F3fAf6eC5F9795057E55e3069A",
            fUSDCx: "0xb9Fef3d4054f1B40f8Dcf4ae975F3486aDF6327C",
        },
    ],
    [
        43113,
        {
            // fDAI: "0x87E00Dced5670e01BEe33a9a724B1dac790937eF",
            // fDAIx: "0x296E9c01f80D408741f6e15d62013DDbe1041f1D",
            // fTUSD: "0x1031b9bD6544d266A7B462744dF28Dc4A7A08CAe",
            // fTUSDx: "0x36BBF94e66f0c953CB451b302e345D6E50c2A215",
            // fUSDC: "0xE01F8743677Da897F4e7De9073b57Bf034FC2433",
            fUSDCx: "0x896C3e90446237be3e27eB4D67e0D3c97FB3b03F",
        },
    ],
]);

async function main() {
    const user = "0x209B6ff4616152187D752eca6efAfdc6E873c48a";
    const signer = await hardhat.ethers.getSigner(user);
    const chainId = (await hardhat.ethers.provider.getNetwork()).chainId;
    const tokens = networkIDToTokensMap.get(chainId);

    if (!tokens) {
        throw new Error("Not a supported chainId");
    }

    // initialize super token and underlying token contracts
    // mint test tokens for user
    // upgrade the newly minted test tokens
    const superTokenAddresses = Object.values(tokens);
    for (let i = 0; i < superTokenAddresses.length; i++) {
        const superTokenAddress = superTokenAddresses[i];
        console.log("SuperToken: ", superTokenAddress);
        const tokenAmount = hardhat.ethers.utils.parseUnits("1000");
        const superToken = (
            await hardhat.ethers.getContractFactory("SuperToken")
        )
            .attach(superTokenAddress)
            .connect(signer);
        const underylingAddress = await superToken.getUnderlyingToken();
        const testToken = (await hardhat.ethers.getContractFactory("TestToken"))
            .attach(underylingAddress)
            .connect(signer);

        // mint test tokens
        console.log("Minting underlying token.");
        const mintTxn = await testToken.mint(user, tokenAmount);
        await mintTxn.wait();

        // approve token spend
        console.log("Approving underlying token spend.");
        const approveTxn = await testToken.approve(
            superTokenAddress,
            tokenAmount
        );
        await approveTxn.wait();

        // upgrade to superToken
        console.log("Upgrading underlying token to supertoken.");
        const upgradeTxn = await superToken.upgrade(tokenAmount);
        await upgradeTxn.wait();
    }
}

main()
    .then(() => process.exit(0))
    .catch((error) => {
        console.error(error);
        process.exit(1);
    });
