Tradable Cash Flow
==================

The Tradeable cashflow provides the deployer with an NFT. Whoever is the owner
of the NFT will receive a consolidated stream for all the streams the contract
receives. It is composed of two contracts: TradeableCashflow and RedirectAll.
The first contains the NFT logic (which is mainly inherited from OpenZeppelin)
and the second contains the logic which condenses the streams, and has a
function which changes the receiver of the condensed stream.

# Usage
1) Create a .env file and add your own information using the format in .env.template
2) Run truffle migrate --network goerli to deploy to the Goerli testnet
3) Interact with your contracts using app.superfluid/dashboard or by running the scripts in the ./scripts folder

To interact with the Tradeable Cashflow contract using scripts, you can run the following command after deploying:

```truffle exec --network goerli ./scripts/createFlow.js```

```truffle exec --network goerli ./scripts/createFlow.js```

```truffle exec --network goerli ./scripts/createFlow.js```

If you'd like to use a different network, be sure to add it as an option in truffle.config, and replace 'goerli' in the above commands with your preferred network. 

## Run tests

```bash
yarn install
yarn build
yarn test
```
