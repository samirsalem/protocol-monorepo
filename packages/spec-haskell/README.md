The formal specification of the Superfluid protocol is written in Haskell.

Namely in Haskell 98, plus some GHC extensions notably Multi-parameter type class, GADTs, and type families.

This package is managed using [cabal](https://www.haskell.org/cabal/) as the `superfluid-protocol-spec` haskell package, which exports:

* a `core` library where Superfluid protocol core type classes and types are defined,
* a `simple` library where Superfluid core type classes are instantiated with simple data types for testing purpose,
* a `superfluid-protocol-validator` executable where production protocol implementations can be validated against the specification.

# Development

**Prerequisite**

* build-essential
* node (for npx command)
* GHC >= 8.6.0
* [Cabal-install](https://www.haskell.org/cabal/download.html) >= 3.6.0.0

**TDD Environment**

```
$ make dev
```

# Core Library

The core library exposes the _Concepts_, _Agreements_ and _System_ modules.

## Concepts Modules

These are the core Superfluid protocol concept expressed in type classes.

**Timestamp**

...

**Liquidty**

...

**RealtimeBalance**

Depends on: _Liquidity_

...

**Agreement**

Depends on: _Timestamp_, _Liquidity_, _RealtimeBalance_

...

**Account**

Depends on: _Timestamp_, _Liquidity_, _RealtimeBalance_, _Agreement_

...

## Agreements Modules

These are instances of the _Agreement_ concept:

* **TransferableBalanceAgreement**: the payment modality as we are all familiar with, i.e. instant settlement between two parties.
* **ConstantFlowAgreement**: a streaming payment modality between two parties in constant flow rate.
* **GeneralDistributionAgreement**: a pubsub-like payment modality allowing publisher pay its subscribers in predefined proportions instantly or in streams.

## System Modules

These encapsulate Superfluid concepts and agreements as SuperfluidToken monad and its monad transformer for building large scale applications.
