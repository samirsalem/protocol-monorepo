The formal specification of the Superfluid protocol is written in Haskell, namely in Haskell 2010 with GHC extensions
notably GADTs and type families.

This package is managed using [cabal](https://www.haskell.org/cabal/) as the `superfluid-protocol-spec` haskell package,
which exports:

-   a `core` library where Superfluid protocol core type classes and types are defined,
-   a `simple` library where Superfluid core type classes are instantiated with simple data types for testing purpose,
-   a `superfluid-protocol-validator` executable where production protocol implementations can be validated against the
    specification.

# Development

**Prerequisite**

-   build-essential
-   node (for npx command)
-   GHC >= 8.6.0
-   [Cabal-install](https://www.haskell.org/cabal/download.html) >= 3.6.0.0

**TDD Environment**

    $ make dev

# Core Library

The core library exposes the _Base Types_, _Concepts_, _Agreements_ and _System_ modules.

## Base Types Module

**Timestamp**

...

**Liquidty**

...

**Address**

...

## Concepts Modules

These are the core Superfluid protocol concept expressed in type classes.

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

-   **TransferableBalanceAgreement**: the payment modality as we are all familiar with, i.e. instant settlement between
    two parties.
-   **ConstantFlowAgreement**: a streaming payment modality allowing sender to stream money to receiver in constant flow
    rate.
-   **GeneralDistributionAgreement**: a pubsub-like payment modality allowing publisher to pay its subscribers in
    predefined proportions instantly or in streams.

## System Modules

These encapsulate Superfluid _concepts_ and _agreements_ as SuperfluidToken monad and its monad transformer for building
large scale applications.

# Simple Library

A simple system where the Superfluid core type classes are instantiated with simple types mainly for testing purpose:

-   _Timestamp_ is _Integer_
-   _Liquidity_ is _Wad_
-   _Account_ is addressable by _Address_ in _String_
-   A _SimpleToken_ type that transforms the _State_ monad with the SuperfluidToken monad.

# Validator Executable

TBD...