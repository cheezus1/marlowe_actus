<p align="center">
  <img width="266" height="185" src="docs/tutorial-v1.3/pix/logo.png">
</p>

# Marlowe

This repository contains Marlowe, a domain-specific language (DSL) for describing financial smart contracts that can be enforced by scripts deployed on a blockchain, as well as some tools for analysing, simulating the execution of contracts written in the DSL, **and generating smart contracts, based on the [ACTUS](https://www.actusfrf.org/) specification**.

## Learning about Marlowe and Marlowe Playground

The [Marlowe tutorials](https://david.marlowe.iohkdev.io/tutorial/) introduce Marlowe and the Marlowe Playground.

## Versions of Marlowe

The `master` branch contains the latest version of Marlowe, version `3.0`.

An earlier version of Marlowe is described in a [paper](https://iohk.io/research/papers/#2WHKDRA8) that was presented at ISoLA 2018. This versin is tagged `v1.3` and a minor update on this is taggedn `v1.3.1`.
Versions `1.x`, and `2.0` can also be found in the `master` branch under `semantics-1.0`, and `semantics-2.0`, respectively.

## Requirements

[Haskell Stack 1.6](https://docs.haskellstack.org/en/stable/README/) or later.

To setup the project, execute the following command:

    $ stack setup

## Generating ACTUS contracts

An example for generating a LAM contract can be found in `src/Main.hs`.

A `ContractConfig` record must be initialized, which is then used to derive the initial state, generate the contract schedules, and schedule the contract events, all done for the specific contract type.

## Running the ACTUS contract example

    $ stack build
    $ stack exec marlowe
