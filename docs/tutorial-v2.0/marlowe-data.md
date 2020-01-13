# Marlowe as a Haskell data type

This tutorial formally introduces Marlowe as a Haskell data type, building on the escrow example from the previous tutorial. It also describes the different types used by the model, as well as discussing a number of assumptions about the infrastructure in which contracts will be run.

## Marlowe

The Marlowe domain-specific language (DSL) is modelled as an algebraic type in Haskell:

```haskell
data Contract =
    Null |
    Commit IdAction IdCommit Person Value Timeout Timeout Contract Contract |
    Pay IdAction IdCommit Person Value Timeout Contract Contract |
    Both Contract Contract |
    Choice Observation Contract Contract |
    When Observation Timeout Contract Contract |
    While Observation Timeout Contract Contract |
    Scale Value Value Value Contract |
    Let LetLabel Contract Contract |
    Use LetLabel
```

Informally, this type provides these contracts, some of which were introduced already when we introduced the [escrow example](./escrow-ex.md):
- A `Null` contract, which does nothing. 
- The next two constructs are the simplest contract-building primitives. They contain two sub-contracts: the first to be followed if the action has been performed successfully, and the second to use in the case of a timeout.
    - `Commit` will wait for a participant to make a commitment, and 
    - `Pay` will wait for a payment to be claimed by the recipient.
 
- The remaining constructors form composite contracts from simpler components, which include `Contract`s and `Observation`s: 
  - `Both` has the behaviour of both its components, 
  - `Choice` chooses between two contracts on the basis of an observation,  
  - `When` is quiescent until a condition – an observation – becomes true,
  - `While` is active until a condition becomes false.
  - `Scale` is used to scale the amounts specified in `Commit`s and `Pay`s in a contract, and
  - `Let` and `Use` allow for making and using local definitions (“shorthand”) of contracts within a contract.

Additionally, many of the contracts have `Timeout`s that also help to determine their behaviour. 


## The model types

The interface between Marlowe contracts and the real world is governed by a series of types that represent the elements that the contracts need to consider.

![Environment](./pix/context.png)

### Inputs

First, contracts need to receive information from the real world. Because Marlowe contracts are passive – they do not make things, such as payments,  happen; rather they allow then to take place –  all interactions are done through inputs.

Inputs can have one of four different kinds: `Commit`, `Pay`, `Choice`, and `Oracle`. From these four, `Commit` and `Pay` are considered to be _actions_ and are typically associated with the transfer of some money between the participant and the contract. On the other hand, `Choice` and `Oracle` inputs are only used to provide information to the contract.

#### Inputs (pt 1): `Commit` and `Pay`

The attentive may have noticed that `Commit` and `Pay` inputs correspond to two types of contract with the same name. Indeed, actions of type `Commit` and `Pay` are enabled by the constructs of the same name. In fact, inputs for `Commit` and `Pay` actions just consist of their identifier (`IdAction`), since the rest of information can be inferred from the contract.

```haskell
data AnyInput = Action IdAction
              | Input Input
```

Inputs of kind `Commit` represent commitments of currency (or “cash”), while inputs of kind `Pay` represent claims of payments by a participant. While informally we might see a commitment to something as being indefinite, as noted earlier, it is important to realise that, on blockchain, a commitment needs to have a _timeout_ so that progress can be ensured in contracts. 

After the timeout period, the cash will be refunded in the next transaction that is signed by the participant that made the commitment. Information about the commitments currently in force forms the `State`, which can be modified at each execution step.

#### Inputs (pt 2): `Choice` and `Oracle`

 `Choice`s are values _chosen_ by participants; while `Oracle`s provide values from a trusted source of information and can change over time. Oracles can be used to provided varying quantities from the real world, for example, the current time, "the price of oil", or "the exchange rate between currencies A and B".

```haskell
data Input = IChoice IdChoice Choice
           | IOracle IdOracle BlockNumber Integer
```

Values provided by `Choice`s and `Oracle`s can be inspected and acted upon by contracts by using a "little language" for that purpose called `Observation`, as used in some of the contracts described by the `Contract` type.

### Participants

All Marlowe contracts have a finite number of participants. In the semantics for Marlowe , we use integers to model participants; in the actual implementation, participants are typically represented by a public key or by its hash. In addition, the semantics takes a list of signatures (represented as a list of integer numbers), but in the implementation we assume that transactions are cryptographically signed using the private key of the participants, and that the implementation will check that these signatures are valid.

### Infrastructure 

The model makes a number of assumptions about the blockchain infrastructure in which it is run.

- It is assumed that cryptographic functions and operations are provided by a layer external to Marlowe, and so they need not be modelled explicitly.
- We assume that time is “coarse grained” and measured by block or slot number, so that, in particular, timeouts are delimited using block/slot numbers.
- Making a commitment is not something that a contract can perform; rather, it can request that a commitment is made, but that then has to be established externally: hence the input of (a set of) commitments at each step.
- The model manages the release of funds back to the committer when a cash commitment expires.

## Notes

- Marlowe 2.0 extends the Marlowe `Contract` type in version 1.3 with local definitions, scaling and a `While` construct.
- For ease of reading, in the `data` type definition at the start of this section, we omit the `!` symbol before every field of all constructors. This makes them  strict in that field. We choose to make Marlowe strict in all arguments to all constructors, so that Marlowe contracts are wholly _finite_ data structures, with no partial or infinite components.

### [Prev](./escrow-ex.md) [Up](./README.md) [Next](./marlowe-semantics.md)
