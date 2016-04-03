Implementation of [RSCoin](http://www0.cs.ucl.ac.uk/staff/S.Meiklejohn/files/ndss16.pdf)
---

*See generated documentation [here](http://lab.serokell.io/rscoin).*

Prototype implementation of RSCoin incorporate protocol implementation with
three binaries to run as a Bank, Mintette and User. It also has a simple coin
generation algorithm.

During implementation of RSCoin the following design decisions and engineering
trade-offs were made:

 + Overall correctness over overall performance
   We have picked synchronous protocol variant before applying any
   optimizations mentioned in the paper, or other ones.

 + Ease of sending values of types over the wire
   We have picked Msgpack as the application-level protocol.

 + Correctness of persistence layer over scalability and performance of
   persistence layer
   We are using acid-state, which stores serialized Haskell terms on disk. That
   is not as fast as modelling the data in relational algebra and then storing
   it in an SQL-system, this is not as scalable as using a clustered
   persistence solution such as Riak, but we argue that clustered persistence
   is inadequate for this purpose altogether and relational system might be
   added if such need occurs by plugging out acid-state and plugging in SQL
   persistence machinery. In the meantime, we want to be free from a class of
   bugs which arise from interaction between strongly typed and weakly typed
   worlds.

 + Forwards-compatibility and simplicity of migrations over simplicity of
   persistence layer
   Instead of just using serializable in conjunction with acid-state for
   persistence layer, we use serializable augmented with safe-copy in
   conjunction with acid-state, which gives us resilience to model changes, as
   our data is versioned against the history of data types we used to represent
   a certain concept.

 + RSCoin is a library, daemons and convenience-cli programs are binaries which
   use this library  In accord with the library-oriented development philosophy,
   which suggests putting as much logic in libraries as possible, so that an
   application is a composition of those libraries with, maybe, some
   configuration and/or command line argument parsing facilities, we are putting
   most of the logic in `rscoin` package, which is a library (as specified in the
   cabal file of the project) with `rscoin-bank`, `rscoin-keygen`, `rscoin-user`,
   and `rscoin-mintette` being relatively thin layers which make use of this
   package.

Some details about commonly used type classes and instances
===

### SafeCopy

As we're using acid-state and heavily for prototyping, and
rely on raw Haskell data types to aid us in handling the data,
we use SafeCopy to mitigate possible migration issues.

For more information, see [this link](https://hackage.haskell.org/package/safecopy-0.9.0.1/docs/Data-SafeCopy.html).

### Contained Put / Get monads

Put is a special Writer, Get is a special State monad. We're using Contained
version of those (Contained `a` means that we can put `a` into the container,
but can't get it out of it). We need it as SafeCopy machinery is responsible
for producing results of the serialization.

### Buildable

To be able to lazily construct Text values via Data.Text.Lazy.Builder,
we want our types to have an instance of Buildable.

For more, see [Builder type](https://hackage.haskell.org/package/text-0.11.2.3/docs/Data-Text-Lazy-Builder.html#t:Builder)
and [Buildable typeclass](https://hackage.haskell.org/package/text-format-0.3.1.1/docs/Data-Text-Buildable.html).

Sadly, `text-format` library doesn't provide a typesafe text building utilities,
so we have to be extremely careful about calls, not to cause runtime problems.
Maybe, a better idea could be to use some variant of printf, sadly, facilities of
statically checked printing aren't mature enough in Haskell for transparent use
with logging.

### Logging

We are usnig MonadLogger to log messages about the system. Notice, that there is
`ToLogStr` constraint in MonadLogger functions,
[which is basically `Buildable`](http://hackage.haskell.org/package/fast-logger-0.3.1/docs/src/System-Log-FastLogger.html#LogStr).

We never make direct calls to Control.Monad.Logger functions, instead using
Template Haskell facilities
[provided by the library](http://hackage.haskell.org/package/monad-logger-0.2.3.1/docs/src/Control-Monad-Logger.html#logDebug).

### Exceptional State Handling

We represent abnormal, but legal state using `Control.Exception` facilities.
On top of things, we sometimes define type-safe coercion mechanisms instead
of bidirectional knowledge propagation about the modules.
[Example of such coercion](https://github.com/serokell/rscoin/blob/master/src/RSCoin/User/Error.hs#L37-L38).

Some details about implementation choices
===

### Newtype wrappers

For mission-critical stuff (which is most of the stuff), in vast majority of
cases, instead of type aliases, we use newtype wrappers.  The reason we're
doing it is threefold:

 1. Make the compiler check semantics of function calls;
 2. Be able to provide custom instances for wrapped types (a great example of
    this benefit is Signature newtype in RSCoin.Core.Crypto, or, say, we need
    another version of Monoid for Int, multiplicative one);
 3. Be explicit about extraneous types we “touched”, and part those from types
    we use as is.

### Cryptography

We are using the following systems:

 + ECDSA with Secp256k1, using [Bitcoin's implementation of Secp256k1](https://github.com/bitcoin/secp256k1)
 + SHA256 for hashing, using [Crypto.Hash.SHA256](https://hackage.haskell.org/package/cryptohash-0.7.1/docs/Crypto-Hash-SHA256.html)

Tests for primitives doing what they are supposed to do are already there.
There is a room for improvement, but without those we would feel very
uncomfortable.

The Programmer's Guide to the RSCoin Implementation
===

At the heart of the system is [RSCoin.Core](https://github.com/serokell/rscoin/tree/master/src/RSCoin/Core)
family of modules.

Let's go through the most fundamental modules of RSCoin.Core.

### [RSCoin.Core.Types](https://github.com/serokell/rscoin/blob/master/src/RSCoin/Core/Types.hs)

Given that you have an undestanding of the paper,
[generated Haddoc documentation of this module](http://lab.serokell.io/rscoin/rscoin-0.1.0.0/RSCoin-Core-Types.html)
is more than enough to understand the purpose of every type there.

*TODO:* Explain the decisions behind strict fields. I though that finite or “compact” fields are strict, while
“long” fields (such as `Mintettes`) are lazy, or as strict as the corresponding constructor is, but I'm
obviously mistaken, as there are lazy fields that are pretty “compact”.

### [RSCoin.Core.Primitives](https://github.com/serokell/rscoin/blob/master/src/RSCoin/Core/Primitives.hs)

This module, again, mirrors the abstractions defined in the paper one-to-one, as
we see from the
[generated documentation](http://lab.serokell.io/rscoin/rscoin-0.1.0.0/RSCoin-Core-Primitives.html),
one `Coin` is equivalent to one satoshi. A coin, in our terminology, is an atom of the currency.

To provide more scalability and future-compatibility, we're considering storing a coefficient which
will denote the size of an atom of a coin. Then `Coin` will become `Atom`, with coefficient constant
showing how many atoms are there in one coin. Now if we want to respond to, say, deflation, by making
the granularity of the currency higher, we can do that by increasing the coefficient in question.

It is also worth noting, that while Id types in `Types` module are just type aliases
(because, semantically, they represent indices in different lists), here all the types that
are basically aliases are wrapped in newtypes. See “Newtype wrappers” section above.

### [RSCoin.Core.Constants](https://github.com/serokell/rscoin/blob/master/src/RSCoin/Core/Constants.hs)

Constants are pretty self-descriptive. However, two notes on things that could be not very
obvious should be made.

 1. `emissionHash` is used when a bank emits some coins to make all the transactions which
    emit coins have different (yet, predictable by an adversary, see issue number 7 in Serokell
    issue tracker about it)
 2. `shardDivider` tells us how many owners will a particular `addrid` have. We take the
    amount of currently approved mintettes and divide it by this constant, taking maximum
    between the quotient and the total amount of mintettes. This constant is `3` in this
    version of RSCoin implementation. So, for two mintettes, shard size is 2, for three — 2,
    …, for eight — 2, for nine — 3, ….
