Implementation of [RSCoin](http://www0.cs.ucl.ac.uk/staff/S.Meiklejohn/files/ndss16.pdf)
---

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
