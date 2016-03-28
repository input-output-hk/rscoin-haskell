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

 + Human-readability over performance  
   We have picked JSONRPC 2.0 as the application-level protocol, which means
   that we don't have to write Wireshark dissector to observe packet flow, and
   also we can easily hand-craft messages and send those via cURL. If we are to
   release a production version of RSCoin framework, we — for sure — will swap
   JSONRPC for Msgpack.

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
