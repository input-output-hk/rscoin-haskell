RSCoin: A scalable distributed cryptographic currency framework
---

RSCoin proposes¹ a framework to implement centrally banked monetary
systems based on cryptographic methods in such way that it is possible
for a system which uses RSCoin framework to meet every of the following
criteria:

  1. System can handle massive amount of users and scales well;
  2. Transaction times are comparable to those of MasterCard and other
     payment systems that issue plastic cards;
  3. The system is auditable and transparent for all the users;
  4. The system allows rapid payments in which instruments are sent between
     two or more commercial banks or other payment systems, which don't
     have a partnership agreement. It is so because compliance is
     ensured by protocol and no direct communication between the systems
     of those banks or payment systems is required.
  5. For central banks and other possible governors of monetary supply,
     RSCoin provides a way to focus on this, off-loading transaction
     handling to commercial banks and other governed entities
  6. With a simple extension, RSCoin provides a way to perform extremely
     fast international transactions with no need for manual work and
     correspondent banks;.

This repository
---

This repository is the first full implementation of RSCoin framework, as
well as its application in form of working deployable system. In this
implementation authors tried to follow paper by Dr. Danezis and Dr.
Meiklejohn as close as possible.

Platform support
---

At the moment, the only supported platform is Linux. Mac OS X isn't
officially supported, but it should be possible to deploy and run RSCoin
on Mac OS X the same way as it is done on Linux.

Windows will be supported at some point, but for now we don't have a
milestone for that.

Installation
---

At the moment, RSCoin has binary distribution served from our servers
through Nix package manager²³. To perform automatic installation, please
run the script which you can find in this repository under
``admin/install.sh``.

To build from source, clone this repository, navigate to the directory
into which the project was cloned and run ``stack build`` there.

Running
---

To understand how to run central bank, mintettes and user nodes, please
refer to ``admin/demo`` script. It is the simplest script which runs
just two mintettes, central bank and allows you to perform transactions
in bank-mode. In order to get a more sophisticated layout with more than
one user and nine mintettes, please refer to script ``admin/demo9``.

---

#### References

¹ — G. Danezis and S. Meiklejohn, “[Centrally Banked Cryptocurrencies](https://eprint.iacr.org/2015/502.pdf)”

² — NixOS Foundation, [Nix Package Manager](https://nixos.org/nix/)

³ — E. Dolstra et al,
“[Nix: A Safe and Policy-Free System for Software Deployment](http://nixos.org/~eelco/pubs/nspfssd-lisa2004-final.pdf)”
