# HOpenPGP #

HOpenPGP is a implementation of the OpenPGP standard defined in 
[RFC 4880](http://tools.ietf.org/html/rfc4880).

OpenPGP.lhs is a literate haskell file which contains the needed parts of 
[RFC 4880](http://tools.ietf.org/html/rfc4880), as well as their implementation.

* At the moment nothing more then some of the basic binary encoding / decoding stuff has implemented.
* OpenPGPTests.hs contains test cases. User `cabal configure -f test` to enable build of test binary.
* But you can also use `runhaskell OpenPGPTests.hs` to run the tests.

 

