# Trivial Asymmetric Request-signing Protocol

This document is a high-level outline of a protocol for authenticating
HTTP requests via an asymmetric digital signature algorithm.

# Canonical requests

A `canonical request` is an HTTP request transormed as described
[here](canonical-request.md). This is the form a request must be in
when it is hashed prior to signing.

# Hash functions

A cryptographic hash function is used at various points in the
protocol, represented from here on as `HASH(data)`. The function used
must be specified in a header in the signed request (as described
below). The function used must be chosen from the following list. Each
conforming implementation must support at least one of the functions
on the list; communicating the list of supported hash functions to
clients is beyond the scope of this protocol, and a server may treat
any unsupported hash function as invalid.

## Acceptable hash functions

 - SHA_256 (mandatory)

# Digital signature algorithms

In the same manner as the hash function above, the digital signature
algorithm used must be specified as a header in the signed request (as
described below). A conforming implementation must support at least
one of the acceptable algorithms, and may treat any unsupported
algorithms as invalid.

## Acceptable digital signature algorithms

 - ED_25519

