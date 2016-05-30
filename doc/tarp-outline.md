# Trivial Asymmetric Request-signing Protocol

This document is a high-level outline of a protocol for authenticating
HTTP requests via an asymmetric digital signature algorithm.

# Purpose

This protocol covers the authentication of HTTP requests from a client
to a server, i.e., establishing to the server that the client sending
a request has control of a particular private key. It does not consider
authenticating the server to the client, or determining whether the
client has sufficient privileges to perform a particular action on the
server.

# Definitions

 - `+` shall refer to string concatenation.
 - "hexadecimal encoding" shall mean the representation of a string of
   bytes of length `n` as the hexadecimal representation in lowercase
   ASCII characters of length `2n`.

# Keys and signatures

The protocol relies on an asymmetric signature scheme to verify the
integrity and origin of the request. For the purposes of this
document, a party which signs requests to be verified is referred to
as a "client"; a party which verifies signed requests is referred to
as a "server".

## Keys

Each client is associated with one or more sets of authentication
details. (A client may hold many keys, but each key may only be
associated with one client.)

Each set of authentication details consists of:

 - A key ID.
 - A private key.
 - A public key.

### Key ID

The key ID is a 22-byte value constructed as `version_string + token`,
where

 - `version_string` is the string "TARPv1".
 - `token` is a string consisting of the hexadecimal encoding of eight
   bytes.

### Public key

The public key is a 64-byte value consisting of the hexadecimal
encoding of 32 bytes.

### Private key

The private key is a 128-byte value consisting of the hexadecimal
encoding of 64 bytes.

### Key jurisdiction

The server shall store the key ID and the public key in a manner such
that both the public key and the client associated with it can be
easily retrieved given the key ID alone; the implementation details
are beyond the scope of this document.

The client shall store the key ID and the private key.

## Key generation

TBD

## Signatures

Signatures are represented as the 64-byte hexadecimal encoding of the
32 bytes of the signature.

# Canonical requests

A `canonical request` is an HTTP request transormed as described
[here](canonical-request.md). This is the form a request must be in
when it is hashed prior to signing.

# Hash functions

A cryptographic hash function is used at various points in the
protocol, represented from here on as `HASH` or `HASH(data)`. The function used
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

# Signed data

The following data are included in the signature of a request:

 - Hash function identifier.
 - Digital signature algorithm identifier.
 - Request date.
 - Key ID.
 - The HASH of the canonical request.
