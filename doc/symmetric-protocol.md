# Symmetric request authentication

This document is a high-level outline of a protocol for authenticating
HTTP requests via a hash-based message authentication code (HMAC).

# Canonical request form

The request is first transformed into a canonical form for signing,
which is constructed as described [here](canonical-request.md).

# Signed data

The following data is included in the request's signature:

 - Algorithm.
 - Request date.
 - Key ID.
 - The SHA256 hash of the canonical request.

## Signing key

The signing key is derived from the secret key via an iterated chain
of SHA256 hashes, as follows:

```haskell
let dateKey = hmacSHA256 ("zodiac-begin" + k) date
    idKey = hmacSHA256 dateKey keyID
    signingKey = hmacSHA256 idKey "zodiac-end"
```
