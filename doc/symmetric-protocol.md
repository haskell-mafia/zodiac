# Symmetric request authentication

This document is a high-level outline of a protocol for authenticating
HTTP requests via a hash-based message authentication code (HMAC).

# Definitions

 - `+` shall refer to string concatenation.
 - "hexadecimal encoding" shall mean the representation of a string of
   bytes of length `n` as the hexadecimal representation in lowercase
   ASCII characters of length `2n`.

# Canonical request form

The request is first transformed into a canonical form for signing,
which is constructed as described [here](canonical-request.md).

# Signed data

The following data is included in the request's signature:

 - Protocol.
 - Algorithm.
 - Request timestamp.
 - Request expiry.
 - Key ID.
 - The SHA256 hash of the canonical request.

## Signing key

The signing key is derived from the secret key via an iterated chain
of SHA256 hashes, as follows:

```haskell
let dateKey = hmacSHA256 ("TSRPv1-begin" + k) date
    idKey = hmacSHA256 dateKey keyID
    signingKey = hmacSHA256 idKey "TSRPv1-end"
```

 - `date` is the ISO 8601 date of the request in UTC, e.g.,
   `"2016-06-24"`.
 - `keyID` is the hexadecimal encoding of the 32-byte key ID, e.g.,
   `"8c57b5cde3dc531dbfa19e781f24605e113e3d2f34f63006f431bf6dec12ad9f"`.

# Authentication headers

The following headers are added to a request before it is signed:

 - `X-Zodiac-Protocol`: "TSRPv1"
 - `X-Zodiac-HashAlgorithm`: "SHA-256"
 - `X-Zodiac-KeyId`: key ID.
 - `X-Zodiac-Timestamp`: ISO 8601-formatted date and time of the request.
 - `X-Zodiac-Expires`: number of seconds from the request timestamp
   for the request to be valid.
 - `X-Zodiac-SignedHeaders`: list of headers included in the signature.
