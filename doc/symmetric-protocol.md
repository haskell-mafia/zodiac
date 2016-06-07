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

These fields are rendered separated by the linefeed character `0x0a` before
signing. 

For example:

```
TSRPv1
HMAC-SHA256
2016-06-06T01:23:45
60
8c57b5cde3dc531dbfa19e781f24605e
7d865e959b2466918c9863afca942d0fb89d7c9ac0c99bafc3749504ded97730
```

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
 - `keyID` is the hexadecimal encoding of the 16-byte key ID, e.g.,
   `"8c57b5cde3dc531dbfa19e781f24605e"`.

# Authentication headers

The following header is added to a request after it is signed:

```
Authorization: $protocol $hash_algorithm $key_id $request_timestamp $request_expiry $signed_headers
```

 - All fields are space-separated.
 - `$protocol` is replaced by "TSRPv1".
 - `$hash_algorithm` is replaced by "SHA-256".
 - `$key_id` is replaced by key ID.
 - `$request_timestamp` is replaced by ISO 8601-formatted date and time of the request.
 - `$request_expiry` is replaced by number of seconds from the request
   timestamp for the request to be valid.
 - `$signed_headers` is replaced by lowercase list of headers included
   in the signature, separated by commas.
