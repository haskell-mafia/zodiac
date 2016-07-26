zodiac - request signing
========================

`zodiac` sits on top of [tinfoil](https://github.com/ambiata/tinfoil)
to provide a friendly interface for API servers and clients to
implement request-signing.

Protocol
========

`zodiac` implements a HMAC-based protocol similar to
[AWSv4](https://docs.aws.amazon.com/general/latest/gr/sigv4_signing.html);
a chain of authenticated hashes is used to derive the message-signing
key from the secret key, the current date, the key ID and the
requested service. This key is used to sign the request (URI, query
parameters, headers and a hash of the request body); the signature is
added as a request header in addition to assorted metadata identifying
the signing protocol, version and hash algorithms used. Once generated
by the client, the signed request is sent to the server, which uses
its copy of the secret key to perform the same signing algorithm and
verifies that the two signatures match.

Implementation and scope
========================

All cryptographic primitives used are implemented in `tinfoil`;
`zodiac` handles the semantics of the protocol (what a request is,
how to format requests for signing, et cetera). `zodiac` does not
implement key storage or retrieval.

In particular, any operation which may be sensitive to timing attacks
(e.g., HMAC comparison) belongs in `tinfoil`, not here.

Example usage
=============

```haskell
signRequest :: KeyId -> SymmetricKey -> RequestTimestamp -> RequestExpiry -> Request -> MAC

verifyRequest :: Request -> MAC -> SymmetricKey -> IO (Either VerificationError Verified)
```
