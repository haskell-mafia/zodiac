# HMAC verification and asymmetric signature verification

## HMAC verification

 - Cheap to compute.
 - Well-known, validated protocol (e.g., AWSv4).
 - We need to store a copy of the secret, which means a data leak
   would result in an attacker being able to forge any request.

## Asymmetric signatures

 - More expensive to compute, though less-so than they used to be.
 - No need to store secret keys on the server side, reducing the
   impact of a data leak.
 - Nobody seems to use them for web request signing, so there's no
   validated standard protocol.

# In zodiac

Summary of conversation between Mark and Sharif on 2016-05-20:

 - Storing secret keys in the database is something we should avoid if
   we can.
 - We can't see any fundamental problems with an HTTP request-signing
   protocol based on an asymmetric signature scheme like ECDSA or
   Ed25519.
 - This hasn't been done before for HTTP requests to our knowledge, so
   we'd be doing the protocol ourselves.
 - We're uncomfortable with relying on a protocol which hasn't been
   validated by anyone external.
 - Developing the protocol from spec and prototype to something that's
   been validated and can be used in production will take
   significantly longer than implementing HMAC validation.

The result of the discussion was deciding to try both approaches in
parallel: finish the HMAC implementation and use it in production
initially, and develop a draft spec and prototype implementation for
the asymmetric protocol. We can then get some external feedback on and
validation of the asymmetric protocol, and if no issues are found we
can start supporting it in the farm. Zodiac will be developed with the
intent of eventually supporting both protocols.
