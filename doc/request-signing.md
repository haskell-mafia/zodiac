# Request signing - general principles

This document applies to both symmetric and asymmetric request signing
in `zodiac`.

# Threat model (with no secure channel)

Alice wants to send a request to Bob, and Mallory is an active
adversary in a privileged position on the network.

## Things Mallory can do

 - Read requests from Alice to Bob.
 - Read responses to Alice from Bob.
 - Insert arbitrary delay into request delivery times (including
   preventing them from reaching Bob entirely).
 - Insert arbitrary delay into response delivery times (including
   preventing them from reaching Alice entirely).
 - Cause messages from Alice to be received multiple times by Bob
   inside the timing window Alice specified when making the request.
 - Add arbitrary headers to Alice's message which are not in the
   `Signed-Headers` list.
 - Modify Bob's responses arbitrarily.
 - Forge responses from Bob.

## Things Mallory cannot do

Herein a statement that Mallory can perform an action taken to have
the implicit suffix "[ ... ] without interfering with Bob's ability to
authenticate the message".

 - Create a message which Bob will authenticate as being from Alice.
 - Modify Alice's request payload, including adding a request payload
   when none existed and removing a request payload entirely.
 - Modify any of the headers Alice has signed without removing them
   from the list of signed headers.
 - Cause a message sent once by Alice to be received by Bob outside
   the timing window Alice specified when making the request.

# Things which should be done by the transport security layer (TLS)

 - Authenticate Bob to Alice.
 - Hide the content of the request from Mallory.
 - Ensure the integrity of the responses Bob sends to Alice.
