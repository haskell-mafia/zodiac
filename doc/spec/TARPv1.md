Trivial Asymmetric Request-signing Protocol, version 1
======================================================

Abstract
========

The Trivial Asymmetric Request-signing Protocol (TARP) is a protocol for
signing HTTP requests from a client to a server using a public-key
cryptosystem. It is designed for use over a secure channel such as
TLS.

Definitions
===========

 - *HTTP* and *HTTP request* shall refer to the HTTP/1.1 protocol
   defined in RFC 7230-7235[0] or any later version of that protocol.

 - *client* shall refer to the party initiating an HTTP request.

 - *server* shall refer to the party receiving an HTTP request from a
   client.

 - *requester* shall refer to a party (whether a person, organization
   or computer system) known to the server, which is authorized to
   request some resource from the server.

 - *secure channel* refers to a channel for message transport with
   the properties of *confidentiality* and *verification of message
   integrity*. TLS is an example of a secure channel for the purposes
   of this specification.

 - *signature* refers to a signature computed using a
   digital-signature scheme which attests to the integrity and origin
   of a message in a manner verifiable using the sender's public key.

 - *EdDSA* refers to the Edwards-curve Digital Signature Algorithm
   as described by Daniel Bernstein and others in the Journal of
   Cryptographic Engineering. [1]

 - *Ed25519* refers to EdDSA computed using the SHA-512 cryptographic
   hash function with a twisted Edwards curve as described by Daniel
   Bernstein and others. [1]

Definitions in specifications of message formats
------------------------------------------------

 - The symbol '+' shall refer to string concatenation.

 - The term *hexadecimal encoding* shall refer to the representation
   of a string of bytes of length `n` as the hexadecimal
   representation of that string in lowercase ASCII characters of
   length `2n`.

 - The symbol '\n' shall refer to the linefeed character, represented
   in ASCII as the byte 0x0a.

Protocol overview
=================

Purpose
-------

The purpose of the TARPv1 protocol is as follows:

 - To allow a server to verify the origin and integrity of requests
   made by a client; and

 - To associate those requests with the identity of a requester known
   to the server; and

 - To do this without the server requiring knowledge of the
   authentication secret used by the requester, in order to limit the
   security impact of a data breach scenario.

For example, this protocol could be used by a publishing company which
provides an HTTP API for retrieval of published documents. The
requestors would be the company's customers, who would sign API
requests with their private key. The server can then use the public
key associated with the customer's account to verify that a request
for a document came from a legitimate customer.

Protocol scope and design goals
-------------------------------

The TARPv1 protocol is designed to be used over a secure channel
which provides

 - confidentiality;

 - authentication of the server to the client via public-key
   infrastructure; and

 - prevention of replay attacks.

Therefore, these properties are for the most part not addressed by
this protocol except as described below.

In this section, "attacker" shall refer to an attacker with the
following capabilities:

 - Reading of arbitrary network packets.

 - Modification of arbitrary network packets.

 - Delay of delivery of arbitrary network packets.

 - Reading arbitrary information from the database and memory of the
   authentication server.

Even where not used over a secure channel, TARPv1 ensures that an
attacker cannot do the following:

 - Learn a requester's private key.

 - Create a request which the server will verify as originating
   from a valid requester.

 - Modify the canonical form of a valid request (as described in the
   section *Constructing a canonical request for signing*)
   without causing the server to reject the request as invalid.

 - Cause a message sent once by the client to be received and
   validated by the server outside the timing window specified by the
   client when creating the request (assuming that both client and
   server have accurate clocks).

Keys
====

Private key
-----------

The *private key* held by the requester shall consist of 38 bytes as
follows.

### Tag

The first 6 bytes of the private key shall be the ASCII string
"LETGZD". This string has been chosen to aid identification of key
material, e.g., as part of checks conducted prior to publishing a
codebase.

The tag bytes shall not be considered part of the key for
cryptographic purposes.

### Key

The last 32 bytes of the private key shall be an Ed25519 secret key,
selected uniformly at random as described by Bernstein et al. [1]

Public key
----------

The *public key* shared by the requester and the server shall consist
of 38 bytes as follows.

### Tag

The first 6 bytes of the public key shall be the ASCII string
"DEPXY1". This string has been chosen to aid identification of key
material, e.g., as part of checks conducted prior to publishing a
codebase.

The tag bytes shall not be considered part of the key for
cryptographic purposes.

### Key

The last 32 bytes of the public key shall be an Ed25519 public key,
computed from the secret key as described by Bernstein et al. [1]

Constructing a canonical request for signing
============================================

Summary
-------

The *canonical request* is the form to which an HTTP request is
converted before the signature of the request is computed. The canonical
request is computed from the following data, which are each separated
by a line-feed character (`\n`):

 - Request method.

 - Request URI.

 - Request query string.

 - Request headers.

 - Hash of request payload.

Definitions
-----------

 - In this section, *URL-encoded* shall mean encoded as a valid URI as
   specified by RFC 3986[2].

Request method
--------------

The *request method* field shall consist of one of the methods
explicitly defined in section 4.1 of RFC 7231.

Example values for this field are "GET" and "POST".

Request URI
-----------

The *request URI* field shall consist of the part of the request URL
between the last character of the host part of the URL and the '?'
character preceding the query-string part of the URL, exclusive. This
is also referred to as the request path. If the URL does not have a
query-string part, the URI shall continue to the end of the URL string.

The request URI field shall be URL-encoded.

Example values for this field are "/" and "/example/page.html".

Request query string
--------------------

The query string field is the query-string part of the URL, from the
initial '?' character (exclusive) to the end of the URL. If the URL
does not have a query string, this field shall be the empty string.

Each parameter in the query string shall be URL-encoded.

Example values for this field are "", "key1=value1&key2=value2" and
"key=value%26with%26ampersands".

Request headers
---------------

The headers field is a lexicographically-sorted (by header name) list
of the HTTP headers in the request in the form of the *header name*,
followed by the colon character ':', followed by a comma-separated
list of *header values* associated with the header in the order they
appear in the request, and separated by a linefeed character.

The list of headers computed by the *client* shall include all headers
in the original request. In particular, canonical requests not
including a "HOST" header as defined by RFC 7230 shall be deemed
invalid and rejected by the server.

The list of headers computed by the *server* shall include all headers
listed in the *signed headers* value described in the section
*Authentication header*. Any extra headers present in the HTTP request
received by the server shall be ignored. Any headers listed in the
*signed headers* value but not present in the request shall be
considered an error.

Each *header name* must be converted to lower-case.

Each *header value* must be transformed (if necessary) such that any
leading or trailing space characters are removed; and such that any
remaining contiguous strings of space characters are replaced by a
single space character. For example, the header value "  example
value" would be transformed to "example value".

An example value for this field is
"host:example.com\nexample-name:example value1,example value2".

Hash of the request payload
---------------------------

The *request payload* is the body of the request as defined by section
3.3 of RFC 7230. The hash of the request payload is computed as the
hexadecimal-encoded SHA-256 hash of the unmodified payload.

If the request does not have a body, the payload hash shall be
computed as the hexadecimal-encoded SHA-256 hash of the empty string.

Constructing a string for signing
=================================

Summary
-------

This section deals with constructing the string from which the request
signature will be computed. It contains the following fields, each of
which is separated by a linefeed character:

 - Protocol designator.
 - Request timestamp.
 - Request expiry.
 - Public key.
 - Hash of the canonical request.

Protocol designator
-------------------

This is the string "TARPv1".

Request timestamp
-----------------

This is the date and time of the request in UTC, to second
precision. It is determined by the client.

This value is formatted according to ISO 8601. An example value for
this field is "2016-01-23T01:23:45", which refers to the 23rd day of
January of the year 2016 CE, at one hour twenty-three minutes and
forty-five seconds past midnight in Coordinated Universal Time (UTC).

If the server receives a request with a request timestamp which is
more than *600 seconds* (ten minutes) after the current time as
determined by the server, the server shall respond to the request with
an error.

Request expiry
--------------

This is the number of seconds after the request timestamp during which
the server will consider the request valid. It is determined by the
client, and the server shall respond with an error to any request
after this window.

This value is formatted as a decimal integer number of seconds. An
example value for this field is "60", which refers to an expiry one
minute after the request timestamp.

The request expiry has a minimum value of 1. It has a maximum value of
31536000, which is the number of seconds in one solar year (rounded to
the nearest integer). The server shall reject any request containing a
request expiry outside of this range.

Public key
----------

This is the public key associated with the private key with which the
requester will sign the request. It is formatted as the 6-character
prefix tag "DEPXY1" followed by the 32-character hexadecimal encoding
of the Ed25519 public key.

An example value for this field is
"DEPXY1deadbeefd574d6e6a54bf436b3a4ba8dc658973b85aa5bfc80f05e38e01d28d7".

Hash of the canonical request
-----------------------------

This is the SHA-256 hash of the *canonical request* as defined in this
specification. It is formatted as the 64-character hexadecimal
encoding of the value.

An example value for this field is
"7d865e959b2466918c9863afca942d0fb89d7c9ac0c99bafc3749504ded97730".

Example
-------

An example of the full string for signing is as follows:

"TARPv1\n" +
"2016-01-23T01:23:45\n" +
"60\n" +
"DEPXY1deadbeefd574d6e6a54bf436b3a4ba8dc658973b85aa5bfc80f05e38e01d28d7\n" +
"7d865e959b2466918c9863afca942d0fb89d7c9ac0c99bafc3749504ded97730"

Authentication header
=====================

The information required to verify a request is included in an
HTTP header which is added to the request by the client. This header
is the *Authorization* header, as specified by section 4.2 of RFC 7235
(note that despite the name, the usual function of this header is
authentication, not authorization).

The header name "Authorization" is case-insensitive, per RFC 7230. The
header value is constructed from the following fields, which are
separated by the space character:

 - Protocol designator.
 - Public key.
 - Request timestamp.
 - Request expiry.
 - List of signed headers.
 - Request signature.

Protocol designator
-------------------

This value is the string "TARPv1".

Public key
----------

This is the hexadecimal-encoded *public key* value as described in the
section *Constructing a string for signing*.

Request timestamp
-----------------

This is the request timestamp in ISO 8601 format as described in the
section *Constructing a string for signing*.

Request expiry
--------------

This is a decimal integer as described in the section *Constructing a
string for signing*.

List of signed headers
-----------------------------

The list of *signed headers* is constructed from the header
names of the headers included in the canonical request. The list is
sorted lexicographically and all values are converted to lowercase;
the list is rendered with values separated by commas.

An example value is "host,myheader1,myheader2,x-content-type".

Request signature
-----------------

The *request signature* is 64 bytes computed as the output of the
Ed25519 signing operation described by Bernstein et al[1], taking the
*string for signing* as the message and the last 32 bytes of the
*private key* as the secret key.

Example
-------

An example of the full header value is
"TARPv1 DEPXY18c57b5cde3dc531dbfa19e781f24605e 2016-01-23T01:23:45 60 host,myheader1,myheader2,x-content-type c08f542854a965d440c0e9b4d5383c022310ff81f468609a6a228a035bc17ced6e9439598e3a853eab6e0ce1c404affb9ba0959479f87eba1f60632338a41eae".

References
==========

[0] https://www.ietf.org/rfc/rfc7230.txt
    https://www.ietf.org/rfc/rfc7231.txt
    https://www.ietf.org/rfc/rfc7232.txt
    https://www.ietf.org/rfc/rfc7233.txt
    https://www.ietf.org/rfc/rfc7234.txt
    https://www.ietf.org/rfc/rfc7235.txt

[1] https://ed25519.cr.yp.to/ed25519-20110926.pdf

[2] https://www.ietf.org/rfc/rfc3986.txt
