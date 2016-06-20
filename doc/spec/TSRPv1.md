Trivial Symmetric Request-signing Protocol, version 1
=====================================================

Abstract
========

The Trivial Symmetric Request-signing Protocol (TSRP) is a protocol for
authenticating HTTP requests from a client to a server via a shared
secret key. It is designed for use over a secure channel such as TLS.

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
 - *MAC* refers to a message authentication code computed using a
   keyed hash function.
 - *SHA256* refers to the hash function of the SHA-2 family described
   by the National Institute of Standards and Technology (NIST)[1].
 - *HMAC* refers to the keyed hash function described in RFC 2104[2].
 - *HMAC_SHA256* refers to HMAC computed using SHA256 as defined
   above.

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

The purpose of the TSRPv1 protocol is to allow a server to verify the
origin and integrity of requests made by a client, and to associate
those requests with the identity of a requester known to the server.

For example, this protocol could be used by a publishing company which
provides an HTTP API for retrieval of published documents by the
company's customers to verify that a request for a document came from
a legitimate customer, and to track the number of documents requested
by each customer.

Technical overview
------------------

TSRPv1 uses the SHA256 hash function and the HMAC_SHA256 keyed hash
function to authenticate messages. The protocol relies on a secret key
known to both the server and the client and associated with a key ID
also known to both parties which uniquely identifies both the secret
key and the client.

The request is rendered in a canonical form by the client and hashed;
this hash is combined with the request timestamp, expiry and key ID
and the MAC of this string is computed with a key derived from the
secret key, which changes based on the date in order to limit the
number of messages authenticated with any single key.

The metadata necessary to compute the values required by the protocol
are included in an HTTP header along with the MAC, which is then added
to the original HTTP request by the client and sent to the server.

The server retrieves the secret key associated with the key ID
included in the request it receives, and then follows the same steps
as the client to compute the request MAC. The server then verifies
that the MAC it computed is the same as that provided by the
client. If at any point in this process the server cannot complete one
of the steps in the protocol (e.g., because it receives a key ID it
has not associated with a requester, or because the MAC it computes
does not match the one provided by the client) it immediately
terminates the protocol and responds to the client with an
authentication error.

Protocol scope and design goals
-------------------------------

The TSRPv1 protocol is designed to be used over a secure channel
which provides confidentiality, authentication of the server to the
client via public-key infrastructure and prevention of replay attacks;
therefore, these properties are for the most part not addressed by
this protocol. However, even where not used over a secure channel,
TSRPv1 is designed to ensure that an attacker with the ability to
read, insert, delay and modify arbitrary packets on the network cannot
do the following (without obtaining a valid key by some means outside
the scope of this protocol, e.g., coercing a requester into revealing
it):

 - Learn a requester's secret key from network traffic.
 - Create a request which the server will authenticate as originating
   from a valid requester.
 - Modify the canonical form of a valid request (as described in the
   section *Constructing a canonical request for authentication*)
   without causing the server to reject the request as invalid.
 - Cause a message sent once by the client to be received and
   validated by the server outside the timing window specified by the
   client when creating the request (assuming that both client and
   server have accurate clocks).

Secret key
==========

The *secret key* shared by the requester and the server shall consist of
32 bytes selected uniformly at random, to be generated by the
server. The distribution of this key to the requester must be via a
secure channel.

Key ID
======

The *key ID* is a value known to both the requester and the server and
associated in a one-to-one manner with the secret key. It shall
consist of 16 bytes, to be generated by the server and distributed to
the requester along with the secret key.

A requester may be associated with many key IDs, but each key ID shall be
associated with only one requester such that verifying that a party
holds the secret key associated with a key ID shall uniquely identify
that party as a requester.

Constructing a canonical request for authentication
===================================================

Summary
-------

The *canonical request* is the form to which an HTTP request is
converted before the MAC of the request is computed. The canonical
request is computed from the following data:

 - Request method.
 - Request URI.
 - Request query string.
 - Request headers.
 - List of authenticated headers.
 - Hash of request payload.

Definitions
-----------

 - In this section, *URL-encoded* shall mean encoded as a valid URI as
   specified by RFC 3986[3].

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
appear in the request, and terminated with a linefeed character.

The list of headers computed by the *client* shall include all headers
in the original request. In particular, canonical requests not
including a "HOST" header as defined by RFC 7230 shall be deemed
invalid and rejected by the server.

The list of headers computed by the *server* shall include all headers
listed in the *authenticated headers* value described in the section
*Authentication header*. Any extra headers present in the HTTP request
received by the server shall be ignored. Any headers listed in the
*authenticated headers* value but not present in the request shall be
considered an error.

Each *header name* must be converted to lower-case.

Each *header value* must be transformed (if necessary) such that any
leading or trailing space characters are removed; and such that any
remaining contiguous strings of space characters are replaced by a
single space character. For example, the header value "  example
value" would be transformed to "example value".

An example value for this field is
"host:example.com\nexample-name:example value1,example value2\n".

List of authenticated headers
-----------------------------

The list of *authenticated headers* is constructed from the header
names of the headers included in the canonical request. The list is
sorted lexicographically and all values are converted to lowercase;
the list is rendered with values separated by commas.

When computed by the *client*, this value shall be the header names of
all headers included in the *Request headers* value described above.

When computed by the *server*, this value shall be the list of
authenticated headers described in the *Authentication header*
section.

An example value is "host,myheader1,myheader2,x-content-type".

Hash of the request payload
---------------------------

The *request payload* is the body of the request as defined by section
3.3 of RFC 7230. The hash of the request payload is computed as the
hexadecimal-encoded SHA256 hash of the unmodified payload.

If the request does not have a body, the payload hash shall be
computed as the hexadecimal-encoded SHA256 hash of the empty string.

Constructing a string for authentication
========================================

Summary
-------

This section deals with constructing the string from which the request
MAC will be computed. It contains the following fields, each of which
is terminated by a linefeed character:

 - Protocol designator.
 - Request timestamp.
 - Request expiry.
 - Key ID.
 - Hash of the canonical request.

Protocol designator
-------------------

This is the string "TSRPv1".

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

Key ID
------

This is the key ID associated with the secret key with which the
client will authenticate the request. It is formatted as the
32-character hexadecimal encoding of the key ID.

An example value for this field is
"8c57b5cde3dc531dbfa19e781f24605e".

Hash of the canonical request
-----------------------------

This is the SHA256 hash of the *canonical request* as defined in this
specification. It is formatted as the 64-character hexadecimal
encoding of the value.

An example value for this field is
"7d865e959b2466918c9863afca942d0fb89d7c9ac0c99bafc3749504ded97730".

Example
-------

An example of the full string for authentication is as follows:

"TSRPv1\n" +
"2016-01-23T01:23:45\n" +
"60\n" +
"8c57b5cde3dc531dbfa19e781f24605e\n" +
"7d865e959b2466918c9863afca942d0fb89d7c9ac0c99bafc3749504ded97730\n"

Authentication key
==================

The request MAC is not computed directly from the secret key, but
instead from an *authentication key* derived from the secret key
through a series of chained keyed hash functions in addition to the
following values:

 - Request date.
 - Key ID.
 - Protocol designator.

Request date
------------

This is the date of the request as determined by the client, formatted
according to ISO 8601. It should be identical to the date part of the
*request timestamp* value used in the string to authenticate.

An example request date value is "2016-01-23".

Key ID
------

This is the hexadecimal-encoded *key ID* value described in the
section *Constructing a string for authentication*.

Protocol designator
-------------------

This value is the string "TSRPv1".

Deriviation of authentication key
---------------------------------

The deriviation of the authentication key is a sequence of
applications of the keyed hash function *HMAC_SHA256*; the result of
each HMAC_SHA256 computation is used as the key in the next
computation, as follows.

 - The *temporary key* is computed as the MAC of the *key ID*
   with a key constructed by appending the *request date* to the
   *secret key*.
 - The *authentication key* is computed as the MAC of the *protocol
   designator* with the *temporary key*.

Note that in the above computation, "MAC" refers to the message
authentication code computed with the HMAC_SHA256 algorithm.

Expressed in pseudocode, taking the first argument of the HMAC_SHA256
function to be the key and the second argument to be the data to be authenticated:

```
temporaryKey <- HMAC_SHA256(secret_key + request_date, key_id)
authenticationKey <- HMAC_SHA256(temporary_key, protocol_designator)
```

Authentication header
=====================

The information required to authenticate a request is included in an
HTTP header which is added to the request by the client. This header
is the *Authorization* header, as specified by section 4.2 of RFC 7235
(note that despite the name, the usual function of this header is
authentication, not authorization).

The header name "Authorization" is case-insensitive, per RFC 7230. The
header value is constructed from the following fields, which are
separated by the space character:

 - Protocol designator.
 - Key ID.
 - Request timestamp.
 - Request expiry.
 - List of authenticated headers.
 - Request MAC.

Protocol designator
-------------------

This value is the string "TSRPv1".

Key ID
------

This is the hexadecimal-encoded *key ID* value as described in the
section *Constructing a string for authentication*.

Request timestamp
-----------------

This is the request timestamp in ISO 8601 format as described in the
section *Constructing a string for authentication*.

Request expiry
--------------

This is a decimal integer as described in the section *Constructing a
string for authentication*.

List of authenticated headers
-----------------------------

This value is a list of header names of headers included in the
*canonical request*. The list is sorted lexicographically and all
values are converted to lowercase; the list is rendered with values
separated by commas.

An example value is "host,myheader1,myheader2,x-content-type".

Example
-------

An example of the full header value is 
"TSRPv1 8c57b5cde3dc531dbfa19e781f24605e 2016-01-23T01:23:45 60 host,myheader1,myheader2,x-content-type bf07a7fbb825fc0aae7bf4a1177b2b31fcf8a3feeaf7092761e18c859ee52a9c".

References
==========

[0] https://www.ietf.org/rfc/rfc7230.txt
    https://www.ietf.org/rfc/rfc7231.txt
    https://www.ietf.org/rfc/rfc7232.txt
    https://www.ietf.org/rfc/rfc7233.txt
    https://www.ietf.org/rfc/rfc7234.txt
    https://www.ietf.org/rfc/rfc7235.txt

[1] https://web.archive.org/20130526224224/http://csrc.nist.gov/groups/STM/cavp/documents/shs/sha256-384-512.pdf

[2] https://www.ietf.org/rfc/rfc2104.txt

[3] https://www.ietf.org/rfc/rfc3986.txt
