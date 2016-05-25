# Canonical requests for signing

# Definitions

 - URI-encoded shall mean "encoded as a valid URI as defined by RFC
   3986".

# Canonicalization of requests

The `canonical request` shall consist of the following fields, each
separated by a line-feed character (`0x0a`).

 - HTTP method.
 - URI.
 - Query string.
 - Headers.
 - Signed header list.
 - A hash of the request payload.

## HTTP method

The HTTP method field shall consist of one of the methods explicitly defined
in section 9 of RFC 2616 (e.g., "GET" or "POST").

## URI

The URI field shall be URI-encoded, and shall consist of the part of
the request URL between the last character of the host part of the URL
and the '?' character preceding the query-string part of the URL,
exclusive. If the URL does not have a query-string part, the URI shall
continue to the end of the URL string.

## Query string

The query string field is the query-string part of the URI, from the
initial '?' character (exclusive) to the end of the URL. Each
parameter in the query string shall be URL-encoded.

## Headers

The headers field is a sorted (by header name) list of HTTP headers in
the form `header_name + ':' + header_values + '\n'`.

 - `+` denotes concatenation.
 - `header_name` is the header name with all ASCII letters converted
   to lowercase.
 - `header_values` is a comma-separated list of values associated with
   a header, in the order the values appear in the request.
 - FIXME: do we care about whitespace in the header values?

## Hash of the request payload

This field is constructed as follows:

 - Hash the request payload, exactly as it appears in the original
   HTTP request. The acceptable hash functions will be specified by
   the signing protocol.
 - Encode the resulting hash as a hexadecimal string, in lowercase.
