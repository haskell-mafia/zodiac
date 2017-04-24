zodiac - request signing
========================

`zodiac` sits on top of [tinfoil](https://github.com/ambiata/tinfoil)
to provide a friendly interface for API servers and clients to
implement request-signing.

Protocols
=========

`zodiac` will implement two protocols: a symmetric protocol called
TSRP and an asymmetric protocol called TARP.

TSRP
----

TSRP (Trivial Symmetric Request-signing Protocol) is an HMAC-based
protocol similar to
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

TARP
----

TARP (Trivial Asymmetric Request-signing Protocol) is a public-key
protocol based on the EdDSA primitive (Edwards-curve digital signature
algorithm). It is a work in progress.

Implementation and scope
========================

All cryptographic primitives used are implemented in `tinfoil`;
`zodiac` handles the semantics of the protocol (what a request is,
how to format requests for signing, et cetera). `zodiac` does not
implement key storage or retrieval.

In particular, any operation which may be sensitive to timing attacks
(e.g., HMAC comparison) belongs in `tinfoil`, not here.

Subprojects
===========

zodiac-core
-----------

`zodiac-core` contains request-signing logic and types, including the
canonical request representation (`CRequest`), but no
protocol-specific logic. Rather than working with `CRequest`s
directly, usually one would use one of the interface packages below.

Types example:

```haskell
-- | A canonical request for signing. Contains all data needed to generate
-- a ByteString which can be signed or verified.
data CRequest =
  CRequest {
      crMethod :: !CMethod
    , crURI :: !CURI
    , crQueryString :: !CQueryString
    -- | HOST header always required.
    , crHeaders :: !CHeaders
    , crPayload :: !CPayload
    } deriving (Show, Generic)
```

zodiac-tsrp
-----------

`zodiac-tsrp` contains logic for the Trivial Symmetric Request-signing
Protocol. It wraps all cryptographic operations needed for handling
keys, hashes and MACs.

Usage example:

```haskell
import qualified Zodiac.TSRP.Key as Z

-- [ ... ]
  (keyId, secretKey) <- (,) <$> Z.genKeyId <*> Z.genSymmetricKey
```

zodiac-raw
----------

`zodiac-raw` is a `ByteString`-based interface which accepts raw HTTP
requests for signing/validation and gives back the same. This package
uses [hadron](https://github.com/ambiata/hadron) as an HTTP protocol
library, providing types and parsers.

Usage example:

```haskell
import           Zodiac.Raw

readAndAuthStdinRequest :: KeyId -> SymmetricKey -> IO ()
readAndAuthStdinRequest keyId secretKey = do
  bs <- BS.getContents -- read request from stdin
  now <- timestampRequest -- current time
  let ex = RequestExpiry 600 -- ten minutes
  authed <- authedRawRequest keyId secretKey ex bs now
  case authed of
    Left e -> T.putStrLn $ renderRequestError e
    Right ar -> BS.putStr ar

readAndVerifyStdinRequest :: KeyId -> SymmetricKey -> IO ()
readAndAuthStdinRequest keyId secretKey = do
  bs <- BS.getContents -- read request from stdin
  now <- timestampRequest -- current time
  verified <- verifyRawRequest keyId secretKey bs
  case verified of
    Verified -> T.putStrLn "good"
    NotVerified -> T.putStrLn "bad"
    VerificationError -> T.putStrLn "bad"
```

zodiac-http-client
------------------

`zodiac-http-client` provides an interface to sign and verify
`http-client` requests and give back `Request` objects which can be
sent on to a TSRP or TARP server.

```haskell
let req = def { -- [ ... whatever ... ]
  } in do
now <- timestampRequest -- now
let re = RequestExpiry 600 -- ten minutes
let authed = authedHttpClientRequest keyId symmetricKey re req now
v <- verifyHttpClientRequest keyId symmetricKey authed -- v == Verified
```

zodiac-export
-------------

`zodiac-export` is a C library which provides bindings to the
`zodiac-raw` Haskell package. It's intended for use where Haskell API
clients aren't an option and interfaces to some other language (e.g.,
Python or C++) are needed.

This is a work in progress.

zodiac-cli
----------

`zodiac-cli` is a command-line interface to signing and verification,
designed for use in scripts (e.g., with `curl`).

Example usage:

```sh
export TSRP_KEY_ID="DWPXY16451eb6f287b4d6b46ec13e36607653b"
export TSRP_SECRET_KEY="LWTGZD89cf43bed574d6e6a54bf436b3a4ba8dc658973b85aa5bfc80f05e38e01d28d7"

cat > req <<EOF
GET /foo HTTP/1.1
Host: localhost
User-Agent: curl/7.49.1
Accept: */*
EOF

cat req |\
    unix2dos |\
    tsrp authenticate -e 600 \
        > req-authed

# exits 0
cat req-authed | tsrp verify
```

### Authenticating `curl` requests

As `curl` does not support writing a request to `stdout` or reading a
request from `stdin`, this is a bit convoluted.

```sh
export TSRP_KEY_ID="DWPXY16451eb6f287b4d6b46ec13e36607653b"
export TSRP_SECRET_KEY="LWTGZD89cf43bed574d6e6a54bf436b3a4ba8dc658973b85aa5bfc80f05e38e01d28d7"

# Listen on port 8080, writing any data we receive to a file.
nc -l 8080 > request &

# Replace with your request path and options.
curl -XPOST -H "foo: bar" localhost:8080/some/path

# Sign the request and send it off; use your target host/port here.
cat request | \
  tsrp authenticate | \
  socat openssl:example.com:443 - > response
```
