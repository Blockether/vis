(ns com.blockether.vis.internal.foundation.shim-urllib3
  "Built-in sandbox SHIM: a `urllib3`-compatible module for the model's Python
   sandbox, implemented as a thin wrapper over the already-installed `requests`
   shim (which rides the sandbox socket via stdlib urllib and honours the network
   guard). No pip, no native wheel, no host bridge.

   The preamble publishes `urllib3` as a PACKAGE and staples it onto builtins:
   every submodule real code imports has its own `sys.modules` entry --
   `urllib3.exceptions`, `.response`, `.poolmanager`, `.connectionpool`,
   `.fields`, `.filepost`, `._collections`, and `urllib3.util` with `.retry` /
   `.timeout` / `.url` / `.request`.

   It exposes the surface agents reach for: `PoolManager` / `ProxyManager` /
   `HTTPConnectionPool` / `HTTPSConnectionPool` with `.request(method, url,
   fields=, headers=, body=, json=)` and `connection_from_url`, a top-level
   `urllib3.request(...)` (urllib3 2.x), an `HTTPResponse` (`.status`, `.data`,
   `.headers`, `.json()`, `.read()`, `.stream()`, `.getheader()`),
   `HTTPHeaderDict`, `Retry`, `Timeout` (turned into the transport's
   connect/read pair), `parse_url` / `Url`, `make_headers`, `RequestField` +
   `encode_multipart_formdata`, `disable_warnings()` (which really installs the
   warnings filter), and the
   `urllib3.exceptions` tree (`HTTPError`, `MaxRetryError`,
   `NewConnectionError`, `ReadTimeoutError`, `ProtocolError`, `IncompleteRead`,
   `ProxySchemeUnknown`, `InsecureRequestWarning`). Real connection pooling and
   retry loops are no-ops, and a proxy URL is recorded rather than dialled: the
   sandbox does its own egress. The TLS options are NOT no-ops -- `cert_reqs`,
   `ca_certs`, `ca_cert_dir`, `cert_file`/`key_file`, `assert_hostname` and
   `ssl_context` map onto the requests shim's `verify=`/`cert=` and reach the
   socket."
  (:require [com.blockether.vis.core :as vis]))


(def vis-extension
  (vis/extension
    {:ext/name "foundation-shim-urllib3"
     :ext/description
     (str "Sandbox `urllib3` package (`PoolManager`, `util`, `fields`, `response`) wrapping the "
          "requests shim. No pip/wheel/host bridge.")
     :ext/version "0.1.0"
     :ext/author "Blockether"
     :ext/owner "vis"
     :ext/license "Apache-2.0"
     :ext/kind "foundation"
     :ext/sandbox-shims
     [{:shim/name "urllib3"
       :shim/imports ["urllib3"]
       :shim/description
       (str
         "`urllib3` as a package: `PoolManager`/`ProxyManager`, `HTTPResponse`, `request`, `util`, "
         "`fields`, `filepost`, `exceptions`. TLS options (`cert_reqs`, `ca_certs`, `ssl_context`) "
         "reach the socket; retries and pooling are best-effort no-ops.")
       :shim/source "vis-shims/urllib3.py"}]}))

(vis/register-extension! vis-extension)
