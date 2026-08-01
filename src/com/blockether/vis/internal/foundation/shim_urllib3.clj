(ns com.blockether.vis.internal.foundation.shim-urllib3
  "Built-in sandbox SHIM: a `urllib3`-compatible module for the model's Python
   sandbox, implemented as a thin wrapper over the already-installed `requests`
   shim (which rides the sandbox socket via stdlib urllib and honours the network
   guard). No pip, no native wheel, no host bridge.

   The preamble publishes a `urllib3` module (plus `urllib3.exceptions`) into
   `sys.modules` and staples it onto builtins. It exposes the surface agents
   reach for: `PoolManager` / `HTTPConnectionPool` / `HTTPSConnectionPool` with
   `.request(method, url, fields=, headers=, body=, json=)`, a top-level
   `urllib3.request(...)` (urllib3 2.x), an `HTTPResponse` (`.status`, `.data`,
   `.headers`, `.json()`, `.read()`, `.getheader()`), `HTTPHeaderDict`,
   `disable_warnings()`, and the `urllib3.exceptions` tree (`HTTPError`,
   `MaxRetryError`, `NewConnectionError`, `ReadTimeoutError`, `ProtocolError`,
   `InsecureRequestWarning`). Real connection pooling / retries are no-ops."
  (:require [com.blockether.vis.core :as vis]))


(def vis-extension
  (vis/extension
    {:ext/name "foundation-shim-urllib3"
     :ext/description
     "Sandbox `urllib3` subset (`PoolManager`, `urllib3.request`) wrapping the requests shim. No pip/wheel/host bridge."
     :ext/version "0.1.0"
     :ext/author "Blockether"
     :ext/owner "vis"
     :ext/license "Apache-2.0"
     :ext/kind "foundation"
     :ext/sandbox-shims
     [{:shim/name "urllib3"
       :shim/imports ["urllib3"]
       :shim/description
       "`urllib3` (`PoolManager`, `HTTPResponse`, `request`) over the requests shim. Retries, pooling, and low-level TLS options are best-effort no-ops."
       :shim/source "vis-shims/urllib3.py"}]}))

(vis/register-extension! vis-extension)
