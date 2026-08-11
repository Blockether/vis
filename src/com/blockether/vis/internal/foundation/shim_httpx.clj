(ns com.blockether.vis.internal.foundation.shim-httpx
  "Built-in sandbox SHIM: an `httpx`-compatible module for the model's Python
   sandbox, implemented as a thin synchronous wrapper over the already-installed
   `requests` shim (which itself rides the sandbox socket via stdlib urllib and
   honours the network guard). No pip, no native wheel, no host bridge.

   The preamble publishes an `httpx` module into `sys.modules` (so `import httpx`
   and `httpx.get(...)` work) and staples it onto builtins. It exposes the sync
   surface agents actually reach for: module-level `get/post/put/patch/delete/
   head/options/request`, a `Client` (with `base_url`, default headers/params,
   context-manager support), an httpx-style `Response` (`.status_code`, `.text`,
   `.content`, `.json()`, `.headers`, `.url`, `.elapsed`, `.is_success/.is_error/
   .is_redirect`, `.raise_for_status()`), `Headers`, `URL`, `Timeout`, and the `httpx` exception
   tree (`HTTPError`, `RequestError`, `HTTPStatusError`, `TimeoutException`,
   `ConnectError`). Async is supported too: an `AsyncClient` whose `request/get/
   post/put/patch/delete/head/options` are awaitable coroutines (with `aclose` and
   `async with` support) over the same sync core."
  (:require [com.blockether.vis.core :as vis]))


(def vis-extension
  (vis/extension
    {:ext/name "foundation-shim-httpx"
     :ext/description
     "Sandbox `httpx` subset (`httpx.get`, `Client`, async `AsyncClient`) wrapping requests. No pip/wheel/host bridge."
     :ext/version "0.1.0"
     :ext/author "Blockether"
     :ext/owner "vis"
     :ext/license "Apache-2.0"
     :ext/kind "foundation"
     :ext/sandbox-shims
     [{:shim/name "httpx"
       :shim/imports ["httpx"]
       :shim/description
       (str
         "`httpx` subset wrapping requests: get/post, `Client`/`AsyncClient`, `Response`, "
         "`raise_for_status`. `AsyncClient` coroutines use synchronous I/O. Not supported: HTTP/2, "
         "concurrent async I/O.")
       :shim/source "vis-shims/httpx.py"}]}))

(vis/register-extension! vis-extension)
