(ns com.blockether.vis.internal.foundation.shim-requests
  "Built-in sandbox SHIM: a `requests`-compatible module for the model's Python
   sandbox, backed PURELY by the stdlib `urllib.request` — NO host/JVM bridge,
   NOT a line of Clojure or babashka. `requests` is a third-party wheel that does
   not ship in GraalPy, so agents that reach for `import requests` out of habit
   would otherwise hit ModuleNotFoundError; this extension contributes a
   `:ext/sandbox-shims` entry that `env-python/build-agent-context` installs into
   every sandbox Context (main + every `sub_loop` fork).

   Because every call travels through the sandbox's OWN socket (urllib ->
   http.client -> socket), it automatically honours the network toggle
   (`allowHostSocketAccess`) AND the allow/deny + anti-SSRF `network-guard-python`
   — a JVM `babashka.http-client` bridge would open an egress path OUTSIDE the
   sandbox and disarm all of that, which is exactly why this stays 100% Python.

   Unlike `shim-yaml`/`shim-matplotlib` there are NO `:shim/bindings`: the shim is
   a self-contained Python preamble with zero host callables. It publishes a
   `requests` module into `sys.modules` (so `import requests` works) and staples
   it onto builtins (so `requests.get(...)` works with NO import, like json/os)."
  (:require [com.blockether.vis.core :as vis]))


(def vis-extension
  (vis/extension
    {:ext/name "foundation-shim-requests"
     :ext/description
     "Sandbox shim: a `requests`-compatible module (import requests / requests.get) backed PURELY by the stdlib urllib. No pip, no native wheel, no host bridge — rides the sandbox socket and honours the network guard."
     :ext/version "0.1.0"
     :ext/author "Blockether"
     :ext/owner "vis"
     :ext/license "Apache-2.0"
     :ext/kind "foundation"
     :ext/sandbox-shims
     [{:shim/name "requests"
       :shim/imports ["requests"]
       :shim/description
       "requests-compatible `requests` backed by stdlib urllib (pure Python). Not supported: HTTP/2 or real connection pooling; `HTTPDigestAuth` is an accepted no-op (sends no auth header)."
       :shim/source "vis-shims/requests.py"}]}))

(vis/register-extension! vis-extension)
