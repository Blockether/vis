(ns com.blockether.vis.internal.foundation.mcp.http
  "One lazy babashka.http-client instance shared by every MCP HTTP subsystem."
  (:require [babashka.http-client :as http]))

(def ^:private connect-timeout-ms 15000)

(defonce client (delay (http/client {:connect-timeout connect-timeout-ms})))
