(ns com.blockether.vis.ext.channel-tui.mcp-model
  "Pure reading of one sanitized MCP inventory row.

   Lives apart from `mcp` so BOTH the Settings dialog (`dialogs`, which `mcp`
   requires) and the MCP manager can describe a server without a dependency
   cycle. Nothing here talks to the gateway."
  (:require [clojure.string :as str]))

(set! *unchecked-math* :warn-on-boxed)

(defn flag
  "Wire rows are string-keyed JSON; a missing flag is false, never nil."
  [row k]
  (boolean (get row k)))

(defn server-on?
  "True when the server is BOTH enabled in config and not killed at runtime —
   the single on/off a settings toggle can show."
  [row]
  (and (flag row "enabled") (not (flag row "is_killed"))))

(defn server-status
  "Human one-liner for one sanitized inventory row.

   Reads the RUNTIME state first — a killed server is killed even though it is
   still enabled in config, which is exactly the distinction the kill verb
   introduces and the one a user cannot otherwise see."
  [row]
  (let [tools (long (or (get row "tools") 0))]
    (str/join " · "
              (cond->
                [(cond (flag row "is_killed") "killed"
                       (not (flag row "enabled")) "disabled"
                       (flag row "is_connected") "connected"
                       :else "idle")]
                (pos? tools)
                (conj (str tools (if (= 1 tools) " tool" " tools")))

                (and (get row "url") (not (flag row "is_authorized")))
                (conj "needs sign-in")

                (and (get row "url") (flag row "is_authorized"))
                (conj "signed in")

                (not (flag row "is_managed"))
                (conj "config file")))))
