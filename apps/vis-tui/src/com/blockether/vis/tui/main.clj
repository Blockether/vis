(ns com.blockether.vis.tui.main
  "The terminal client process entry point.

   The terminal application is a gateway CONSUMER, exactly like the companion
   app: it owns no engine, no database, no provider credential and no session
   store. Every fact it paints arrives over HTTP/SSE from a Vis gateway, so this
   entry point does only what a client's front door owes - decide WHICH gateway
   to talk to, hand the session flags to the screen, and turn a user error into
   one line on the real terminal instead of a stack trace.

   Which gateway, in order: `--gateway` / `--gateway-token`, then
   `VIS_GATEWAY_URL` / `VIS_GATEWAY_TOKEN`, then the loopback default. A bare
   `HOST[:PORT]` is accepted and read as `http://`."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [com.blockether.vis.tui.client :as vis]
            [com.blockether.vis.tui.screen :as screen])
  (:gen-class))

(def usage
  "vis-agent tui [--gateway HOST[:PORT]] [--gateway-token TOKEN] [--session-id ID | --resume | --continue]")

(def ^:private help-text
  [usage "" "The Vis terminal application. It talks to a Vis gateway over HTTP and SSE;"
   "start one with `vis-agent gateway start` if none is running." ""
   "  --gateway HOST[:PORT]  gateway address (env VIS_GATEWAY_URL)"
   "  --gateway-token TOKEN  gateway token (env VIS_GATEWAY_TOKEN)"
   "  --session-id ID        open one existing session"
   "  --resume, -r           pick a session to resume"
   "  --continue, -c         reopen the most recent session"
   "  --version, -V          print the version" "  --help, -h             print this help"])

(defn- version
  "This build's release version: the `vis-tui/VERSION` resource written at build
   time from the repo-root VIS_VERSION, verbatim, else `dev` for a checkout."
  []
  (or (some-> (io/resource "vis-tui/VERSION")
              slurp
              str/trim
              not-empty)
      "dev"))

(defn- print-line!
  [^String s]
  (.println ^java.io.PrintStream vis/original-stdout s)
  (.flush ^java.io.PrintStream vis/original-stdout))

(defn- missing-value? [v] (or (nil? v) (str/starts-with? v "--")))

(defn- flag-value
  [flag more]
  (let [v (first more)]
    (when (missing-value? v)
      (throw (ex-info (str flag " requires a value" "\nUsage: " usage) {:vis/user-error true})))
    v))

(defn parse-args
  "Split the command line into this front door's own options and the arguments
   the screen parses itself. Unknown flags are NOT rejected here - the screen
   owns the session vocabulary and its own usage error."
  [args]
  (loop [args
         (seq args)

         opts
         {:screen-args []}]

    (if-not args
      opts
      (let [arg
            (first args)

            more
            (next args)]

        (case arg
          "--gateway"
          (recur (next more) (assoc opts :gateway (flag-value arg more)))

          "--gateway-token"
          (recur (next more) (assoc opts :gateway-token (flag-value arg more)))

          ("--help" "-h" "help")
          (recur more (assoc opts :help true))

          ("--version" "-V" "version")
          (recur more (assoc opts :version true))

          (recur more (update opts :screen-args conj arg)))))))

(defn -main
  [& args]
  (let [{:keys [gateway gateway-token help screen-args] :as opts}
        (try (parse-args args)
             (catch clojure.lang.ExceptionInfo e
               (print-line! (str "vis-agent tui: " (.getMessage e)))
               (System/exit 2)))]
    (cond help (doseq [line help-text]
                 (print-line! line))
          (:version opts) (print-line! (version))
          :else (do (vis/configure! {:url gateway :token gateway-token})
                    (screen/channel-main screen-args)))))
