(ns com.blockether.vis.internal.gateway.cli
  "`vis-agent gateway` commands: start the daemon, report its status, pair a
   companion, stop it and run the terminal client under a gateway lease."
  (:require [clojure.string :as str]
            [com.blockether.vis.contract.gateway :as gateway-contract]
            [com.blockether.vis.internal.commandline :as commandline]
            [com.blockether.vis.internal.config.core :as config]
            [com.blockether.vis.internal.extension.registry :as registry]
            [com.blockether.vis.internal.gateway.client :as gateway-client]
            [com.blockether.vis.internal.gateway.pairing :as pairing]
            [com.blockether.vis.internal.gateway.runtime :as gateway-runtime]
            [com.blockether.vis.internal.gateway.server :as gateway-server]))

(defn- advertise-option
  "The address the pairing link must lead with, in precedence order: `--advertise`
   on the command, `VIS_GATEWAY_ADVERTISE` in the environment, then
   `gateway: advertise:` in the Vis config.

   A machine reachable on one route only needs that answer on EVERY pair, not
   just on the command somebody remembered to decorate. The config tier is the
   one a service-managed daemon can still read, because a launchd/systemd unit
   sources no shell profile; it comes off the RAW merged config, like the
   database path, so a gateway on a machine with no providers saved still gets
   it. All three stay per machine: the address that works here - a LAN IP, a
   forwarded router port, a hostname - names a stranger's box on the next
   network, so no built-in default is safe."
  ([parsed]
   (advertise-option parsed
                     (System/getenv "VIS_GATEWAY_ADVERTISE")
                     (get-in (config/load-config-raw) ["gateway" "advertise"])))
  ([parsed from-env from-config]
   (some #(not-empty (str/trim (str %))) [(get parsed "advertise") from-env from-config])))

(defn- cli-gateway-start!
  "Run the HTTP/SSE gateway daemon. Lazy resolve keeps
   Ring/Jetty class loading off every other command's startup path."
  [parsed _residual]
  (config/init-cli!)
  (when-let [db (get parsed "db")]
    (System/setProperty "vis.db.path" db))
  (gateway-server/serve-main!
    {:port (get parsed "port")
     :host (get parsed "host")
     :token-file (get parsed "token-file")
     :require-token? (boolean (get parsed "require-token"))
     :pair? (boolean (get parsed "pair"))
     :advertise (advertise-option parsed)
     :managed? (= "1" (System/getenv "VIS_GATEWAY_MANAGED"))
     :db (config/resolve-db-spec (when-let [db (get parsed "db")]
                                   (if (= db ":memory") :memory {:backend :sqlite :path db})))}))

(defn- plural
  "`n` with `one` pluralized by an s - the shape every gateway line counts in."
  [n one]
  (str n " " one (when (not= 1 (long n)) "s")))

(defn- build-label
  "How a Vis build names ITSELF to a human, read off its handshake
   (`{:version :build}`): the release version, or - when a source checkout has no
   release to be ordered by - the commit that is its identity, because \"dev\"
   alone names no code."
  [{:keys [version build]}]
  (if (and (= "dev" version) build) (str version " (" build ")") version))

(defn- this-handshake
  "What THIS runtime advertises about itself ([[runtime/handshake]])."
  []
  (gateway-runtime/handshake))

(defn- stale-daemon-note
  "The line `gateway status` adds when THIS runtime (`ours`, a handshake) is newer
   code than the daemon `status` describes: what picks the new build up, and what
   still holds the old one. nil when it is not.

   Both halves come out of the handshake the status map already carries, so the
   answer costs no extra round trip, and it applies the SAME two rules an attach
   does - [[runtime/superseded?]] for the verdict, [[client/daemon-idle?]] for
   whether anything is in the way. A status line must never promise a replacement
   the next client would refuse to make."
  [status ours]
  (let [peer
        (get status "protocol")

        superseded?
        (gateway-runtime/superseded? {:our-version (:version ours)
                                      :their-version (get peer "version")
                                      :our-build (:build ours)
                                      :their-build (get peer "build")})

        {:keys [reason clients running-turns pid]}
        (gateway-client/daemon-idle? status)]

    (when superseded?
      (str "this build is " (build-label ours)
           " - " (case reason
                   :idle
                   "the next session starts on it"

                   :user-owned
                   (str "the running daemon is user-owned"
                        (when pid (str " (pid " pid ")"))
                        " - stop and start it yourself to pick it up")

                   (if (and clients running-turns)
                     (str "it is picked up once nothing is using this one ("
                          (plural clients "client")
                          ", "
                          (plural running-turns "running turn")
                          ")")
                     "it is picked up once this one is no longer in use"))))))

(defn- newer-daemon-note
  "The line `gateway status` adds when the RUNNING daemon is newer code than this
   runtime (`ours`, a handshake): the update is installed and this build is the half
   that is behind. nil when it is not.

   The mirror of [[stale-daemon-note]], and the only place this side says so: a
   client never replaces a daemon that is ahead of it, so silence here would read as
   \"nothing is new\". It applies to a remote gateway too, where the version a human
   can act on is the one on THIS device."
  [status ours]
  (let [peer
        (get status "protocol")

        theirs
        (get peer "version")]

    (when (gateway-contract/newer-release? theirs (:version ours))
      (str "a newer Vis is running the gateway: "
           theirs
           " - this build is "
           (build-label ours)
           " - install it here with: vis-agent update"))))

(defn- cli-gateway-status!
  [parsed _residual]
  (config/init-cli!)
  (when-let [db (get parsed "db")]
    (System/setProperty "vis.db.path" db))
  (let [{:strs [status pid host port db clients running_turns require_token] :as m}
        (gateway-client/status)]
    (if (= "running" status)
      (let [peer (get m "protocol")]
        (commandline/stdout!
          (str "gateway running pid="
               pid
               " url=http://"
               host
               ":"
               port
               " db="
               db
               " clients="
               clients
               " running-turns="
               running_turns
               " auth="
               (if require_token "token" "loopback-disabled")
               " version="
               (or (get peer "version") "unknown")
               (when-let [build (get peer "build")]
                 (str " build=" build))))
        ;; "why is my update not in effect yet" is what this command gets asked, and
        ;; the answer is already in the map. A --gateway target is another machine's
        ;; lifecycle, so nothing here is ever going to replace it.
        (let [ours (this-handshake)]
          (when-let [note (newer-daemon-note m ours)]
            (commandline/stdout! note))
          (when-not (gateway-client/remote-gateway)
            (when-let [note (stale-daemon-note m ours)]
              (commandline/stdout! note)))))
      (commandline/stdout! (str "gateway stopped"
                                (when-let [db (get m "db")]
                                  (str " db=" db)))))))

(defn- cli-gateway-pair!
  "Print a companion pairing QR for the gateway ALREADY running for this DB, so
   you can pair without stopping/restarting it. Refuses a loopback-bound daemon
   (a phone can never reach 127.0.0.1) with a copy-paste fix, unless --advertise
   names the route that does reach it."
  [parsed _residual]
  (config/init-cli!)
  (when-let [db (get parsed "db")]
    (System/setProperty "vis.db.path" db))
  (let [{:keys [running? host port token loopback?]}
        (gateway-client/pairing-info)

        advertise
        (advertise-option parsed)]

    (cond (not running?)
          (throw (ex-info (str "no gateway is running for this DB. Start one reachable first:\n"
                               "  vis-agent gateway start --host 0.0.0.0 --require-token --pair")
                          {:vis/user-error true}))
          (and loopback? (empty? (str advertise)))
          (throw (ex-info
                   (let [ts (first (pairing/tailscale-hosts))]
                     (str
                       "the running gateway is bound to " host
                       " (loopback) \u2014 a phone cannot reach it.\n"
                       "Restart it on a reachable host:\n"
                       "  vis-agent gateway stop\n"
                       (if ts
                         (str
                           "  vis-agent gateway start --host " ts
                           " --require-token --pair"
                           "   # your Tailscale IP \u2014 reachable from the phone on your tailnet")
                         "  vis-agent gateway start --host 0.0.0.0 --require-token --pair")))
                   {:vis/user-error true}))
          :else (pairing/print-pairing! {:host host
                                         :port port
                                         :token token
                                         :require-token? (boolean token)
                                         :advertise advertise
                                         :emit commandline/stdout!}))))

(defn- gateway-stop-if-idle!
  "`--if-idle`: release the daemon only when releasing it is free, and say why when
   it is not. Never fails - `vis-agent update` runs exactly this after installing a
   new runtime, and an update must not report failure because someone had a TUI
   open. Silent when nothing is running, so a plain update prints nothing extra.

   A daemon left alone here is not left stale: the next client to attach it with
   nobody using it replaces it itself (`client/stale-bounce-verdict`), so the advice
   printed for a busy one is to finish and close the session, not to run anything."
  []
  (let [{:keys [stopped? reason clients running-turns pid]}
        (gateway-client/stop-daemon-if-idle!)

        version
        (build-label (this-handshake))]

    (cond stopped? (commandline/stdout! (str "gateway stopped - next session starts on " version))
          (= :not-running reason) nil
          (= :remote reason) (commandline/stdout!
                               "gateway is a --gateway target on another machine - left alone")
          (= :user-owned reason) (commandline/stdout!
                                   (str "gateway is user-owned" (when pid (str " (pid " pid ")"))
                                        " - restart it yourself when ready:\n"
                                        "  vis-agent gateway stop && vis-agent gateway start"))
          :else (commandline/stdout! (str "gateway still running ("
                                          (plural clients "client")
                                          ", "
                                          (plural running-turns "running turn")
                                          ") - left alone, still serving the old build.\n"
                                          "  Quit those sessions and the next vis picks up " version
                                          " by itself; to bounce it now:\n"
                                          "  vis-agent gateway stop")))))

(defn- cli-gateway-stop!
  [parsed _residual]
  (config/init-cli!)
  (when-let [db (get parsed "db")]
    (System/setProperty "vis.db.path" db))
  (if (get parsed "if-idle")
    (gateway-stop-if-idle!)
    (let [{:keys [stopping status type host port pid recovery escalated clients running-turns]}
          (gateway-client/stop-daemon!)

          cost
          (str/join ", "
                    (cond-> []
                      (pos? (long (or clients 0)))
                      (conj (str "releasing " (plural clients "client")))

                      (pos? (long (or running-turns 0)))
                      (conj (str "draining " (plural running-turns "running turn")))))]

      (commandline/stdout!
        (cond stopping (str "gateway stopping"
                            (when pid (str " (pid " pid ")"))
                            (when (seq cost) (str " - " cost)))
              (= "stopped" status)
              (if escalated
                (str "gateway stopped by " (if (= :kill escalated) "SIGKILL" "SIGTERM")
                     " - it had stopped answering" (when pid (str " (pid " pid ")")))
                "gateway stopped")
              (= :gateway/orphaned-daemon type) (str "gateway stop found a live orphan at "
                                                     host
                                                     ":"
                                                     port
                                                     (when pid (str " (registered PID " pid ")"))
                                                     ". "
                                                     recovery)
              :else (str "gateway stop requested" (when pid (str " (pid " pid ")"))
                         " - it reported no final state. Check it with:\n"
                         "  vis-agent gateway status"))))))

(def command
  {:cmd/name "gateway"
   :cmd/doc "Start, inspect, or stop the long-lived gateway daemon."
   :cmd/usage
   "vis-agent [--jvm] [--gateway HOST[:PORT] --gateway-token TOKEN] gateway <start|status|stop|pair> [--db PATH]"
   :cmd/examples ["vis-agent gateway start --jvm"]
   :cmd/subcommands #(registry/registered-under ["gateway"])})

(def subcommands
  "Subcommands registered under `vis-agent gateway`."
  [{:cmd/name "tui"
    :cmd/parent ["gateway"]
    :cmd/doc "Run the terminal client with a local gateway lease (used by the launcher)."
    :cmd/run-fn (fn [_ args]
                  (let [exit (gateway-client/run-tui! (vec (drop-while #{"--"} args)))]
                    (shutdown-agents)
                    (System/exit (int exit))))}
   {:cmd/name "start"
    :cmd/parent ["gateway"]
    :cmd/doc
    "Start the long-lived gateway daemon (HTTP + SSE runtime) in the foreground, always on THIS machine. Use --jvm to run it on the JVM without changing the installed track."
    :cmd/usage
    "vis-agent gateway start [--jvm] [--port 7890] [--host 127.0.0.1] [--token-file PATH] [--pair] [--advertise URL]"
    :cmd/args
    [{:name "port" :kind :flag :type :string :doc "TCP port to listen on (default 7890)."}
     {:name "host"
      :kind :flag
      :type :string
      :doc
      "Bind host (default 127.0.0.1, or a phone-reachable host when --pair is given; non-loopback always requires the token)."}
     {:name "token-file"
      :kind :flag
      :type :string
      :doc "Bearer-token file (default ~/.vis/gateway.token, minted on first run)."}
     {:name "db"
      :kind :flag
      :type :string
      :doc "SQLite DB path this daemon owns (default ~/.vis/vis.mdb or VIS_DB_PATH)."}
     {:name "require-token"
      :kind :flag
      :type :boolean
      :doc
      "Require the bearer token on loopback too (auth is OFF by default on 127.0.0.1; a non-loopback bind always requires it)."}
     {:name "pair"
      :kind :flag
      :type :boolean
      :doc
      "Print a VIS companion pairing QR (URL + bearer token). Implies a phone-reachable bind (Tailscale IP, else 0.0.0.0) unless --host says otherwise."}
     {:name "advertise"
      :kind :flag
      :type :string
      :doc
      "Address the pairing link should carry instead of the detected one, as HOST, HOST:PORT or a full URL. Use it when the client must dial a port forward, a proxy, or the single address your network allows. Set VIS_GATEWAY_ADVERTISE, or `gateway: advertise:` in the Vis config, to apply the same address to every start on this machine."}]
    :cmd/examples ["vis-agent gateway start" "vis-agent gateway start --jvm"
                   "vis-agent gateway start --port 8080" "vis-agent gateway start --pair"
                   "vis-agent gateway start --host 0.0.0.0 --require-token --pair"]
    :cmd/run-fn cli-gateway-start!}
   {:cmd/name "status"
    :cmd/parent ["gateway"]
    :cmd/doc
    "Show the gateway this invocation drives — the --gateway target, else the daemon registered for the current DB — without starting it."
    :cmd/usage "vis-agent gateway status [--db PATH]"
    :cmd/args
    [{:name "db"
      :kind :flag
      :type :string
      :doc
      "SQLite DB path whose gateway registry should be inspected (ignored when --gateway names a remote gateway)."}]
    :cmd/examples ["vis-agent gateway status"
                   "vis-agent --gateway 10.0.0.5 --gateway-token TOKEN gateway status"]
    :cmd/run-fn cli-gateway-status!}
   {:cmd/name "stop"
    :cmd/parent ["gateway"]
    :cmd/doc
    "Stop the gateway daemon registered for the current DB - never a --gateway target, which vis attaches to but never manages."
    :cmd/usage "vis-agent gateway stop [--if-idle] [--db PATH]"
    :cmd/args
    [{:name "db" :kind :flag :type :string :doc "SQLite DB path whose gateway should be stopped."}
     {:name "if-idle"
      :kind :flag
      :type :boolean
      :doc
      "Stop it only when stopping is free: an auto-spawned daemon with no client and no turn still moving. Prints why it did not otherwise, is silent when none runs, and always succeeds."}]
    :cmd/examples ["vis-agent gateway stop" "vis-agent gateway stop --if-idle"]
    :cmd/run-fn cli-gateway-stop!}
   {:cmd/name "pair"
    :cmd/parent ["gateway"]
    :cmd/doc
    "Print a companion pairing QR for the gateway already running for this DB, or for the --gateway target."
    :cmd/usage "vis-agent gateway pair [--db PATH] [--advertise URL]"
    :cmd/examples ["vis-agent gateway pair"
                   "vis-agent --gateway 10.0.0.5 --gateway-token TOKEN gateway pair"]
    :cmd/args
    [{:name "db"
      :kind :flag
      :type :string
      :doc "SQLite DB path whose running gateway should be paired."}
     {:name "advertise"
      :kind :flag
      :type :string
      :doc
      "Address the pairing link should carry instead of the detected one (HOST, HOST:PORT or a full URL). Defaults to VIS_GATEWAY_ADVERTISE, then to `gateway: advertise:` in the Vis config."}]
    :cmd/run-fn cli-gateway-pair!}
   {:cmd/name "mcp"
    :cmd/parent ["gateway"]
    :cmd/doc
    "Manage gateway-owned MCP servers: add, list, test, enable/disable, kill/start, and OAuth sign-in."
    :cmd/usage
    "vis-agent gateway mcp <list|add|test|remove|enable|disable|kill|start|auth-start|auth-complete|auth-poll|auth-cancel|auth-logout>"
    :cmd/subcommands #(registry/registered-under ["gateway" "mcp"])}])
