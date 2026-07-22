(ns com.blockether.vis.internal.process-jail
  "OS-level process CONTAINMENT — the 'jail' — that wraps the shell executors'
   argv so an allowed child is physically confined to the session workspace roots
   and, when network is off, cannot open a socket. This is real containment, a
   real containment boundary — not a cooperative name/argv check, which can be
   walked around since argv[0] is `bash` and the real binary hides inside the
   `-lc` string;
   the jail constrains what the child can DO once it runs, regardless of what a
   script inside it tries (curl, python -c, /dev/tcp — all hit the same wall).

   POLICY, NOT GUARDS. The jail is driven by a declarative *policy* compiled from
   vis.yml + the LIVE session roots, not by hand-written guard functions. The
   policy is a plain VALUE passed per spawn (never a process-global singleton, so
   many concurrent sessions in one gateway never stomp each other). Its shape:

     {:roots-fn     (fn [] [root-strings])  ; live session RW roots, re-read/spawn
      :net-enabled? <bool>                  ; whole shell-child network on/off
      :allow-write  [<path> …]              ; extra writable paths (beyond roots)
      :deny-write   [<path> …]              ; protect within writable (deny wins)
      :allow-read   [<path> …]              ; re-allow read within a denied region
      :deny-read    [<path> …]}             ; protect a read region (deny wins)

   The filesystem model mirrors Anthropic's sandbox-runtime:
     - WRITE is allow-only: denied everywhere except the session roots + tmp +
       `:allow-write`; `:deny-write` carves protected exceptions (deny wins).
     - READ is default-deny here (workspace-focused, stronger than srt's
       read-everywhere default): system code/config + the RW roots + `:allow-read`
       are readable; `:deny-read` protects a subtree even inside an allowed root.

   `wrap-argv` compiles the policy, per spawn, into the OS enforcement primitive:

     - macOS  : a Seatbelt profile handed to the system `sandbox-exec -p` — ships
                with the OS, ZERO install. IMPLEMENTED + verified.
     - Linux  : Landlock LSM + seccomp via a re-exec helper — TODO. `supported?`
                returns false so callers degrade to the cooperative gate.

   Two locks learned from the kernel, baked in here:
     1. sandbox-exec matches RESOLVED real paths, so every root is realpath'd
        before templating (`/tmp` -> `/private/tmp`, else the rule never matches).
     2. a default-deny profile MUST `(import \"system.sb\")` or dyld/sysctl startup
        reads are denied and every binary aborts before `main`."
  (:require [clojure.string :as str])
  (:import (java.io File)
           (java.nio.file LinkOption Paths)))

(def ^:private link-opts (make-array LinkOption 0))

(defn- expand-home
  "Expand a leading `~` / `~/` to the user's home. Config paths (mirroring
   sandbox-runtime's `~/.ssh`) are written home-relative; sandbox-exec is not."
  [s]
  (let
    [s
     (str s)

     home
     (System/getProperty "user.home")]

    (cond (= s "~") home
          (str/starts-with? s "~/") (str home (subs s 1))
          :else s)))

(defn- real-path
  "Canonical real-path string of `s`, or nil when it can't be resolved. sandbox-exec
   matches on RESOLVED paths, so roots MUST pass through here before templating."
  [s]
  (let [s (expand-home s)]
    (when-not (str/blank? (str s))
      (try (.toString (.toRealPath (Paths/get (str s) (make-array String 0)) link-opts))
           (catch Throwable _ nil)))))

(defn- deny-path
  "Resolve a DENY target: prefer its real path, else fall back to the raw expanded
   string. Deny lists must fail safe — a not-yet-existing secret still denies."
  [s]
  (or (real-path s)
      (let [e (expand-home s)]
        (when-not (str/blank? (str e)) (str e)))))

(defn- os-kind
  []
  (let [n (str/lower-case (str (System/getProperty "os.name")))]
    (cond (str/includes? n "mac") :macos
          (str/includes? n "linux") :linux
          (str/includes? n "windows") :windows
          :else :other)))

(def ^:private macos-sandbox-exec "/usr/bin/sandbox-exec")

(defn supported?
  "True when the current OS can ENFORCE a jail. macOS: Seatbelt via the system
   `sandbox-exec`. Linux/Windows: not yet (returns false) — callers keep the
   cooperative admission gate as the floor."
  []
  (and (= :macos (os-kind)) (.canExecute (File. ^String macos-sandbox-exec))))

(defn- inherited-jail?
  "True inside a child already confined by this process-jail contract. Seatbelt
   restrictions are inherited across exec, and macOS rejects a second
   `sandbox-exec` application, so descendants must not wrap themselves again."
  []
  (= "1" (System/getenv "VIS_SEATBELT_ACTIVE")))

;; Read paths every Mach-O binary + dyld needs to even reach main(), plus the
;; standard package prefixes real tools live in (Homebrew/MacPorts/local) and
;; the shared system config dir (TLS CAs, resolv.conf/hosts — needed once net is
;; on). All read-only + world-readable code/config: no user secrets, and writes
;; here stay denied (only the session roots are RW). A Homebrew-linked `bash`
;; that can't read its own libreadline aborts before main, so this must be broad
;; enough to actually launch tools.
(def ^:private macos-system-read-roots
  ["/usr" "/bin" "/sbin" "/System" "/Library" "/private/var/db/dyld" "/private/var/select"
   "/private/etc" "/opt/homebrew" "/usr/local" "/opt/local"])

(defn- sbpl-quote
  [s]
  (str "\""
       (-> (str s)
           (str/replace "\\" "\\\\")
           (str/replace "\"" "\\\""))
       "\""))

(defn- subpaths [roots] (str/join (map #(str "(subpath " (sbpl-quote %) ")") roots)))

(defn macos-profile
  "Compile a Seatbelt (SBPL) profile string from a RESOLVED policy map
   `{:rw [..] :ro [..] :deny-write [..] :deny-read [..] :net-enabled? <bool>}`
   (all paths already canonical). Rules are emitted in Seatbelt's LAST-match-wins
   order: allow reads (system + rw + ro), allow writes (rw), then the deny carve-
   outs so `:deny-write`/`:deny-read` win over the allows. One-line string for
   `sandbox-exec -p`."
  ^String [{:keys [rw ro deny-write deny-read net-enabled? proxy-port loopback-port]}]
  (let
    [rw
     (->> rw
          (keep real-path)
          distinct
          vec)

     ro
     (->> ro
          (keep real-path)
          distinct
          vec)

     dw
     (->> deny-write
          (keep deny-path)
          distinct
          vec)

     dr
     (->> deny-read
          (keep deny-path)
          distinct
          vec)]

    (str
      "(version 1)"
      "(import \"system.sb\")"
      "(deny default)"
      "(allow process-fork process-exec)"
      "(allow sysctl-read)"
      "(allow file-read-metadata)"
      "(allow file-read*"
      (subpaths macos-system-read-roots)
      "(literal \"/dev/null\")(literal \"/dev/zero\")(literal \"/dev/random\")(literal \"/dev/urandom\"))"
      (when (seq ro) (str "(allow file-read*" (subpaths ro) ")"))
      "(allow file-read* file-write*"
      "(literal \"/dev/null\")(literal \"/dev/tty\")(literal \"/dev/stdout\")(literal \"/dev/stderr\")"
      (subpaths rw)
      ")"
      (when (seq dw) (str "(deny file-write*" (subpaths dw) ")"))
      (when (seq dr) (str "(deny file-read*" (subpaths dr) ")"))
      ;; Network: a proxy endpoint is the sole outbound destination. A managed
      ;; nREPL additionally needs to bind a server socket. Seatbelt's
      ;; `network-bind` accepts the address class but not a reliable host:port
      ;; constraint, so bind is limited to local IP sockets while inbound traffic
      ;; is restricted to the one preselected nREPL port.
      (let
        [server-rules (when loopback-port
                        (str "(allow network-bind (local ip))"
                             "(allow network-inbound (local ip \"*:"
                             loopback-port
                             "\"))"))]
        (cond proxy-port (str "(deny network*)"
                              server-rules
                              "(allow network-outbound (remote ip \"localhost:"
                              proxy-port
                              "\"))")
              net-enabled? "(allow network*)"
              :else (str "(deny network*)" server-rules))))))

(defn compile-policy
  "Resolve a raw jail policy VALUE into the canonical map `macos-profile` consumes:
   read the LIVE session roots via `:roots-fn`, add the always-writable temp dirs,
   fold in the vis.yml `:allow-write`/`:allow-read`/`:deny-write`/`:deny-read`
   paths (home-expanded + realpath'd). Called per spawn, so each child jails to
   the CURRENT roots and CURRENT config."
  [{:keys [roots-fn net-enabled? allow-write allow-read deny-write deny-read proxy-port
           loopback-port]}]
  (let
    [session-roots
     (when roots-fn (try (roots-fn) (catch Throwable _ nil)))

     tmps
     [(System/getProperty "java.io.tmpdir") "/tmp"]

     rw
     (->> (concat session-roots tmps allow-write)
          (keep real-path)
          distinct
          vec)

     ro
     (->> allow-read
          (keep real-path)
          distinct
          vec)]

    {:rw rw
     :ro ro
     :deny-write (vec deny-write)
     :deny-read (vec deny-read)
     :net-enabled? (boolean net-enabled?)
     :proxy-port proxy-port
     :loopback-port loopback-port}))

(defn wrap-argv
  "Given the base executor argv and a jail POLICY value, return the argv to spawn.
   On macOS the first managed process is wrapped with `sandbox-exec`; descendants
   inherit that kernel policy and are left unwrapped because Seatbelt rejects a
   second sandbox application. Unsupported platforms remain explicit passthroughs."
  [argv policy]
  (if (and policy (supported?) (not (inherited-jail?)))
    (case (os-kind)
      :macos
      (into ["sandbox-exec" "-p" (macos-profile (compile-policy policy))] argv)

      argv)
    argv))

(defn- java-proxy-options
  [{:keys [proxy-port java-trust-store java-trust-store-password java-proxy? loopback-port]}]
  (when (and java-proxy? proxy-port)
    (str (when loopback-port
           ;; Keep the nREPL listener on AF_INET; its launcher binds 127.0.0.1 and
           ;; the Seatbelt profile admits inbound traffic only on `loopback-port`.
           "-Djava.net.preferIPv4Stack=true ")
         "-Dhttp.proxyHost=127.0.0.1"
         " -Dhttp.proxyPort="
         proxy-port
         " -Dhttps.proxyHost=127.0.0.1"
         " -Dhttps.proxyPort=" proxy-port
         ;; Empty means even loopback HTTP destinations use the policy proxy. The
         ;; nREPL transport itself is a raw socket and is unaffected by this JVM option.
         " -Dhttp.nonProxyHosts=" (when java-trust-store
                                    (str " -Djavax.net.ssl.trustStore="
                                         java-trust-store
                                         " -Djavax.net.ssl.trustStoreType=PKCS12"
                                         " -Djavax.net.ssl.trustStorePassword="
                                         java-trust-store-password)))))

(defn proxy-env
  "Environment additions for a confined child. `VIS_SEATBELT_ACTIVE` records that
   the kernel policy is already inherited, preventing invalid nested
   `sandbox-exec` calls. When a gateway proxy endpoint is present, common proxy and
   CA variables cover curl/git/Python/Bun/etc.; managed JVM children additionally
   receive proxy + ephemeral truststore properties through JAVA_TOOL_OPTIONS."
  [policy]
  (let
    [jail-env (if (and policy (or (inherited-jail?) (supported?))) {"VIS_SEATBELT_ACTIVE" "1"} {})]
    (if-let [port (:proxy-port policy)]
      (let
        [token (:proxy-token policy)
         url (str "http://" (when token (str token "@")) "127.0.0.1:" port)
         ca (:ca-file policy)
         java-opts (java-proxy-options policy)]

        (cond->
          (merge jail-env
                 {"http_proxy" url
                  "https_proxy" url
                  "all_proxy" url
                  "HTTP_PROXY" url
                  "HTTPS_PROXY" url
                  "ALL_PROXY" url
                  "no_proxy" ""
                  "NO_PROXY" ""})
          ca
          (merge {"CURL_CA_BUNDLE" ca
                  "SSL_CERT_FILE" ca
                  "REQUESTS_CA_BUNDLE" ca
                  "NODE_EXTRA_CA_CERTS" ca
                  "GIT_SSL_CAINFO" ca
                  "PIP_CERT" ca
                  "AWS_CA_BUNDLE" ca
                  "CARGO_HTTP_CAINFO" ca
                  "DENO_CERT" ca})

          java-opts
          (assoc "JAVA_TOOL_OPTIONS"
            (str (when-let [existing (not-empty (System/getenv "JAVA_TOOL_OPTIONS"))]
                   (str existing " "))
                 java-opts))))
      jail-env)))

;; ── Standard language-process jail contract ────────────────────────────────
;; Language packs spawn managed REPLs and project test runners via raw
;; ProcessBuilder calls outside the shell executors. Every such spawn obtains its
;; argv + proxy environment from `session-process-launch`, which resolves one live
;; per-session policy atomically and fails closed when the session is unavailable.

(def ^:private repl-toolchain-read-dirs
  "Installed language runtimes/toolchains a managed process may READ to boot. They
   remain read-only: a REPL may execute them but cannot replace its JVM/Python/Bun."
  ["~/.sdkman" "~/.asdf" "~/.jenv" "~/.pyenv" "~/.local/bin" "~/.local/share/mise"])

(def ^:private repl-toolchain-cache-dirs
  "Dependency/artifact caches a managed language process may read and write on a
   cold start. No unrelated home directory is exposed."
  ["~/.m2" "~/.gitlibs" "~/.clojure" "~/.deps.clj" "~/.cpcache" "~/.bun" "~/.npm" "~/.cache"
   "~/.cargo" "~/.deno" "~/.gradle" "~/.ivy2" "~/.lein" "~/.sbt" "~/.coursier"])

(def ^:private language-process-runtime-dirs
  "Vis-owned runtime state managed language processes may read and write. REPL
   managers write lifecycle logs here, including when exercised from a jailed
   project test runner."
  ["~/.vis/logs"])

(defn language-process-policy
  "Derive a managed-language jail policy from a session's base policy. It keeps
   filesystem confinement, adds read-only runtime installations plus writable
   dependency caches/logs, and replaces the shell proxy endpoint with this
   session's gateway-attributed language-process endpoint.

   Direct network access is always disabled. HTTPS remains MITM-capable: CA-aware
   runtimes receive the combined PEM bundle, while JVM children additionally
   receive an ephemeral PKCS12 truststore through JAVA_TOOL_OPTIONS.
   `loopback-port` permits bind/inbound only for the managed nREPL's selected port;
   it does not permit direct outbound traffic."
  [base loopback-port]
  (when base
    (-> base
        (update :allow-write
                #(vec (concat % repl-toolchain-cache-dirs language-process-runtime-dirs)))
        (update :allow-read
                #(vec (concat %
                              repl-toolchain-read-dirs
                              repl-toolchain-cache-dirs
                              language-process-runtime-dirs)))
        (assoc :net-enabled? false
               :proxy-port (:repl-proxy-port base)
               :proxy-token nil
               :ca-file (:repl-ca-file base)
               :java-proxy? true
               :loopback-port loopback-port))))

(defn repl-policy
  "Derive the managed-nREPL variant for its selected loopback listener port."
  [base loopback-port]
  (language-process-policy base loopback-port))

(defonce ^:private session-jail-policies (atom {}))

(defn register-session-jail!
  "Register (or replace) this session's live base jail-policy function."
  [session-id policy-fn]
  (when session-id (swap! session-jail-policies assoc session-id policy-fn)))

(defn prepare-session-jail!
  "Bind the language surface's live session env to the managed-process contract.
   Missing session identity or policy fails closed before a language handler can
   start/restart a REPL or project test process. Safe and idempotent per dispatch."
  [{:keys [session-id jail-policy-fn]}]
  (when-not session-id
    (throw (ex-info "Managed language process denied: session id is unavailable"
                    {:type ::session-jail-missing})))
  (when-not jail-policy-fn
    (throw (ex-info "Managed language process denied: session jail policy is unavailable"
                    {:type ::session-jail-missing :session-id session-id})))
  (register-session-jail! session-id jail-policy-fn)
  session-id)

(defn unregister-session-jail!
  "Drop this session's registered jail policy (loop dispose)."
  [session-id]
  (when session-id (swap! session-jail-policies dissoc session-id)))

(defn- session-base-policy!
  [session-id]
  (let [policy-fn (get @session-jail-policies session-id)]
    (when-not policy-fn
      (throw (ex-info "Managed language process denied: session jail is not registered"
                      {:type ::session-jail-missing :session-id session-id})))
    (let
      [policy (try (policy-fn)
                   (catch Throwable t
                     (throw (ex-info "Managed language process denied: session jail policy failed"
                                     {:type ::session-jail-failed :session-id session-id}
                                     t))))]
      (when-not policy
        (throw (ex-info "Managed language process denied: session jail policy is unavailable"
                        {:type ::session-jail-missing :session-id session-id})))
      policy)))

(defn session-process-launch
  "THE language-surface launch contract. Atomically resolve `session-id`'s policy
   and return `{:argv ... :env ...}` for one managed language subprocess.

   `:loopback-port` is reserved for a managed nREPL's preselected local listener.
   Unknown, nil, disposed, or failing sessions are denied before spawn; the
   contract never silently returns a naked argv. On an OS without a supported
   enforcer, `:argv` remains unchanged (the platform capability gap is explicit),
   while the policy lookup still must succeed."
  ([session-id argv] (session-process-launch session-id argv nil))
  ([session-id argv {:keys [loopback-port]}]
   (let [policy (language-process-policy (session-base-policy! session-id) loopback-port)]
     {:argv (wrap-argv argv policy) :env (proxy-env policy)})))
