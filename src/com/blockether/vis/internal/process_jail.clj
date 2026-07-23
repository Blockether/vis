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
      :allow-read-write [<path> …]           ; concise full read+write grant
      :allow-write      [<path> …]           ; legacy writable paths (also readable)
      :deny-write       [<path> …]           ; protect within writable (deny wins)
      :allow-read       [<path> …]           ; additional read-only paths
      :deny-read        [<path> …]           ; protect a read region (deny wins)
      :inbound-ports    [<int> …]}           ; extra local ports a child may ACCEPT on
                                             ; (bind is local-only; accept is port-gated)

   The filesystem model mirrors Anthropic's sandbox-runtime:
     - WRITE is allow-only: denied everywhere except the session roots + tmp +
       `:allow-read-write` + `:allow-write`; `:deny-write` wins.
     - READ is default-deny here (workspace-focused, stronger than srt's
       read-everywhere default): system code/config + RW paths + `:allow-read`
       are readable; `:deny-read` wins.

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

;; Directory-EXISTENCE probes (home lookup, dyld, cwd resolution) need metadata on
;; the traversal ancestors of granted roots. These are `literal` (the directory
;; itself, revealing only that it exists) — never `subpath` — so a confined child
;; can stat `$HOME` and `/Users` but cannot read the size/mtime of
;; `~/.ssh/id_ed25519` and other secrets beneath them. This scoped metadata grant
;; replaces a former global `(allow file-read-metadata)` that leaked file existence
;; + size + mtime for every path on the host.
(def ^:private macos-metadata-literals
  (into ["/" "/Users" "/Volumes" "/private" "/opt" "/etc" "/var" "/tmp" "/home"]
        (remove nil?)
        [(System/getProperty "user.home") (System/getProperty "java.io.tmpdir")]))

(defn- sbpl-quote
  [s]
  (str "\""
       (-> (str s)
           (str/replace "\\" "\\\\")
           (str/replace "\"" "\\\""))
       "\""))

(defn- subpaths [roots] (str/join (map #(str "(subpath " (sbpl-quote %) ")") roots)))

(defn- ancestor-dirs
  "Every ancestor directory of `p`, from its parent up to `/`, as absolute
   strings. Canonicalizing a path under a granted root lstats/readlinks each
   component on the way DOWN, so every ancestor needs `file-read-metadata` even
   though only the root itself carries `file-read*`/`file-write*`. Without this a
   confined child cannot `getCanonicalPath` a file it just created under a root
   whose ancestors aren't otherwise granted — most notably the darwin per-user
   temp dir (`/private/var/folders/<hash>/T`), whose `/private/var`,
   `/private/var/folders`, ... chain is granted nowhere else."
  [p]
  (loop
    [cur
     (when-let [s (not-empty p)]
       (.getParentFile (java.io.File. ^String s)))

     acc
     []]

    (if cur
      (recur (.getParentFile ^java.io.File cur) (conj acc (.getPath ^java.io.File cur)))
      acc)))

(defn macos-profile
  "Compile a Seatbelt (SBPL) profile string from a RESOLVED policy map
   `{:rw [..] :ro [..] :deny-write [..] :deny-read [..] :net-enabled? <bool>}`
   (all paths already canonical). Rules are emitted in Seatbelt's LAST-match-wins
   order: allow reads (system + rw + ro), allow writes (rw), then the deny carve-
   outs so `:deny-write`/`:deny-read` win over the allows. One-line string for
   `sandbox-exec -p`."
  ^String
  [{:keys [rw ro deny-write deny-read deny-exec net-enabled? proxy-port loopback-port
           inbound-ports]}]
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
          vec)

     dex
     (->> deny-exec
          (keep deny-path)
          distinct
          vec)]

    (str
      "(version 1)"
      "(import \"system.sb\")"
      "(deny default)"
      "(allow process-fork process-exec)"
      "(allow sysctl-read)"
      ;; GraalVM Native Image uses a named POSIX semaphore for signal delivery on
      ;; macOS. Without this narrow IPC permission, tools such as spel and bb abort
      ;; during VM startup before their main function runs.
      "(allow ipc-posix-sem)"
      "(allow file-read-metadata"
      (apply str
        (map #(str "(literal " (sbpl-quote %) ")")
             (distinct (concat macos-metadata-literals (mapcat ancestor-dirs (concat rw ro))))))
      (subpaths (concat macos-system-read-roots rw ro))
      ")"
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
      ;; Block EXECUTION of specific binaries (`jail.deny-exec`). Overrides the
      ;; blanket `(allow process-fork process-exec)` above; a plain file-read deny
      ;; does NOT stop exec on macOS (the kernel maps a signed/allowed binary
      ;; without a file-read* check), so this is the real command block.
      (when (seq dex) (str "(deny process-exec*" (subpaths dex) ")"))
      ;; Network: a proxy endpoint is the sole outbound destination. A managed
      ;; nREPL — and any explicitly allowlisted dev/server port — additionally
      ;; needs to bind a server socket. Seatbelt's `network-bind` accepts the
      ;; address class but not a reliable host:port constraint, so bind is limited
      ;; to local IP sockets while inbound traffic is restricted, port by port, to
      ;; the preselected nREPL port plus each `:inbound-ports` entry. Binding is
      ;; broad (any local port); ACCEPTING a connection is the gated capability.
      (let
        [inbound
         (->> (cons loopback-port inbound-ports)
              (remove nil?)
              distinct)

         server-rules
         (when (seq inbound)
           (str "(allow network-bind (local ip))"
                (apply str
                  (map (fn [p]
                         (str "(allow network-inbound (local ip \"*:" p "\"))"))
                       inbound))))]

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
   fold in the environment-snapshotted `:allow-read-write`/`:allow-write`/
   `:allow-read`/`:deny-write`/`:deny-read` paths (home-expanded + realpath'd).
   Called per spawn, so each child gets the CURRENT live roots without re-reading
   model-writable project config. `:allow-read-write` is the concise equivalent of
   granting the same path through both legacy allow lists."
  [{:keys [roots-fn net-enabled? allow-read-write allow-write allow-read deny-write deny-read
           deny-exec proxy-port loopback-port inbound-ports]}]
  (let
    [session-roots
     (when roots-fn (try (roots-fn) (catch Throwable _ nil)))

     tmps
     [(System/getProperty "java.io.tmpdir") "/tmp"]

     rw
     (->> (concat session-roots tmps allow-read-write allow-write)
          (keep real-path)
          distinct
          vec)

     ro
     (->> (concat allow-read-write allow-read)
          (keep real-path)
          distinct
          vec)

     ;; Extra inbound ports are sanitized to distinct integers in the legal TCP
     ;; range; anything else (nil, junk, out-of-range) is dropped so a bad config
     ;; value can never widen the profile or corrupt the emitted SBPL.
     inbound-ports
     (->> inbound-ports
          (keep (fn [p]
                  (let
                    [n (cond (integer? p) (long p)
                             (string? p) (parse-long (str/trim p)))]
                    (when (and n (<= 1 n 65535)) n))))
          distinct
          vec)]

    {:rw rw
     :ro ro
     :deny-write (vec deny-write)
     :deny-read (vec deny-read)
     :deny-exec (vec deny-exec)
     :net-enabled? (boolean net-enabled?)
     :proxy-port proxy-port
     :loopback-port loopback-port
     :inbound-ports inbound-ports}))

(defn wrap-argv
  "Given the base executor argv and a jail POLICY value, return the argv to spawn.
   On macOS the first managed process is wrapped with `sandbox-exec`; descendants
   inherit that kernel policy and are left unwrapped because Seatbelt rejects a
   second sandbox application. Unsupported platforms remain explicit passthroughs."
  [argv policy]
  (if (and policy (not (:disabled? policy)) (supported?) (not (inherited-jail?)))
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
  (if (:disabled? policy)
    {}
    (let
      [jail-env
       (if (and policy (or (inherited-jail?) (supported?))) {"VIS_SEATBELT_ACTIVE" "1"} {})]
      (if-let [port (:proxy-port policy)]
        (let
          [token (:proxy-token policy)
           url (str "http://" (when token (str token "@")) "127.0.0.1:" port)
           ;; SOCKS5 shares the SAME loopback port (multiplexed by first byte).
           ;; `ALL_PROXY` is the fallback for non-HTTP schemes (ssh/git+ssh/db/raw
           ;; TCP) — it points at the SOCKS lane, while `http(s)_proxy` keep the
           ;; HTTP proxy so HTTPS verb/path MITM is preserved for web traffic.
           socks-url (str "socks5h://" (when token (str token "@")) "127.0.0.1:" port)
           ca (:ca-file policy)
           java-opts (java-proxy-options policy)]

          (cond->
            (merge jail-env
                   {"http_proxy" url
                    "https_proxy" url
                    "all_proxy" socks-url
                    "HTTP_PROXY" url
                    "HTTPS_PROXY" url
                    "ALL_PROXY" socks-url
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
        jail-env))))

(def ^:private env-passthrough-names
  "Non-secret operator vars a confined child may inherit. Everything else in the
   operator environment — API keys, tokens, credentials — is dropped."
  #{"PATH" "HOME" "USER" "LOGNAME" "SHELL" "LANG" "LANGUAGE" "TERM" "TERMINFO" "TZ" "TMPDIR" "PWD"
    "HOSTNAME" "COLORTERM" "DISPLAY"})

(def ^:private env-passthrough-prefixes ["LC_"])

(defn- env-passthrough?
  [extra ^String k]
  (or (contains? env-passthrough-names k)
      (contains? extra k)
      (boolean (some #(str/starts-with? k %) env-passthrough-prefixes))))

(defn jailed-child-env
  "The COMPLETE environment for a confined child. Only an allowlist of safe,
   non-secret operator vars (PATH/HOME/LANG/…) plus any `jail.env` opt-ins are
   inherited; every API key / token / credential in the operator environment is
   DROPPED. This session's proxy + CA variables are then added. Returns nil when
   the policy is not enforcing — the caller keeps the parent environment as-is
   (unjailed platforms/`sandbox: false`), so non-confined behavior is unchanged."
  [policy]
  (when (and policy (not (:disabled? policy)) (or (inherited-jail?) (supported?)))
    (let
      [extra
       (into #{} (comp (map str) (remove str/blank?)) (:env-passthrough policy))

       inherited
       (into {}
             (filter (fn [[k _]]
                       (env-passthrough? extra k)))
             (System/getenv))]

      (merge inherited (proxy-env policy)))))

;; ── Standard language-process jail contract ────────────────────────────────
;; Language packs spawn managed REPLs and project test runners via raw
;; ProcessBuilder calls outside the shell executors. Every such spawn obtains its
;; argv + proxy environment from `session-process-launch`, which resolves one live
;; per-session policy atomically and fails closed when the session is unavailable.

(def ^:private repl-toolchain-read-dirs
  "Installed language runtimes/toolchains a managed process may READ to boot. They
   remain read-only: a REPL may execute them but cannot replace its JVM/Python/Bun."
  ["~/.sdkman" "~/.asdf" "~/.jenv" "~/.pyenv" "~/.local/bin" "~/.local/share/mise"])

(defn- normalize-cache-entry
  "Normalize one `:language-cache-dirs` entry to `{:path .. :write? <bool>}`.
   A bare string grants READ+WRITE (a dependency cache normally must be
   populated). A map `{:path .. :access ..}` chooses `read-only`/`ro` (read
   only) vs anything else (read+write). Blank/`:path`-less entries drop."
  [entry]
  (letfn [(g [m k] (or (get m k) (get m (name k))))]
    (cond (string? entry) (when (seq entry) {:path entry :write? true})
          (map? entry) (when-let [p (g entry :path)]
                         (let
                           [a (some-> (g entry :access)
                                      name
                                      str/lower-case)]
                           {:path p :write? (not (contains? #{"read-only" "readonly" "ro"} a))})))))

(def ^:private language-process-runtime-dirs
  "Vis-owned runtime state managed language processes may read and write. REPL
   managers write lifecycle logs here, including when exercised from a jailed
   project test runner."
  ["~/.vis/logs"])

(defn language-process-policy
  "Derive a managed-language jail policy from a session's base policy. It keeps
   filesystem confinement, adds read-only runtime installations plus Vis-owned log
   directories, and replaces the shell proxy endpoint with this session's
   attributed language-process endpoint.

   Dependency caches are opt-in only. `jail.filesystem.language-caches` is the sole
   source. Each entry is a plain path string (read/write) or a map with `path` and
   `access: read-only`; unset means no dependency caches.

   Direct network access is disabled. CA-aware runtimes receive the combined PEM
   bundle, while JVM children also receive an ephemeral PKCS12 truststore.
   `loopback-port` permits only the managed nREPL's selected listener port."
  [base loopback-port]
  (cond (nil? base) nil
        (:disabled? base) base
        :else (let
                [entries
                 (keep normalize-cache-entry (:language-cache-dirs base))

                 cache-rw
                 (into [] (comp (filter :write?) (map :path)) entries)

                 cache-ro
                 (into [] (comp (remove :write?) (map :path)) entries)]

                (-> base
                    (update :allow-write #(vec (concat % cache-rw language-process-runtime-dirs)))
                    (update :allow-read
                            #(vec (concat %
                                          repl-toolchain-read-dirs
                                          cache-rw
                                          cache-ro
                                          language-process-runtime-dirs)))
                    ;; Managed REPL/test children get ONLY their own nREPL
                    ;; loopback port; the shell dev-server inbound allowlist is
                    ;; not theirs to inherit (least privilege).
                    (dissoc :language-cache-dirs :inbound-ports)
                    (assoc :net-enabled? false
                           :proxy-port (:repl-proxy-port base)
                           :proxy-token nil
                           :ca-file (:repl-ca-file base)
                           :java-proxy? true
                           :loopback-port loopback-port)))))

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
   (let
     [policy
      (language-process-policy (session-base-policy! session-id) loopback-port)

      full
      (jailed-child-env policy)]

     ;; Confined child ⇒ FULL scrubbed env replaces the operator's (secrets dropped);
     ;; unenforced platform ⇒ additions merged onto the inherited env (unchanged).
     (if full
       {:argv (wrap-argv argv policy) :env full :replace-env? true}
       {:argv (wrap-argv argv policy) :env (proxy-env policy) :replace-env? false}))))
