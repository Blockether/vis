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
      :mach-services    [<name> …]           ; macOS Mach services a child may look up
      :inbound-ports    [<int> …]            ; extra local ports a child may ACCEPT on
                                             ; (bind is local-only; accept is port-gated)
      :env-values       {<NAME> <value>}     ; RESOLVED project env (`.env` +
                                             ; `environment:`) with ONE call's own
                                             ; `env` delta merged on, per spawn
      :env-removals     #{<NAME> …}          ; names THAT call asked to UNSET
      :inherit-host-env? <bool>}             ; `jail.environment: inherit` — the child
                                             ; also keeps the operator's ambient env

   The filesystem model mirrors Anthropic's sandbox-runtime:
     - WRITE is allow-only: denied everywhere except the session roots + tmp +
       `:allow-read-write` + `:allow-write`; `:deny-write` wins.
     - READ is default-deny here (workspace-focused, stronger than srt's
       read-everywhere default): system code/config + RW paths + `:allow-read`
       are readable; `:deny-read` wins.

   `spawn!` compiles the policy per process and hands it to the adjacent
   `libvisjail`: Seatbelt on macOS, embedded bubblewrap on Linux. No enforcer
   executable is searched on PATH or installed by the operator. Other operating
   systems are unsupported and an enabled policy fails before spawn.

   Two locks learned from the kernel, baked in here:
     1. Seatbelt matches RESOLVED real paths, so every root is realpath'd before
        templating (`/tmp` -> `/private/tmp`, else the rule never matches).
     2. a default-deny profile must import system.sb or dyld/sysctl startup reads
        are denied and every binary aborts before `main`."
  (:require [clojure.string :as str]
            [com.blockether.vis-python-runtime :as python-runtime]
            [com.blockether.vis.internal.python-runtime :as vis-python-runtime]
            [com.blockether.vis.internal.config :as config]
            [com.blockether.vis.internal.paths :as paths]
            [com.blockether.vis.internal.util :as util])
  (:import (java.io File)
           (java.nio.file LinkOption Paths)
           (java.util HashMap)))

(def ^:private link-opts (make-array LinkOption 0))

(defn- real-path
  "Canonical real-path string of `s`, or nil when it cannot be resolved. Seatbelt
   matches resolved paths, so roots must pass through here before templating."
  [s]
  (let [s (paths/expand-home s)]
    (when-not (str/blank? (str s))
      (try (.toString (.toRealPath (Paths/get (str s) (make-array String 0)) link-opts))
           (catch Throwable _ nil)))))

(defn- deny-path
  "Resolve a DENY target: prefer its real path, else fall back to the raw expanded
   string. Deny lists must fail safe — a not-yet-existing secret still denies."
  [s]
  (or (real-path s)
      (let [e (paths/expand-home s)]
        (when-not (str/blank? (str e)) (str e)))))

(defn- os-kind
  []
  (let [n (str/lower-case (str (System/getProperty "os.name")))]
    (cond (str/includes? n "mac") :macos
          (str/includes? n "linux") :linux
          :else :other)))

;; WSL detection. WSL2 runs a real Linux kernel, so user and mount namespaces work
;; and it is treated as ordinary Linux. WSL1 is a syscall-translation shim with no
;; real kernel namespaces, so embedded bubblewrap cannot enforce anything; it is
;; reported UNENFORCEABLE rather than silently passing children through. The
;; kernel `osrelease` distinguishes them: WSL2 = `...-microsoft-standard-WSL2`,
;; WSL1 = `...-Microsoft` (no `WSL2` marker).
(defn- linux-osrelease [] (try (slurp "/proc/sys/kernel/osrelease") (catch Throwable _ "")))

(defn- wsl1?
  "True only on WSL1 (a `microsoft` kernel WITHOUT the `WSL2` real-kernel marker)."
  []
  (let [r (str/lower-case (linux-osrelease))]
    (and (str/includes? r "microsoft") (not (str/includes? r "wsl2")))))

(defn supported?
  "True when the current OS has a matching `libvisjail` beside libvispython.
   WSL1 is excluded because it has no real Linux namespaces."
  []
  (and (#{:macos :linux} (os-kind))
       (not (wsl1?))
       (try (boolean (python-runtime/resolve-jail)) (catch Throwable _ false))))

(defn unenforceable-reason
  "Nil when `supported?`, else a human explanation of the platform/runtime gap."
  []
  (when-not (supported?)
    (cond (= :other (os-kind)) "the OS process jail is not available on this operating system"
          (wsl1?) "WSL1 has no real Linux kernel namespaces; the jail needs WSL2"
          :else "the selected Python runtime has no matching libvisjail")))

(defn- inherited-jail?
  "True inside a child already confined by this process-jail contract. Seatbelt
   restrictions are inherited across exec, and macOS rejects applying a second
   profile, so descendants must not wrap themselves again."
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
  ;; A `delay`, never an eager read: `native-image` initializes this namespace at
  ;; BUILD time, so `user.home`/`java.io.tmpdir` would be the BUILDER's.
  (delay (into ["/" "/Users" "/Volumes" "/private" "/opt" "/etc" "/var" "/tmp" "/home"]
               (remove nil?)
               [(System/getProperty "user.home") (System/getProperty "java.io.tmpdir")])))

;; Linux read-only system roots a confined child needs to launch a real toolchain
;; (dynamic loader, shared libs, shell, and -- once net is on -- TLS CAs + resolver
;; config under /etc). All world-readable code/config, bind-mounted read-only; no
;; user secrets (those live under $HOME, which is NOT bound unless it is a session
;; root). `*-try` variants tolerate a path missing on a given distro.
(def ^:private linux-system-read-roots
  ["/usr" "/bin" "/sbin" "/lib" "/lib64" "/lib32" "/etc" "/opt" "/nix" "/run" "/var/lib"])

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
  (loop [cur
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
   outs so `:deny-write`/`:deny-read` win over the allows. One-line string consumed
   directly by `libvisjail`."
  ^String
  [{:keys [rw ro deny-write deny-read deny-exec net-enabled? proxy-port loopback-port inbound-ports
           mach-services]}]
  (let [rw
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
      ;; macOS. Without this narrow IPC permission, executables such as bb abort
      ;; during VM startup before their main function runs.
      "(allow ipc-posix-sem)"
      ;; Seatbelt denies EVERY Mach lookup by default, which is what breaks macOS
      ;; Keychain reads (`security`, `gh auth token`, `git credential-osxkeychain`)
      ;; inside the jail. Only the services the operator granted are opened.
      (when-let [ms (seq (distinct (remove str/blank? (filter string? mach-services))))]
        (str "(allow mach-lookup"
             (apply str (map #(str "(global-name " (sbpl-quote %) ")") ms))
             ")"))
      "(allow file-read-metadata"
      (apply str
        (map #(str "(literal " (sbpl-quote %) ")")
             (distinct (concat @macos-metadata-literals (mapcat ancestor-dirs (concat rw ro))))))
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
      (let [inbound
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
           deny-exec proxy-port loopback-port inbound-ports mach-services]}]
  (let [session-roots
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
                     (let [n (cond (integer? p) (long p)
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
     :inbound-ports inbound-ports
     :mach-services
     (into [] (comp (filter string?) (remove str/blank?) (distinct)) mach-services)}))

(def ^:private keychain-denial-markers
  "How the macOS Security framework reports a lookup it could not complete. The
   first two are exactly what a DENIED Mach lookup produces under Seatbelt."
  ["SecKeychainSearchCreateFromAttributes" "SecKeychainItemCopyContent"
   "errSecInteractionNotAllowed" "User interaction is not allowed"
   "could not be found in the keychain"])

(defn keychain-denial?
  "True when captured output shows a macOS Keychain lookup that failed the way a
   denied Mach lookup fails. Pure text test; `keychain-denial-hint` decides
   whether confinement is the explanation. ONE `output` string, because a command
   runs under a pty where stdout and stderr are one stream."
  [output]
  (let [text (str output)]
    (boolean (some #(str/includes? text %) keychain-denial-markers))))

(defn keychain-denial-hint
  "One actionable line when a command's output shows a Keychain lookup THIS jail
   denied, else nil. Silent when the jail is off (`:disabled?`) or the keychain
   services are already granted — the failure is then a real Keychain miss and
   naming the sandbox would send the caller the wrong way."
  [{:keys [disabled? mach-services]} output]
  (when (and (not disabled?)
             (not (some #{"com.apple.SecurityServer"} mach-services))
             (keychain-denial? output))
    (str "Keychain lookup blocked by the sandbox: Seatbelt denies every Mach lookup by default."
         " Set jail.mach_services.keychain: true in config to grant com.apple.SecurityServer,"
         " com.apple.ocspd and com.apple.trustd.agent plus read access to the keychain"
         " databases.")))

(defn linux-bwrap-args
  "Compile embedded-bubblewrap policy flags (ending in `--`) from a RESOLVED
   policy map. Only bound paths exist in the child. Later read-only/masked binds
   make deny-write, deny-read and deny-exec win. Filtered egress and managed
   inbound ports omit `--unshare-net` because libvisjail creates the private network
   namespace and bridges only those named loopback endpoints."
  ^java.util.List
  [{:keys [rw ro deny-write deny-read deny-exec net-enabled? proxy-port loopback-port]}]
  (let [rw
        (->> rw
             (keep real-path)
             distinct
             vec)

        ro
        ;; System roots are bound at their LITERAL path (not real-path'd): on merged-usr
        ;; distros `/lib`,`/lib64`,`/bin`,`/sbin` are symlinks into `/usr`, and the ELF
        ;; interpreter is hardcoded (`/lib64/ld-linux-x86-64.so.2`, `/lib/ld-linux-aarch64.so.1`).
        ;; Canonicalizing them collapses the loader mount point so EVERY binary fails to
        ;; exec (ENOENT on its interpreter). `--ro-bind-try` tolerates any absent on a distro.
        ;; User `:ro` allow-read paths stay canonicalized for dedup/symlink safety.
        (->> (concat linux-system-read-roots (keep real-path ro))
             distinct
             vec)

        dw
        (->> deny-write
             (keep deny-path)
             distinct
             vec)

        dex
        (->> deny-exec
             (keep deny-path)
             distinct
             vec)

        ro-flags
        (mapcat (fn [p]
                  ["--ro-bind-try" p p])
                ro)

        rw-flags
        (mapcat (fn [p]
                  ["--bind-try" p p])
                rw)

        dw-flags
        (mapcat (fn [p]
                  ["--ro-bind-try" p p])
                dw)

        dr-flags
        (mapcat (fn [p]
                  (let [rp (deny-path p)]
                    (cond (nil? rp) nil
                          (.isDirectory (File. ^String rp)) ["--tmpfs" rp]
                          :else ["--ro-bind-try" "/dev/null" rp])))
                (distinct deny-read))

        ;; Mask each denied binary with /dev/null (a char device): `execve` on it fails
        ;; (exit 126). Bound AFTER the allow binds so it wins over a binary inside an
        ;; allowed `:ro` root -- the Linux equivalent of macOS `(deny process-exec*)`.
        ;; On merged-usr distros the same binary is reachable via BOTH `/usr/bin/<n>` and
        ;; `/bin/<n>` (distinct bwrap mounts), so masking only the canonical path leaves
        ;; the PATH alias runnable -- mask every EXISTING bin-dir alias of the basename.
        ;; `--ro-bind-try` aborts if the destination is absent on a read-only bind, so the
        ;; alias set is filtered to files that actually exist on the host.
        dex-flags
        (mapcat (fn [p]
                  (let [n (.getName (File. ^String p))]
                    (->> (cons p
                               (map #(str % "/" n)
                                    ["/usr/bin" "/bin" "/usr/sbin" "/sbin" "/usr/local/bin"
                                     "/usr/local/sbin"]))
                         (filter #(.exists (File. ^String %)))
                         distinct
                         (mapcat (fn [t]
                                   ["--ro-bind-try" "/dev/null" t])))))
                dex)

        ;; A named proxy or inbound endpoint asks libvisjail to own the private
        ;; namespace and its two narrow loopback bridges. With neither endpoint,
        ;; bubblewrap itself closes the network unless policy explicitly opens it.
        net
        (if (or proxy-port loopback-port net-enabled?) [] ["--unshare-net"])

        bwrap-args
        (vec (concat ["--die-with-parent" "--proc" "/proc" "--dev" "/dev"]
                     ro-flags
                     rw-flags
                     dw-flags
                     dr-flags
                     dex-flags
                     net
                     ["--"]))]

    bwrap-args))



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
   the kernel policy is already inherited, preventing an invalid nested profile.
   When a gateway proxy endpoint is present, common proxy and CA variables cover
   curl/git/Python/Bun/etc.; managed JVM children additionally receive proxy plus
   ephemeral truststore properties through JAVA_TOOL_OPTIONS."
  [policy]
  (if (:disabled? policy)
    {}
    (let [jail-env
          (if (and policy (or (inherited-jail?) (supported?))) {"VIS_SEATBELT_ACTIVE" "1"} {})]
      (if-let [port (:proxy-port policy)]
        (let [token (:proxy-token policy)
              url (str "http://" (when token (str token "@")) "127.0.0.1:" port)
              ;; SOCKS5 shares the SAME loopback port (multiplexed by first byte).
              ;; `ALL_PROXY` is the fallback for non-HTTP schemes (ssh/git+ssh/db/raw
              ;; TCP) — it points at the SOCKS lane, while `http(s)_proxy` keep the
              ;; HTTP proxy so HTTPS verb/path MITM is preserved for web traffic.
              socks-url (str "socks5h://" (when token (str token "@")) "127.0.0.1:" port)
              ca (:ca-file policy)
              java-opts (java-proxy-options policy)]

          (cond-> (merge jail-env
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
  "Non-secret operator vars a confined child may inherit under the default
   `jail.environment: declared`. Everything else in the operator environment —
   API keys, tokens, credentials — is dropped; a variable a child legitimately
   needs is DECLARED in `environment:` and arrives as a resolved value
   (`:env-values`), not by widening this list. `jail.environment: inherit`
   (`:inherit-host-env?`) hands the child the ambient environment WHOLE instead —
   the operator's explicit choice to give up ambient secrecy, and the only thing
   that widens this."
  #{"PATH" "HOME" "USER" "LOGNAME" "SHELL" "LANG" "LANGUAGE" "TERM" "TERMINFO" "TZ" "TMPDIR" "PWD"
    "HOSTNAME" "COLORTERM" "DISPLAY"})

(def ^:private env-passthrough-prefixes ["LC_"])

;; ── Variables that hijack the PRE-POLICY child bootstrap ───────────────────────
;;
;; `libvisjail` creates the child and installs its kernel policy before the requested
;; command starts. Loader and runtime variables still affect native setup before the
;; command reaches `main`, and can redirect code loading outside the policy's intended
;; executable graph. They therefore never cross this trust boundary.
;;
;; These names are therefore refused UNCONDITIONALLY — declaring one under
;; `environment:` cannot re-enable it, because the whole point of a declaration
;; is to hand a variable to the CONFINED child, and these never reach it: they
;; are consumed earlier, outside the jail. Declaring `LD_PRELOAD` for a sandbox
;; is not a use case, it is the exploit.

(def ^:private pre-exec-hijack-names
  "Exact names that run attacker code during another program's startup."
  #{"GCONV_PATH" "LOCPATH" "NLSPATH" "HOSTALIASES" "RESOLV_HOST_CONF" "BASH_ENV" "ENV" "SHELLOPTS"
    "BASHOPTS" "IFS"})

(def ^:private pre-exec-hijack-prefixes
  "Prefix families with the same power. `LD_*`/`DYLD_*` are the dynamic loader's
   own injection hooks (`LD_PRELOAD`, `LD_AUDIT`, `LD_LIBRARY_PATH`,
   `DYLD_INSERT_LIBRARIES`); `PERL*` configures the detacher itself (`PERL5OPT`
   loads arbitrary modules); `BASH_FUNC_*` smuggles shell functions. Matched by
   prefix on purpose — enumerating each name is a losing game."
  ["LD_" "DYLD_" "PERL" "BASH_FUNC_"])

(defn- pre-exec-hijack?
  "True when `k` can execute code in the unconfined detacher/enforcer hops."
  [^String k]
  (or (contains? pre-exec-hijack-names k)
      (boolean (some #(str/starts-with? k %) pre-exec-hijack-prefixes))))

(defn- env-passthrough?
  [^String k]
  (and (not (pre-exec-hijack? k))
       (or (contains? env-passthrough-names k)
           (boolean (some #(str/starts-with? k %) env-passthrough-prefixes)))))

(defn declared-env
  "The policy's RESOLVED project environment, as string pairs: the workspace's
   `.env`/`.env.local` with the operator's `environment:` declarations on top
   (`config/child-environment-values`). Every value already came from the source
   that produced it, so the jail hands them over verbatim — a confined child can
   read `.env` out of the workspace it was granted anyway, so withholding those
   values would confine nothing. A [[pre-exec-hijack?]] name is dropped even
   here, whether it came from a declaration or from a project `.env`: it would
   run code in the UNCONFINED detacher/enforcer hops, so nothing can buy it
   back."
  [policy]
  (into {}
        (keep (fn [[k v]]
                (let [k (str k)]
                  (when (and (not (str/blank? k)) (not (pre-exec-hijack? k)) (some? v))
                    [k (str v)]))))
        (:env-values policy)))

(defn jailed-child-env
  "The COMPLETE environment for a confined child: an allowlist of non-secret
   operator variables, plus the policy's resolved project environment (the
   workspace's `.env` plus the `environment:` declarations), plus this session's
   proxy + CA variables. Every API key / token / credential the operator happens
   to have exported is DROPPED — an AMBIENT variable a child needs is named in
   `environment:` — and so is every [[pre-exec-hijack?]] name, which would run
   code in the unconfined detacher/enforcer before the jail exists.

   `jail.environment: inherit` (`:inherit-host-env?`) replaces the allowlist with
   the operator's WHOLE ambient environment, secrets included: filesystem,
   network, exec and Mach confinement are unchanged, but ambient secrecy is
   given up on purpose. The [[pre-exec-hijack?]] scrub still applies — that one
   is not confinement of the child, it is the jail's own installation.

   Returns nil when the policy is not enforcing — the caller keeps the parent
   environment and merges [[child-env-additions]] instead (unjailed
   platforms/`jail.enabled: false`), so non-confined behavior is unchanged."
  [policy]
  (when (and policy (not (:disabled? policy)) (or (inherited-jail?) (supported?)))
    (let [inherited
          (into {}
                (if (:inherit-host-env? policy)
                  (map identity)
                  (filter (fn [[k _]]
                            (env-passthrough? k))))
                (System/getenv))

          ;; A name ONE call asked to unset is simply never built into the map —
          ;; a confined child's environment is assembled here from nothing, so
          ;; there is nothing to remove later.
          removals
          (set (:env-removals policy))]

      ;; Total: the scrub also covers the declared + proxy additions, so no later
      ;; edit to either can reintroduce a pre-exec hijack name.
      (into {}
            (remove (fn [[k _]]
                      (or (pre-exec-hijack? k) (contains? removals k))))
            (merge inherited (declared-env policy) (proxy-env policy))))))

(defn child-env-additions
  "What an UNCONFINED child gets ON TOP of the inherited host environment: the
   resolved project environment (workspace `.env` + `environment:` declarations)
   plus this session's proxy + CA variables. They apply whether or not the jail
   is enabled — the project says where a variable comes from, and the jail only
   decides what ELSE a child may keep."
  [policy]
  (merge (declared-env policy) (proxy-env policy)))
(defn process-environment
  "Build the COMPLETE child environment for `policy`, then overlay trusted
   host-owned `extra`. Confined children start from the scrubbed allowlist;
   disabled policies preserve the ambient environment and apply removals."
  ([policy] (process-environment policy nil))
  ([policy extra]
   (let [^HashMap environment (if-let [full (jailed-child-env policy)]
                                (HashMap. ^java.util.Map full)
                                (doto (HashMap. ^java.util.Map (System/getenv))
                                  (.putAll ^java.util.Map (child-env-additions policy))))]
     (doseq [k (:env-removals policy)]
       (.remove environment ^String k))
     (when (seq extra) (.putAll environment ^java.util.Map extra))
     environment)))

(defn spawn!
  "Spawn `argv` through the runtime-owned process boundary. An enabled policy
   is compiled to Seatbelt or embedded-bubblewrap input; a disabled policy still
   gets libvisjail's detached process group, PTY and lifecycle implementation.

   Options: `:directory`, an exact `:environment` or trusted
   `:extra-environment`, `:pty?`, `:merge-stderr?`, `:rows`, and `:columns`."
  ([argv directory policy] (spawn! argv directory policy nil))
  ([argv directory policy {:keys [environment extra-environment pty? merge-stderr? rows columns]}]
   (let [wanted?
         (and policy (not (:disabled? policy)))

         inherited?
         (and wanted? (inherited-jail?))

         confined?
         (and wanted? (not inherited?))

         _
         (when (and confined? (not (supported?)))
           (throw (ex-info (str "Process denied: " (unenforceable-reason))
                           {:type ::jail-unavailable})))

         compiled
         (when confined? (compile-policy policy))

         ;; The spawn itself is the runtime library's (`Jail/spawn`), so the
         ;; cdylib has to be resolvable HERE — in the process doing the spawning,
         ;; which is not necessarily one that ever started an interpreter. It used
         ;; to be resolved as a side effect of building a session's Python; a
         ;; session with a worker of its own starts no interpreter in this
         ;; process, and `shell` began answering "No vis-python runtime library"
         ;; in a live gateway. Ensuring is a no-op once it resolves.
         _
         (try (vis-python-runtime/ensure-library!) (catch Throwable _ nil))]

     (python-runtime/spawn-process!
       (mapv str argv)
       {:environment (or environment (process-environment policy extra-environment))
        :directory (some-> directory
                           str)
        :confined? confined?
        :seatbelt-profile (when (= :macos (os-kind))
                            (some-> compiled
                                    macos-profile))
        :linux-arguments (if (= :linux (os-kind)) (if compiled (linux-bwrap-args compiled) []) [])
        :proxy-port (when (= :linux (os-kind)) (:proxy-port compiled))
        :inbound-port (when (= :linux (os-kind)) (:loopback-port compiled))
        :pty? (boolean pty?)
        :merge-stderr? (boolean merge-stderr?)
        :rows (int (or rows 0))
        :columns (int (or columns 0))}))))
;; ── ONE call's own environment ──────────────────────────────────────────────
;; `environment:` says what EVERY child of Vis gets. A verb that SPAWNS also
;; carries what THIS child gets on top, and it carries it as an ARGUMENT of the
;; call — `shell(cmd, {"env": …})`, `repl_start({"env": …})` — never as
;; an ambient binding wrapped around a block. The record of the call is what
;; says which variables the child ran with: a scope opened three lines earlier
;; is gone the moment that block is folded, and independent spawns awaited
;; together would turn "which one saw the variable" into a question about
;; coroutines instead of about arguments.

(def ^:private call-env-name-pattern
  "A POSIX environment variable name — nothing else can be exported at all."
  #"[A-Za-z_][A-Za-z0-9_]*")

(def ^:private call-env-source-keys
  "Declaration keys ONE call's value may name — `environment:`'s own vocabulary,
   resolved by its own funnel (`config/inline-environment-value`)."
  ["env" "dotenv" "keychain" "command" "literal"])

(defn- refuse-call-env
  "Refuse the spawn and NAME the key. A per-call delta is the author's own line
   of code, so a variable dropped quietly here would be debugged as a bug in the
   program that never received it — the opposite of the `environment:` scrub,
   which filters a standing declaration nobody is watching."
  [name reason]
  (throw (ex-info (str "env " name ": " reason) {:type ::call-env-refused :name name})))

(defn- call-env-value
  "ONE delta value: nil to unset, a literal as its string, or a source map
   resolved exactly as the `environment:` block resolves the same shape."
  [k v]
  (cond (nil? v) nil
        (string? v) v
        (or (number? v) (boolean? v)) (str v)
        (map? v) (let [entry
                       (into {}
                             (map (fn [[ek ev]]
                                    [(if (keyword? ek) (name ek) (str ek)) ev]))
                             v)

                       source
                       (some (fn [sk]
                               (when (some? (get entry sk)) sk))
                             call-env-source-keys)]

                   (when-not source
                     (refuse-call-env
                       k
                       (str "a map value must name its source — "
                            (str/join ", " (map #(str "{\"" % "\": …}") call-env-source-keys))
                            " — got "
                            (pr-str v)
                            ".")))
                   (when (= "command" source)
                     (let [command (get entry source)]
                       (when-not (and (sequential? command)
                                      (seq command)
                                      (every? util/non-blank-string? command))
                         (refuse-call-env
                           k
                           (str "command source must be a non-empty argv list of non-blank strings"
                                (if (string? command)
                                  ", not a shell string."
                                  (str " — got " (pr-str command) ".")))))))
                   (or (config/inline-environment-value k entry)
                       (refuse-call-env k (str "the " source ": source resolved to no value."))))
        :else (refuse-call-env k
                               (str "value must be a literal (string, number, boolean), a source"
                                    " map, or null to unset — got "
                                    (pr-str v)
                                    "."))))

(defn call-env-values
  "Resolve ONE call's `env` delta into `{NAME value-or-nil}`, where nil means
   UNSET that name for this child. A DELTA: it is merged over the project
   environment (`config/child-environment-values`), never a replacement for it,
   so a workspace `.env` still reaches a child whose call names one variable.

   A value is either a LITERAL (string/number/boolean) or a SOURCE map — the
   same `{env|dotenv|keychain|command|literal}` shape `environment:` declares. That
   split is not style: this map is an ARGUMENT, so a literal is written into the
   session journal and the transcript for good. Literals are for SWITCHES
   (`NODE_ENV`, `RUST_LOG`, `PYTHONHASHSEED`); a secret names its source and
   only the child ever sees the value.

   Every refusal is LOUD and names the key: a name that is not a variable name,
   a [[pre-exec-hijack?]] name (which the jail would drop anyway, silently), a
   map naming no source, and a source that produced nothing — an explicit
   request for ONE variable that resolved to nothing is an error here, not the
   quiet `:unset` a standing declaration is allowed."
  [env]
  (cond (nil? env) {}
        (not (map? env))
        (throw (ex-info (str "env must be a map of NAME → value (a literal, or"
                             " {\"env\"|\"dotenv\"|\"keychain\"|\"command\"|\"literal\": …}), got "
                             (pr-str env))
                        {:type ::call-env-refused}))
        :else (into {}
                    (map (fn [[k v]]
                           (let [k (str/trim (str (if (keyword? k) (name k) k)))]
                             (when-not (re-matches call-env-name-pattern k)
                               (refuse-call-env (pr-str k) "not an environment variable name."))
                             (when (pre-exec-hijack? k)
                               (refuse-call-env
                                 k
                                 (str "runs code in the unconfined detacher/enforcer hops,"
                                      " before the jail exists — no call may set it.")))
                             [k (call-env-value k v)])))
                    env)))

(defn with-call-env
  "`policy` with ONE call's resolved delta merged over its project environment.
   Names set to nil become `:env-removals`: a confined child's environment is
   built from nothing so they are simply never added, while an UNCONFINED child
   inherits this process' environment and must have them removed before the
   complete map crosses the native spawn boundary.

   A nil policy is a spawn with no jail at all, and the delta still applies —
   a caller cannot lose its own variables by running where the jail is off."
  [policy overrides]
  (if (empty? overrides)
    policy
    (let [policy
          (or policy {:disabled? true})

          base
          (into {}
                (map (fn [[k v]]
                       [(str k) v]))
                (:env-values policy))

          removals
          (into #{} (comp (filter (comp nil? val)) (map key)) overrides)

          sets
          (into {} (remove (comp nil? val)) overrides)]

      (assoc policy
        :env-values (merge (apply dissoc base removals) sets)
        :env-removals (into (set (:env-removals policy)) removals)))))

(defn env-fingerprint
  "`{NAME \"<digest>\"}` for one resolved delta — its SHAPE without its values.
   This is what a status prints and what a REUSED process is compared against,
   and both of those are read by a model and written to a log, so the value
   itself can never appear: a name set from a keychain must compare equal to
   itself and to nothing else. An unset name fingerprints as \"unset\"."
  [values]
  (into (sorted-map)
        (map (fn [[k v]]
               [(str k) (if (nil? v) "unset" (subs (util/sha256-hex (str v)) 0 12))]))
        values))
(defn env-difference
  "Variable NAMES whose value differs between the env a live process is running
   with and the one a new start asked for. Both sides are FINGERPRINTS, so this
   compares digests and answers names — the only thing either side may keep."
  [running requested]
  (vec (sort (into #{}
                   (remove (fn [k]
                             (= (get running k) (get requested k))))
                   (concat (keys running) (keys requested))))))

(defn env-mismatch-refusal
  "`{:message :differing}` when a REPL is already running with an env OTHER than
   the one this start named, else nil. Every language pack answers this same
   refusal, because `repl_start` must mean ONE thing across languages: a live
   REPL is reused, never silently replaced, and an env it was not started with
   is a different REPL. Names and digests only — a value never reaches it."
  [id running requested]
  (when-let [differing (seq (env-difference running requested))]
    {:differing (vec differing)
     :message (str "repl_start for " id
                   " is already running with a different env (" (str/join ", " differing)
                   "). There is no restart:"
                   " repl_stop that REPL, then start it with this env.")}))

;; ── Standard language-process jail contract ────────────────────────────────
;; Language packs spawn managed REPLs and project test runners through
;; `session-process-spawn!`, which resolves one live per-session policy atomically
;; and crosses the one libvisjail boundary.

(def ^:private repl-toolchain-read-dirs
  "Installed language runtimes/toolchains a managed process may READ to boot. They
   remain read-only: a REPL may execute them but cannot replace its JVM/Python/Bun."
  ["~/.sdkman" "~/.asdf" "~/.jenv" "~/.pyenv" "~/.local/bin" "~/.local/share/mise"])

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

   Dependency caches enter through the shared `workspace.filesystem` catalog and
   are already present on the base policy's read/write roots; this pass only adds
   the read-only language runtimes/toolchains plus Vis-owned log directories.

   Direct network access is disabled. CA-aware runtimes receive the combined PEM
   bundle, while JVM children also receive an ephemeral PKCS12 truststore.
   `loopback-port` permits only the managed nREPL's selected listener port."
  [base loopback-port]
  (cond (nil? base) nil
        (:disabled? base) base
        :else (-> base
                  (update :allow-write #(vec (concat % language-process-runtime-dirs)))
                  (update :allow-read
                          #(vec (concat % repl-toolchain-read-dirs language-process-runtime-dirs)))
                  ;; Managed REPL/test children get ONLY their own nREPL
                  ;; loopback port; the shell dev-server inbound allowlist is
                  ;; not theirs to inherit (least privilege).
                  (dissoc :inbound-ports)
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
   start a REPL or project test process. Safe and idempotent per dispatch."
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
    (let [policy (try (policy-fn)
                      (catch Throwable t
                        (throw (ex-info
                                 "Managed language process denied: session jail policy failed"
                                 {:type ::session-jail-failed :session-id session-id}
                                 t))))]
      (when-not policy
        (throw (ex-info "Managed language process denied: session jail policy is unavailable"
                        {:type ::session-jail-missing :session-id session-id})))
      policy)))

(defn session-process-spawn!
  "THE managed-language launch contract. Resolve `session-id` atomically, derive
   its REPL/test policy, merge this call's environment delta, and spawn through
   [[spawn!]]. Unknown, disposed, or failing sessions are denied before spawn.

   Options additionally accept `:loopback-port`, `:env`, and every [[spawn!]]
   option. The returned value is a `java.lang.Process`."
  ([session-id argv directory] (session-process-spawn! session-id argv directory nil))
  ([session-id argv directory {:keys [loopback-port env] :as opts}]
   (let [policy (-> (session-base-policy! session-id)
                    (language-process-policy loopback-port)
                    (with-call-env (call-env-values env)))]
     (spawn! argv directory policy (dissoc opts :loopback-port :env)))))
