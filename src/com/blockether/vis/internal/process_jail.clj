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
  ^String [{:keys [rw ro deny-write deny-read net-enabled? proxy-port]}]
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
      ;; Network: when a `proxy-port` is set the child is walled to the loopback
      ;; egress proxy ONLY (net-off-except-loopback) — every byte must traverse the
      ;; gateway proxy, so a raw socket / curl --noproxy has nowhere to go and the
      ;; vis.yml domain+verb rules become real. Else: plain on/off. NOTE: Seatbelt only
      ;; accepts `localhost`/`*` as the remote host (not a `127.0.0.1` literal), and
      ;; `localhost:port` DOES match a 127.0.0.1 loopback connect — verified.
      (cond proxy-port (str "(deny network*)"
                            "(allow network-outbound (remote ip \"localhost:"
                            proxy-port
                            "\"))")
            net-enabled? "(allow network*)"
            :else "(deny network*)"))))

(defn compile-policy
  "Resolve a raw jail policy VALUE into the canonical map `macos-profile` consumes:
   read the LIVE session roots via `:roots-fn`, add the always-writable temp dirs,
   fold in the vis.yml `:allow-write`/`:allow-read`/`:deny-write`/`:deny-read`
   paths (home-expanded + realpath'd). Called per spawn, so each child jails to
   the CURRENT roots and CURRENT config."
  [{:keys [roots-fn net-enabled? allow-write allow-read deny-write deny-read proxy-port]}]
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
     :proxy-port proxy-port}))

(defn wrap-argv
  "Given the base executor argv (e.g. `[bash --noprofile --norc -lc <cmd>]`) and a
   jail POLICY value (or nil), return the argv to ACTUALLY spawn: the same argv
   wrapped in the OS jail when a policy is given and this OS can enforce it, else
   the argv unchanged. Pure — the policy is an explicit per-session value, never a
   global; the session roots inside it are resolved fresh on every call."
  [argv policy]
  (if (and policy (supported?))
    (case (os-kind)
      :macos
      (into ["sandbox-exec" "-p" (macos-profile (compile-policy policy))] argv)

      argv)
    argv))

(defn proxy-env
  "Environment additions for a jailed child whose policy routes through the loopback
   egress proxy: point every common HTTP-client proxy var at 127.0.0.1:<proxy-port> so
   curl/wget/git/requests/… send their traffic to the one door the jail leaves open.
   Returns {} when the policy carries no `:proxy-port`. Both letter-cases are set
   because tools disagree (curl reads lowercase, many libraries read uppercase)."
  [policy]
  (if-let [port (:proxy-port policy)]
    (let [url (str "http://127.0.0.1:" port)]
      {"http_proxy" url
       "https_proxy" url
       "all_proxy" url
       "HTTP_PROXY" url
       "HTTPS_PROXY" url
       "ALL_PROXY" url})
    {}))
