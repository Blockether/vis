(ns com.blockether.vis.internal.process-jail
  "OS-level process CONTAINMENT — the 'jail' — that wraps the shell executors'
   argv so an allowed child is physically confined to the session workspace roots
   and, when network is off, cannot open a socket. This is real containment, a
   a real containment boundary — not a cooperative name/argv check, which can be
   walked around since argv[0] is `bash` and the real binary hides inside the
   `-lc` string;
   the jail constrains what the child can DO once it runs, regardless of what a
   script inside it tries (curl, python -c, /dev/tcp — all hit the same wall).

   POLICY, NOT GUARDS. The jail is driven by a declarative *policy* compiled from
   vis.yml + the LIVE session roots, not by hand-written guard functions. The
   active policy the executors consult is:

     {:roots-fn     (fn [] [root-strings])  ; live session RW roots, re-read/spawn
      :net-enabled? <bool>}                 ; whole shell-child network on/off

   which `wrap-argv` compiles, per spawn, into the OS enforcement primitive:

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

(defn- real-path
  "Canonical real-path string of `s`, or nil when it can't be resolved. sandbox-exec
   matches on RESOLVED paths, so roots MUST pass through here before templating."
  [s]
  (when-not (str/blank? (str s))
    (try (.toString (.toRealPath (Paths/get (str s) (make-array String 0)) link-opts))
         (catch Throwable _ nil))))

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

;; Read paths every Mach-O binary + dyld needs to even reach main(). Kept tight:
;; system code + linker caches, nothing user-writable, no secret-bearing dirs.
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
   `{:rw [<canonical root strings>] :net-enabled? <bool>}`: default-deny, allow the
   dyld/system reads every binary needs, read+write on the session roots, and
   network only when `:net-enabled?`. One-line string for `sandbox-exec -p`."
  ^String [{:keys [rw net-enabled?]}]
  (let
    [rw-roots (->> rw
                   (keep real-path)
                   distinct)]
    (str
      "(version 1)" "(import \"system.sb\")"
      "(deny default)" "(allow process-fork process-exec)"
      "(allow sysctl-read)" "(allow file-read-metadata)"
      "(allow file-read*" (subpaths macos-system-read-roots)
      "(literal \"/dev/null\")(literal \"/dev/zero\")(literal \"/dev/random\")(literal \"/dev/urandom\"))"
      "(allow file-read* file-write*"
      "(literal \"/dev/null\")(literal \"/dev/tty\")(literal \"/dev/stdout\")(literal \"/dev/stderr\")"
      (subpaths rw-roots)
      ")" (if net-enabled? "(allow network*)" "(deny network*)"))))

;; ---------------------------------------------------------------------------
;; Active policy — the single live handle the shell executors consult. nil =
;; jail OFF = argv unchanged = exactly today's behavior (opt-in, no surprise).
;; Set by the loop when building an environment, gated by vis.yml `:shell :jail`.
;; ---------------------------------------------------------------------------
(defonce ^:private active-policy (atom nil))

(defn set-active-policy!
  "Install the live jail policy (or nil to disable). `policy` is
   `{:roots-fn (fn [] [root-strings]) :net-enabled? <bool>}`."
  [policy]
  (reset! active-policy policy))

(defn current-policy [] @active-policy)

(defn active?
  "True when a policy is installed AND this OS can enforce it — the honest signal
   for a capability description (real jail vs cooperative-gate-only)."
  []
  (boolean (and @active-policy (supported?))))

(defn- resolve-rw
  "Session roots (live, per spawn) plus the always-writable temp dirs, canonicalized."
  [policy]
  (let
    [roots
     (when-let [f (:roots-fn policy)]
       (try (f) (catch Throwable _ nil)))

     tmps
     [(System/getProperty "java.io.tmpdir") "/tmp"]]

    (->> (concat roots tmps)
         (keep real-path)
         distinct
         vec)))

(defn wrap-argv
  "Given the base executor argv (e.g. `[bash --noprofile --norc -lc <cmd>]`), return
   the argv to ACTUALLY spawn: the same argv wrapped in the OS jail when a policy is
   active and this OS can enforce it, else the argv unchanged. Live — resolves the
   session roots on every call, so each fresh child jails to the CURRENT roots."
  [argv]
  (let [pol @active-policy]
    (if (and pol (supported?))
      (case (os-kind)
        :macos
        (into ["sandbox-exec" "-p"
               (macos-profile {:rw (resolve-rw pol) :net-enabled? (boolean (:net-enabled? pol))})]
              argv)

        argv)
      argv)))
