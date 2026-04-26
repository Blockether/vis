(ns com.blockether.vis.commandline.main
  "CLI dispatcher entry point.

   This namespace is the `vis` binary. It owns nothing about specific
   commands \u2014 every subcommand (built-in or plug-in) registers itself
   into `com.blockether.vis.commandline`'s global registry. `-main`
   composes the registered commands into a root tree, walks it, and
   dispatches.

   Discovery happens through dynaload \u2014 we attempt to load
   `com.blockether.vis.channel`, `com.blockether.vis.extension`, and
   `com.blockether.vis.persistance.core`'s discovery fns IF those jars
   are on the classpath, but never require them at compile time. That
   makes vis-commandline genuinely standalone: dropping just this jar
   gives you a working CLI that prints empty help. Add more jars and
   their `META-INF/vis/commandline.edn` registrations show up\n   automatically."
  (:require [borkdude.dynaload :as dl]
            [clojure.string :as str]
            [com.blockether.vis.commandline :as cmd]))

;; =============================================================================
;; Optional plug-in discovery (dynaloaded \u2014 zero compile-time deps)
;; =============================================================================

(def ^:private discover-extensions!
  (dl/dynaload 'com.blockether.vis.extension/discover-extensions!
    {:default (constantly 0)}))

(def ^:private discover-channels!
  (dl/dynaload 'com.blockether.vis.channel/discover-channels!
    {:default (constantly 0)}))

(def ^:private discover-persistance-backends!
  (dl/dynaload 'com.blockether.vis.persistance.core/discover-backends!
    {:default (constantly 0)}))

(def ^:private discover-providers!
  (dl/dynaload 'com.blockether.vis.provider/discover-providers!
    {:default (constantly 0)}))

(defn discover-all!
  "Run every plug-in surface's classpath autodiscovery. Each call is
   guarded against missing jars via `borkdude.dynaload` defaults so a
   stripped distribution doesn't crash. Returns nil."
  []
  (try (discover-extensions!)            (catch Throwable _ nil))
  (try (discover-channels!)              (catch Throwable _ nil))
  (try (discover-providers!)             (catch Throwable _ nil))
  (try (discover-persistance-backends!)  (catch Throwable _ nil))
  (cmd/discover-commands!)
  nil)

;; =============================================================================
;; Root command
;;
;; The dispatcher's root has NO hard-coded subcommands. Every entry
;; comes from the global commandline registry. Built-ins (run, auth,
;; doctor, \u2026) are registered by vis-core; the `vis channel` and
;; `vis ext` parents are registered by vis-extension. Add a third-
;; party jar with its own `cmd/register-global!` calls and its
;; commands appear here without any code change.
;; =============================================================================

(def ^:private DEFAULT_DOC
  (str/join "\n"
    ["vis \u2014 SCI powered RLM iterative coding harness"
     ""
     "  No tool calls. No message accumulation. No compaction."
     "  The model writes Clojure. A sandboxed SCI interpreter executes it."
     "  Results flow back as a compact journal. State lives in named vars"
     "  and SQLite, not in the token budget. One context message per"
     "  iteration \u2014 constant size, never grows."
     ""
     "  Drop more package jars onto the classpath to extend this CLI;"
     "  every command listed below comes from a registered plug-in."]))

(defn root-command
  "Build the root `vis` command tree. Subcommands are pulled fresh on
   every call so newly registered plug-ins show up immediately."
  []
  (cmd/command
    {:cmd/name        "vis"
     :cmd/doc         DEFAULT_DOC
     :cmd/subcommands #(cmd/registered-under [])}))

;; =============================================================================
;; Pre-redirect stderr for TTY-owning channels
;;
;; Some leaves (TUI, ncurses) take over the controlling terminal and
;; need stderr re-routed to a log file BEFORE any further class loading
;; triggers JVM warnings. The check is data-driven via
;; `:cmd/owns-tty?`. Channels mark themselves through the channel
;; bridge in vis-extension; nothing here is channel-aware.
;; =============================================================================

(defn- pre-redirect-stderr! [args]
  (when-let [{:keys [command]} (cmd/find-leaf (root-command) (cons "vis" args))]
    (when (:cmd/owns-tty? command)
      (let [log-dir (java.io.File. (str (System/getProperty "user.home") "/.vis"))]
        (when-not (.exists log-dir) (.mkdirs log-dir))
        (System/setErr (java.io.PrintStream.
                         (java.io.FileOutputStream.
                           (str log-dir "/vis.log") true) true))))))

;; =============================================================================
;; Main
;; =============================================================================

(defn- unknown-command?
  "True when the user typed something the tree doesn't recognize.
   Detected by walking the tree: if `find-leaf` resolves only to the
   ROOT (path length 1) AND there's a residual that isn't a help
   request, the user gave us an unknown command."
  [root args]
  (when (seq args)
    (let [{:keys [path residual]} (cmd/find-leaf root (cons (:cmd/name root) args))]
      (and (= 1 (count path))
        (seq residual)
        (not-any? #{"--help" "-h"} residual)))))

(defn -main
  "Discover plug-ins, walk the command tree, dispatch.

   Behavior:
     - No args                → top-level help
     - `--help` / `-h`        → help for the resolved command
     - Recognized command     → invoke its `:cmd/run-fn`
     - Unknown command        → top-level help + exit 1

   No magical fallback to a `run`-as-prompt shortcut — the dispatcher
   is a pure command tree. Anyone who wants the old single-arg
   ergonomics can register a `:cmd/run-fn` on the root via a custom
   plug-in."
  [& args]
  (discover-all!)
  (pre-redirect-stderr! args)
  (let [root      (root-command)
        full-args (cons "vis" args)]
    (cond
      (empty? args)
      (println (cmd/render-tree root))

      (unknown-command? root args)
      (do (println (cmd/render-tree root))
        (println)
        (println (str "Unknown command: " (str/join " " args)))
        (System/exit 1))

      :else
      (cmd/dispatch! root full-args))))
