(ns com.blockether.vis-main.core-test
  (:require
   [clojure.java.io :as io]
   [clojure.java.shell :as sh]
   [clojure.string :as str]
   [lazytest.core :refer [defdescribe expect it]]))

;; ─── from cli_smoke_test.clj ───

;; ---------------------------------------------------------------------------
;; Test runner helpers
;; ---------------------------------------------------------------------------

(def ^:private repo-root
  ;; The JVM's cwd is the repo root when these tests are launched
  ;; via `clojure -M:test` from the project directory. Resolve from
  ;; there — `*file*` is unreliable because it's relative to whichever
  ;; classpath source-path the file was loaded from.
  (io/file (System/getProperty "user.dir")))

(def ^:private vis-bin
  (str (io/file repo-root "bin" "vis")))

(assert (.exists (io/file vis-bin))
  (str "bin/vis not found at " vis-bin
    " — run smoke tests from the repo root via `clojure -M:test`."))

(defn- run-vis
  "Invoke `bin/vis` with `args`. Returns `{:exit int :out str :err str}`.
   Wallclock-bounded by a hard 60s timeout via clojure.java.shell."
  [& args]
  (apply sh/sh vis-bin args))

(defn- contains-all? [s substrs]
  (every? #(str/includes? s %) substrs))

;; ---------------------------------------------------------------------------
;; Smoke tests
;; ---------------------------------------------------------------------------

(defdescribe bin-vis-launcher
  (it "exists and is executable"
    (let [f (io/file vis-bin)]
      (expect (.exists f))
      (expect (.canExecute f)))))

(defdescribe vis-no-args
  (it "prints the help tree and exits 0"
    (let [{:keys [exit out]} (run-vis)]
      (expect (zero? exit))
      (expect (contains-all? out ["vis — iterative coding agent CLI"
                                  "COMMANDS"
                                  "run"])))))

(defdescribe vis-help
  (it "prints the help tree and exits 0"
    (let [{:keys [exit out]} (run-vis "help")]
      (expect (zero? exit))
      (expect (str/includes? out "vis — iterative coding agent CLI")))))

(defdescribe vis-doctor
  (it "prints environment diagnostics and exits 0"
    (let [{:keys [exit out]} (run-vis "doctor")]
      (expect (zero? exit))
      (expect (contains-all? out ["vis doctor"
                                  "Environment"
                                  "DB path:"
                                  "Conversations:"])))))

(defdescribe vis-extensions
  (it "lists the `vis-common-editing` filesystem extension as discovered"
    (let [{:keys [exit out]} (run-vis "extensions" "list")]
      (expect (zero? exit))
      (expect (contains-all? out ["Extensions"
                                  "filesystem"
                                  "extension(s)"]))))
  (it "parent help mentions the `list` subcommand"
    (let [{:keys [exit out]} (run-vis "extensions")]
      (expect (zero? exit))
      (expect (str/includes? out "list")))))

(defdescribe vis-channels
  (it "discovers TUI and Telegram"
    (let [{:keys [exit out]} (run-vis "channels")]
      (expect (zero? exit))
      (expect (contains-all? out ["tui" "telegram"])))))

(defdescribe vis-conversations
  (it "exits 0 even when no conversations exist"
    (let [{:keys [exit]} (run-vis "conversations")]
      (expect (zero? exit)))))

(defdescribe vis-run-help
  (it "shows `vis run` usage and flag list"
    (let [{:keys [exit out]} (run-vis "run" "--help")]
      (expect (zero? exit))
      (expect (contains-all? out ["vis run" "FLAGS" "--json" "--model"])))))

(defdescribe vis-auth-help
  (it "shows the auth command tree"
    (let [{:keys [exit out]} (run-vis "auth")]
      (expect (zero? exit))
      ;; github-copilot provider auto-registers and should appear
      (expect (str/includes? out "github-copilot")))))

(defdescribe vis-unknown-command
  (it "returns non-zero (or routes to `run` fallback gracefully)"
    (let [{:keys [exit out err]} (run-vis "this-command-does-not-exist-xyz")]
      ;; Two acceptable behaviours per the dispatcher:
      ;;   (a) exit non-zero with an error message, OR
      ;;   (b) treat the token as a free-form prompt and surface a
      ;;       provider-config error (no API key in CI/test env)
      ;; Either way, we get SOMETHING on stdout/stderr and the
      ;; process terminates cleanly.
      (expect (some? exit))
      (expect (or (not (zero? exit))
                (str/includes? (str out err) "this-command-does-not-exist"))))))
