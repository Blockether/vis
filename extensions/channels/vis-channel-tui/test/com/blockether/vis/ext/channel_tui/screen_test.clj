(ns com.blockether.vis.ext.channel-tui.screen-test
  "Tests for the TUI channel entry point. The bulk of the namespace
   is Lanterna-bound and exercised by the integration smoke + render
   benchmark; this suite focuses on the pure helpers — currently the
   `--conversation-id` / `--resume` argument parser, where a silent
   accept of unknown flags previously masked typos like
   `--conversations-id`."
  (:require
   [com.blockether.vis.ext.channel-tui.input :as input]
   [com.blockether.vis.ext.channel-tui.screen :as screen]
   [lazytest.core :refer [defdescribe it expect]]))

(def ^:private parse-args
  (deref #'screen/parse-args))

(def ^:private current-hint
  (deref #'screen/current-hint))

(defn- user-error?
  "True when `f` throws an ex-info carrying the `:vis/user-error` flag —
   the contract the channel entry point relies on to print a clean
   `vis: <msg>` line and exit 2 instead of a Java stack trace."
  [f]
  (try (f) false
    (catch clojure.lang.ExceptionInfo e
      (true? (:vis/user-error (ex-data e))))))

(defdescribe hint-test
  (it "empty input advertises arrow-key history instead of removed Ctrl+P/N chords"
    (let [hint (current-hint {:input (input/empty-input)})]
      (expect (re-find #"↑↓ history" hint))
      (expect (not (re-find #"Ctrl\+P/N" hint))))))

(defdescribe parse-args-test
  (it "no args -> empty opts map"
    (expect (= {} (parse-args []))))

  (it "--resume sets :resume true"
    (expect (= {:resume true} (parse-args ["--resume"]))))

  (it "--conversation-id captures the next token as the id"
    (expect (= {:conversation-id "abc123"}
              (parse-args ["--conversation-id" "abc123"]))))

  (it "--conversation-id + --resume coexist (caller decides precedence)"
    (expect (= {:conversation-id "abc123" :resume true}
              (parse-args ["--conversation-id" "abc123" "--resume"]))))

  (it "unknown flag throws :vis/user-error (regression: typo silently swallowed)"
    ;; `vis channels tui --conversations-id <uuid>` used to succeed
    ;; silently and start a fresh conversation. The user reported it
    ;; explicitly: the flag with a stray "s" must blow up.
    (expect (user-error?
              #(parse-args ["--conversations-id" "d8aff512-d60d-42b6-a009-041f1bec3891"]))))

  (it "unknown flag error message names the bad flag and shows usage"
    (try (parse-args ["--conversations-id" "x"])
      (expect false "expected ex-info")
      (catch clojure.lang.ExceptionInfo e
        (let [msg (.getMessage e)]
          (expect (re-find #"--conversations-id" msg))
          (expect (re-find #"Usage:" msg))))))

  (it "--conversation-id without a value -> :vis/user-error"
    (expect (user-error? #(parse-args ["--conversation-id"]))))

  (it "--conversation-id followed by another flag -> :vis/user-error (no value)"
    ;; Catches the case where the user types `--conversation-id --resume`
    ;; and `--resume` would otherwise be silently treated as the id.
    (expect (user-error? #(parse-args ["--conversation-id" "--resume"]))))

  (it "non-flag positional arg also errors (no positional API today)"
    (expect (user-error? #(parse-args ["stray-positional"])))))
