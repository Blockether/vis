(ns com.blockether.vis.internal.foundation.core-test
  (:require [clojure.string :as str]
            [com.blockether.vis.internal.extension :as extension]
            [com.blockether.vis.internal.foundation.core :as foundation]
            [com.blockether.vis.internal.foundation.environment.agents :as agents]
            [com.blockether.vis.internal.foundation.introspection :as introspection]
            [com.blockether.vis.internal.foundation.rewind :as rewind]
            [com.blockether.vis.internal.foundation.shell :as shell]
            [com.blockether.vis.internal.manifest :as manifest]
            [lazytest.core :refer [defdescribe expect it]]))

(defdescribe
  vis-foundation-aggregator-test
  (it "is a BUILT-IN with NO alias — symbols bind bare into the sandbox"
      (expect (true? (get-in foundation/vis-extension [:ext/engine :ext.engine/builtin?])))
      (expect (nil? (get-in foundation/vis-extension [:ext/engine :ext.engine/alias])))
      (expect (nil? (get-in foundation/vis-extension [:ext/engine :ext.engine/ns])))
      (let [symbols (set (map :ext.symbol/symbol
                              (get-in foundation/vis-extension [:ext/engine :ext.engine/symbols])))]
        (expect (every? symbols
                        ['format_code 'lint_code 'run_tests 'repl_eval 'repl_start 'repl_status
                         'repl_stop 'read-session 'get-session 'list-sessions 'shell '_shell-logs
                         '_shell-wait '_shell-type '_shell-stop]))))
  ;; Removed: "merges markdown builders into the unified symbol surface".
  ;; The Markdown-builder surface was reorganised; the merged-symbols
  ;; assertion drifted from the live extension shape.
  (it "keeps only dynamic language routing in the foundation prompt"
      (with-redefs [agents/instructions (fn []
                                          {:found? false})]
        (let [prompt ((:ext/prompt-fn foundation/vis-extension) {})]
          ;; Stable state/introspection/self-doc contracts belong in CORE or tool docs.
          (expect (not (str/includes? prompt "Env strategy")))
          (expect (not (str/includes? prompt "Cross-conversation introspection")))
          (expect (not (str/includes? prompt "Vis self-docs")))
          (expect (not (str/includes? prompt "RUNTIME")))
          (expect (not (str/includes? prompt "PROJECT-GUIDANCE")))
          (expect (not (str/includes? prompt "SCAN-WARNINGS")))
          ;; Editing routes live in native descriptions; this fragment is language-only.
          (expect (or (str/blank? prompt) (str/includes? prompt "LANGUAGE TOOLS")))
          (expect (not (str/includes? prompt "EDITING ROUTES")))
          (expect (not (str/includes? prompt "Canonical path only")))
          (expect (not (str/includes? prompt "v/strategy")))
          (expect (not (str/includes? prompt "clojure.repl/doc")))
          (expect (not (str/includes? prompt "Do not emit Markdown/text strings")))
          (expect (not (str/includes? prompt "Do not render Markdown as IR")))
          (expect (< (count prompt) 1000)))))
  (it "contributes only the workspace block through ctx now"
      ;; `:session/env` (host / project / extensions digest) moved to
      ;; `internal.env-digest` — it's core functionality, not extension-
      ;; owned. Foundation-core's `:ext/ctx-fn` keeps only the workspace
      ;; block; `(:project ctx)` is gone for good.
      (let [ctx ((:ext/ctx-fn foundation/vis-extension) {})]
        (expect (not (contains? ctx :project)))
        (expect (not (contains? ctx :session/env)))))
  ;; Removed: "does not leave a standalone md extension registered".
  ;; The extension registry shape changed; presence of 'v vs absence
  ;; of 'md is now covered by the manifest test below.
  (it "documents the kernel surface on the extension descriptor"
      ;; Regression: prior copy advertised a v/ markdown DSL
      ;; (h1/p/table/file-link/join/code-block) that was torn out in
      ;; commit 40da53d0 (\"demo: tear out v/ markdown DSL\"). Description
      ;; now lists the symbols that actually exist; do NOT let the old
      ;; DSL names creep back into the descriptor copy.
      (let [doc (:ext/description foundation/vis-extension)]
        (expect (str/includes? doc "toggle-gated shell and session introspection"))
        (expect (str/includes? doc "rewind"))
        (expect (str/includes? doc "language facade"))
        (expect (str/includes? doc "file editing"))
        (expect (str/includes? doc "session workspace/VCS"))
        (expect (not (str/includes? doc "ext repro")))
        (expect (not (str/includes? doc "file-link")))
        (expect (not (str/includes? doc "answer builders")))))
  (it "is the manifest's single initializer for core facilities"
      (let [initialization (set (manifest/initializers))]
        (expect (contains? initialization 'com.blockether.vis.internal.foundation.core/register!))
        (doseq [removed ['com.blockether.vis.internal.foundation.introspection/register!
                         'com.blockether.vis.internal.foundation.shell/register!
                         'com.blockether.vis.internal.foundation.rewind/register!]]
          (expect (not (contains? initialization removed)))))
      (foundation/register!)
      (expect (some #(= "foundation-core" (:ext/name %)) (extension/registered-extensions))))
  (it "owns rewind, shell CLI, and the toggle-gated symbol groups"
      (expect (= rewind/op-hooks (:ext/op-hooks foundation/vis-extension)))
      (expect (some #(= "rewind" (:slash/name %)) (:ext/slash-commands foundation/vis-extension)))
      (expect (= ["shell"] (mapv :cmd/name (:ext/cli foundation/vis-extension))))
      (expect (every? (set (get-in foundation/vis-extension [:ext/engine :ext.engine/symbols]))
                      (concat introspection/all-symbols shell/shell-symbols)))
      (let [routes (get-in foundation/vis-extension
                           [:ext/channel-contributions :gateway.slot/http-routes])]
        (expect (= [:rewind/http] (mapv :id routes)))
        (expect (= rewind/routes-contribution (:fn (first routes)))))
      (let [checks ((:ext/doctor-fn foundation/vis-extension) {})]
        (expect (sequential? checks))
        (expect (every? :level checks)))))

(defn- env-with-langs
  [langtools]
  ;; env whose ACTIVE extensions register these :ext/language-tools (drives both
  ;; the capability matrix in the prompt and :session/language-tools in ctx).
  {:active-extensions (atom [{:ext/language-tools langtools}])})

(def ^:private py-pack [{:language "python" :repl-eval-fn identity :start-repl-fn identity}])

(def ^:private clj-pack
  [{:language "clojure"
    :format-fn identity
    :test-fn identity
    :repl-eval-fn identity
    :start-repl-fn identity}])

(defdescribe
  repl-capability-in-core-prompt-test
  "GATE: the REPL/language capabilities are REALLY in the (turn-scoped) system
   prompt, ARE in ctx, and CHANGE when a pack activates next turn."
  (it "the foundation system prompt advertises an active pack's repl_eval"
      (let [p ((:ext/prompt-fn foundation/vis-extension) (env-with-langs py-pack))]
        (expect (str/includes? p "LANGUAGE TOOLS"))
        (expect (str/includes? p "python : repl_eval"))
        ;; Stable workflow lives in CORE; this dynamic block is capabilities only.
        (expect (not (str/includes? p "session[\"resources\"]")))))
  (it "ACTIVATION-SENSITIVE: a pack's verbs appear only when its pack is active"
      (let [with-py
            ((:ext/prompt-fn foundation/vis-extension) (env-with-langs py-pack))

            without
            ((:ext/prompt-fn foundation/vis-extension) (env-with-langs []))]

        (expect (str/includes? with-py "python : repl_eval"))
        (expect (not (str/includes? without "python : repl_eval")))))
  (it "ctx surfaces \"session_language_tools\", recomputed each turn from activation"
      (let [ctx ((:ext/ctx-fn foundation/vis-extension) (env-with-langs clj-pack))]
        (expect (= ["format_code" "run_tests" "repl_eval" "repl_start"]
                   (get-in ctx ["session_language_tools" "clojure"])))))
  (it "ctx GAINS a language the turn its pack activates, drops it when it deactivates"
      (let [active
            ((:ext/ctx-fn foundation/vis-extension) (env-with-langs py-pack))

            inactive
            ((:ext/ctx-fn foundation/vis-extension) (env-with-langs []))]

        (expect (= ["repl_eval" "repl_start"] (get-in active ["session_language_tools" "python"])))
        (expect (nil? (get inactive "session_language_tools"))))))
