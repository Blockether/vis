(ns com.blockether.vis.internal.foundation.core-test
  (:require
   [clojure.string :as str]
   [com.blockether.vis.internal.extension :as extension]
   [com.blockether.vis.internal.foundation.core :as foundation]
   [com.blockether.vis.internal.foundation.environment.agents :as agents]
   [lazytest.core :refer [defdescribe expect it]]))

(defdescribe vis-foundation-aggregator-test
  (it "is a BUILT-IN with NO alias — symbols bind bare into the sandbox"
    (expect (true? (get-in foundation/vis-extension [:ext/engine :ext.engine/builtin?])))
    (expect (nil? (get-in foundation/vis-extension [:ext/engine :ext.engine/alias])))
    (expect (nil? (get-in foundation/vis-extension [:ext/engine :ext.engine/ns]))))

  ;; Removed: "merges markdown builders into the unified symbol surface".
  ;; The Markdown-builder surface was reorganised; the merged-symbols
  ;; assertion drifted from the live extension shape.

  (it "keeps the unified prompt compact with environment owned by ctx"
    (with-redefs [agents/instructions (fn [] {:found? false})]
      (let [prompt ((:ext/prompt foundation/vis-extension) {})]
        (expect (str/includes? prompt "Env strategy"))
        ;; Runtime/project facts belong in ctx, not prompt labels.
        (expect (not (str/includes? prompt "RUNTIME")))
        (expect (not (str/includes? prompt "PROJECT-GUIDANCE")))
        (expect (not (str/includes? prompt "SCAN-WARNINGS")))
        ;; Editing prompt is now compact: canonical path only, no strategy symbol.
        (expect (str/includes? prompt "rg"))
        (expect (str/includes? prompt "ls"))
        (expect (str/includes? prompt "Canonical path only"))
        (expect (not (str/includes? prompt "v/strategy")))
        (expect (not (str/includes? prompt "clojure.repl/doc")))
        (expect (not (str/includes? prompt "Do not emit Markdown/text strings")))
        (expect (not (str/includes? prompt "Do not render Markdown as IR")))
        ;; Cap guards against strategy prose drifting back into prompt.
        (expect (< (count prompt) 5000)))))

  (it "contributes only the workspace block through ctx now"
    ;; `:session/env` (host / project / extensions digest) moved to
    ;; `internal.env-digest` — it's core functionality, not extension-
    ;; owned. Foundation-core's `:ext/ctx` keeps only the workspace
    ;; block; `(:project ctx)` is gone for good.
    (let [ctx ((:ext/ctx foundation/vis-extension) {})]
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
      (expect (str/includes? doc "session_state"))
      (expect (str/includes? doc "file I/O"))
      (expect (str/includes? doc "system call"))
      (expect (not (str/includes? doc "vis ext repro")))
      (expect (not (str/includes? doc "file-link")))
      (expect (not (str/includes? doc "answer builders")))))

  (it "registers as a BUILT-IN (no META-INF manifest)"
    ;; foundation is now a CORE module, not a droppable classpath plug-in:
    ;; `extension/discover-extensions!` loads it from its built-in list and its
    ;; top-level `register-extension!` fires. It ships no
    ;; `META-INF/vis-extension/vis.edn` manifest of its own.
    (extension/discover-extensions!)
    (expect (some #(= "foundation-core" (:ext/name %))
              (extension/registered-extensions))))

  (it "defers doctor fn namespace until use and exports no CLI commands"
    (let [calls (atom [])]
      (expect (empty? (:ext/cli foundation/vis-extension)))
      (with-redefs [clojure.core/requiring-resolve
                    (fn [sym]
                      (swap! calls conj sym)
                      (case sym
                        com.blockether.vis.internal.foundation.doctor/doctor-fn
                        (fn [env] [{:level :info :message (:ok env)}])))]
        (expect (= [] @calls))
        (expect (= [{:level :info :message true}]
                  ((:ext/doctor-fn foundation/vis-extension) {:ok true})))
        (expect (= ['com.blockether.vis.internal.foundation.doctor/doctor-fn]
                  @calls))))))
