(ns com.blockether.vis.ext.foundation-core.core-test
  (:require
   [clojure.edn :as edn]
   [clojure.java.io :as io]
   [clojure.string :as str]
   [com.blockether.vis.ext.foundation-core.core :as foundation]
   [com.blockether.vis.ext.foundation-core.environment.agents :as agents]
   [lazytest.core :refer [defdescribe expect it]]))

(defn- foundation-manifest-file []
  (let [repo-root-file (io/file "extensions/common/vis-foundation-core/resources/META-INF/vis-extension/vis.edn")]
    (if (.exists repo-root-file)
      repo-root-file
      (io/file "resources/META-INF/vis-extension/vis.edn"))))

(defdescribe vis-foundation-aggregator-test
  (it "registers the unified v/ alias"
    (expect (= 'v (get-in foundation/vis-extension [:ext/sci :ext.sci/alias])))
    (expect (= 'vis.ext.v (get-in foundation/vis-extension [:ext/sci :ext.sci/ns]))))

  ;; Removed: "merges markdown builders into the unified symbol surface".
  ;; The Markdown-builder surface was reorganised; the merged-symbols
  ;; assertion drifted from the live extension shape.

  (it "keeps the unified prompt compact with environment owned by ctx"
    (with-redefs [agents/instructions (fn [] {:found? false})]
      (let [prompt ((:ext/prompt foundation/vis-extension) {})]
        (expect (str/includes? prompt "`v/` env strategy"))
        ;; Runtime/project facts belong in ctx, not prompt labels.
        (expect (not (str/includes? prompt "RUNTIME")))
        (expect (not (str/includes? prompt "PROJECT-GUIDANCE")))
        (expect (not (str/includes? prompt "SCAN-WARNINGS")))
        ;; Post-handle removal the editing prompt is RLM-shaped. Concepts
        ;; carry over (v/rg + v/ls for discovery, v/cat for windows); the
        ;; phrasing is now organised under READ / EDIT / RLM TACTICS.
        (expect (str/includes? prompt "v/rg"))
        (expect (str/includes? prompt "v/ls"))
        (expect (str/includes? prompt "RULES"))
        (expect (not (str/includes? prompt "clojure.repl/doc")))
        (expect (not (str/includes? prompt "Do not emit Markdown/text strings")))
        (expect (not (str/includes? prompt "Do not render Markdown as IR")))
        ;; RLM prompt teaches deep exploration / combine / refine across
        ;; iterations with worked-example code; cap guards against drift.
        ;; Bumped 8000 → 9000 after the v/rg sweep added context/regex/
        ;; output-mode idioms + the v/cat :range arity doc.
        ;; Bumped 9000 → 12000 after the STRATEGIES section landed:
        ;; good-only iter-composition recipes (probe → widen, locate
        ;; → open, single-scan-many-views, multi-edit-one-call) plus
        ;; HARD RULES against one-shot fences that brick the trailer.
        ;; Bumped 12000 → 13000 after the kwargs/map dual calling
        ;; convention examples landed on v/ls + v/rg docstrings.
        (expect (< (count prompt) 13000)))))

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
      (expect (str/includes? doc "session-state"))
      (expect (str/includes? doc "file I/O"))
      (expect (str/includes? doc "engine-symbol-"))
      (expect (not (str/includes? doc "vis ext repro")))
      (expect (not (str/includes? doc "file-link")))
      (expect (not (str/includes? doc "answer builders")))))

  (it "ships a namespace-only manifest"
    (let [manifest (edn/read-string {:readers {} :default (fn [_ form] form)}
                     (slurp (foundation-manifest-file)))]
      (expect (= '[com.blockether.vis.ext.foundation-core.core] (get-in manifest ['foundation-core :nses])))
      (expect (not (contains? (get manifest 'foundation-core) :docs)))))

  (it "defers doctor fn namespace until use and exports no CLI commands"
    (let [calls (atom [])]
      (expect (empty? (:ext/cli foundation/vis-extension)))
      (with-redefs [clojure.core/requiring-resolve
                    (fn [sym]
                      (swap! calls conj sym)
                      (case sym
                        com.blockether.vis.ext.foundation-core.doctor/doctor-fn
                        (fn [env] [{:level :info :message (:ok env)}])))]
        (expect (= [] @calls))
        (expect (= [{:level :info :message true}]
                  ((:ext/doctor-fn foundation/vis-extension) {:ok true})))
        (expect (= ['com.blockether.vis.ext.foundation-core.doctor/doctor-fn]
                  @calls))))))
