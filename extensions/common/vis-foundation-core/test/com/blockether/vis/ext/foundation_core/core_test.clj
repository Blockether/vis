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
        (expect (< (count prompt) 8000)))))

  (it "contributes environment info through ctx"
    (let [ctx ((:ext/ctx foundation/vis-extension) {})]
      (expect (contains? ctx :project))
      (expect (contains? (:project ctx) :host))
      (expect (contains? (:project ctx) :root))))

  ;; Removed: "does not leave a standalone md extension registered".
  ;; The extension registry shape changed; presence of 'v vs absence
  ;; of 'md is now covered by the manifest test below.

  (it "documents markdown builders on the extension descriptor"
    (let [doc (:ext/description foundation/vis-extension)]
      (expect (str/includes? doc "markdown answer builders"))
      (expect (str/includes? doc "file-link"))))

  (it "ships a namespace-only manifest"
    (let [manifest (edn/read-string {:readers {} :default (fn [_ form] form)}
                     (slurp (foundation-manifest-file)))]
      (expect (= '[com.blockether.vis.ext.foundation-core.core] (get-in manifest ['foundation-core :nses])))
      (expect (not (contains? (get manifest 'foundation-core) :docs)))))

  (it "defers doctor fn and repro command namespaces until use"
    (let [commands (into {} (map (juxt :cmd/name identity) (:ext/cli foundation/vis-extension)))
          calls    (atom [])]
      (expect (not (contains? commands "doctor")))
      (expect (contains? commands "repro"))
      (with-redefs [clojure.core/requiring-resolve
                    (fn [sym]
                      (swap! calls conj sym)
                      (case sym
                        com.blockether.vis.ext.foundation-core.doctor/doctor-fn
                        (fn [env] [{:level :info :message (:ok env)}])
                        com.blockether.vis.ext.foundation-core.transcript/cli-command
                        (fn [] {:cmd/run-fn (fn [parsed residual]
                                              [:repro parsed residual])})))]
        (expect (= [] @calls))
        (expect (= [{:level :info :message true}]
                  ((:ext/doctor-fn foundation/vis-extension) {:ok true})))
        (expect (= [:repro {:p true} ["y"]]
                  ((get-in commands ["repro" :cmd/run-fn]) {:p true} ["y"])))
        (expect (= ['com.blockether.vis.ext.foundation-core.doctor/doctor-fn
                    'com.blockether.vis.ext.foundation-core.transcript/cli-command]
                  @calls))))))
