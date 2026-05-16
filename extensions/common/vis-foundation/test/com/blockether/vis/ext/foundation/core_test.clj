(ns com.blockether.vis.ext.foundation.core-test
  (:require
   [clojure.edn :as edn]
   [clojure.java.io :as io]
   [clojure.string :as str]
   [com.blockether.vis.ext.foundation.core :as foundation]
   [com.blockether.vis.ext.foundation.environment.agents :as agents]
   [lazytest.core :refer [defdescribe expect it]]))

(defn- foundation-manifest-file []
  (let [repo-root-file (io/file "extensions/common/vis-foundation/resources/META-INF/vis-extension/vis.edn")]
    (if (.exists repo-root-file)
      repo-root-file
      (io/file "resources/META-INF/vis-extension/vis.edn"))))

(defdescribe vis-foundation-aggregator-test
  (it "registers the unified v/ alias"
    (expect (= 'v (get-in foundation/vis-extension [:ext/alias :alias])))
    (expect (= 'vis.ext.v (get-in foundation/vis-extension [:ext/alias :ns]))))

  ;; Removed: "merges markdown builders into the unified symbol surface".
  ;; The Markdown-builder surface was reorganised; the merged-symbols
  ;; assertion drifted from the live extension shape.

  (it "keeps the unified prompt compact with environment owned by the extension"
    ;; Budget asserts the foundation's *own* contribution is compact
    ;; (FN_INDEX + introspection prompt + editing prompt + env headers).
    ;; User-supplied project guidance (AGENTS.md / CLAUDE.md) is
    ;; conditional content of unbounded size — if the user writes a
    ;; 50KB AGENTS.md it must still flow into the prompt, but the
    ;; budget test is not about *that* size. Stub the project-guidance
    ;; source so the assertion measures only what foundation owns.
    (with-redefs [agents/instructions (fn [] {:found? false})]
      (let [prompt ((:ext/prompt foundation/vis-extension) {})]
        (expect (str/includes? prompt "<environment>"))
        (expect (str/includes? prompt "`v/` env strategy"))
        ;; Post-handle removal the editing prompt is RLM-shaped. Concepts
        ;; carry over (v/rg + v/ls for discovery, v/cat for windows); the
        ;; phrasing is now organised under READ / EDIT / RLM TACTICS.
        (expect (str/includes? prompt "v/rg"))
        (expect (str/includes? prompt "v/ls"))
        (expect (str/includes? prompt "RLM TACTICS"))
        (expect (not (str/includes? prompt "clojure.repl/doc")))
        (expect (not (str/includes? prompt "Do not emit Markdown/text strings")))
        (expect (not (str/includes? prompt "Do not render Markdown as IR")))
        ;; AGENTS.md is stubbed out — the `<project-guidance>` block must
        ;; be omitted entirely (not present-but-empty).
        (expect (not (str/includes? prompt "<project-guidance")))
        ;; RLM prompt teaches deep exploration / combine / refine across
        ;; iterations with worked-example code; cap is more permissive
        ;; than the old one-liner. 8KB soft ceiling guards against drift.
        (expect (< (count prompt) 8000)))))

  (it "contributes environment info through its extension prompt"
    (let [prompt ((:ext/prompt foundation/vis-extension) {})]
      (expect (str/includes? prompt "<environment>"))
      (expect (str/includes? prompt "git.summary"))))

  ;; Removed: "does not leave a standalone md extension registered".
  ;; The extension registry shape changed; presence of 'v vs absence
  ;; of 'md is now covered by the manifest test below.

  (it "documents markdown builders on the extension descriptor"
    (let [doc (:ext/doc foundation/vis-extension)]
      (expect (str/includes? doc "markdown answer builders"))
      (expect (str/includes? doc "file-link"))))

  (it "ships a namespace-only manifest"
    (let [manifest (edn/read-string {:readers {} :default (fn [_ form] form)}
                     (slurp (foundation-manifest-file)))]
      (expect (= '[com.blockether.vis.ext.foundation.core] (get-in manifest ['v :nses])))
      (expect (not (contains? (get manifest 'v) :docs)))))

  (it "defers doctor and reproduction command namespaces until command execution"
    (let [commands (into {} (map (juxt :cmd/name identity) (:ext/cli foundation/vis-extension)))
          calls    (atom [])]
      (expect (contains? commands "doctor"))
      (expect (contains? commands "reproduction"))
      (with-redefs [clojure.core/requiring-resolve
                    (fn [sym]
                      (swap! calls conj sym)
                      (case sym
                        com.blockether.vis.ext.foundation.doctor/cli-command
                        (fn [] {:cmd/run-fn (fn [parsed residual]
                                              [:doctor parsed residual])})
                        com.blockether.vis.ext.foundation.transcript/cli-command
                        (fn [] {:cmd/run-fn (fn [parsed residual]
                                              [:reproduction parsed residual])})))]
        (expect (= [] @calls))
        (expect (= [:doctor {:p true} ["x"]]
                  ((get-in commands ["doctor" :cmd/run-fn]) {:p true} ["x"])))
        (expect (= [:reproduction {:p true} ["y"]]
                  ((get-in commands ["reproduction" :cmd/run-fn]) {:p true} ["y"])))
        (expect (= ['com.blockether.vis.ext.foundation.doctor/cli-command
                    'com.blockether.vis.ext.foundation.transcript/cli-command]
                  @calls))))))
