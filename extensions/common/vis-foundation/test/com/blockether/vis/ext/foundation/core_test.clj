(ns com.blockether.vis.ext.foundation.core-test
  (:require
   [clojure.edn :as edn]
   [clojure.java.io :as io]
   [clojure.string :as str]
   [com.blockether.vis.core :as vis]
   [com.blockether.vis.ext.foundation.core :as foundation]
   [lazytest.core :refer [defdescribe expect it]]))

(defn- foundation-manifest-file []
  (let [repo-root-file (io/file "extensions/common/vis-foundation/resources/META-INF/vis-extension/vis.edn")]
    (if (.exists repo-root-file)
      repo-root-file
      (io/file "resources/META-INF/vis-extension/vis.edn"))))

(defdescribe vis-foundation-aggregator-test
  (it "registers the unified v/ alias"
    (expect (= 'v (get-in foundation/vis-extension [:ext/ns-alias :alias])))
    (expect (= 'vis.ext.v (get-in foundation/vis-extension [:ext/ns-alias :ns]))))

  (it "merges markdown builders into the unified symbol surface"
    (let [syms (set (map :ext.symbol/sym (:ext/symbols foundation/vis-extension)))]
      ;; Existing areas still present.
      (expect (contains? syms 'extensions))
      (expect (contains? syms 'cat))
      (expect (contains? syms 'bash))
      (expect (contains? syms 'snapshot))
      ;; Markdown builders now live under the same alias.
      (expect (contains? syms 'h1))
      (expect (contains? syms 'p))
      (expect (contains? syms 'table))
      (expect (contains? syms 'file-link))
      (expect (contains? syms 'join))))

  (it "keeps the unified prompt strategy-only"
    (let [prompt ((:ext/prompt foundation/vis-extension) {})]
      (expect (str/includes? prompt "`v/` strategy"))
      (expect (str/includes? prompt "combine v/rg/v/glob/v/ls"))
      (expect (not (str/includes? prompt "clojure.repl/doc")))
      (expect (not (str/includes? prompt "Do not emit Markdown/text strings")))
      (expect (not (str/includes? prompt "Do not render Markdown as IR")))
      (expect (< (count prompt) 1600))))

  (it "contributes environment info through the dedicated hook"
    (expect (fn? (:ext/environment-info-fn foundation/vis-extension))))

  (it "does not leave a standalone md extension registered"
    (expect (contains? (set (vis/registered-extension-ids)) 'v))
    (expect (not (contains? (set (vis/registered-extension-ids)) 'md))))

  (it "documents markdown builders on the extension descriptor"
    (let [doc (:ext/doc foundation/vis-extension)]
      (expect (str/includes? doc "markdown answer builders"))
      (expect (str/includes? doc "file-link"))))

  (it "ships manifest docs with current envelope syntax only"
    (let [manifest (edn/read-string {:readers {} :default (fn [_ form] form)}
                     (slurp (foundation-manifest-file)))
          readme   (get-in manifest ['v :docs "README.md" :content])]
      (expect (str/includes? readme "[:result :lines]"))
      (expect (str/includes? readme "Never use [:result :lines]"))
      (expect (not (str/includes? readme "[:info :files]")))
      (expect (not (str/includes? readme "v/preview")))))

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
