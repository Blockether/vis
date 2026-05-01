(ns com.blockether.vis.ext.foundation.core-test
  (:require
   [clojure.string :as str]
   [com.blockether.vis.core :as vis]
   [com.blockether.vis.ext.foundation.core :as foundation]
   [lazytest.core :refer [defdescribe expect it]]))

(defdescribe vis-foundation-aggregator-test
  (it "registers the unified v/ alias"
    (expect (= 'v (get-in foundation/vis-extension [:ext/ns-alias :alias])))
    (expect (= 'vis.ext.v (get-in foundation/vis-extension [:ext/ns-alias :ns]))))

  (it "merges markdown builders into the unified symbol surface"
    (let [syms (set (map :ext.symbol/sym (:ext/symbols foundation/vis-extension)))]
      ;; Existing areas still present.
      (expect (contains? syms 'extensions))
      (expect (contains? syms 'cat))
      (expect (contains? syms 'snapshot))
      ;; Markdown builders now live under the same alias.
      (expect (contains? syms 'h1))
      (expect (contains? syms 'p))
      (expect (contains? syms 'table))
      (expect (contains? syms 'file-link))
      (expect (contains? syms 'join))))

  (it "includes the markdown prompt fragment in the unified prompt"
    (let [prompt ((:ext/prompt foundation/vis-extension) {})]
      (expect (str/includes? prompt "v/h1"))
      (expect (str/includes? prompt "v/file-link"))
      (expect (str/includes? prompt "v/join"))))

  (it "does not leave a standalone md extension registered"
    (expect (contains? (set (vis/registered-extension-ids)) 'v))
    (expect (not (contains? (set (vis/registered-extension-ids)) 'md))))

  (it "documents markdown builders on the extension descriptor"
    (let [doc (:ext/doc foundation/vis-extension)]
      (expect (str/includes? doc "markdown answer builders"))
      (expect (str/includes? doc "file-link")))))
