(ns com.blockether.vis.ext.lang-clojure.repair-test
  (:require
   [babashka.fs :as fs]
   [clojure.string :as str]
   [com.blockether.vis.ext.lang-clojure.repair :as repair]
   [lazytest.core :refer [defdescribe expect it]]))

(defn- temp-root []
  (doto (fs/file "target/lang-clojure-repair-test")
    fs/delete-tree
    fs/create-dirs))

(defn- write-temp! [rel content]
  (let [f (fs/file (temp-root) rel)]
    (fs/create-dirs (fs/parent f))
    (spit f content)
    (str f)))

(defdescribe repair-source-test
  (it "reuses parinfer for delimiter rebalance"
    (let [out (repair/repair-source "(defn bad [x]\n  (inc x)\n")]
      (expect (true? (:changed? out)))
      (expect (= :parinfer (:engine out)))
      (expect (= "(defn bad [x]\n  (inc x))\n" (:source out)))
      (expect (true? (:parseable-after? out)))))

  (it "reuses parse-diagnose quote rebalance"
    (let [out (repair/repair-source "(str \"foo\" \"bar\" \")")]
      (expect (true? (:changed? out)))
      (expect (= :quote (:engine out)))
      (expect (true? (:parseable-after? out)))
      (expect (= :unbalanced-quote (get-in out [:diagnostic :reason]))))))

(defdescribe repair-range-tool-test
  (it "repairs only the selected range and writes by default"
    (let [path      (write-temp! "range/core.clj" "(ns demo)\n(defn bad [x]\n  (inc x)\n\n(def ok 1)\n")
          repair-fn (:ext.symbol/fn repair/repair-range-symbol)
          out       (repair-fn {:path path :range [[2 1] [4 1]]})]
      (expect (true? (:success? out)))
      (expect (= :parinfer (get-in out [:result :engine])))
      (expect (true? (get-in out [:result :changed?])))
      (expect (= "(ns demo)\n(defn bad [x]\n  (inc x))\n\n(def ok 1)\n"
                (slurp path)))
      (expect (= "(defn bad [x]\n  (inc x)\n" (get-in out [:metadata :files 0 :before])))
      (expect (= "(defn bad [x]\n  (inc x))\n" (get-in out [:metadata :files 0 :after])))))

  (it "accepts locator-row :span and supports dry-run"
    (let [path      (write-temp! "range/quote.clj" "(ns demo)\n(def broken (str \"foo\" \"bar\" \"))\n(def ok 1)\n")
          repair-fn (:ext.symbol/fn repair/repair-range-symbol)
          row       {:path path :span [[2 13] [2 32]] :dry-run? true}
          out       (repair-fn row)]
      (expect (true? (:success? out)))
      (expect (= :quote (get-in out [:result :engine])))
      (expect (true? (get-in out [:result :changed?])))
      (expect (true? (get-in out [:result :dry-run?])))
      (expect (= "(ns demo)\n(def broken (str \"foo\" \"bar\" \"))\n(def ok 1)\n" (slurp path)))
      (expect (str/includes? (get-in out [:metadata :files 0 :after]) "(str"))))

  (it "repairs locator rows through z/repair-locator"
    (let [path      (write-temp! "range/locator.clj" "(ns demo)\n(def broken (str \"foo\" \"bar\" \"))\n(def ok 1)\n")
          repair-fn (:ext.symbol/fn repair/repair-locator-symbol)
          locator   {:path path :span [[2 13] [2 32]]}
          out       (repair-fn locator {:dry-run? true})]
      (expect (true? (:success? out)))
      (expect (= :quote (get-in out [:result :engine])))
      (expect (true? (get-in out [:result :dry-run?])))
      (expect (= :z/repair-locator (:symbol out)))
      (expect (= "(ns demo)\n(def broken (str \"foo\" \"bar\" \"))\n(def ok 1)\n" (slurp path)))))

  (it "repairs a whole file through z/repair-file"
    (let [path      (write-temp! "file/core.clj" "(ns demo)\n(defn bad [x]\n  (inc x)\n")
          repair-fn (:ext.symbol/fn repair/repair-file-symbol)
          out       (repair-fn path {:dry-run? true})]
      (expect (true? (:success? out)))
      (expect (= :parinfer (get-in out [:result :engine])))
      (expect (true? (get-in out [:result :changed?])))
      (expect (= "(ns demo)\n(defn bad [x]\n  (inc x)\n" (slurp path))))))
