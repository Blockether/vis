(ns com.blockether.vis.ext.lang-clojure.repair-test
  (:require
   [babashka.fs :as fs]
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

  (it "returns unchanged + parse error when parinfer can't repair (e.g. odd quotes)"
    ;; Quote rebalance was a parse-diagnose delegation; that ns is gone.
    ;; Parinfer doesn't fix string delimiters, so an odd-quote source is
    ;; now surfaced as a hard parse error and the model self-corrects.
    (let [out (repair/repair-source "(str \"foo\" \"bar\" \")")]
      (expect (false? (:changed? out)))
      (expect (false? (:parseable-after? out)))
      (expect (nil? (:engine out)))
      (expect (string? (:error out))))))

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
      (expect (= "(defn bad [x]\n  (inc x))\n" (get-in out [:metadata :files 0 :after])))
      (expect (string? (get-in out [:result :parse-error-before])))
      (expect (nil? (get-in out [:result :parse-error-after])))
      (expect (false? (get-in out [:result :whole-file-parseable-before?])))
      (expect (true? (get-in out [:result :whole-file-parseable-after?])))
      (expect (= :parinfer (get-in out [:metadata :files 0 :engine])))))

  (it "accepts locator-row :span and supports dry-run (parinfer)"
    ;; Switched fixture from an odd-quote (was: parse-diagnose territory)
    ;; to a missing-close-paren so parinfer is the active engine.
    (let [path      (write-temp! "range/paren.clj" "(ns demo)\n(defn broken [x]\n  (inc x)\n(def ok 1)\n")
          repair-fn (:ext.symbol/fn repair/repair-range-symbol)
          row       {:path path :span [[2 1] [4 1]] :dry-run? true}
          out       (repair-fn row)]
      (expect (true? (:success? out)))
      (expect (= :parinfer (get-in out [:result :engine])))
      (expect (true? (get-in out [:result :changed?])))
      (expect (true? (get-in out [:result :dry-run?])))
      ;; dry-run: file unchanged on disk
      (expect (= "(ns demo)\n(defn broken [x]\n  (inc x)\n(def ok 1)\n" (slurp path)))))

  (it "repairs locator rows through z/repair-locator (parinfer)"
    (let [path      (write-temp! "range/locator.clj" "(ns demo)\n(defn broken [x]\n  (inc x)\n(def ok 1)\n")
          repair-fn (:ext.symbol/fn repair/repair-locator-symbol)
          locator   {:path path :span [[2 1] [4 1]]}
          out       (repair-fn locator {:dry-run? true})]
      (expect (true? (:success? out)))
      (expect (= :parinfer (get-in out [:result :engine])))
      (expect (true? (get-in out [:result :dry-run?])))
      (expect (= :z/repair-locator (:symbol out)))
      (expect (= "(ns demo)\n(defn broken [x]\n  (inc x)\n(def ok 1)\n" (slurp path)))))

  (it "repairs a whole file through z/repair-file"
    (let [path      (write-temp! "file/core.clj" "(ns demo)\n(defn bad [x]\n  (inc x)\n")
          repair-fn (:ext.symbol/fn repair/repair-file-symbol)
          out       (repair-fn path {:dry-run? true})]
      (expect (true? (:success? out)))
      (expect (= :parinfer (get-in out [:result :engine])))
      (expect (true? (get-in out [:result :changed?])))
      (expect (= "(ns demo)\n(defn bad [x]\n  (inc x)\n" (slurp path))))))
