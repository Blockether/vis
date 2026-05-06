(ns com.blockether.vis.ext.lang-clojure.patch-test
  (:require
   [babashka.fs :as fs]
   [clojure.string :as str]
   [com.blockether.vis.ext.lang-clojure.patch :as patch]
   [lazytest.core :refer [defdescribe expect it throws?]]))

(defn- private-fn [name]
  (deref (resolve (symbol "com.blockether.vis.ext.lang-clojure.patch" name))))

(defn- temp-root []
  (doto (fs/file "target/lang-clojure-patch-test")
    fs/create-dirs))

(defn- write-temp! [rel content]
  (let [f (fs/file (temp-root) rel)]
    (fs/create-dirs (fs/parent f))
    (spit f content)
    (str f)))

(defdescribe zpatch-surface-test
  (it "exposes only z/patch-shaped guidance"
    (expect (str/includes? patch/z-prompt "`z/` Clojure/EDN zipper patching"))
    (expect (str/includes? patch/z-prompt "(z/patch {:path p :search locator :replace replacement})"))
    (expect (str/includes? patch/z-prompt "Same map shape as v/patch"))
    (expect (str/includes? patch/z-prompt "z/locators"))
    (expect (str/includes? patch/z-prompt "z/symbols"))
    (expect (str/includes? patch/z-prompt "z/locator-for-symbol"))
    (expect (str/includes? patch/z-prompt "z/subedit->"))
    (expect (not (str/includes? patch/z-prompt "z/zedit")))
    (expect (< (count patch/z-prompt) 1000))
    (expect (= 'patch (:ext.symbol/sym patch/patch-symbol)))
    (expect (str/includes? (:ext.symbol/doc patch/patch-symbol)
              "Same input shape as v/patch"))))

(defdescribe zpatch-behavior-test
  (it "patch replaces the form found by a zipper locator when search is unique"
    (let [path     (write-temp! "patch/core.clj" "(ns demo)\n(def x 1)\n")
          patch-fn (private-fn "patch-safe")]
      (expect (= [{:path path
                   :before "(ns demo)\n(def x 1)\n"
                   :after "(ns demo)\n(def x 2)\n"}]
                (mapv #(select-keys % [:path :before :after])
                  (patch-fn [{:path path :search "(def x 1)" :replace "(def x 2)"}]))))
      (expect (= "(ns demo)\n(def x 2)\n" (slurp path)))
      (expect (throws? clojure.lang.ExceptionInfo
                #(patch-fn [{:path path :search "missing" :replace "x"}])))
      (spit path "dup\ndup\n")
      (expect (throws? clojure.lang.ExceptionInfo
                #(patch-fn [{:path path :search "dup" :replace "x"}])))))

  (it "patches a nested symbol locator without disturbing surrounding source"
    (let [path     (write-temp! "patch/symbol.clj" "(ns demo)\n;; keep me\n(defn f []\n  old-sym)\n")
          patch-fn (private-fn "patch-safe")]
      (patch-fn [{:path path :search "old-sym" :replace "new-sym"}])
      (expect (= "(ns demo)\n;; keep me\n(defn f []\n  new-sym)\n" (slurp path)))))

  (it "supports non-string locator and replacement values"
    (let [path     (write-temp! "patch/value.clj" "(ns demo)\n(defn f [] old-value)\n")
          patch-fn (private-fn "patch-safe")]
      (patch-fn {:path path :search 'old-value :replace 'new-value})
      (expect (= "(ns demo)\n(defn f [] new-value)\n" (slurp path)))))

  (it "accepts the single-map v/patch shape"
    (let [path     (write-temp! "patch/single.clj" "(ns demo)\n(def y 1)\n")
          patch-fn (private-fn "patch-safe")]
      (expect (= [{:path path
                   :before "(ns demo)\n(def y 1)\n"
                   :after "(ns demo)\n(def y 3)\n"}]
                (mapv #(select-keys % [:path :before :after])
                  (patch-fn {:path path :search "(def y 1)" :replace "(def y 3)"}))))
      (expect (= "(ns demo)\n(def y 3)\n" (slurp path)))))

  (it "applies multiple edits to the same file before writing"
    (let [path     (write-temp! "patch/multi.clj" "(ns demo)\n(def a 1)\n(def b 2)\n")
          patch-fn (private-fn "patch-safe")]
      (patch-fn [{:path path :search "(def a 1)" :replace "(def a 10)"}
                 {:path path :search "(def b 2)" :replace "(def b 20)"}])
      (expect (= "(ns demo)\n(def a 10)\n(def b 20)\n" (slurp path)))))

  (it "z/locators and z/symbols return bounded agent-usable zipper locator rows"
    (let [path        (write-temp! "patch/locators.clj" "(ns demo)\n(defn f [] old-sym :k \"s\")\n")
          locators-fn (:ext.symbol/fn patch/locators-symbol)
          symbols-fn  (:ext.symbol/fn patch/symbols-symbol)
          locators    (locators-fn path {:limit 100})
          symbols     (symbols-fn path {:limit 100})
          old-row     (first (filter #(= 'old-sym (:value %)) (:result locators)))]
      (expect (true? (:success? locators)))
      (expect (true? (:success? symbols)))
      (expect (every? #(= path (:path %)) (:result locators)))
      (expect (every? #(integer? (:index %)) (:result locators)))
      (expect (every? #(= #{:path :index :tag :value :locator :source :span}
                         (set (keys %)))
                (:result locators)))
      (expect (= {:path path
                  :tag :token
                  :value 'old-sym
                  :locator "old-sym"
                  :source "old-sym"
                  :span [[2 12] [2 19]]}
                (dissoc old-row :index)))
      (expect (every? #(symbol? (:value %)) (:result symbols)))
      (expect (every? #(= path (:path %)) (:result symbols)))
      (expect (some #(= 'old-sym (:value %)) (:result symbols)))))

  (it "z/locators defaults to 10 rows and supports focused symbol/source lookup"
    (let [path          (write-temp! "patch/locators-focused.clj"
                          (str "(ns demo)\n"
                            (str/join "\n" (map #(str "(def sym" % " " % ")") (range 20)))
                            "\n(defn needle [] target-sym)\n"))
          locators-fn   (:ext.symbol/fn patch/locators-symbol)
          symbols-fn    (:ext.symbol/fn patch/symbols-symbol)
          by-default    (locators-fn path)
          by-symbol     (locators-fn path {:symbol 'target-sym})
          by-source     (locators-fn path {:source-contains "needle" :limit 20})
          symbol-filter (symbols-fn path {:name 'target-sym})]
      (expect (= 10 (count (:result by-default))))
      (expect (true? (get-in by-default [:provenance :truncated?])))
      (expect (= ['target-sym] (mapv :value (:result by-symbol))))
      (expect (some #(str/includes? (:source %) "needle") (:result by-source)))
      (expect (= ['target-sym] (mapv :value (:result symbol-filter))))))

  (it "z/locator-for-symbol returns one symbol row without dumping the namespace"
    (let [path       (write-temp! "patch/locator-for-symbol.clj" "(ns demo)\n(defn f [] target-sym)\n")
          locator-fn (:ext.symbol/fn patch/locator-for-symbol-symbol)
          out        (locator-fn path 'target-sym)]
      (expect (true? (:success? out)))
      (expect (= :z/locator-for-symbol (get-in out [:provenance :op])))
      (expect (= 'target-sym (get-in out [:result :value])))))

  (it "accepts locator rows from z/symbols as span-specific search locators"
    (let [path        (write-temp! "patch/locator-row.clj" "(ns demo)\n(def a old-sym)\n(def b old-sym)\n")
          patch-fn    (private-fn "patch-safe")
          symbols-fn  (:ext.symbol/fn patch/symbols-symbol)
          old-symbols (filterv #(= 'old-sym (:value %)) (:result (symbols-fn path)))
          second-old  (second old-symbols)]
      (expect (= 2 (count old-symbols)))
      (patch-fn {:path path :search second-old :replace 'new-sym})
      (expect (= "(ns demo)\n(def a old-sym)\n(def b new-sym)\n" (slurp path)))))

  (it "applies span-specific locator row edits, including reader-anonymous fn forms"
    (let [path        (write-temp! "patch/locator-row-multi.clj" "(ns demo)\n(def a old-sym)\n(def b old-sym)\n")
          patch-fn    (private-fn "patch-safe")
          symbols-fn  (:ext.symbol/fn patch/symbols-symbol)
          old-symbols (filterv #(= 'old-sym (:value %)) (:result (symbols-fn path)))]
      (expect (= 2 (count old-symbols)))
      (patch-fn [(assoc (first old-symbols) :replace 'new-a)
                 (assoc (second old-symbols) :replace 'new-b)])
      (expect (= "(ns demo)\n(def a new-a)\n(def b new-b)\n" (slurp path))))
    (let [path        (write-temp! "patch/locator-row-anon-fn.clj"
                        "(ns demo)\n(defn f [xs] (mapv #(inc %) xs))\n")
          patch-fn    (private-fn "patch-safe")
          locators-fn (:ext.symbol/fn patch/locators-symbol)
          row         (->> (:result (locators-fn path {:source-contains "mapv" :limit 10}))
                        (filter #(and (= :list (:tag %))
                                   (str/includes? (:source %) "#(inc %)")))
                        first)]
      (patch-fn {:path path
                 :search row
                 :replace "(mapv (fn [x] (inc x)) xs)"})
      (expect (str/includes? (slurp path) "(mapv (fn [x] (inc x)) xs)"))))

  (it "accepts locator rows as replacements via their source"
    (let [path        (write-temp! "patch/locator-replace.clj" "(ns demo)\n(def a source-sym)\n(def b target-sym)\n")
          patch-fn    (private-fn "patch-safe")
          symbols-fn  (:ext.symbol/fn patch/symbols-symbol)
          source-row  (first (filter #(= 'source-sym (:value %)) (:result (symbols-fn path))))]
      (patch-fn {:path path :search 'target-sym :replace source-row})
      (expect (= "(ns demo)\n(def a source-sym)\n(def b source-sym)\n" (slurp path)))))

  (it "z/patch public symbol returns a tool envelope with diff provenance"
    (let [path     (write-temp! "patch/tool.clj" "(ns demo)\n(def z 1)\n")
          patch-fn (:ext.symbol/fn patch/patch-symbol)
          out      (patch-fn {:path path :search "(def z 1)" :replace "(def z 3)"})]
      (expect (true? (:success? out)))
      (expect (= :z/patch (get-in out [:provenance :op])))
      (expect (= [{:path path}] (:result out)))
      (expect (= "(ns demo)\n(def z 1)\n"
                (get-in out [:provenance :files 0 :before])))
      (expect (= "(ns demo)\n(def z 3)\n"
                (get-in out [:provenance :files 0 :after])))
      (expect (= "(ns demo)\n(def z 3)\n" (slurp path))))))
