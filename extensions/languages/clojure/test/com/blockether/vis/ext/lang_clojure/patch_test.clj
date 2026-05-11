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
  (it "exposes z/patch-shaped guidance and semantic reading tools"
    (expect (str/includes? patch/z-prompt "`z/` Clojure/EDN zipper patching"))
    (expect (str/includes? patch/z-prompt "Same map shape as v/patch"))
    (expect (str/includes? patch/z-prompt "z/forms"))
    (expect (str/includes? patch/z-prompt "z/locators"))
    (expect (str/includes? patch/z-prompt "z/symbols"))
    (expect (str/includes? patch/z-prompt "z/locator-for-symbol"))
    (expect (str/includes? patch/z-prompt "z/inspect"))
    (expect (str/includes? patch/z-prompt "z/subedit->"))
    (expect (not (str/includes? patch/z-prompt "z/zedit")))
    (expect (< (count patch/z-prompt) 1200))
    (expect (= 'patch (:ext.symbol/sym patch/patch-symbol)))
    (expect (= 'forms (:ext.symbol/sym patch/forms-symbol)))
    (expect (= 'inspect (:ext.symbol/sym patch/inspect-symbol)))
    (expect (str/includes? (:ext.symbol/doc patch/patch-symbol)
              "Same input shape as v/patch"))))

(defdescribe zpatch-check-test
  (it "reports valid? true and 1 match for a unique-search edit (no write)"
    (let [path  (write-temp! "patch-check/ok.clj" "(ns demo)\n(def x 1)\n")
          out   (patch/patch-check [{:path path :search "(def x 1)" :replace "(def x 2)"}])]
      (expect (true? (:valid? out)))
      (expect (= 1 (count (:checks out))))
      (expect (empty? (:failures out)))
      (expect (= 1 (:matches (first (:checks out)))))
      ;; Dry-run must not mutate the file.
      (expect (= "(ns demo)\n(def x 1)\n" (slurp path)))))

  (it "reports valid? false with failure rows for a missing search"
    (let [path (write-temp! "patch-check/miss.clj" "(ns demo)\n(def x 1)\n")
          out  (patch/patch-check [{:path path :search "(def nope 1)" :replace "x"}])]
      (expect (false? (:valid? out)))
      (expect (= 1 (count (:failures out))))
      (expect (= 0 (:matches (first (:failures out)))))))

  (it "is exposed as a registered z/patch-check symbol"
    (expect (= 'patch-check (:ext.symbol/sym patch/patch-check-symbol)))))

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

  (it "z/forms, z/locators, and z/symbols return semantic agent-usable locator rows"
    (let [path        (write-temp! "patch/locators.clj" "(ns demo)\n(defn f \"doc\" [] old-sym :k \"s\")\n")
          forms-fn    (:ext.symbol/fn patch/forms-symbol)
          locators-fn (:ext.symbol/fn patch/locators-symbol)
          symbols-fn  (:ext.symbol/fn patch/symbols-symbol)
          forms       (forms-fn path {:limit 100})
          locators    (locators-fn path {:depth :all :limit 100})
          symbols     (symbols-fn path {:limit 100})
          form-row    (first (filter #(= 'f (:name %)) (:op/result forms)))
          old-row     (first (filter #(= 'old-sym (:value %)) (:op/result locators)))]
      (expect (true? (:op/success? forms)))
      (expect (true? (:op/success? locators)))
      (expect (true? (:op/success? symbols)))
      (expect (every? #(= path (:path %)) (:op/result forms)))
      (expect (every? #(integer? (:index %)) (:op/result forms)))
      (expect (= :defn (:kind form-row)))
      (expect (= 'f (:name form-row)))
      (expect (= true (:doc? form-row)))
      (expect (str/includes? (:digest form-row) "defn f"))
      (expect (contains? form-row :source-preview))
      ;; Back-compat patch handles stay available.
      (expect (every? #(every? (set (keys %)) [:path :index :tag :value :locator :source :span :kind :digest :source-preview])
                (:op/result forms)))
      (expect (= {:path path
                  :tag :token
                  :kind :symbol
                  :name nil
                  :value 'old-sym
                  :locator "old-sym"
                  :source "old-sym"
                  :span [[2 18] [2 25]]}
                (select-keys old-row [:path :tag :kind :name :value :locator :source :span])))
      (expect (every? #(symbol? (:value %)) (:op/result symbols)))
      (expect (every? #(= path (:path %)) (:op/result symbols)))
      (expect (some #(= 'old-sym (:value %)) (:op/result symbols)))))

  (it "z/locators defaults to top-level rows and supports focused lookup"
    (let [path          (write-temp! "patch/locators-focused.clj"
                          (str "(ns demo)\n"
                            (str/join "\n" (map #(str "(def sym" % " " % ")") (range 20)))
                            "\n(defn needle [] target-sym)\n"))
          locators-fn   (:ext.symbol/fn patch/locators-symbol)
          forms-fn      (:ext.symbol/fn patch/forms-symbol)
          symbols-fn    (:ext.symbol/fn patch/symbols-symbol)
          by-default    (locators-fn path)
          by-kind       (forms-fn path {:kind :defn})
          by-name       (forms-fn path {:name 'needle})
          by-symbol     (locators-fn path {:symbol 'target-sym})
          by-source     (locators-fn path {:source-contains "needle" :limit 20})
          symbol-filter (symbols-fn path {:name 'target-sym})]
      (expect (= 22 (count (:op/result by-default))))
      (expect (false? (get-in by-default [:op/metadata :truncated?])))
      (expect (= [:defn] (mapv :kind (:op/result by-kind))))
      (expect (= ['needle] (mapv :name (:op/result by-name))))
      (expect (= ['target-sym] (mapv :value (:op/result by-symbol))))
      (expect (some #(str/includes? (:source %) "needle") (:op/result by-source)))
      (expect (= ['target-sym] (mapv :value (:op/result symbol-filter))))))

  (it "z/locator-for-symbol returns one symbol row without dumping the namespace"
    (let [path       (write-temp! "patch/locator-for-symbol.clj" "(ns demo)\n(defn f [] target-sym)\n")
          locator-fn (:ext.symbol/fn patch/locator-for-symbol-symbol)
          out        (locator-fn path 'target-sym)]
      (expect (true? (:op/success? out)))
      (expect (= :z/locator-for-symbol (:op/symbol out)))
      (expect (= :z/locator-for-symbol (get-in out [:op/metadata :op])))
      (expect (= 'target-sym (get-in out [:op/result :value])))))

  (it "renders locators through the canonical Vis renderers with compact semantic summaries"
    (let [tool-fn (:ext.symbol/fn patch/forms-symbol)
          path    (write-temp! "patch/render.clj" "(ns demo)\n(defn f [] :ok)\n")
          out     (tool-fn path {:kind :defn})
          journal (vis/journal-render-tool-result out)
          channel (vis/channel-render-tool-result out)]
      (expect (str/includes? journal "1 zipper locator"))
      (expect (str/includes? journal "defn f"))
      (expect (str/includes? journal "(defn f"))
      (expect (not (str/includes? journal "\n  :ok")))
      (expect (str/includes? channel "Patch by adding :replace"))
      (expect (str/includes? channel path)))))

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

  (it "z/patch public symbol returns a tool envelope with diff info"
    (let [path     (write-temp! "patch/tool.clj" "(ns demo)\n(def z 1)\n")
          patch-fn (:ext.symbol/fn patch/patch-symbol)
          out      (patch-fn {:path path :search "(def z 1)" :replace "(def z 3)"})]
      (expect (true? (:op/success? out)))
      (expect (= :z/patch (get-in out [:info :op])))
      (expect (= [{:path path}] (:result out)))
      (expect (= "(ns demo)\n(def z 1)\n"
                (get-in out [:info :files 0 :before])))
      (expect (= "(ns demo)\n(def z 3)\n"
                (get-in out [:info :files 0 :after])))
      (expect (= "(ns demo)\n(def z 3)\n" (slurp path))))))

(defdescribe zpatch-file-extension-precheck-test
  (it "rejects z/patch on a non-Clojure file before any rewrite-clj parse (regression: ANALYSIS.md §1)"
    (let [md-path  (write-temp! "ext-check/AGENTS.md" "# heading\n\nbody with infinity ∞ symbol.\n")
          patch-fn (private-fn "patch-safe")]
      (expect (throws? clojure.lang.ExceptionInfo
                #(patch-fn [{:path md-path :search "heading" :replace "title"}])))))

  (it "rejects z/locators on a non-Clojure file"
    (let [yml-path (write-temp! "ext-check/config.yaml" "key: value\n")
          locators (private-fn "locators-file")]
      (expect (throws? clojure.lang.ExceptionInfo #(locators yml-path)))))

  (it "rejects z/symbols on a non-Clojure file"
    (let [json-path (write-temp! "ext-check/data.json" "{\"x\":1}\n")
          symbols   (private-fn "symbols-file")]
      (expect (throws? clojure.lang.ExceptionInfo #(symbols json-path)))))

  (it "accepts every legal Clojure/EDN extension"
    (doseq [ext ["clj" "cljc" "cljs" "edn"]]
      (let [path     (write-temp! (str "ext-check/ok." ext) "(def x 1)\n")
            patch-fn (private-fn "patch-safe")]
        (expect (= [{:path path :before "(def x 1)\n" :after "(def x 2)\n"}]
                  (mapv #(select-keys % [:path :before :after])
                    (patch-fn [{:path path :search "(def x 1)" :replace "(def x 2)"}])))))))

  (it "extension check is case-insensitive"
    ;; macOS / case-insensitive FS users frequently end up with .CLJ / .EDN.
    (let [path     (write-temp! "ext-check/upper.CLJ" "(def y 1)\n")
          patch-fn (private-fn "patch-safe")]
      (expect (= [{:path path :before "(def y 1)\n" :after "(def y 2)\n"}]
                (mapv #(select-keys % [:path :before :after])
                  (patch-fn [{:path path :search "(def y 1)" :replace "(def y 2)"}])))))))
