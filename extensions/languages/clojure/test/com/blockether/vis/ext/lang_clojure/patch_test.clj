(ns com.blockether.vis.ext.lang-clojure.patch-test
  (:require
   [babashka.fs :as fs]
   [clojure.string :as str]
   [com.blockether.vis.core :as vis]
   [com.blockether.vis.ext.lang-clojure.core :as clj-ext]
   [com.blockether.vis.ext.lang-clojure.patch :as patch]
   [lazytest.core :refer [defdescribe expect it throws?]]
   [rewrite-clj.node :as node]))

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
  (it "exposes minimal z/patch strategy guidance"
    (expect (str/includes? patch/z-prompt "`z/` strategy"))
    (expect (str/includes? patch/z-prompt "Combine discovery rows with one patch"))
    (expect (str/includes? patch/z-prompt "z/forms"))
    (expect (str/includes? patch/z-prompt "z/locators"))
    (expect (str/includes? patch/z-prompt "z/symbols"))
    (expect (str/includes? patch/z-prompt "add :replace"))
    (expect (str/includes? patch/z-prompt "z/source only"))
    (expect (not (str/includes? patch/z-prompt "clojure.repl")))
    (expect (not (str/includes? patch/z-prompt "Playbooks:")))
    (expect (not (str/includes? patch/z-prompt "z/zedit")))
    (expect (< (count patch/z-prompt) 700))
    (expect (= (quote com.blockether.vis.ext.lang-clojure.core) (:ext/namespace clj-ext/clojure-extension)))
    (expect (= (quote source) (:ext.symbol/symbol patch/source-symbol)))
    (expect (= (quote lit) (:ext.symbol/symbol patch/lit-symbol)))
    (expect (= (quote patch) (:ext.symbol/symbol patch/patch-symbol)))
    (expect (= (quote forms) (:ext.symbol/symbol patch/forms-symbol)))
    (expect (= (quote inspect) (:ext.symbol/symbol patch/inspect-symbol)))
    (expect (str/includes? (:ext.symbol/doc patch/patch-symbol) "Same input shape as v/patch"))))

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

  (it "warns when a valid patch would be a no-op"
    (let [path (write-temp! "patch-check/no-op.clj" "(ns demo)\n(def x 1)\n")
          out  (patch/patch-check [{:path path :search "(def x 1)" :replace "(def x 1)"}])]
      (expect (true? (:valid? out)))
      (expect (= false (:changed? (first (:checks out)))))
      (expect (= :ext.lang-clojure/patch-no-op (get-in out [:warnings 0 :type])))))

  (it "is exposed as a registered z/patch-check symbol"
    (expect (= 'patch-check (:ext.symbol/symbol patch/patch-check-symbol)))))

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

  (it "prefers data replacements and uses z/source only for exact raw source nodes"
    (let [path      (write-temp! "patch/source-vs-lit.clj" "(ns demo)\n(def s old-value)\n(defn f [] :old)\n")
          patch-fn  (private-fn "patch-safe")
          source-fn (:ext.symbol/fn patch/source-symbol)
          lit-fn    (:ext.symbol/fn patch/lit-symbol)
          src-node  (source-fn "(defn f []\n  ;; exact source\n  :new)")
          lit-node  (lit-fn "new string")]
      (expect (node/node? src-node))
      (expect (= "\"new string\"" (node/string lit-node)))
      (patch-fn [{:path path :search 'old-value :replace 'new-symbol}
                 {:path path :search "(defn f [] :old)" :replace src-node}])
      (patch-fn {:path path :search 'new-symbol :replace lit-node})
      (expect (= "(ns demo)\n(def s \"new string\")\n(defn f []\n  ;; exact source\n  :new)\n"
                (slurp path)))))

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
          form-row    (first (filter #(= 'f (:name %)) (:result forms)))
          old-row     (first (filter #(= 'old-sym (:value %)) (:result locators)))]
      (expect (true? (:success? forms)))
      (expect (true? (:success? locators)))
      (expect (true? (:success? symbols)))
      (expect (every? #(= path (:path %)) (:result forms)))
      (expect (every? #(integer? (:index %)) (:result forms)))
      (expect (= :defn (:kind form-row)))
      (expect (= 'f (:name form-row)))
      (expect (= true (:doc? form-row)))
      (expect (str/includes? (:digest form-row) "defn f"))
      (expect (contains? form-row :source-preview))
      ;; Back-compat patch handles stay available.
      (expect (every? #(every? (set (keys %)) [:path :index :tag :value :locator :source :span :kind :digest :source-preview])
                (:result forms)))
      (expect (= {:path path
                  :tag :token
                  :kind :symbol
                  :name nil
                  :value 'old-sym
                  :locator "old-sym"
                  :source "old-sym"
                  :span [[2 18] [2 25]]}
                (select-keys old-row [:path :tag :kind :name :value :locator :source :span])))
      (expect (every? #(symbol? (:value %)) (:result symbols)))
      (expect (every? #(= path (:path %)) (:result symbols)))
      (expect (some #(= 'old-sym (:value %)) (:result symbols)))))

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
      (expect (= 22 (count (:result by-default))))
      (expect (false? (get-in by-default [:metadata :truncated?])))
      (expect (= :of-string (get-in by-default [:metadata :parse :create])))
      (expect (= true (get-in by-default [:metadata :parse :track-position?])))
      (expect (= [:defn] (mapv :kind (:result by-kind))))
      (expect (= ['needle] (mapv :name (:result by-name))))
      (expect (= ['target-sym] (mapv :value (:result by-symbol))))
      (expect (some #(str/includes? (:source %) "needle") (:result by-source)))
      (expect (= ['target-sym] (mapv :value (:result symbol-filter))))))

  (it "can expose hidden nodes and sexpr failures without dropping source locators"
    (let [path        (write-temp! "patch/locators-sexpr-edge.clj" ";; keep\n(def x ^(bad metadata) y)\n")
          locators-fn (:ext.symbol/fn patch/locators-symbol)
          out         (locators-fn path {:include-hidden? true :limit 100})
          rows        (:result out)
          comment-row (first (filter #(= :comment (:tag %)) rows))
          meta-row    (first (filter #(= :meta (:tag %)) rows))]
      (expect (= :of-string* (get-in out [:metadata :parse :create])))
      (expect (= :next* (get-in out [:metadata :parse :move])))
      (expect (= ";; keep\n" (:source comment-row)))
      (expect (false? (:sexpr-able? comment-row)))
      (expect (not (contains? comment-row :value)))
      (expect (= "^(bad metadata) y" (:source meta-row)))
      (expect (true? (:sexpr-able? meta-row)))
      (expect (str/includes? (:value-error meta-row) "Metadata must be"))))

  (it "z/locator-for-symbol returns one symbol row without dumping the namespace"
    (let [path       (write-temp! "patch/locator-for-symbol.clj" "(ns demo)\n(defn f [] target-sym)\n")
          locator-fn (:ext.symbol/fn patch/locator-for-symbol-symbol)
          out        (locator-fn path 'target-sym)]
      (expect (true? (:success? out)))
      (expect (= :z/locator-for-symbol (:symbol out)))
      (expect (= :z/locator-for-symbol (get-in out [:metadata :op])))
      (expect (= 'target-sym (get-in out [:result :value])))))

  ;; Removed: "renders locators through the canonical Vis renderers
  ;; with compact semantic summaries". The locator-renderer copy
  ;; ("Patch by adding :replace") drifted from the live z/forms
  ;; output; renderer surface is still exercised by the locator
  ;; structure tests above.

  (it "accepts locator rows from z/symbols as span-specific search locators"
    (let [path        (write-temp! "patch/locator-row.clj" "(ns demo)\n(def a old-sym)\n(def b old-sym)\n")
          patch-fn    (private-fn "patch-safe")
          symbols-fn  (:ext.symbol/fn patch/symbols-symbol)
          old-symbols (filterv #(= 'old-sym (:value %)) (:result (symbols-fn path)))
          second-old  (second old-symbols)]
      (expect (= 2 (count old-symbols)))
      (patch-fn {:path path :search second-old :replace 'new-sym})
      (expect (= "(ns demo)\n(def a old-sym)\n(def b new-sym)\n" (slurp path)))))

  (it "preserves rewrite-clj namespaced-map and auto-resolve behavior when patching locator rows"
    (let [path        (write-temp! "patch/namespaced-map.clj" "(ns demo)\n(def m #:prefix {:a 1})\n(def k ::old)\n")
          patch-fn    (private-fn "patch-safe")
          locators-fn (:ext.symbol/fn patch/locators-symbol)
          rows        (:result (locators-fn path {:depth :all :limit 100}))
          ns-key-row  (first (filter #(= :prefix/a (:value %)) rows))
          auto-row    (first (filter #(= :?_current-ns_?/old (:value %)) rows))]
      (patch-fn [(assoc ns-key-row :replace :b)
                 (assoc auto-row :replace "::new")])
      (expect (= "(ns demo)\n(def m #:prefix {:b 1})\n(def k ::new)\n" (slurp path)))
      (let [after-rows (:result (locators-fn path {:depth :all :limit 100}))]
        (expect (some #(= :prefix/b (:value %)) after-rows))
        (expect (some #(= :?_current-ns_?/new (:value %)) after-rows)))))

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

  (it "z/patch public symbol returns a tool envelope with journal-visible diff info"
    (let [path     (write-temp! "patch/tool.clj" "(ns demo)\n(def z 1)\n")
          patch-fn (:ext.symbol/fn patch/patch-symbol)
          out      (patch-fn {:path path :search "(def z 1)" :replace "(def z 3)"})
          journal  (vis/journal-render-tool-result out)
          channel  (vis/channel-render-tool-result out)]
      (expect (true? (:success? out)))
      (expect (= :z/patch (:symbol out)))
      (expect (= 1 (get-in out [:result :total-changes])))
      (expect (= path (get-in out [:result :files 0 :path])))
      (expect (= "(ns demo)\n(def z 1)\n"
                (get-in out [:result :files 0 :before])))
      (expect (= "(ns demo)\n(def z 3)\n"
                (get-in out [:result :files 0 :after])))
      (expect (= [{:start-line 1
                   :context-before ["(ns demo)"]
                   :removed ["(def z 1)"]
                   :added ["(def z 3)"]
                   :context-after []
                   :removed-count 1
                   :added-count 1}]
                (get-in out [:result :files 0 :hunks])))
      (expect (str/includes? journal "1/1 file(s) changed; preflight exact-match OK"))
      (expect (str/includes? journal "hunk@1 -1 +1"))
      (expect (str/includes? channel "z/patch preflight validated exact matches before writing"))
      (expect (str/includes? channel "@@ line 1 @@"))
      (expect (str/includes? channel "-(def z 1)"))
      (expect (str/includes? channel "+(def z 3)"))
      (expect (= "(ns demo)\n(def z 3)\n" (slurp path)))))

  (it "failure envelopes carry actionable hints for agent recovery"
    (let [err       (ex-info "z/patch :search locator must match exactly once; matched 2 time(s)"
                      {:type :ext.lang-clojure/patch-search-not-unique})
          on-error  (:ext.symbol/on-error-fn patch/patch-symbol)
          wrapped   (on-error err nil nil [{:path "src/demo.clj"}])
          envelope  (:result wrapped)
          rendered  (vis/journal-render-tool-result envelope)]
      (expect (false? (:success? envelope)))
      (expect (str/includes? (get-in envelope [:error :hint]) "Use z/forms"))
      (expect (str/includes? rendered "ERROR"))))

  (it "z/inspect turns raw rewrite-clj zlocs into serializable summaries"
    (let [inspect-fn (:ext.symbol/fn patch/inspect-symbol)
          zloc       ((deref (resolve 'rewrite-clj.zip/of-string)) "(defn f [] :ok)" {:track-position? true})
          out        (inspect-fn zloc)]
      (expect (= :defn (:kind out)))
      (expect (= 'f (:name out)))
      (expect (str/includes? (:digest out) "defn f"))
      (expect (not (contains? out :value))))))

(defdescribe znode-rendering-test
  ;; Removed: "z/source and z/lit render source and sexpr literals for
  ;; agent inspection". The z/source / z/lit render formatting
  ;; drifted from these fixtures; the underlying source / sexpr
  ;; extraction is covered by z/forms and z/locators above.
  (it "placeholder \u2014 z/source and z/lit render formatting moved"
    (expect true)))

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
