(ns com.blockether.vis.ext.language-clojure.render-test
  "Verify the channel renderers emit valid IR vectors with the shapes
   that downstream channel sinks expect:

     [:ir {} <block> ...]   root
     [:p  {} <inline> ...]  paragraph
     [:c  {} <text>]        inline code
     [:code {:lang ...} <body>]  fenced code

   The renderers MUST tolerate the realistic failure shapes too
   (timeouts, missing keys, error result maps) without throwing,
   because they fire from the channel sink with no try/catch above."
  (:require
   [clojure.string :as str]
   [com.blockether.vis.ext.language-clojure.render :as r]
   [lazytest.core :refer [defdescribe expect it]]))

(defn- ir? [x] (and (vector? x) (= :ir (first x))))

(defn- flat-text
  "Concatenate every string leaf in an IR tree. Used by tests so
   inline span boundaries (introduced by `ir-p` wrapping each arg)
   don't fragment regex matches."
  [node]
  (cond
    (string? node) node
    (vector? node) (str/join "" (map flat-text (rest node)))
    :else ""))

(defdescribe render-ports-test
  (it "renders zero ports as a single badge headline (no body)"
    (let [v (r/render-ports {:default nil :ports []})]
      (expect (ir? v))
      (expect (some #(and (vector? %) (= :p (first %))) v))
      ;; No listing body when n <= 1.
      (expect (not-any? #(and (vector? %) (= :code (first %))) v))))

  (it "drops the listing when only one port is visible (badge already carries default=)"
    (let [v    (r/render-ports {:default 7888
                                :ports   [{:port 7888 :source "/x/.nrepl-port"}]})
          flat (pr-str v)]
      (expect (ir? v))
      (expect (re-find #"default=7888" flat))
      ;; The path used to leak as a code-block row — verify it is
      ;; gone, otherwise the user reads it as duplicate noise next
      ;; to the badge.
      (expect (not (re-find #"\.nrepl-port" flat)))))

  (it "renders multiple ports as a code block (disambiguates which file the default came from)"
    (let [v (r/render-ports {:default 7888
                             :ports   [{:port 7888 :source "/x/.nrepl-port"}
                                       {:port 60001 :source "/y/.nrepl-port"}]})
          flat (pr-str v)]
      (expect (ir? v))
      (expect (re-find #"7888" flat))
      (expect (re-find #"60001" flat))
      (expect (re-find #"\.nrepl-port" flat)))))

(defdescribe render-eval-test
  (it "produces an EVAL header on success"
    (let [v (r/render-eval {:value "42" :ns "user" :status #{"done"} :ms 5 :port 7888})
          t (flat-text v)]
      (expect (ir? v))
      (expect (re-find #"EVAL" t))
      (expect (re-find #":7888" t))
      (expect (re-find #"42" t))))

  (it "produces a TIMEOUT header when timed-out?"
    (let [v (r/render-eval {:timed-out? true :status #{"done" "timeout"} :port 7888})]
      (expect (re-find #"TIMEOUT" (flat-text v)))))

  (it "shows :out and :err blocks when present"
    (let [v (r/render-eval {:value nil :out "hi\n" :err "boom\n" :status #{"done"} :port 7888})
          t (flat-text v)]
      (expect (re-find #":out" t))
      (expect (re-find #":err" t))
      (expect (re-find #"hi" t))
      (expect (re-find #"boom" t)))))

(defdescribe render-outline-test
  (it "shows ns, counts and a forms code block"
    (let [v (r/render-outline
              {:path "src/a.clj" :bytes 123 :total 2
               :ns {:name "a" :doc nil}
               :counts {:defn 2}
               :forms [{:kind :defn :name "x" :line 3 :arglists [["a"]] :doc "doc"}
                       {:kind :defn :name "y" :line 7 :arglists [["b"]]}]})
          s (pr-str v)]
      (expect (ir? v))
      (expect (re-find #"src/a.clj" s))
      (expect (re-find #"2×defn" s))
      (expect (re-find #"defn  x" s))
      (expect (re-find #"defn  y" s))))

  (it "passes :error through without throwing"
    (let [v (r/render-outline {:path "x.clj" :error "parse-failed" :forms [] :total 0})
          t (flat-text v)]
      (expect (re-find #"OUTLINE!" t))
      (expect (re-find #"parse-failed" t)))))

(defdescribe render-find-test
  (it "renders header counts and a match block"
    (let [v (r/render-find
              {:scanned 12 :elapsed-ms 4 :truncated? false
               :matches [{:path "src/a.clj" :line 3 :kind :defn :name "foo"}
                         {:path "src/b.clj" :line 9 :kind :defmacro :name "bar"}]})
          t (flat-text v)]
      (expect (re-find #"2 matches" t))
      (expect (re-find #"foo" t))
      (expect (re-find #"src/b.clj:9" t)))))

(defdescribe render-edit-test
  (it "renders an OK edit with delta"
    (let [v (r/render-edit
              {:status :ok :path "src/a.clj" :op :clj/edit :edit-op :replace :target "foo"
               :bytes {:before 100 :after 110} :delta 10})
          s (pr-str v)]
      (expect (re-find #":replace" s))
      (expect (re-find #"foo" s))
      (expect (re-find #"src/a.clj" s))
      (expect (re-find #"Δ=\+10" s))))

  (it "renders an :error edit"
    (let [v (r/render-edit {:status :error :error "target not found" :target "ghost"})]
      (expect (re-find #"EDIT FAILED" (pr-str v)))
      (expect (re-find #"target not found" (pr-str v))))))
