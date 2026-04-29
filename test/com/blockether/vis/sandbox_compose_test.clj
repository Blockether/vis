(ns com.blockether.vis.sandbox-compose-test
  "Validates every Clojure pattern advertised in
   `com.blockether.vis.internal.prompt/CORE_SYSTEM_PROMPT`'s COMPOSE
   section against the LIVE SCI sandbox.

   Why this test exists: the COMPOSE block is a contract with the
   LLM. If we tell the model `(comp str inc inc)` returns the string
   `9` and the sandbox actually returns something else, the model
   wastes iterations chasing the lie. Every line in the prompt's
   COMPOSE section MUST round-trip through this test. When you add
   a new pattern to the prompt, add the matching case here in the
   same commit.

   Failure modes this catches:
     * SCI version drift (a core fn's behavior changed).
     * Sandbox build regression (a core fn got dropped from the
       :namespaces map in `env/create-sci-context`).
     * Patched fn regression (e.g. `str/split` losing the
       string-delim auto-promotion patch).
     * Aliases drift (`str/`, `json/`, `c+/` no longer resolving)."
  (:require
   [com.blockether.vis.internal.env :as env]
   [lazytest.core :refer [defdescribe expect it throws?]]
   [sci.core :as sci]))

;; -----------------------------------------------------------------------------
;; Harness — boot the sandbox once per `it`, eval inside ns `sandbox`.
;; -----------------------------------------------------------------------------

(defn- fresh-ctx []
  (:sci-ctx (env/create-sci-context nil)))

(defn- eval-in
  "Evaluate `snippet` inside the sandbox ns. Returns the value or
   throws — never swallow."
  [ctx snippet]
  (let [ns (sci/find-ns ctx 'sandbox)]
    (sci/binding [sci/ns ns]
      (sci/eval-string* ctx snippet))))

(defn- sandbox-eval
  "One-shot: fresh ctx + evaluate. Most cases want this."
  [snippet]
  (eval-in (fresh-ctx) snippet))

;; -----------------------------------------------------------------------------
;; def / defn / let — the basics the prompt advertises first.
;; -----------------------------------------------------------------------------

(defdescribe def-defn-let-test
  (it "def stashes a scalar across the eval"
    (expect (= 42 (sandbox-eval "(do (def n 42) n)"))))

  (it "def stashes a map and reads via keyword"
    (expect (= 1 (sandbox-eval "(do (def m {:a 1 :b 2}) (:a m))"))))

  (it "redef bumps the value (var-history will see both)"
    (expect (= 2 (sandbox-eval "(do (def x 1) (def x 2) x)"))))

  (it "defn one-arg arity"
    (expect (= 81 (sandbox-eval "(do (defn sq [n] (* n n)) (sq 9))"))))

  (it "defn multi-arity dispatches by arg count"
    (expect (= ["hi" "hi vis"]
              (sandbox-eval
                "(do (defn greet ([] \"hi\") ([who] (str \"hi \" who)))
                     [(greet) (greet \"vis\")])"))))

  (it "defn destructures map args without a helper let"
    (expect (= [3 4]
              (sandbox-eval "(do (defn pt [{:keys [x y]}] [x y]) (pt {:x 3 :y 4}))"))))

  (it "let with sequential bindings"
    (expect (= 2 (sandbox-eval "(let [a 2 b (* a a) c (- b a)] c)"))))

  (it "let with map destructure"
    (expect (= 30 (sandbox-eval "(let [{:keys [x y]} {:x 10 :y 20}] (+ x y))")))))

;; -----------------------------------------------------------------------------
;; Threading — -> ->> as-> some-> cond->>
;; -----------------------------------------------------------------------------

(defdescribe threading-test
  (it "-> threads first-arg through ops"
    (expect (= 15 (sandbox-eval "(-> 5 (+ 3) (* 2) (- 1))"))))

  (it "->> threads last-arg over a seq"
    (expect (= 120
              (sandbox-eval
                "(->> (range 10) (filter even?) (map #(* % %)) (reduce +))"))))

  (it "as-> mid-pipeline rename"
    (expect (= "=22"
              (sandbox-eval "(as-> 10 n (+ n 1) (* n 2) (str (char 61) n))"))))

  (it "some-> short-circuits on nil"
    (expect (nil? (sandbox-eval "(some-> nil (+ 1))"))))

  (it "cond->> applies last-arg pipeline conditionally"
    (expect (= [1 2 3 4 5]
              (sandbox-eval
                "(cond->> (range 5) true (map inc) false (map dec))")))))

;; -----------------------------------------------------------------------------
;; Function composition — comp / partial / #() / fn / complement / juxt
;; -----------------------------------------------------------------------------

(defdescribe composition-test
  (it "comp builds a right-to-left pipeline"
    (expect (= "9" (sandbox-eval "((comp str inc inc) 7)"))))

  (it "partial captures leading args"
    (expect (= 15 (sandbox-eval "((partial + 10) 5)"))))

  (it "#(...) reader-literal anon fn maps over a vec"
    (expect (= [2 4 6] (sandbox-eval "(map #(* 2 %) [1 2 3])"))))

  (it "fn with named positional args"
    (expect (= ":k=7" (sandbox-eval "((fn [a b] (str a \"=\" b)) :k 7)"))))

  (it "complement flips a predicate"
    (expect (= [1 2 3]
              (sandbox-eval "(filter (complement nil?) [1 nil 2 nil 3])"))))

  (it "juxt returns a vector of fn results"
    (expect (= [1 2 3]
              (sandbox-eval "((juxt :a :b :c) {:a 1 :b 2 :c 3})"))))

  (it "juxt + min/max gives a range tuple"
    (expect (= [1 9]
              (sandbox-eval
                "((juxt #(apply min %) #(apply max %)) [3 1 4 1 5 9 2 6])")))))

;; -----------------------------------------------------------------------------
;; Reduce / transduce / reduce-kv
;; -----------------------------------------------------------------------------

(defdescribe reduce-test
  (it "reduce with init"
    (expect (= 55 (sandbox-eval "(reduce + 0 (range 11))"))))

  (it "reduce-kv folds a map"
    (expect (= {:a 2 :b 3}
              (sandbox-eval
                "(reduce-kv (fn [acc k v] (assoc acc k (inc v))) {} {:a 1 :b 2})"))))

  (it "transduce with composed xform skips intermediate seqs"
    (expect (= 165
              (sandbox-eval
                "(transduce (comp (filter odd?) (map #(* % %))) + 0 (range 11))")))))

;; -----------------------------------------------------------------------------
;; clojure+.core macros — cond+ / if+ / when+
;; -----------------------------------------------------------------------------

(defdescribe clojure-plus-macros-test
  (it "c+/cond+ with :let intro mid-cond"
    (expect (= :odd
              (sandbox-eval
                "(c+/cond+ :let [n 7] (odd? n) :odd (even? n) :even)"))))

  (it "c+/cond+ supports :do side-effect with default branch"
    (expect (= :ok
              (sandbox-eval "(c+/cond+ :do (def side-eff 1) :else :ok)"))))

  (it "if+ binds and branches in one form"
    (expect (= 1 (sandbox-eval "(if+ [m {:k 1}] (:k m) :missing)"))))

  (it "if+ takes the else branch when the binding is falsy"
    (expect (= :missing
              (sandbox-eval "(if+ [m (get {:a 1} :no)] (:k m) :missing)"))))

  (it "when+ short body, no else"
    (expect (= 10 (sandbox-eval "(when+ [v (get {:a 1} :a)] (* v 10))")))))

;; -----------------------------------------------------------------------------
;; Map shaping — group-by / frequencies / update / assoc-in / update-in
;; -----------------------------------------------------------------------------

(defdescribe shape-test
  (it "group-by parity"
    (expect (= {true [0 2 4] false [1 3 5]}
              (sandbox-eval "(group-by even? (range 6))"))))

  (it "frequencies counts occurrences"
    (expect (= {:a 3 :b 2 :c 1}
              (sandbox-eval "(frequencies [:a :b :a :c :b :a])"))))

  (it "update bumps a counter"
    (expect (= {:n 1} (sandbox-eval "(update {:n 0} :n inc)"))))

  (it "assoc-in deep set"
    (expect (= {:a {:b 99}}
              (sandbox-eval "(assoc-in {:a {:b 0}} [:a :b] 99)"))))

  (it "update-in deep apply"
    (expect (= {:xs [1 3 3]}
              (sandbox-eval "(update-in {:xs [1 2 3]} [:xs 1] inc)")))))

;; -----------------------------------------------------------------------------
;; for / doseq / iterate — comprehensions and bounded infinite seqs
;; -----------------------------------------------------------------------------

(defdescribe seq-test
  (it "for with :when filter"
    (expect (= [0 4 16]
              (sandbox-eval "(for [x (range 6) :when (even? x)] (* x x))"))))

  (it "for with :let intro and :when guard"
    (expect (= [[2 20] [3 30]]
              (sandbox-eval
                "(for [x [1 2 3] :let [y (* x 10)] :when (> y 10)] [x y])"))))

  (it "doseq + atom counter (side-effect loop)"
    (expect (= 10
              (sandbox-eval
                "(let [a (atom 0)] (doseq [n (range 5)] (swap! a + n)) @a)"))))

  (it "iterate + take produces a bounded prefix"
    (expect (= [0 1 2 3 4]
              (sandbox-eval "(take 5 (iterate inc 0))")))))

;; -----------------------------------------------------------------------------
;; Aliases promised by the prompt — str/ json/ — including vis-patched split.
;; -----------------------------------------------------------------------------

(defdescribe alias-surface-test
  (it "str/split with auto-promoted string delimiter (vis-patched)"
    (expect (= ["a" "b" "c"]
              (sandbox-eval "(str/split \"a,b,c\" \",\")"))))

  (it "str/replace with explicit regex"
    (expect (= "foo_bar"
              (sandbox-eval "(str/replace \"foo.bar\" #\"\\.\" \"_\")"))))

  (it "json round-trips via threading"
    (expect (= {:x 1 :y [2 3]}
              (sandbox-eval
                "(-> {:x 1 :y [2 3]} json/write-json-str (json/read-json :key-fn keyword))")))))

;; -----------------------------------------------------------------------------
;; Banned-fn assertion. The prompt promises slurp is gone; prove it.
;; -----------------------------------------------------------------------------

(defdescribe banned-fn-test
  (it "slurp is replaced by a banner that throws on call"
    (expect (throws? Throwable
              #(sandbox-eval "(slurp \"/etc/hosts\")")))))

;; -----------------------------------------------------------------------------
;; Cross-check: every pattern shown in CORE_SYSTEM_PROMPT either appears
;; verbatim above, or reduces to one that does. If you add a pattern to
;; the prompt, add the case here. This guard test fails loud if the
;; prompt drifts away from the test surface.
;; -----------------------------------------------------------------------------

(def ^:private prompt-required-tokens
  "Tokens that must appear in CORE_SYSTEM_PROMPT for the cases above
   to be a meaningful contract. If someone deletes a section from the
   prompt, this assertion fires."
  ["COMPOSE"
   "(let "
   "(def n 42)"
   "(defn sq"
   "(-> 5"
   "(->> xs"
   "as->"
   "some->"
   "cond->>"
   "(comp "
   "(partial "
   "complement"
   "juxt"
   "reduce-kv"
   "transduce"
   "group-by"
   "frequencies"
   "assoc-in"
   "update-in"
   "for [x xs"
   "c+/cond+"
   "if+"
   "when+"
   "str/split"
   "json/read-json"
   "Banned: `slurp`"])

(defdescribe prompt-contract-test
  (it "every pattern this test asserts is also surfaced in CORE_SYSTEM_PROMPT"
    (require 'com.blockether.vis.internal.prompt)
    (let [prompt (deref (resolve 'com.blockether.vis.internal.prompt/CORE_SYSTEM_PROMPT))]
      (doseq [tok prompt-required-tokens]
        (expect (boolean (clojure.string/includes? prompt tok))
          (str "prompt missing token: " tok))))))
