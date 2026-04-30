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
   [clojure.string :as str]
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
;; (shape v) — sandbox-bound structural describe. The model's first-aid kit
;; for unknown values: scalar -> type kw, string -> [:string N], collection ->
;; [tag size <element-shape>], map -> [:map size <sorted-keys>]. Recursion
;; depth caps at 2 so the output stays one line for a vec-of-maps.
;; -----------------------------------------------------------------------------

(defdescribe shape-fn-test
  (it "scalars resolve to their type keyword"
    (expect (= :nil     (sandbox-eval "(shape nil)")))
    (expect (= :bool    (sandbox-eval "(shape true)")))
    (expect (= :bool    (sandbox-eval "(shape false)")))
    (expect (= :int     (sandbox-eval "(shape 7)")))
    (expect (= :float   (sandbox-eval "(shape 1.5)")))
    (expect (= :keyword (sandbox-eval "(shape :a/b)")))
    (expect (= :symbol  (sandbox-eval "(shape 'sym)"))))

  (it "strings carry their character count"
    (expect (= [:string 5]  (sandbox-eval "(shape \"hello\")")))
    (expect (= [:string 0]  (sandbox-eval "(shape \"\")"))))

  (it "empty collections show their tag plus a 0 size"
    (expect (= [:vec 0]  (sandbox-eval "(shape [])")))
    (expect (= [:map 0]  (sandbox-eval "(shape {})")))
    (expect (= [:set 0]  (sandbox-eval "(shape #{})")))
    (expect (= [:list 0] (sandbox-eval "(shape (list))"))))

  (it "non-empty homogeneous vec/seq/set/list reports first-element shape"
    (expect (= [:vec 3 :int]      (sandbox-eval "(shape [1 2 3])")))
    (expect (= [:set 2 :keyword]  (sandbox-eval "(shape #{:a :b})")))
    (expect (= [:list 2 :symbol]  (sandbox-eval "(shape (list 'x 'y))")))
    ;; bare seq via `seq` over a vec
    (let [out (sandbox-eval "(shape (seq [1 2 3]))")]
      (expect (#{[:vec 3 :int] [:seq 3 :int] [:list 3 :int]} out))))

  (it "map shape lists keys sorted by str"
    (expect (= [:map 2 [:a :b]]
              (sandbox-eval "(shape {:b 2 :a 1})"))))

  (it "recurses one level: vec of maps reports the inner map's keys"
    (expect (= [:vec 2 [:map 2 [:k :v]]]
              (sandbox-eval "(shape [{:k 1 :v 2} {:k 3 :v 4}])"))))

  (it "depth knob caps recursion"
    ;; depth = 0 — outer collection only, no element walk.
    (expect (= [:vec 2]
              (sandbox-eval "(shape [{:k 1} {:k 2}] 0)")))
    (expect (= [:map 1]
              (sandbox-eval "(shape {:a {:b 1}} 0)")))
    ;; depth = 1 — outer collection + first element typed but not
    ;; further unpacked (a map is reported as `[:map N]`, no keys).
    (expect (= [:vec 2 [:map 1]]
              (sandbox-eval "(shape [{:k 1} {:k 2}] 1)"))))

  (it "large maps clip their key list with a trailing …"
    ;; SHAPE_MAX_KEYS = 16. Build 20 keys and verify the tail collapses.
    (let [out (sandbox-eval
                "(shape (into {} (for [i (range 20)] [(keyword (str \"k\" i)) i])))")]
      (expect (vector? out))
      (expect (= :map (first out)))
      (expect (= 20   (second out)))
      ;; 16 explicit keys + a trailing … sentinel.
      (let [ks (nth out 2)]
        (expect (= 17 (count ks)))
        (expect (= '… (last ks))))))

  (it "unknown JVM types fall back to a class-derived keyword"
    ;; StringBuilder is in the sandbox classes allowlist and matches none
    ;; of `shape`'s explicit predicates — the catch-all branch lowercases
    ;; the simple class name.
    (expect (= :stringbuilder
              (sandbox-eval "(shape (java.lang.StringBuilder. \"hi\"))")))))

;; -----------------------------------------------------------------------------
;; Cross-check: every pattern shown in CORE_SYSTEM_PROMPT either appears
;; verbatim above, or reduces to one that does. If you add a pattern to
;; the prompt, add the case here. This guard test fails loud if the
;; prompt drifts away from the test surface.
;; -----------------------------------------------------------------------------

(def ^:private prompt-required-tokens
  "Tokens that must appear in CORE_SYSTEM_PROMPT for the prompt to be
   a meaningful contract. The original list also enumerated every
   COMPOSE-primer Clojure idiom; that primer was removed when we
   compressed the prompt to its essence (`code -> data -> answer`,
   the `(def x …) x` journal pattern, answer rules, SYSTEM vars).
   The compose-test cases below still verify those idioms work in
   the live SCI sandbox — they just no longer need to be
   simultaneously enumerated in the prompt as a contract."
  ["write code -> get data -> process data -> emit answer"
   "(def x (v/cat"
   "<journal>"
   "<var_index>"
   "TURN_USER_REQUEST"
   "CONVERSATION_TITLE"
   "(answer ARG)"
   "(conversation-title ARG)"
   ;; Skills + shape — explicit so the model never re-confuses skills
   ;; with extensions (regression guard for the empty-bullet bug we hit
   ;; on conversation 9b1e460d-c8ed-457e-bf00-b84157235c38).
   "TURN_ACCESSIBLE_SKILLS"
   "(v/skill name)"
   "(shape x)"])

(defdescribe prompt-contract-test
  (it "every pattern this test asserts is also surfaced in CORE_SYSTEM_PROMPT"
    (require 'com.blockether.vis.internal.prompt)
    (let [prompt (deref (resolve 'com.blockether.vis.internal.prompt/CORE_SYSTEM_PROMPT))]
      (doseq [tok prompt-required-tokens]
        (expect (boolean (str/includes? prompt tok))
          (str "prompt missing token: " tok))))))
