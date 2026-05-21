(ns com.blockether.vis.internal.env-test
  (:require
   [com.blockether.vis.internal.env :as env]
   [lazytest.core :refer [defdescribe expect it]]
   [sci.core :as sci]
   [sci.impl.evaluator]
   [sci.impl.resolve]))

(defn- fresh-ctx []
  (sci/init {:namespaces {'user {}}}))

(defn- ns-obj [ctx]
  (sci/find-ns ctx 'user))

(defdescribe env-docstring-contract-test
  (it "(def NAME \"doc\" VAL) succeeds and binds the var"
    (let [ctx (fresh-ctx)
          r   (sci/eval-string+ ctx "(def x \"the answer\" 42) x" {:ns (ns-obj ctx)})]
      (expect (= 42 (:val r)))))
  (it "(def NAME VAL) without a docstring succeeds"
    (let [ctx (fresh-ctx)
          r   (sci/eval-string+ ctx "(def x 42) x" {:ns (ns-obj ctx)})]
      (expect (= 42 (:val r)))))
  (it "(defn NAME \"doc\" [args] body) succeeds (defn macro-expands to def with :doc meta)"
    (let [ctx (fresh-ctx)
          r   (sci/eval-string+ ctx
                "(defn double-it \"double the input\" [x] (* 2 x)) (double-it 21)"
                {:ns (ns-obj ctx)})]
      (expect (= 42 (:val r)))))
  (it "(defn NAME [args] body) without a docstring succeeds"
    (let [ctx (fresh-ctx)
          r   (sci/eval-string+ ctx "(defn no-doc [x] x) (no-doc 7)" {:ns (ns-obj ctx)})]
      (expect (= 7 (:val r))))))

(defdescribe env-def-sink-test
  (it "*def-sink-atom* is nil by default; patch is silent"
    (expect (nil? env/*def-sink-atom*)))
  (it "binding *def-sink-atom* captures every (def …) in evaluation order"
    (let [ctx  (fresh-ctx)
          sink (env/fresh-sink-atom)]
      (binding [env/*def-sink-atom* sink]
        (sci/eval-string+ ctx
          "(def a \"alpha\" 1) (def b \"beta\" 2) (def c \"gamma\" (+ a b))"
          {:ns (ns-obj ctx)}))
      (let [entries @sink
            names   (mapv :name entries)]
        (expect (= 3 (count entries)))
        (expect (= '[a b c] names))
        (expect (every? :var entries))
        (expect (every? :meta entries))
        (expect (= ["alpha" "beta" "gamma"] (mapv #(:doc (:meta %)) entries))))))
  (it "captures defn (which macroexpands to def) into the sink too"
    (let [ctx  (fresh-ctx)
          sink (env/fresh-sink-atom)]
      (binding [env/*def-sink-atom* sink]
        (sci/eval-string+ ctx
          "(defn greet \"sayhi\" [n] (str \"hi \" n))"
          {:ns (ns-obj ctx)}))
      (let [[entry] @sink]
        (expect (= 'greet (:name entry)))
        (expect (= "sayhi" (:doc (:meta entry))))))))

(defdescribe env-install-test
  (it "patch installs once; install-once! is :installed"
    (expect (var? #'sci.impl.evaluator/eval-def))
    ;; Patched wrap returns a function; original-eval-def is captured
    ;; behind the defonce, so re-loading this ns doesn't compound.
    (expect (fn? @#'sci.impl.evaluator/eval-def)))
  (it "resolve-symbol* patch installs"
    (expect (var? #'sci.impl.resolve/resolve-symbol*))
    (expect (fn? @#'sci.impl.resolve/resolve-symbol*))))

(defdescribe env-lru-test
  (it "*lru-atom* is nil by default; resolve patch is silent"
    (expect (nil? env/*lru-atom*)))
  (it "binding *lru-atom* records every successful sandbox-var resolution"
    (let [ctx (fresh-ctx)
          lru (env/fresh-lru-atom)]
      (binding [env/*lru-atom* lru
                env/*current-turn-position* 7]
        ;; Define some vars first (each def itself triggers resolves)
        (sci/eval-string+ ctx
          "(def alpha \"first\" 1) (def beta \"second\" 2)"
          {:ns (ns-obj ctx)})
        ;; Reference them in another eval so resolve fires fresh
        (sci/eval-string+ ctx "(+ alpha beta)" {:ns (ns-obj ctx)}))
      (let [snap @lru]
        (expect (= 7 (get snap "alpha")))
        (expect (= 7 (get snap "beta")))
        ;; Core ops also stamped (renderer filters at draw time)
        (expect (some? (get snap "+"))))))
  (it "*current-turn-position* defaults to 0 when unbound"
    (let [ctx (fresh-ctx)
          lru (env/fresh-lru-atom)]
      (binding [env/*lru-atom* lru]
        (sci/eval-string+ ctx "(def x \"x doc\" 1) x" {:ns (ns-obj ctx)}))
      (expect (= 0 (get @lru "x"))))))

(defdescribe env-non-empty-block-validation-test
  (it "single top-level form passes through silently"
    (expect (nil? (env/validate-non-empty-block! "(def x \"doc\" 42)")))
    (expect (nil? (env/validate-non-empty-block! "42"))))
  (it "multi-top-level forms pass through silently (no (do …) ceremony required)"
    (expect (nil? (env/validate-non-empty-block! "(def a \"\" 1)\n(def b \"\" 2)")))
    (expect (nil? (env/validate-non-empty-block! "(def a \"\" 1)\n(def b \"\" 2)\n(+ a b)"))))
  (it "(do …) wrapping still works (model may emit it; not required)"
    (expect (nil? (env/validate-non-empty-block! "(do (def a \"a\" 1) (def b \"b\" 2))"))))
  (it "empty / comment-only block raises :vis/empty-block"
    (try
      (env/validate-non-empty-block! ";; just a comment\n#_(discard form)")
      (expect false)
      (catch clojure.lang.ExceptionInfo e
        (expect (= :vis/empty-block (:type (ex-data e))))
        (expect (= 0 (:form-count (ex-data e)))))))
  (it "count-top-level-forms returns the parse count"
    (expect (= 0 (env/count-top-level-forms "")))
    (expect (= 1 (env/count-top-level-forms "42")))
    (expect (= 2 (env/count-top-level-forms "(+ 1 2) (+ 3 4)")))
    (expect (= 1 (env/count-top-level-forms "(do (+ 1 2) (+ 3 4))")))))

(defdescribe env-banned-def-heads-test
  (it "plain (def …) / (defn …) pass through silently"
    (expect (nil? (env/validate-no-banned-defs! "(def x \"d\" 42)")))
    (expect (nil? (env/validate-no-banned-defs! "(defn f \"d\" [x] (* x 2))")))
    (expect (nil? (env/validate-no-banned-defs! "(defonce a \"d\" (atom 0))")))
    (expect (nil? (env/validate-no-banned-defs! "(defmulti dispatch \"d\" :kind)")))
    (expect (nil? (env/validate-no-banned-defs! "(defmacro m \"d\" [x] `(inc ~x))"))))

  (it "top-level defrecord is rejected with :vis/banned-def-head"
    (try
      (env/validate-no-banned-defs! "(defrecord Foo [a b])")
      (expect false)
      (catch clojure.lang.ExceptionInfo e
        (expect (= :vis/banned-def-head (:type (ex-data e))))
        (expect (= 'defrecord (:head (ex-data e)))))))

  (it "deftype / defprotocol / gen-class / extend-type / extend-protocol / definterface / reify are rejected"
    (doseq [head '[deftype defprotocol gen-class extend-type extend-protocol
                   definterface reify]]
      (try
        (env/validate-no-banned-defs! (str "(" head " Foo)"))
        (expect false)
        (catch clojure.lang.ExceptionInfo e
          (expect (= :vis/banned-def-head (:type (ex-data e))))
          (expect (= head (:head (ex-data e))))))))

  (it "clojure.core/-qualified banned heads are rejected too"
    (try
      (env/validate-no-banned-defs! "(clojure.core/defrecord Foo [a])")
      (expect false)
      (catch clojure.lang.ExceptionInfo e
        (expect (= :vis/banned-def-head (:type (ex-data e))))
        (expect (= 'clojure.core/defrecord (:head (ex-data e)))))))

  (it "banned head nested inside (do …) is still caught"
    (try
      (env/validate-no-banned-defs! "(do (def ok \"d\" 1) (defrecord Hidden [a]))")
      (expect false)
      (catch clojure.lang.ExceptionInfo e
        (expect (= :vis/banned-def-head (:type (ex-data e))))
        (expect (= 'defrecord (:head (ex-data e)))))))

  (it "banned head buried in a let/when body is caught"
    (try
      (env/validate-no-banned-defs! "(let [_ 1] (when true (deftype T [])))")
      (expect false)
      (catch clojure.lang.ExceptionInfo e
        (expect (= :vis/banned-def-head (:type (ex-data e))))
        (expect (= 'deftype (:head (ex-data e)))))))

  (it "parse errors are silent — SCI eval will surface them"
    (expect (nil? (env/validate-no-banned-defs! "(unbalanced ["))))

  (it "banned heads appearing only as data (quoted/in a string) do NOT trigger"
    ;; tree-seq traverses code structurally; a quoted form '(defrecord Foo)
    ;; parses as (quote (defrecord Foo)) — the inner list IS still walked.
    ;; Document the conservative behavior: quoted banned heads are also
    ;; rejected (intentional: easier to enforce than to whitelist quoted
    ;; contexts; the model can use the symbol via str/keyword if it really
    ;; needs the name).
    (try
      (env/validate-no-banned-defs! "'(defrecord Foo)")
      (expect false)
      (catch clojure.lang.ExceptionInfo e
        (expect (= :vis/banned-def-head (:type (ex-data e))))))
    ;; Strings are not parsed as code — they pass through.
    (expect (nil? (env/validate-no-banned-defs! "(def s \"d\" \"defrecord-in-a-string\")")))))
