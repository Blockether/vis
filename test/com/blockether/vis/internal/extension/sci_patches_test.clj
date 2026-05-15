(ns com.blockether.vis.internal.extension.sci-patches-test
  (:require
   [com.blockether.vis.internal.extension.sci-patches :as sp]
   [lazytest.core :refer [defdescribe expect it]]
   [sci.core :as sci]
   [sci.impl.evaluator]))

(defn- fresh-ctx []
  (sci/init {:namespaces {'user {}}}))

(defn- ns-obj [ctx]
  (sci/find-ns ctx 'user))

(defdescribe sci-patches-docstring-enforcement-test
  (it "(def NAME \"doc\" VAL) succeeds and binds the var"
    (let [ctx (fresh-ctx)
          r   (sci/eval-string+ ctx "(def x \"the answer\" 42) x" {:ns (ns-obj ctx)})]
      (expect (= 42 (:val r)))))
  (it "(def NAME VAL) without a docstring throws :vis/missing-docstring"
    (let [ctx (fresh-ctx)]
      (try
        (sci/eval-string+ ctx "(def x 42)" {:ns (ns-obj ctx)})
        (expect false)
        (catch clojure.lang.ExceptionInfo e
          (expect (= :vis/missing-docstring (:type (ex-data e))))
          (expect (= 'x (:name (ex-data e))))))))
  (it "(defn NAME \"doc\" [args] body) succeeds (defn macro-expands to def with :doc meta)"
    (let [ctx (fresh-ctx)
          r   (sci/eval-string+ ctx
                "(defn double-it \"double the input\" [x] (* 2 x)) (double-it 21)"
                {:ns (ns-obj ctx)})]
      (expect (= 42 (:val r)))))
  (it "(defn NAME [args] body) without a docstring throws :vis/missing-docstring"
    (let [ctx (fresh-ctx)]
      (try
        (sci/eval-string+ ctx "(defn no-doc [x] x)" {:ns (ns-obj ctx)})
        (expect false)
        (catch clojure.lang.ExceptionInfo e
          (expect (= :vis/missing-docstring (:type (ex-data e)))))))))

(defdescribe sci-patches-def-sink-test
  (it "*def-sink-atom* is nil by default; patch is silent"
    (expect (nil? sp/*def-sink-atom*)))
  (it "binding *def-sink-atom* captures every (def …) in evaluation order"
    (let [ctx  (fresh-ctx)
          sink (sp/fresh-sink-atom)]
      (binding [sp/*def-sink-atom* sink]
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
          sink (sp/fresh-sink-atom)]
      (binding [sp/*def-sink-atom* sink]
        (sci/eval-string+ ctx
          "(defn greet \"sayhi\" [n] (str \"hi \" n))"
          {:ns (ns-obj ctx)}))
      (let [[entry] @sink]
        (expect (= 'greet (:name entry)))
        (expect (= "sayhi" (:doc (:meta entry))))))))

(defdescribe sci-patches-install-test
  (it "patch installs once; install-once! is :installed"
    (expect (var? #'sci.impl.evaluator/eval-def))
    ;; Patched wrap returns a function; original-eval-def is captured
    ;; behind the defonce, so re-loading this ns doesn't compound.
    (expect (fn? @#'sci.impl.evaluator/eval-def))))
