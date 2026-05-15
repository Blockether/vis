(ns com.blockether.vis.internal.loop-test
  (:require
   [com.blockether.svar.core :as svar]
   [com.blockether.vis.internal.loop :as lp]
   [lazytest.core :refer [defdescribe it expect]]))

(defn- captured-ask-code-opts
  [opts]
  (let [seen (atom nil)]
    (with-redefs-fn {#'lp/get-router (fn [] ::router)
                     #'svar/ask-code! (fn [router opts]
                                        (reset! seen {:router router :opts opts})
                                        {:blocks [] :raw ""})}
      #(lp/ask-code! opts))
    @seen))

(defdescribe ask-code-idle-timeout-test
  (it "uses a five-minute ask-code idle timeout by default"
    (expect (= (* 5 60 1000) lp/ASK_CODE_IDLE_TIMEOUT_MS))
    (let [{:keys [router opts]} (captured-ask-code-opts {:lang "clojure" :messages []})]
      (expect (= ::router router))
      (expect (= lp/ASK_CODE_IDLE_TIMEOUT_MS (:idle-timeout-ms opts)))
      (expect (not (contains? opts :semantic-timeout-ms)))))

  (it "preserves explicit ask-code idle timeout overrides"
    (expect (= 42 (:idle-timeout-ms (:opts (captured-ask-code-opts {:idle-timeout-ms 42})))))
    (expect (contains? (:opts (captured-ask-code-opts {:idle-timeout-ms nil})) :idle-timeout-ms))
    (expect (nil? (:idle-timeout-ms (:opts (captured-ask-code-opts {:idle-timeout-ms nil}))))))

  (it "keeps semantic timeout opt-in"
    (expect (nil? lp/ASK_CODE_SEMANTIC_TIMEOUT_MS))
    (let [opts (:opts (captured-ask-code-opts {:semantic-timeout-ms 180000}))]
      (expect (= 180000 (:semantic-timeout-ms opts)))
      (expect (= lp/ASK_CODE_IDLE_TIMEOUT_MS (:idle-timeout-ms opts))))))

;; ---------------------------------------------------------------------------
;; def-sink -> vars-snapshot (per-var precise source extraction)
;; ---------------------------------------------------------------------------

(defn- mock-var
  "Stub IDeref so def-sink->vars-snapshot can produce a value without
   needing a live SCI context."
  [v]
  (reify clojure.lang.IDeref (deref [_] v)))

(defdescribe def-sink-vars-snapshot-test
  (it "single top-level def: per-var :code is the def form itself"
    (let [snap (lp/def-sink->vars-snapshot
                 [{:name 'x :var (mock-var 42)}]
                 "(def x \"doc\" 42)"
                 nil)]
      (expect (= 1 (count snap)))
      (expect (= "x" (:name (first snap))))
      (expect (= 42 (:value (first snap))))
      (expect (= "(def x \"doc\" 42)" (:code (first snap))))))

  (it "(do (def a) (def b)): each var carries its own precise source"
    (let [code "(do (def a \"doc\" 1) (def b \"doc\" 2))"
          snap (lp/def-sink->vars-snapshot
                 [{:name 'a :var (mock-var 1)}
                  {:name 'b :var (mock-var 2)}]
                 code
                 nil)
          by-name (into {} (map (juxt :name identity)) snap)]
      (expect (= "(def a \"doc\" 1)" (:code (by-name "a"))))
      (expect (= "(def b \"doc\" 2)" (:code (by-name "b"))))
      ;; Restore guarantee: re-eval of (by-name "a") :code MUST NOT
      ;; mention 'b — otherwise restoring a re-introduces b's side
      ;; effects.
      (expect (not (re-find #"\bb\b" (:code (by-name "a")))))
      (expect (not (re-find #"\ba\b" (:code (by-name "b")))))))

  (it "defn round-trips: precise (defn NAME doc args body) lands as :code"
    (let [code "(do (def base \"doc\" 10)\n     (defn add \"adds base\" [x] (+ x base)))"
          snap (lp/def-sink->vars-snapshot
                 [{:name 'base :var (mock-var 10)}
                  {:name 'add  :var (mock-var (fn [_] :stub))}]
                 code
                 nil)
          by-name (into {} (map (juxt :name identity)) snap)]
      (expect (re-find #"defn add" (:code (by-name "add"))))
      (expect (not (re-find #"defn add" (:code (by-name "base")))))
      (expect (re-find #"\bbase\b" (:code (by-name "base"))))))

  (it "sink entry without a matching parsed form falls back to whole block"
    ;; Simulates a macro-expanded def (e.g. `(s/def ::kw …)` — sink
    ;; captures the var name but parsed source contains no
    ;; `(def kw ...)` shape).
    (let [code "(s/def ::kw int?)"
          snap (lp/def-sink->vars-snapshot
                 [{:name 'kw :var (mock-var :stub)}]
                 code
                 nil)]
      (expect (= code (:code (first snap))))))

  (it "iteration-time-ms attaches to every var when positive"
    (let [snap (lp/def-sink->vars-snapshot
                 [{:name 'x :var (mock-var 1)}
                  {:name 'y :var (mock-var 2)}]
                 "(do (def x \"d\" 1) (def y \"d\" 2))"
                 17)]
      (expect (every? #(= 17 (:time-ms %)) snap))))

  (it "sink entry with nil :var is skipped"
    (let [snap (lp/def-sink->vars-snapshot
                 [{:name 'x :var nil}
                  {:name 'y :var (mock-var 2)}]
                 "(do (def x \"d\" 1) (def y \"d\" 2))"
                 nil)]
      (expect (= ["y"] (mapv :name snap))))))
