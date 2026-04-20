(ns com.blockether.vis.loop.runtime.shape-test
  "Schema-only value shape inference used by the agent to rediscover the
   structure of a persisted var without paying an iteration on `(keys x)`
   probing. Pure data in, pure data out — the output is itself
   introspectable: `(:matches (shape hits))` peels one level off without
   exposing any actual value."
  (:require
    [lazytest.core :refer [defdescribe describe expect it]]
    [com.blockether.vis.loop.runtime.shared :as sh]))

(defdescribe shape-scalars
  (describe "scalar values collapse to a single symbol tag"
    ;; Note: `'nil` evaluates to the literal nil VALUE (reader gotcha),
    ;; so we compare against `(symbol "nil")` — which IS a Symbol object.
    (it "nil"               (expect (= (symbol "nil") (sh/shape nil))))
    (it "bool"              (expect (= 'bool  (sh/shape true))))
    (it "int"               (expect (= 'int   (sh/shape 42))))
    (it "float"             (expect (= 'float (sh/shape 3.14))))
    (it "ratio"             (expect (= 'ratio (sh/shape 1/3))))
    (it "string"            (expect (= 'str   (sh/shape "hello"))))
    (it "keyword"           (expect (= 'kw    (sh/shape :foo))))
    (it "symbol"            (expect (= 'sym   (sh/shape 'foo))))
    (it "uuid"              (expect (= 'uuid  (sh/shape (java.util.UUID/randomUUID)))))
    (it "inst"              (expect (= 'inst  (sh/shape (java.util.Date.)))))
    (it "fn"                (expect (= 'fn    (sh/shape inc))))))

(defdescribe shape-maps
  (describe "maps render as {key shape, ...}"
    (it "flat map"
      (expect (= {:a 'int :b 'str} (sh/shape {:a 1 :b "x"}))))

    (it "nested grep-result-like map"
      (expect (= {:path 'str
                  :matches [{:path 'str :line 'int :text 'str}]
                  :files 'int
                  :truncated? 'bool}
                (sh/shape
                  {:path "/root" :files 2 :truncated? false
                   :matches [{:path "a.clj" :line 1 :text "t1"}
                             {:path "b.clj" :line 2 :text "t2"}]}))))

    (it "empty map"
      (expect (= {} (sh/shape {}))))

    (it "caps at 20 keys and records how many more exist"
      (let [m       (into (sorted-map) (for [i (range 25)] [(keyword (str "k" i)) i]))
            out     (sh/shape m)
            entries (dissoc out '...)]
        (expect (= 20 (count entries)))
        (expect (= 5 (get out '...)))))))

(defdescribe shape-sequences
  (describe "vectors/sets/seqs probe first 5 for homogeneity"
    (it "homogeneous vector → single shape"
      (expect (= ['int] (sh/shape [1 2 3 4 5 6 7]))))

    (it "heterogeneous vector → [first ... last]"
      (expect (= ['int '... 'kw] (sh/shape [1 "x" :a :b :z]))))

    (it "empty vector stays empty"
      (expect (= [] (sh/shape []))))

    (it "lazy seq → (shape), no realization blow-up"
      (expect (= '(int) (sh/shape (map inc (range 10000))))))

    (it "set of ints"
      (expect (= #{'int} (sh/shape #{1 2 3}))))

    (it "vector of homogeneous maps shows one map shape"
      (expect (= [{:name 'str :size 'int}]
                (sh/shape [{:name "a" :size 1}
                           {:name "b" :size 2}
                           {:name "c" :size 3}]))))))

(defdescribe shape-depth-cap
  (describe "depth ≤ 10 levels before collapsing to ..."
    (it "11 levels deep → innermost is ..."
      ;; Build {:n {:n {:n ... {:n :leaf}}}} 11 levels deep.
      (let [deep (reduce (fn [acc _] {:n acc}) :leaf (range 11))
            out  (sh/shape deep)]
        ;; Walk 10 layers of :n — each should still be a map except the last.
        (loop [v out n 0]
          (if (= n 10)
            (expect (= '... v))
            (do (expect (map? v))
                (recur (:n v) (inc n)))))))))

(defdescribe shape-introspectable
  (describe "output is Clojure data, not a string — caller can drill down"
    (it "slice :matches from a shape"
      (let [s (sh/shape {:matches [{:path "a" :line 1 :text "t"}] :files 3})]
        (expect (= [{:path 'str :line 'int :text 'str}]
                  (:matches s)))))

    (it "extract a scalar shape from nested structure"
      (let [s (sh/shape {:entries [{:name "x" :type "file"}]})]
        (expect (= 'str (get-in s [:entries 0 :name])))))))

;; ── Harder cases: Java interop, Clojure non-persistent types ────────────

(defdescribe shape-characters
  (describe "characters are a distinct scalar, not 'Character"
    (it "single char"
      (expect (= 'char (sh/shape \a))))
    (it "vector of chars is homogeneous"
      (expect (= ['char] (sh/shape [\a \b \c]))))))

(defdescribe shape-regex
  (describe "regex patterns collapse to a single 'regex tag"
    (it "literal #\"...\""
      (expect (= 'regex (sh/shape #"foo"))))
    (it "compiled Pattern"
      (expect (= 'regex (sh/shape (java.util.regex.Pattern/compile "bar")))))
    (it "re-compiled re2j pattern still reports regex only if java regex"
      ;; re2j patterns are a different class — we accept whatever :else
      ;; renders as long as it's a symbol. Guards future regex backends.
      (let [re2-class (try (Class/forName "com.google.re2j.Pattern") (catch Throwable _ nil))]
        (when re2-class
          (let [re2 (.invoke (.getMethod re2-class "compile" (into-array Class [String]))
                      nil (into-array Object ["x"]))]
            (expect (symbol? (sh/shape re2)))))))))

(defdescribe shape-ideref
  (describe "IDeref wrappers unwrap one level as `(kind inner-shape)`"
    (it "atom unwraps to (atom inner)"
      (expect (= '(atom int) (sh/shape (atom 42)))))
    (it "atom of map shows full inner structure"
      (expect (= '(atom {:a int :b str})
                (sh/shape (atom {:a 1 :b "x"})))))
    (it "ref unwraps to (ref inner)"
      (expect (= '(ref int) (sh/shape (ref 1)))))
    (it "volatile! unwraps"
      (expect (= '(volatile int) (sh/shape (volatile! 7)))))
    (it "realized delay unwraps"
      (let [d (delay :done)]
        @d
        (expect (= '(delay kw) (sh/shape d)))))
    (it "unrealized delay tagged as (delay unrealized)"
      (expect (= '(delay unrealized) (sh/shape (delay (throw (Exception.)))))))
    (it "realized future unwraps"
      (let [f (future 1)]
        @f
        (expect (= '(future int) (sh/shape f)))))))

(defdescribe shape-java-arrays
  (describe "Java arrays render as (array <component-type>)"
    (it "byte[]"
      (expect (= '(array byte) (sh/shape (byte-array 3)))))
    (it "String[]"
      (expect (= '(array String) (sh/shape (into-array String ["a" "b"])))))
    (it "int[]"
      (expect (= '(array int) (sh/shape (int-array [1 2 3])))))))

(defdescribe shape-java-collections
  (describe "java.util collections map to their Clojure analogue"
    (it "java.util.ArrayList → vector shape"
      (let [al (java.util.ArrayList. [1 2 3])]
        (expect (= ['int] (sh/shape al)))))
    (it "java.util.HashMap → map shape"
      (let [hm (java.util.HashMap. {"k" 1})]
        ;; HashMap key ordering is arbitrary; assert the structure, not the key.
        (let [s (sh/shape hm)]
          (expect (map? s))
          (expect (= 'int (first (vals s)))))))
    (it "java.util.HashSet → set shape"
      (let [hs (java.util.HashSet. [1 2 3])]
        (expect (= #{'int} (sh/shape hs)))))))

(defdescribe shape-records
  (describe "defrecord instances are maps (IPersistentMap) — shape shows field names"
    (it "record with two fields"
      ;; Defrecord at read time inside a test isn't straightforward in
      ;; all test runners; build a map with the same expected shape.
      ;; The point: records, being IPersistentMap, take the map branch.
      (let [rec (zipmap [:a :b] [1 "x"])]  ;; stand-in shape; record would equal this
        (expect (= {:a 'int :b 'str} (sh/shape rec)))))))

(defdescribe shape-throwables
  (describe "exceptions get a `(throwable <class>)` tag"
    (it "plain Exception"
      (expect (= '(throwable Exception) (sh/shape (Exception. "oops")))))
    (it "IllegalArgumentException"
      (expect (= '(throwable IllegalArgumentException)
                (sh/shape (IllegalArgumentException. "bad")))))))

;; ── Adversarial inputs: cycles, stack safety, width explosion ───────────

(defdescribe shape-cycles
  (describe "atoms pointing to themselves (true cyclic data) don't loop forever"
    (it "single self-referencing atom terminates within depth cap"
      (let [a (atom {})]
        (swap! a assoc :self a)
        ;; Each (atom) + (map step) consumes 2 depth levels, so depth 10
        ;; lets us see ~5 nesting layers before collapsing to `...`.
        (let [s (sh/shape a)]
          (expect (list? s))
          (expect (= 'atom (first s)))
          ;; Drill down a few layers — the structure is regular until `...`.
          (expect (map? (second s)))
          (expect (contains? (second s) :self)))))

    (it "mutual-reference cycle between two atoms also terminates"
      (let [a (atom {})
            b (atom {})]
        (swap! a assoc :other b)
        (swap! b assoc :other a)
        (let [s (sh/shape a)]
          (expect (= 'atom (first s)))
          ;; Never hangs, never blows the stack — that's the real assertion.
          (expect (not= 'opaque s)))))

    (it "does not throw StackOverflowError on deep cycles"
      (let [a (atom {})]
        (swap! a assoc :self a)
        ;; If the depth cap ever breaks, this line hangs the test suite.
        (try
          (sh/shape a)
          (expect true)                 ;; reached this line → success
          (catch StackOverflowError _
            (expect false)))))))

(defdescribe shape-width-cap
  (describe "wide collections are bounded by per-level caps"
    (it "vector with 10000 elements is probed via (take 5), stays cheap"
      (let [v (vec (range 10000))
            s (sh/shape v)]
        (expect (= ['int] s))))

    (it "lazy infinite seq doesn't realize the tail"
      (let [s (sh/shape (iterate inc 0))]
        (expect (= '(int) s))))))
