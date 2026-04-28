(ns com.blockether.vis.loop.mustache
  "Mustache template rendering for RLM answers via jmustache.

   Works natively with Clojure maps (keyword, string, or symbol keys),
   vectors, sets, atoms/delays. No Java conversion needed.

   Features:
   - {{var}} interpolation from sandbox locals
   - {{#list}}...{{/list}} section iteration over collections
   - {{^val}}...{{/val}} inverted sections (falsy/empty)
   - {{list.size}} collection length (dot-path navigation)
   - {{person.name}} nested map access via dot-path
   - Atoms/delays auto-deref'd
   - Missing vars throw (strict mode) — error fed back to LLM"
  (:require [clojure.string :as str])
  (:import [com.samskivert.mustache Mustache Mustache$Collector Mustache$Compiler
            Mustache$Formatter Mustache$VariableFetcher]))

(defn- clj-fetch
  "Resolve a Mustache variable name against a Clojure context (map/coll).
   Handles dot-path navigation, .size on collections, keyword/string/symbol
   key lookup, and IDeref (atoms, delays)."
  [ctx ^String name]
  (if (str/includes? name ".")
    (let [parts (str/split name #"\.")]
      (reduce (fn [c part]
                (cond
                  (nil? c) (reduced nil)
                  (and (= part "size") (or (sequential? c) (set? c)))
                  (reduced (count c))
                  (map? c)
                  (let [v (or (get c (keyword part))
                            (get c part)
                            (get c (symbol part)))]
                    (if (instance? clojure.lang.IDeref v) @v v))
                  :else (reduced nil)))
        ctx parts))
    (cond
      (and (= name "size") (or (sequential? ctx) (set? ctx)))
      (count ctx)
      (map? ctx)
      (let [v (or (get ctx (keyword name))
                (get ctx name)
                (get ctx (symbol name)))]
        (when (some? v)
          (if (instance? clojure.lang.IDeref v) @v v)))
      :else nil)))

(def ^:private clj-collector
  "jmustache Collector that works natively with Clojure persistent data."
  (reify Mustache$Collector
    (toIterator [_ val]
      (cond
        (sequential? val) (.iterator ^Iterable val)
        (set? val)        (.iterator ^Iterable (vec val))
        (instance? Iterable val) (.iterator ^Iterable val)
        :else (.iterator java.util.Collections/EMPTY_LIST)))
    (createFetcher [_ _ctx _name]
      (reify Mustache$VariableFetcher
        (get [_ ctx name]
          (clj-fetch ctx name))))
    (createFetcherCache [_] (java.util.HashMap.))))

(def ^:private clj-formatter
  "jmustache Formatter for Clojure values — keywords render as name, rest as str."
  (reify Mustache$Formatter
    (format [_ val]
      (cond
        (nil? val)     ""
        (string? val)  val
        (keyword? val) (name val)
        :else          (str val)))))

(def ^:private compiler
  "Shared jmustache compiler configured for Clojure data."
  (-> (Mustache/compiler)
    (.withCollector clj-collector)
    (.withFormatter clj-formatter)))

(defn render
  "Render a Mustache template string against a Clojure map context.
   Throws on missing vars (strict mode) — caller should catch and feed
   the error back to the LLM.

   `template` — Mustache template string.
   `ctx`      — Clojure map (keyword/string/symbol keys). Values may be
                 strings, numbers, booleans, collections, nested maps,
                 atoms, delays."
  ^String [^String template ctx]
  (let [tpl (.compile ^Mustache$Compiler compiler template)]
    (.execute tpl ctx)))


