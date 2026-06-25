(ns com.blockether.vis.internal.foundation.editing.zipper
  "Language-neutral STRUCTURAL ZIPPER over the tree-sitter pack (306+ langs) —
   the unified cursor the name-based `structural` ops were missing.

   A node's location is a STATELESS PATH: a vector of NAMED-child indices from
   the root (e.g. `[2 0]` = first named child of the third named child of the
   file). Stateless means it round-trips cleanly through async tool calls — no
   live native cursor to keep between calls. Relative moves (down/up/next/prev)
   are pure path arithmetic on top, so the model navigates like a rewrite-clj
   zipper but over EVERY language tree-sitter understands.

   Edits splice the target node's UTF-8 byte range and RE-PARSE, refusing a
   result that introduces a syntax error the original didn't have — the same
   safety contract as `structural`. Pairs with the name-based ops: locate a def
   by name, then walk into it by path.

   All native handles (Parser/Tree/Node) are opened and closed inside each call;
   only plain Clojure data escapes."
  (:require [clojure.string :as str]
            [com.blockether.vis.internal.foundation.editing.outline :as outline]
            ;; Side-effecting require: selects + loads the platform native lib.
            [com.blockether.tree-sitter-language-pack])
  (:import [dev.kreuzberg.treesitterlanguagepack Parser Tree Node Point]
           [java.nio.charset StandardCharsets]
           [java.util Arrays]))

(defn- utf8 ^bytes [^String s] (.getBytes s StandardCharsets/UTF_8))

(defn- byte-slice ^String [^bytes bs ^long start ^long end]
  (String. (Arrays/copyOfRange bs (int start) (int end)) StandardCharsets/UTF_8))

(defn- byte-splice
  "New UTF-8 bytes with `[start,end)` of `bs` replaced by `ins`."
  ^bytes [^bytes bs ^long start ^long end ^bytes ins]
  (let [out (byte-array (+ start (alength ins) (- (alength bs) end)))]
    (System/arraycopy bs 0 out 0 start)
    (System/arraycopy ins 0 out start (alength ins))
    (System/arraycopy bs end out (+ start (alength ins)) (- (alength bs) end))
    out))

(defn detect-language [path] (outline/detect-language path))

(defn- parse-tree
  "Parse `source` as `lang` → a Tree (CALLER CLOSES), or nil. The tree is
   independent of the parser once parsed, so the parser is closed immediately."
  ^Tree [^String lang ^String source]
  (let [p (Parser/create)]
    (try
      (.setLanguage p lang)
      (.orElse (.parse p source) nil)
      (finally (.close p)))))

(defn- syntax-broken?
  "True when `source` parses to a tree whose root carries an ERROR node."
  [^String lang ^String source]
  (when-let [^Tree t (parse-tree lang source)]
    (try
      (let [^Node r (.rootNode t)]
        (try (.hasError r) (finally (.close r))))
      (finally (.close t)))))

(defn- node-data
  "Plain-data view of `n` (+ its immediate named children when `children?`).
   `n`'s text is sliced from `src-bytes` by the node's UTF-8 byte range."
  [^bytes src-bytes ^Node n children?]
  (let [sb (.startByte n) eb (.endByte n)
        ^Point sp (.startPosition n) ^Point ep (.endPosition n)]
    (cond-> {:kind (.kind n)
             :named? (.isNamed n)
             :start-line (inc (.row sp)) :start-col (.column sp)
             :end-line (inc (.row ep)) :end-col (.column ep)
             :start-byte sb :end-byte eb
             :text (byte-slice src-bytes sb eb)
             :sexp (.toSexp n)
             :named-child-count (.namedChildCount n)
             :has-error? (.hasError n)}
      children?
      (assoc :children
        (vec (for [i (range (.namedChildCount n))
                   :let [c (.orElse (.namedChild n (int i)) nil)]
                   :when c]
               (try
                 {:idx i
                  :kind (.kind c)
                  :head (first (str/split-lines
                                 (byte-slice src-bytes (.startByte c) (.endByte c))))}
                 (finally (.close c)))))))))

(defn- descend-call
  "Recursively walk `node` down `at` (named-child indices), then call
   `(f target src-bytes)`. Explicit recursion (not loop/recur) so each level can
   close its child handle in a `finally`. Returns `{:error …}` on a bad path."
  [^Node node src-bytes at f]
  (if (empty? at)
    (f node src-bytes)
    (let [i (int (first at))
          child (.orElse (.namedChild node i) nil)]
      (if child
        (try (descend-call child src-bytes (rest at) f)
          (finally (.close child)))
        {:error {:reason :bad-path
                 :message (str "no named child at index " i)}}))))

(defn- with-target
  "Parse `source` as `lang`, descend to `at`, and call `(f target src-bytes)`.
   Opens/closes every native handle; returns `(f …)` data or `{:error …}`."
  [lang source at f]
  (if-not lang
    {:error {:reason :unknown-language
             :message "unknown language for this file — use patch(...)"}}
    (let [src-bytes (utf8 source)
          ^Tree tree (parse-tree lang source)]
      (if-not tree
        {:error {:reason :parse-failed :message (str "could not parse as " lang)}}
        (try
          (let [^Node root (.rootNode tree)]
            (try (descend-call root src-bytes (vec at) f)
              (finally (.close root))))
          (finally (.close tree)))))))

(defn inspect
  "Structural view of the node at `at` (vector of named-child indices) in
   `source` (language `lang`): its kind, line span, text, s-expression, and a
   pick-list of immediate named children with indices. `at = []` is the file
   root. Pure data."
  [lang source at]
  (let [r (with-target lang source at
            (fn [^Node node src-bytes]
              (assoc (node-data src-bytes node true) :ok? true :path (vec at))))]
    r))

(defn edit
  "Splice the node at `at`: `op` ∈ #{:replace :insert-before :insert-after}
   with `code`. RE-PARSES and refuses a result that introduces a syntax error
   the original file didn't have. Returns `{:ok? true :new-source S}` or
   `{:error …}`."
  [lang source at op code]
  (with-target lang source at
    (fn [^Node node src-bytes]
      (let [sb (.startByte node) eb (.endByte node)
            ins (utf8 (str code))
            new-bytes (case op
                        :replace        (byte-splice src-bytes sb eb ins)
                        :insert-before  (byte-splice src-bytes sb sb ins)
                        :insert-after   (byte-splice src-bytes eb eb ins)
                        nil)]
        (if-not new-bytes
          {:error {:reason :bad-op :message (str "unknown op " op)}}
          (let [new-source (String. new-bytes StandardCharsets/UTF_8)]
            (if (and (syntax-broken? lang new-source)
                  (not (syntax-broken? lang source)))
              {:error {:reason :syntax-broken
                       :message (str "refused: " (name op) " at " (vec at)
                                  " would introduce a syntax error")}}
              {:ok? true :new-source new-source})))))))
