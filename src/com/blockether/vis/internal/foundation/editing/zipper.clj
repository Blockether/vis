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

(defn- byte-slice
  ^String [^bytes bs ^long start ^long end]
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
    (try (.setLanguage p lang) (.orElse (.parse p source) nil) (finally (.close p)))))

(defn syntax-broken?
  "True when `source` parses to a tree whose root carries an ERROR node (nil when
   the language can't be parsed at all — fail-open, so callers never block on an
   unparseable input). Public so `patch` can re-parse its result and refuse a
   syntax-breaking edit, the same guard `struct_patch`/`symbol_rename` already run."
  [^String lang ^String source]
  (when-let [^Tree t (parse-tree lang source)]
    (try (let [^Node r (.rootNode t)]
           (try (.hasError r) (finally (.close r))))
         (finally (.close t)))))
(defn error-nodes
  "Every ERROR / MISSING node tree-sitter finds in `source` (parsed as `lang`),
   as [{:line :col :end-line :end-col :kind :missing? :text} …] in document
   order (1-based line, 0-based col). Empty when the source parses clean or the
   language can't be parsed. Public so an edit guard can turn a bare
   \"N syntax error(s)\" rejection into a LOCATED, actionable message — a MISSING
   node even NAMES the delimiter the parser expected (`:kind` = `]`, `)`, …)."
  [lang ^String source]
  (if-let [^Tree tree (and lang (parse-tree lang source))]
    (let [src-bytes (utf8 source)
          acc (transient [])]

      (try (let [^Node root (.rootNode tree)]
             (try (letfn [(walk [^Node n]
                            (when (or (.isError n) (.isMissing n))
                              (let [^Point sp (.startPosition n)
                                    ^Point ep (.endPosition n)]

                                (conj! acc
                                       {:line (inc (.row sp))
                                        :col (.column sp)
                                        :end-line (inc (.row ep))
                                        :end-col (.column ep)
                                        :kind (.kind n)
                                        :missing? (.isMissing n)
                                        :text (byte-slice src-bytes (.startByte n) (.endByte n))})))
                            (dotimes [i (.childCount n)]
                              (when-let [^Node c (.orElse (.child n (int i)) nil)]
                                (try (walk c) (finally (.close c))))))]
                    (walk root))
                  (finally (.close root))))
           (finally (.close tree)))
      (persistent! acc))
    []))

(defn- one-line
  "`s` collapsed to a single spaced line, truncated to `n` chars with an ellipsis."
  [s ^long n]
  (let [t (str/trim (str/replace (str s) #"\s+" " "))]
    (if (> (count t) n) (str (subs t 0 n) "…") t)))

(defn describe-syntax-errors
  "One-line, model-actionable summary of the ERROR/MISSING nodes in `source`
   (parsed as `lang`), or nil when it parses clean. Names WHERE the parser broke
   and, when tree-sitter knows it, WHICH delimiter it expected — so a rejected
   edit stops the model from blind paren-counting."
  [lang ^String source]
  (let [errs
        (error-nodes lang source)

        missing
        (filter :missing? errs)

        broken
        (remove :missing? errs)]

    (when (seq errs)
      (let [n
            (count errs)

            u
            (first broken)

            m
            (first missing)]

        (str n
             " tree-sitter parse-error node"
             (when (> n 1) "s")
             (when (> n 1)
               (str " (a parse CASCADE from usually ONE fault, not " n " separate mistakes)"))
             (when u
               (str "; first unexpected token at line "
                    (:line u)
                    " col "
                    (:col u)
                    " → `"
                    (one-line (:text u) 40)
                    "`"))
             (when m
               (str "; parser expected a `"
                    (:kind m)
                    "` at line "
                    (:line m)
                    " col "
                    (:col m)
                    " (a delimiter/token is missing or mismatched there)"))
             (when (> n 1)
               (str ". Count >1 usually means a bracket-TYPE mismatch (a `[`/`{`"
                    " closed with `)`, or vice-versa) or a mis-nest — NOT a trailing-paren"
                    " miscount, so stop counting parens and check delimiter TYPES"))
             ".")))))

(defn- node-data
  "Plain-data view of `n` (+ its immediate named children when `children?`).
   `n`'s text is sliced from `src-bytes` by the node's UTF-8 byte range."
  [^bytes src-bytes ^Node n children?]
  (let [sb
        (.startByte n)

        eb
        (.endByte n)

        ^Point sp
        (.startPosition n)

        ^Point ep
        (.endPosition n)]

    (cond-> {:kind (.kind n)
             :named? (.isNamed n)
             :start-line (inc (.row sp))
             :start-col (.column sp)
             :end-line (inc (.row ep))
             :end-col (.column ep)
             :start-byte sb
             :end-byte eb
             :text (byte-slice src-bytes sb eb)
             :sexp (.toSexp n)
             :named-child-count (.namedChildCount n)
             :has-error? (.hasError n)}
      children?
      (assoc :children
        (vec (for [i
                   (range (.namedChildCount n))

                   :let [c
                         (.orElse (.namedChild n (int i)) nil)]
                   :when c]

               (try {:idx i
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
    (let [i
          (int (first at))

          child
          (.orElse (.namedChild node i) nil)]

      (if child
        (try (descend-call child src-bytes (rest at) f) (finally (.close child)))
        {:error {:reason :bad-path :message (str "no named child at index " i)}}))))

(defn- with-target
  "Parse `source` as `lang`, descend to `at`, and call `(f target src-bytes)`.
   Opens/closes every native handle; returns `(f …)` data or `{:error …}`."
  [lang source at f]
  (if-not lang
    {:error {:reason :unknown-language :message "unknown language for this file — use patch(...)"}}
    (let [src-bytes
          (utf8 source)

          ^Tree tree
          (parse-tree lang source)]

      (if-not tree
        {:error {:reason :parse-failed :message (str "could not parse as " lang)}}
        (try (let [^Node root (.rootNode tree)]
               (try (descend-call root src-bytes (vec at) f) (finally (.close root))))
             (finally (.close tree)))))))

(defn inspect
  "Structural view of the node at `at` (vector of named-child indices) in
   `source` (language `lang`): its kind, line span, text, s-expression, and a
   pick-list of immediate named children with indices. `at = []` is the file
   root. Pure data."
  [lang source at]
  (let [r (with-target lang
                       source
                       at
                       (fn [^Node node src-bytes]
                         (assoc (node-data src-bytes node true)
                           :ok? true
                           :path (vec at))))]
    r))

(defn edit
  "Splice the node at `at`. `op` ∈ #{:replace :insert-before :insert-after
   :append-child :prepend-child} with `code` (clojure.zip vocabulary). RE-PARSES
   and refuses a result that introduces a syntax error the original file didn't
   have. Returns `{:ok? true :new-source S}` or `{:error …}`.
   :append-child / :prepend-child insert after the LAST / before the FIRST named
   child of the node at `at` (delete = :replace with \"\")."
  [lang source at op code]
  (if (#{:append-child :prepend-child} op)
    (let [n (or (:named-child-count (inspect lang source at)) 0)]
      (if (pos? (long n))
        (if (= op :append-child)
          (edit lang source (conj (vec at) (dec (long n))) :insert-after code)
          (edit lang source (conj (vec at) 0) :insert-before code))
        {:error {:reason :no-children
                 :message (str (name op)
                               ": node at " (vec at)
                               " has no named "
                               "children — navigate down and insert, or use replace")}}))
    (with-target
      lang
      source
      at
      (fn [^Node node src-bytes]
        (let [sb
              (.startByte node)

              eb
              (.endByte node)

              ins
              (utf8 (str code))

              new-bytes
              (case op
                :replace
                (byte-splice src-bytes sb eb ins)

                :insert-before
                (byte-splice src-bytes sb sb ins)

                :insert-after
                (byte-splice src-bytes eb eb ins)

                nil)]

          (if-not new-bytes
            {:error {:reason :bad-op :message (str "unknown op " op)}}
            (let [new-source (String. new-bytes StandardCharsets/UTF_8)]
              (if (and (syntax-broken? lang new-source) (not (syntax-broken? lang source)))
                {:error {:reason :syntax-broken
                         :message (str "refused: " (name op)
                                       " at " (vec at)
                                       " would introduce a syntax error"
                                       (when-let [d (describe-syntax-errors lang new-source)]
                                         (str " — " d)))}}
                {:ok? true :new-source new-source}))))))))

;; ── ZIPPER CURSOR — relative navigation (clojure.zip / rewrite-clj vocabulary) ──
(def ^:private move-aliases
  "Direction-first move names + single-letter shortcuts. `t`op = up toward the
   root, `b`ottom = down toward the leaves; `l`eft/`r`ight = previous/next
   SIBLING; `next`/`prev` (n/p) = DEPTH-FIRST traversal (clojure.zip semantics)."
  {"up" :up
   "u" :up
   "t" :up
   "top" :up
   "down" :down
   "d" :down
   "b" :down
   "bottom" :down
   "left" :left
   "l" :left
   "right" :right
   "r" :right
   "leftmost" :leftmost
   "first" :leftmost
   "<" :leftmost
   "rightmost" :rightmost
   "last" :rightmost
   ">" :rightmost
   "root" :root
   "home" :root
   "next" :dfs-next
   "n" :dfs-next
   "prev" :dfs-prev
   "previous" :dfs-prev
   "p" :dfs-prev})

(defn- norm-move
  [m]
  ;; Move specs arrive from Python: a scalar move string ("up", "d") or a dict
  ;; with string keys ({"child": 2}, {"find": "text"}). No keywords cross the
  ;; boundary, so read string keys directly — no keyword fallback.
  (cond (map? m) (let [c
                       (get m "child")

                       f
                       (get m "find")

                       fk
                       (or (get m "find_kind") (get m "kind"))]

                   (cond (some? c) [:child (int c)]
                         (some? f) [:find (str f)]
                         (some? fk) [:find-kind (str fk)]
                         :else nil))
        (some? m) (when-let [k (move-aliases (str/lower-case (str m)))]
                    [k])
        :else nil))

(defn- named-count
  "Named-child count of the node at `path`, or nil for a bad path."
  [lang source path]
  (let [r (inspect lang source path)]
    (when (:ok? r) (:named-child-count r))))

(defn- dfs-next
  "Depth-first NEXT node after `path`: down to the first child, else the next
   sibling, else up until a node has a next sibling. nil at the tree's end."
  [lang source path]
  (if (pos? (long (or (named-count lang source path) 0)))
    (conj (vec path) 0)
    (loop [p (vec path)]
      (when (seq p)
        (let [i (long (peek p))
              parent (pop p)
              pc (named-count lang source parent)]

          (if (and pc (< (inc i) (long pc))) (conj parent (inc i)) (recur parent)))))))

(defn- dfs-prev
  "Depth-first PREV node before `path`: the previous sibling's deepest-last
   descendant, else the parent. nil at the root."
  [lang source path]
  (let [path (vec path)]
    (when (seq path)
      (let [i (long (peek path))
            parent (pop path)]

        (if (zero? i)
          parent
          (loop [p (conj parent (dec i))]
            (let [n (named-count lang source p)]
              (if (and n (pos? (long n))) (recur (conj p (dec (long n)))) p))))))))

(defn- dfs-find
  "DFS-FORWARD from `start` (find-NEXT: begins AFTER the current node) for the
   first node whose data satisfies `pred`, or nil."
  [lang source start pred]
  (loop [p (dfs-next lang source start)]
    (cond (nil? p) nil
          (pred (inspect lang source p)) p
          :else (recur (dfs-next lang source p)))))

(defn- find-text
  "Locate the TIGHTEST node whose text contains `needle`: the first pre-order
   match after `start`, then descend into whichever child still contains the
   whole needle. So {find: \"(* base 2)\"} lands ON `(* base 2)`, not on the
   outermost form that merely encloses it. nil when nothing matches."
  [lang source start needle]
  (when-let [hit (dfs-find lang source start #(str/includes? (str (:text %)) needle))]
    (loop [p hit]
      (let [n (or (named-count lang source p) 0)
            child (some (fn [i]
                          (let [cp (conj p i)]
                            (when (str/includes? (str (:text (inspect lang source cp))) needle)
                              cp)))
                        (range n))]

        (if child (recur child) p)))))

(defn navigate
  "Resolve `at` (a named-child index path) + a sequence of relative `moves`
   against the ACTUAL tree → {:ok? true :path [...]} or {:error …}. The full
   clojure.zip / rewrite-clj cursor vocabulary (single-letter aliases):
     sibling/parent/child : up|u|t  down|d|b  left|l  right|r  leftmost|first
                            rightmost|last  root|home  {child: i}
     depth-first          : next|n  prev|p
     search (rewrite-clj) : {find: \"text\"}   {find_kind: \"if_statement\"}
   Boundary / not-found moves FAIL CLOSED instead of silently going nowhere."
  [lang source at moves]
  (loop [path
         (vec (or at []))

         ms
         (keep norm-move (or moves []))]

    (if (empty? ms)
      {:ok? true :path path}
      (let [[op arg]
            (first ms)

            step
            (case op
              :root
              []

              :up
              (if (seq path) (pop path) :err-root)

              :leftmost
              (if (seq path) (conj (pop path) 0) :err-root)

              :left
              (if (seq path)
                (let [i (peek path)]
                  (if (pos? i) (conj (pop path) (dec i)) :err-edge))
                :err-root)

              :down
              (let [n (named-count lang source path)]
                (if (and n (pos? n)) (conj path 0) :err-leaf))

              :child
              (let [n (named-count lang source path)]
                (if (and n (< (long arg) (long n))) (conj path arg) :err-child))

              :right
              (if (seq path)
                (let [pc
                      (named-count lang source (pop path))

                      i
                      (peek path)]

                  (if (and pc (< (inc i) pc)) (conj (pop path) (inc i)) :err-edge))
                :err-root)

              :rightmost
              (if (seq path)
                (let [pc (named-count lang source (pop path))]
                  (if (and pc (pos? pc)) (conj (pop path) (dec pc)) :err-edge))
                :err-root)

              :dfs-next
              (or (dfs-next lang source path) :err-end)

              :dfs-prev
              (or (dfs-prev lang source path) :err-end)

              :find
              (or (find-text lang source path (str arg)) :err-find)

              :find-kind
              (or (dfs-find lang source path #(= (str arg) (:kind %))) :err-find))]

        ;; `step` is INTERNAL: `case op` yields either a path vector or an
        ;; `:err-*` sentinel keyword minted right here (never model/Python data),
        ;; so this keyword? check stays — it is not a boundary read.
        (if (keyword? step)
          {:error {:reason :bad-move
                   :at path
                   :message (str "nav " (name op)
                                 " from " path
                                 ": " (case step
                                        :err-root
                                        "already at the root"

                                        :err-edge
                                        "no sibling in that direction"

                                        :err-leaf
                                        "node has no named children"

                                        :err-child
                                        (str "no child " arg)

                                        :err-end
                                        "no further node (end of tree)"

                                        :err-find
                                        (str "no node found matching " (pr-str arg))))}}
          (recur step (rest ms)))))))

(defn moves-available
  "Which cursor moves are possible from `path`, so the model sees its options
   instead of probing — down (HAS named children), up, left / right (a sibling
   exists), next / prev (depth-first) — plus its `index` among siblings and the
   sibling count (so `lefts` = index, `rights` = siblings-1-index)."
  [lang source path]
  ;; This map is embedded verbatim into the model-facing sexpr `"can"` result,
  ;; so it crosses the strings-only boundary — build it with string keys.
  (let [path
        (vec (or path []))

        own
        (named-count lang source path)

        i
        (when (seq path) (peek path))

        pc
        (when (seq path) (named-count lang source (pop path)))]

    {"down" (boolean (and own (pos? (long own))))
     "up" (boolean (seq path))
     "left" (boolean (and i (pos? (long i))))
     "right" (boolean (and pc i (< (inc (long i)) (long pc))))
     "next" (boolean (dfs-next lang source path))
     "prev" (boolean (seq path))
     "index" (when i (long i))
     "siblings" (when pc (long pc))}))
