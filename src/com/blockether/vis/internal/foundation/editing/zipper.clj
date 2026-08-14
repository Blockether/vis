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
   safety contract as `structural`. A caller that hands `edit` a `:balancer` (a
   language pack's delimiter repair) gets one bounded second chance instead: the
   repair runs over the WHOLE new source and is kept only when it ADDS delimiters the
   splice omitted, on the lines that splice wrote (`balance`); one the splice wrote
   is never deleted. Pairs with the name-based ops:
   locate a def by name, then walk into it by path.

   All native handles (Parser/Tree/Node) are opened and closed inside each call;
   only plain Clojure data escapes."
  (:require [clojure.string :as str]
            [com.blockether.vis.internal.foundation.editing.balance :as balance]
            [com.blockether.vis.internal.foundation.editing.index :as index]
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

(defn- ws-byte?
  "ASCII-whitespace test on a raw byte. ASCII bytes never occur inside a
   multi-byte UTF-8 sequence, so scanning bytes for whitespace is codepoint-safe."
  [b]
  (case (long b)
    (9 10 13 32)
    true

    false))

(defn- gap-after
  "The run of whitespace starting at byte offset `from`, as a string."
  ^String [^bytes bs ^long from]
  (loop [i from]
    (if (and (< i (alength bs)) (ws-byte? (aget bs i))) (recur (inc i)) (byte-slice bs from i))))

(defn- gap-before
  "The run of whitespace ending at byte offset `to`, as a string."
  ^String [^bytes bs ^long to]
  (loop [i to]
    (if (and (pos? i) (ws-byte? (aget bs (dec i)))) (recur (dec i)) (byte-slice bs i to))))

(defn- newline-count
  ^long [^String s]
  (reduce (fn [^long n c]
            (if (= c \newline) (inc n) n))
          0
          s))

(defn- sibling-separator
  "Whitespace separator to REUSE when splicing a sibling on `side` (:before/:after)
   of the node spanning [sb,eb). Picks whichever adjacent inter-sibling gap spans
   the most lines (near side wins ties), so an inserted top-level form / block
   statement inherits the file's existing blank-line rhythm instead of gluing onto
   its neighbour. When NEITHER gap spans a line — an inline/tight collection
   (e.g. `(+ p q)`) — falls back to the near gap, or a single space when that is
   empty, so a spliced sibling never fuses onto its neighbour's token."
  ^String [^bytes bs ^long sb ^long eb side]
  (let
    [after
     (gap-after bs eb)

     before
     (gap-before bs sb)

     near
     (if (= side :after) after before)

     far
     (if (= side :after) before after)

     spanning
     (if (>= (newline-count near) (newline-count far)) near far)]

    (if (pos? (newline-count spanning)) spanning (if (pos? (count near)) near " "))))

(defn- last-in-container?
  "True when byte offset `i` (the end of a node's trailing whitespace) is at EOF or on a
   closing delimiter — i.e. the node is the LAST child of its container, so a delete should
   reclaim the whitespace gap BEFORE it rather than the gap after it."
  [^bytes bs ^long i]
  (or (>= i (alength bs))
      (case (long (aget bs i))
        (41 93 125)
        true

        false)))

(defn- delete-span
  "Byte span [start end) to REMOVE when deleting the node at [sb,eb): the node plus ONE
   adjacent whitespace gap, so the surviving neighbours keep a single separator instead of
   an orphaned blank line. Reclaims the TRAILING gap normally; the LEADING gap when the node
   is the last child of its container (its trailing whitespace runs into a close delim / EOF)."
  ^longs [^bytes bs ^long sb ^long eb]
  (let [ea (long (+ eb (count (gap-after bs eb))))]
    (if (last-in-container? bs ea)
      (long-array [(- sb (long (count (gap-before bs sb)))) eb])
      (long-array [sb ea]))))

(defn detect-language [path] (index/detect-language path))

(defn- parse-tree
  "Parse `source` as `lang` → a Tree (CALLER CLOSES), or nil. The tree is
   independent of the parser once parsed, so the parser is closed immediately."
  ^Tree [^String lang ^String source]
  (let [p (Parser/create)]
    (try (.setLanguage p lang) (.orElse (.parse p source) nil) (finally (.close p)))))

(defn syntax-broken?
  "True when `source` parses to a tree whose root carries an ERROR node (nil when
   the language can't be parsed at all — fail-open, so callers never block on an
   unparseable input). Public so a caller can re-parse its result and refuse a
   syntax-breaking edit, the same guard `struct_patch` already runs."
  [^String lang ^String source]
  (when-let [^Tree t (parse-tree lang source)]
    (try (let [^Node r (.rootNode t)]
           (try (.hasError r) (finally (.close r))))
         (finally (.close t)))))

(def ^:private quote-kinds
  "Literal quote tokens grammars may leave directly under an ERROR when a string
   consumes the rest of the file."
  #{"\"" "'" "`" "\"\"\"" "'''"})

(def ^:private delimiter-kinds
  "Literal syntax delimiters that are actionable when left directly beneath an
   ERROR node. Keywords and identifiers are deliberately excluded."
  (into #{"(" ")" "[" "]" "{" "}"} quote-kinds))

(defn- fault-delimiter
  "The most actionable unpaired delimiter directly inside ERROR node `n`, as
   {:line :byte-col :kind}, or nil.

   tree-sitter normally re-parents each well-formed sibling as a NAMED child of
   an ERROR, leaving the delimiter that failed to pair as an unnamed child. A
   recovery wrapper can contain more than one such delimiter, though: Java, for
   example, leaves both the class `{` and the actual unterminated string quote
   beneath one file-wide ERROR. Prefer the last quote (the lexical fault), then
   the last bracket (the closest structural fault), rather than blaming the
   first innocent opener at the start of the file."
  [^Node n]
  (loop
    [i
     0

     quote
     nil

     bracket
     nil]

    (if (< i (.childCount n))
      (if-let [^Node c (.orElse (.child n (int i)) nil)]
        (let
          [kind (.kind c)
           hit? (and (not (.isNamed c)) (contains? delimiter-kinds kind))
           data (when hit?
                  (let [^Point sp (.startPosition c)]
                    {:line (inc (.row sp)) :byte-col (.column sp) :kind kind}))]

          (.close c)
          (recur (inc i)
                 (if (and data (contains? quote-kinds kind)) data quote)
                 (if (and data (not (contains? quote-kinds kind))) data bracket)))
        (recur (inc i) quote bracket))
      (or quote bracket))))

(defn- character-column
  "Convert tree-sitter's 0-based UTF-8 byte column to a user-facing Unicode
   code-point column on `line`. Parser points always fall on UTF-8 boundaries."
  ^long [^String line ^long byte-col]
  (let
    [^bytes bs
     (utf8 line)

     end
     (min (max 0 byte-col) (alength bs))

     ^String prefix
     (byte-slice bs 0 end)]

    (.codePointCount prefix 0 (.length prefix))))

(defn- source-line
  "1-based line `line` of `source`, or nil."
  [^String source ^long line]
  (let [ls (str/split-lines (str source))]
    (when (<= 1 line (count ls)) (nth ls (dec line)))))

(defn error-nodes
  "Every ERROR / MISSING node tree-sitter finds in `source` (parsed as `lang`),
   as [{:line :col :byte-col :end-line :end-col :start-byte :end-byte :kind
   :missing? :text} …] in document order (1-based line, 0-based Unicode
   code-point col; `:byte-col` preserves tree-sitter's raw UTF-8 column). Empty
   when the source parses clean or the language can't be parsed. Public so an
   edit guard can turn a bare \"N syntax error(s)\" rejection into a LOCATED,
   actionable message — a MISSING node even NAMES the delimiter the parser
   expected (`:kind` = `]`, `)`, …).

   An ERROR node reports the most actionable UNBALANCED DELIMITER directly inside
   it, not necessarily the node's own start: an unclosed form can make tree-sitter
   open one ERROR over the whole file whose start is line 1. Those rows carry
   `:delimiter` and `:error-line` (where recovery began), and `:text` is the
   offending LINE. Raw byte spans remain available so a diagnostic can recognize
   and look through a broad recovery wrapper that contains a more specific ERROR."
  [lang ^String source]
  (if-let [^Tree tree (and lang (parse-tree lang source))]
    (let
      [src-bytes (utf8 source)
       acc (transient [])]

      (try
        (let [^Node root (.rootNode tree)]
          (try
            (letfn
              [(walk [^Node n]
                 (when (or (.isError n) (.isMissing n))
                   (let
                     [^Point sp (.startPosition n)
                      ^Point ep (.endPosition n)
                      d (when (.isError n) (fault-delimiter n))
                      line (long (or (:line d) (inc (.row sp))))
                      byte-col (long (or (:byte-col d) (.column sp)))
                      line-text (source-line source line)
                      col (if line-text (character-column line-text byte-col) byte-col)]

                     (conj! acc
                            (cond->
                              {:line line
                               :col col
                               :byte-col byte-col
                               :end-line (inc (.row ep))
                               :end-col (.column ep)
                               :start-byte (.startByte n)
                               :end-byte (.endByte n)
                               :kind (.kind n)
                               :missing? (.isMissing n)
                               :text (or (when d line-text)
                                         (byte-slice src-bytes (.startByte n) (.endByte n)))}
                              d
                              (assoc :delimiter
                                (:kind d) :error-line
                                (inc (.row sp)))))))
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

(defn- clip-line
  "`s` truncated to `n` chars, keeping leading whitespace so a caret still lines up."
  [s ^long n]
  (let [s (str s)]
    (if (> (count s) n) (str (subs s 0 n) " …") s)))

(def ^:private sexpr-langs
  "Languages whose delimiters all look alike, where the classic fault is closing
   a `[` with a `)` rather than losing count of a single delimiter type."
  #{"clojure" "clojurescript" "edn" "scheme" "lisp" "commonlisp" "elisp" "fennel" "janet" "racket"})

(defn- syntax-fault-hint
  "The likeliest CAUSE for `lang`, phrased as something to look AT. Language-aware:
   paren advice on a TSX file is noise, not help."
  [lang]
  (if (contains? sexpr-langs (str lang))
    "a `[`/`{` closed with `)` or vice-versa, or a mis-nest — check delimiter TYPES, not the count"
    "an unclosed `{`, `(`, `[`, string/template literal or JSX tag OPENED before this point"))

(defn- source-excerpt
  "Lines `line`-2 .. `(max line last)`+1 (1-based) with a `^` under `col`,
   gutter-numbered — so the break can be SEEN instead of counted to in a file
   that was never written. `last` extends the window to cover a second marker
   (the expected-delimiter position) when it sits nearby."
  [^String source ^long line ^long col ^long last]
  (let
    [lines
     (str/split-lines (str source))

     total
     (count lines)]

    (when (<= 1 line total)
      (let
        [from
         (max 1 (- line 2))

         to
         (min total (inc (max line last)))

         width
         (count (str to))

         gutter
         (fn [label]
           (str "  " (format (str "%" width "s") (str label)) " │ "))]

        (str/join
          "\n"
          (mapcat (fn [^long i]
                    (let [text (clip-line (nth lines (dec i)) 100)]
                      (if (= i line)
                        [(str (gutter i) text)
                         (str (gutter "") (apply str (repeat (min col (count text)) " ")) "^")]
                        [(str (gutter i) text)])))
                  (range from (inc to))))))))

(defn describe-syntax-errors
  "Model-actionable, MULTI-LINE report of the ERROR/MISSING nodes in `source`
   (parsed as `lang`), or nil when it parses clean. Names WHERE the parser broke,
   SHOWS those lines with a caret, names WHICH delimiter tree-sitter expected when
   it knows, and closes with a language-appropriate cause — so a rejected edit
   never degenerates into blind paren-counting."
  [lang ^String source]
  (let
    [errs
     (error-nodes lang source)

     missing
     (filter :missing? errs)

     broken
     (remove :missing? errs)]

    (when (seq errs)
      (let
        [n
         (count errs)

         ;; A broad recovery ERROR can contain a narrower ERROR. Discard a
         ;; location-less wrapper. Also discard the characteristic file-wide
         ;; class/module wrapper: a line-1 bracket around a nested quote error is
         ;; parser recovery, not evidence that the class opener itself is wrong.
         ;; Otherwise retain an outer actionable delimiter because it may be the
         ;; FIRST independent fault (an unclosed form before a later mismatch).
         diagnostic-broken
         (remove (fn [outer]
                   (some (fn [inner]
                           (and (not (identical? outer inner))
                                (<= (long (:start-byte outer)) (long (:start-byte inner)))
                                (>= (long (:end-byte outer)) (long (:end-byte inner)))
                                (or (< (long (:start-byte outer)) (long (:start-byte inner)))
                                    (> (long (:end-byte outer)) (long (:end-byte inner))))
                                (or (nil? (:delimiter outer))
                                    (and (= 1 (long (:line outer)))
                                         (= 1 (long (:error-line outer)))
                                         (not (contains? quote-kinds (:delimiter outer)))
                                         (contains? quote-kinds (:delimiter inner))))))
                         broken))
           broken)

         u
         (or (first diagnostic-broken) (first broken))

         m
         (first missing)

         focus
         (or u m)

         quote?
         (contains? quote-kinds (:delimiter u))

         opener?
         (contains? #{"(" "[" "{"} (:delimiter u))

         ;; One window when both markers are close, two when they are far apart:
         ;; never quote a screen of untouched code between them.
         near?
         (boolean (and u m (<= (abs (- (long (:line m)) (long (:line u)))) 4)))]

        (->>
          [(if (> n 1)
             (str n
                  " parse errors — ONE fault usually CASCADES into the rest,"
                  " so fix the FIRST, not all "
                  n
                  ".")
             "1 parse error.")
           (when u
             (str "  break     line "
                  (:line u)
                  " col "
                  (:col u)
                  " → `"
                  (one-line (:text u) 40)
                  "`"))
           ;; The delimiter ITSELF, when tree-sitter left one unpaired. The
           ;; ERROR's own start is only where recovery began, so naming a
           ;; quote/bracket is the difference between a location and a red
           ;; herring.
           (when (:delimiter u)
             (cond quote? (str "  unclosed  string quote `"
                               (:delimiter u)
                               "` opened there — nothing closes it before the end of the file")
                   opener? (str "  unclosed  `"
                                (:delimiter u)
                                "` opened there — nothing closes it before the end of the file")
                   :else (str
                           "  unmatched `"
                           (:delimiter u)
                           "` there — it closes nothing open (one too many, or the wrong TYPE)")))
           (when m
             (str "  expected  a `"
                  (:kind m)
                  "` at line "
                  (:line m)
                  " col "
                  (:col m)
                  " — missing or mismatched there"))
           (when focus
             (source-excerpt source
                             (long (:line focus))
                             (long (:col focus))
                             (if near? (long (:line m)) (long (:line focus)))))
           (when (and u m (not near?))
             (source-excerpt source (long (:line m)) (long (:col m)) (long (:line m))))
           (str "  likely    " (syntax-fault-hint lang))]
          (remove nil?)
          (str/join "\n"))))))

(defn- node-data
  "Plain-data view of `n` (+ its immediate named children when `children?`).
   `n`'s text is sliced from `src-bytes` by the node's UTF-8 byte range."
  [^bytes src-bytes ^Node n children?]
  (let
    [sb
     (.startByte n)

     eb
     (.endByte n)

     ^Point sp
     (.startPosition n)

     ^Point ep
     (.endPosition n)]

    (cond->
      {:kind (.kind n)
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
        (vec (for
               [i
                (range (.namedChildCount n))

                :let [^Node c
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
    (let
      [i
       (int (first at))

       ^Node child
       (.orElse (.namedChild node i) nil)]

      (if child
        (try (descend-call child src-bytes (rest at) f) (finally (.close child)))
        {:error {:reason :bad-path :message (str "no named child at index " i)}}))))

(defn- with-target
  "Parse `source` as `lang`, descend to `at`, and call `(f target src-bytes)`.
   Opens/closes every native handle; returns `(f …)` data or `{:error …}`."
  [lang source at f]
  (if-not lang
    {:error {:reason :unknown-language :message "unknown language for this file"}}
    (let
      [src-bytes
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
  (let
    [r (with-target lang
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
   child of the node at `at`. Insert-before/after reuse the file's inter-sibling gap so
   a spliced form keeps the blank-line rhythm; delete (= :replace with \"\") reclaims one
   adjacent gap so the survivors keep a single separator, not an orphaned blank line.

   `opts` may carry `:balancer` — a language pack's delimiter repair, `String ->
   String|nil`. Given one, a splice that would NEWLY break the parse gets ONE
   rebalance attempt over the whole new source, kept only when it ADDS what the splice
   omitted inside the lines this op wrote (`balance/rebalance`), reported as
   `:repairs`. Without a
   balancer a broken splice is refused, which is what every caller gets until its
   tool layer hands one down: the repair is a POLICY of the editing tools, never an
   ambient effect of the zipper."
  ([lang source at op code] (edit lang source at op code nil))
  ([lang source at op code opts]
   (if (#{:append-child :prepend-child} op)
     (let [n (or (:named-child-count (inspect lang source at)) 0)]
       (if (pos? (long n))
         (if (= op :append-child)
           (edit lang source (conj (vec at) (dec (long n))) :insert-after code opts)
           (edit lang source (conj (vec at) 0) :insert-before code opts))
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
         (let
           [sb
            (.startByte node)

            eb
            (.endByte node)

            new-bytes
            (case op
              :replace
              (if (= "" (str code))
                (let [^longs span (delete-span src-bytes sb eb)]
                  (byte-splice src-bytes (aget span 0) (aget span 1) (utf8 "")))
                (byte-splice src-bytes sb eb (utf8 (str code))))

              :insert-before
              (let
                [sep
                 (sibling-separator src-bytes sb eb :before)

                 ins
                 (if sep (str (str/trim (str code)) sep) (str code))]

                (byte-splice src-bytes sb sb (utf8 ins)))

              :insert-after
              (let
                [sep
                 (sibling-separator src-bytes sb eb :after)

                 ins
                 (if sep (str sep (str/trim (str code))) (str code))]

                (byte-splice src-bytes eb eb (utf8 ins)))

              nil)]

           (if-not new-bytes
             {:error {:reason :bad-op :message (str "unknown op " op)}}
             (let
               [new-source
                (String. ^bytes new-bytes StandardCharsets/UTF_8)

                broke?
                (and (syntax-broken? lang new-source) (not (syntax-broken? lang source)))

                ;; A structural splice writes ONE contiguous region, so a delimiter
                ;; repair is allowed exactly the lines this op wrote and no others.
                ;; See `balance`: repairing the `code` argument on its own balances a
                ;; partial form into a complete one that parses and means something
                ;; else, which is worse than the refusal it replaced.
                repair
                (when broke?
                  (balance/rebalance {:balancer (:balancer opts)
                                      :parses-clean? (fn [^String s]
                                                       (not (syntax-broken? lang s)))
                                      :source new-source
                                      :spans (some-> (balance/changed-span source new-source)
                                                     vector)}))]

               (cond (not broke?) {:ok? true :new-source new-source}
                     (:ok? repair)
                     {:ok? true :new-source (:content repair) :repairs (:notes repair)}
                     :else {:error {:reason :syntax-broken
                                    :message
                                    (str "refused: "
                                         (name op)
                                         " at "
                                         (vec at)
                                         " would introduce a syntax error"
                                         (when-let [d (describe-syntax-errors lang new-source)]
                                           (str "\n" d))
                                         (when-let [why (:why repair)]
                                           (str "\n" why " — fix the code you passed.")))}})))))))))

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
  (cond (map? m) (let
                   [c
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
        (let
          [i (long (peek p))
           parent (pop p)
           pc (named-count lang source parent)]

          (if (and pc (< (inc i) (long pc))) (conj parent (inc i)) (recur parent)))))))

(defn- dfs-prev
  "Depth-first PREV node before `path`: the previous sibling's deepest-last
   descendant, else the parent. nil at the root."
  [lang source path]
  (let [path (vec path)]
    (when (seq path)
      (let
        [i (long (peek path))
         parent (pop path)]

        (if (zero? i)
          parent
          (loop [p (conj parent (dec i))]
            (let [n (named-count lang source p)]
              (if (and n (pos? (long n))) (recur (conj p (dec (long n)))) p))))))))

(defn- path-after?
  "True when pre-order path `p` comes strictly AFTER `start`. A DESCENDANT of
   `start` counts, because depth-first next visits children before siblings."
  [p start]
  (let
    [n
     (count start)

     m
     (count p)]

    (loop [i 0]
      (cond (= i n) (> m n)
            (= i m) false
            :else (let
                    [a (long (nth p i))
                     b (long (nth start i))]

                    (cond (> a b) true
                          (< a b) false
                          :else (recur (inc i))))))))

(defn- scan-node
  "Pre-order scan of the ALREADY-PARSED subtree rooted at `node` (itself at
   `path`): the first path strictly after `start` whose node satisfies `match?`,
   never entering a subtree `descend?` rejects. Every child handle is closed."
  [^Node node path start match? descend?]
  (or (when (and (path-after? path start) (match? node)) path)
      (when (descend? node)
        (let [n (.namedChildCount node)]
          (loop [i 0]
            (when (< i n)
              (or (when-let [^Node c (.orElse (.namedChild node (int i)) nil)]
                    (try (scan-node c (conj path i) start match? descend?) (finally (.close c))))
                  (recur (inc i)))))))))

(defn- scan-tree
  "Search `source` with ONE parse and ONE depth-first walk. The previous
   `inspect`-per-node search re-parsed the whole file (and re-rendered its text
   and s-expression) for EVERY node it visited, so a `{find: …}` on a 7k-line
   file never returned."
  [lang source start match? descend?]
  (when-let [^Tree tree (and lang (parse-tree lang source))]
    (try (let [^Node root (.rootNode tree)]
           (try (scan-node root [] (vec start) match? descend?) (finally (.close root))))
         (finally (.close tree)))))

(defn- find-text
  "Locate the TIGHTEST node whose text contains `needle`: the first pre-order
   match after `start`, then descend into whichever child still contains the
   whole needle. So {find: \"(* base 2)\"} lands ON `(* base 2)`, not on the
   outermost form that merely encloses it. nil when nothing matches. A node
   without the needle cannot have a descendant with it, so its whole subtree is
   skipped."
  [lang source start needle]
  (let
    [src-bytes
     (utf8 source)

     has?
     (fn [^Node n]
       (str/includes? (byte-slice src-bytes (.startByte n) (.endByte n)) needle))

     tightest?
     (fn [^Node n]
       (and (has? n)
            (not (some (fn [i]
                         (when-let [^Node c (.orElse (.namedChild n (int i)) nil)]
                           (try (has? c) (finally (.close c)))))
                       (range (.namedChildCount n))))))]

    (scan-tree lang source start tightest? has?)))

(defn- find-kind
  "First node after `start` whose tree-sitter kind is `kind`, or nil."
  [lang source start kind]
  (scan-tree lang
             source
             start
             (fn [^Node n]
               (= kind (.kind n)))
             (fn [_]
               true)))

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
  (loop
    [path
     (vec (or at []))

     ms
     (keep norm-move (or moves []))]

    (if (empty? ms)
      {:ok? true :path path}
      (let
        [[op arg]
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
             (let [i (long (peek path))]
               (if (pos? i) (conj (pop path) (dec i)) :err-edge))
             :err-root)

           :down
           (let [n (named-count lang source path)]
             (if (and n (pos? (long n))) (conj path 0) :err-leaf))

           :child
           (let [n (named-count lang source path)]
             (if (and n (< (long arg) (long n))) (conj path arg) :err-child))

           :right
           (if (seq path)
             (let
               [pc
                (named-count lang source (pop path))

                i
                (long (peek path))]

               (if (and pc (< (inc i) (long pc))) (conj (pop path) (inc i)) :err-edge))
             :err-root)

           :rightmost
           (if (seq path)
             (let [pc (named-count lang source (pop path))]
               (if (and pc (pos? (long pc))) (conj (pop path) (dec (long pc))) :err-edge))
             :err-root)

           :dfs-next
           (or (dfs-next lang source path) :err-end)

           :dfs-prev
           (or (dfs-prev lang source path) :err-end)

           :find
           (or (find-text lang source path (str arg)) :err-find)

           :find-kind
           (or (find-kind lang source path (str arg)) :err-find))]

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
  ;; This map is embedded verbatim into the model-facing struct_node `"can"` result,
  ;; so it crosses the strings-only boundary — build it with string keys.
  (let
    [path
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

;; ── LINE → PATH — enter the zipper straight from a struct_index row ───────────
;; The 1-based line every struct_index row carries IS the zipper entry handle:
;; resolve it to the NAMED-child index path of the node that begins there. One
;; hop from a listed row to its structural cursor, no `nav [{find …}]` text-probe.

(defn- named-path-at-line
  "Pre-order DFS for the OUTERMOST named node whose 1-based START line == `line`.
   Returns its named-child index path (a vector), or nil. The file `root` itself
   (empty `path`) is never a match — we want a node WITHIN the file. Outermost:
   a node is checked BEFORE its children, so a line on `(defn foo …` resolves
   to the whole def form, not an inner symbol that happens to share the line."
  [^Node node ^long line path]
  (if (and (seq path) (= line (inc (long (.row (.startPosition node))))))
    path
    (let [n (.namedChildCount node)]
      (loop [i 0]
        (when (< i n)
          (let [^Node child (.orElse (.namedChild node (int i)) nil)]
            (if child
              (let
                [res (try (named-path-at-line child line (conj path (int i)))
                          (finally (.close child)))]
                (or res (recur (inc i))))
              (recur (inc i)))))))))

(defn path-at-line
  "Resolve a 1-based `line` (a struct_index row's SOLE position) to a named-child
   index PATH — the zipper entry point for that row. The OUTERMOST named node
   beginning on that line is located. Returns `{:ok? true :path [...] :line L}` or
   `{:error {:reason KW :message S}}`. Language-neutral (every tree-sitter lang)."
  [lang source line]
  (let [line (long (or line 0))]
    (cond (not lang) {:error {:reason :unknown-language :message "unknown language for this file"}}
          (< line 1) {:error {:reason :invalid-line
                              :message (str "line " line " is not a 1-based line number")}}
          :else (let [^Tree tree (parse-tree lang source)]
                  (if-not tree
                    {:error {:reason :parse-failed :message (str "could not parse as " lang)}}
                    (try (let [^Node root (.rootNode tree)]
                           (try (if-let [p (named-path-at-line root line [])]
                                  {:ok? true :path p :line line}
                                  {:error {:reason :line-no-node
                                           :message
                                           (str "no structural node begins at line "
                                                line
                                                " — the line points INSIDE a form, not at its "
                                                "start; use struct_nodes and nav to the node "
                                                "instead.")}})
                                (finally (.close root))))
                         (finally (.close tree))))))))
