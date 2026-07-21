(ns com.blockether.vis.internal.foundation.editing.patch
  "Pure hashline primitives for content-addressed reading and editing.

   Public surface:
     split-content-lines / char-offset-at-line
     line-hash / lines->anchors              text -> lineno:hash / {ln anchor}
     render-hashline-block / -range-block   tuples -> `<hash>| text` gutter
     indices-matching-hash / resolve-anchor-edit-span  self-locating range replace"
  (:require [clojure.string :as str]))

(defn split-content-lines
  "Split a file blob into a vec of lines. Trailing empty element (from a
   final newline) is dropped, matching the convention used by the
   anchor patch planner."
  [^String s]
  (let
    [arr
     (.split s "\n" -1)

     v
     (vec arr)]

    (if (and (pos? (count v)) (= "" (peek v))) (pop v) v)))

(defn char-offset-at-line
  "Char offset in `content` where 0-based line `line-idx` starts.
   Returns `(count content)` if `line-idx` reaches past the last line.
   Public so the anchor patch planner can map line indices back to char
   positions for substring splicing."
  ^long [^String content ^long line-idx]
  (loop
    [pos
     0

     i
     0]

    (if (= i line-idx)
      pos
      (let [nl (str/index-of content "\n" pos)]
        (if nl (recur (inc (long nl)) (inc i)) (count content))))))
;; =============================================================================
;; Hashline layer — the single, reusable line+content-addressed editing surface.
;;
;; An anchor is `<line-number>:<hash>` (Can Bölük's original hashline shape).
;; The line number LOCATES the line; the content hash VERIFIES it. Two
;; coordinates: a reused/stale hash can no longer silently land an edit on the
;; wrong line — if the content sits far from the stated line, patch refuses
;; (`:hashline-misplaced`) instead of corrupting. This block owns every reusable
;; piece so callers never recompute the scheme:
;;
;;   line-hash                text            -> hash-width-hex content hash
;;   line-anchor              ln text         -> "<ln>:<hash>"   (one anchor)
;;   lines->anchors            [[ln text]…]    -> {ln "ln:hash"}  (model map)
;;   render-hashline-block    [[ln text]…]    -> "<ln>:<hash>│ text…" (gutter)
;;   render-hashline-range-block ranges       -> headered gutter blocks
;;   indices-matching-hash    lines hash      -> [0-based idx …]  (content only)
;;   resolve-anchor-edit-span   content a a2 rep -> {:start :end :replacement}|{:error}
;;
;; The gutter the reader SEES is the edit address — `patch :from_anchor` parses
;; the same `<line-number>:<hash>` back against live content. The line number
;; is part of the address now, not just navigation.
;; =============================================================================
(def hash-width
  "Hex chars in a line's content hash. The line number locates; this hash
   verifies within `hash-line-drift-tolerance`. Three hex chars keep anchors
   compact while the line coordinate disambiguates collisions."
  3)

(def ^:private hash-mask
  "Low `hash-width` hex digits as a bit mask: (16^hash-width) - 1."
  (long (dec (bit-shift-left 1 (* 4 (long hash-width))))))

(def ^:private hash-zero-pad
  "`hash-width` zero chars, for left-padding a short `Integer/toHexString`."
  (apply str (repeat (long hash-width) \0)))

(defn line-hash
  "Stable `hash-width`-hex-char content hash of `line` (trimmed). Folds
   the spec'd `String/hashCode` algorithm over the whitespace-trimmed
   line, so it is deterministic across JVM runs. The line coordinate
   disambiguates identical hashes.

   Hot path: runs once per line on every `cat` render AND every patch
   resolve. Formats with `Integer/toHexString` + a left-pad rather than
   java.util.Formatter, which benches ~1.5x slower; the trimmed
   `String/hashCode` is a JIT intrinsic so we lean on it instead of a
   hand loop."
  ^String [line]
  (let
    [h
     (int (bit-and (.hashCode (str/trim (str line)))
                   #_{:clj-kondo/ignore [:redundant-primitive-coercion]}
                   (long hash-mask)))

     hex
     (Integer/toHexString h)

     c
     (.length hex)]

    (if (< c (long hash-width)) (str (subs hash-zero-pad c) hex) hex)))

(def ^:const hashline-anchor-sep
  "Separator between the line number and the content hash inside an anchor
   (`<lineno>:<hash>`). A single char so the gutter stays narrow."
  ":")

(defn line-anchor
  "The editable anchor for a line: `<line-number>:<content-hash>` (e.g.
   `325:0e3`). The line number LOCATES the line; the hash VERIFIES its
   content. `patch :from_anchor` parses this back via `resolve-anchor-range`,
   matching the line number against live content and refusing if the hash no
   longer agrees (the line changed) or that content now lives far from the
   stated line (a misattributed / stale anchor). Two coordinates, so a single
   reused hash can no longer silently land an edit on the wrong line."
  [ln text]
  (str ln hashline-anchor-sep (line-hash text)))

(defn lines->anchors
  "`{line-number anchor}` map of every non-blank line in `tuples`, where each
   anchor is `<line-number>:<content-hash>` (`line-anchor`). The canonical
   `:anchors` payload `cat` returns — the SINGLE place it is built (read-file /
   read-file-ranges / tail-file / rg all route here). Blank lines are omitted:
   the model only ever sees anchors it can actually edit by. Line numbers come
   straight from the `[ln text]` tuples, so a windowed read (range / tail /
   by-hash) carries the file's real line numbers with NO second full-file pass
   — the `#N` file-wide-ordinal scheme (and its whole-file rescan) is gone now
   that the line number, not the hash, disambiguates duplicate lines."
  [tuples]
  (into {}
        (keep (fn [[ln s]]
                (when-not (str/blank? (str s)) [ln (line-anchor ln s)])))
        tuples))

(defn anchor->line
  "Parse the line number out of a `<lineno>:<hash>` anchor."
  ^long [anchor]
  (let
    [s
     (str anchor)

     i
     (str/index-of s hashline-anchor-sep)]

    (Long/parseLong (subs s 0 (long i)))))

(defn lines->anchor-map
  "Ordered `{anchor {\"text\" line}}` map for `[[ln text]…]` tuples: a REAL
   `java.util.LinkedHashMap` (natively insertion-ordered), built in line order
   — each KEY is the line's `<lineno>:<hash>` `line-anchor`, each VALUE a
   `{\"text\" <line>}` map. This MIRRORS rg's hit value, so a consumer reads
   `v[\"text\"]` uniformly across `cat` and `rg` (the whole point of the shape).
   Being an ordered hashmap (not a Clojure map), it stays in file order across
   the Clojure → JSON/charred → GraalPy dict boundary at ANY size, with NO
   comparator. EVERY line is keyed (blanks included, so the read stays
   gap-free); duplicate text differs by line number, so keys are unique. THE
   single model-facing line payload `cat` returns; the key IS the
   `patch :from_anchor`."
  ^java.util.LinkedHashMap [tuples]
  (let [m (java.util.LinkedHashMap.)]
    (doseq [[ln s] tuples]
      (.put m (line-anchor ln s) {"text" s}))
    m))

(defn anchor-value-text
  "Extract the line text from an `:anchors` / `before` / `after` map VALUE.
   Every model-facing anchor value is a `{\"text\" <line>}` map (see
   `lines->anchor-map`)."
  [v]
  (when-not (and (map? v) (contains? v "text"))
    (throw (ex-info "anchor value must be a map containing 'text'"
                    {:type :ext.foundation.editing/invalid-anchor-value :value v})))
  (get v "text"))

(defn anchor-map->tuples
  "Inverse of `lines->anchor-map`: `{anchor {\"text\" text}}` → `[[ln text]…]`
   tuples, the line number parsed from each `<lineno>:<hash>` key and the text
   pulled from the value's `\"text\"`, sorted by line. For the channel/human
   gutter and internal consumers that need tuples."
  [m]
  (->> m
       (map (fn [[a t]]
              [(anchor->line a) (anchor-value-text t)]))
       (sort-by first)
       vec))

(def ^:const hashline-gutter
  "Separator between the anchor and the line text in rendered output."
  "│ ")

(defn render-lineno-block
  "Render `[line-number text]` tuples as a HUMAN line-number gutter
   `<ln>│ <text>`, line numbers right-aligned to the widest number in
   the block. Unlike `render-hashline-block` (the MODEL surface, whose
   gutter is the editable `:from_anchor` anchor), this is the channel/TUI
   display surface: humans navigate by line number, not by content hash."
  [tuples]
  (let
    [tuples
     (vec tuples)

     width
     (reduce (fn [^long w [ln _]]
               (max w (long (count (str ln)))))
             1
             tuples)]

    (->> tuples
         (map (fn [[ln s]]
                (str (format (str "%" width "s") (str ln)) hashline-gutter s)))
         (str/join "\n"))))

(defn render-lineno-range-block
  "`render-lineno-block` analogue for `:ranges` windows — `-- range S-E --`
   headers followed by the human line-number gutter for each window."
  [ranges]
  (->> ranges
       (map (fn [{:keys [range lines]}]
              (let [[start end] range]
                (str "-- range " start
                     "-" end
                     " --" (when (seq lines) (str "\n" (render-lineno-block lines)))))))
       (str/join "\n\n")))

(defn tuples->ranges
  "Split flat `[[ln text]…]` tuples into contiguous `:ranges` windows\n   `[{:range [start end] :lines [[ln text]…]}…]`, breaking the run whenever\n   the line number jumps by more than 1. Produces exactly the shape\n   `render-lineno-range-block` / `render-hashline-range-block` consume, so a\n   flat tuple list (e.g. grouped grep hits) renders with the same\n   `-- range S-E --` gap headers as a native multi-range read."
  [tuples]
  (->> tuples
       (reduce (fn [groups [ln :as t]]
                 (let
                   [ln
                    (long ln)

                    g
                    (peek groups)]

                   (if g
                     (let [last-ln (long (first (peek g)))]
                       (if (= ln (inc last-ln)) (conj (pop groups) (conj g t)) (conj groups [t])))
                     (conj groups [t]))))
               [])
       (mapv (fn [g]
               {:range [(ffirst g) (first (peek g))] :lines g}))))

(defn- line-span->char-span
  "Convert a 0-based [line-start line-end) span to a [char-start char-end]
   substring span in `content`, keeping a trailing `\n` OUTSIDE the
   replaced region."
  [^String content ^long line-start ^long line-end]
  (let
    [char-start
     (char-offset-at-line content line-start)

     char-end-raw
     (char-offset-at-line content line-end)

     char-end
     (if (and (< char-end-raw (count content))
              (pos? char-end-raw)
              (= \newline (.charAt content (dec char-end-raw))))
       (dec char-end-raw)
       char-end-raw)]

    [char-start char-end]))

(defn indices-matching-hash
  "0-based indices of `lines` whose content `line-hash` equals the bare hash
   `h`. Pure content match — the line-number coordinate is applied separately
   by `resolve-one-anchor`, so this just answers \"which lines carry this
   content\"."
  [lines h]
  (let [h (str h)]
    (into []
          (keep-indexed (fn [i l]
                          (when (= h (line-hash l)) i)))
          lines)))

(def hash-line-drift-tolerance
  "How far (in lines) a content hash may sit from its stated line number
   before `resolve-one-anchor` calls the anchor MISPLACED and refuses. The
   common path never needs it — within one patch batch every hunk resolves
   against the file's ORIGINAL snapshot, so the stated line number is exact and
   the fast path fires. This window only forgives small drift when the model
   reuses anchors across patch calls without re-reading; anything larger (the
   ~200-line gap of a genuinely wrong/stale anchor — the corruption this whole
   scheme exists to stop) is refused so the model re-reads. Deliberately tight:
   a false refuse costs one re-read, a false accept corrupts the file."
  40)

(defn- unwrap-anchor
  "Tolerate an anchor that arrives wrapped in stray whitespace or quote chars — a
   common JSON/LLM mistake where the `lineno:hash` string is re-quoted, so
   `\"325:0e3\"` arrives WITH the literal quote characters and `parse-long` chokes
   on the leading quote. A real anchor is only digits, a colon and hex, so
   trimming surrounding whitespace + matching `\"'` quotes can never corrupt a
   valid one; it just lets the mis-quoted case parse (mirrors rg's arg coercion)."
  ^String [^String s]
  (-> s
      str/trim
      (str/replace #"^['\"`]+" "")
      (str/replace #"['\"`]+$" "")
      str/trim))

(defn- parse-anchor
  "Parse a `<line-number>:<hash>` anchor into `{:line L :hash H}` (L a 1-based
   long, H the hex content hash). The line number is REQUIRED: an anchor with no
   `:` separator (or a non-numeric line part) parses to `{:malformed true :raw S}`
   and `resolve-one-anchor` refuses it (`:hashline-malformed`). Every
   `:from_anchor` must carry BOTH coordinates so the line LOCATES and the hash
   VERIFIES."
  [anchor]
  (let
    [s
     (unwrap-anchor (str anchor))

     i
     (.indexOf s (int \:))

     line
     (when-not (neg? i) (parse-long (subs s 0 i)))]

    (if (and (not (neg? i)) line) {:line line :hash (subs s (inc i))} {:malformed true :raw s})))

(defn- resolve-one-anchor
  "Resolve a single parsed `{:line :hash}` anchor to a 0-based index in
   `lines`, or `{:error {:reason KW ...}}`. The LINE locates; the hash VERIFIES
   but a NON-UNIQUE hash never blocks a well-located edit:
     1. exact     - the stated line still hashes to `hash`          -> use it.
     2. drifted   - the line moved a little and `hash` is at EXACTLY one line
                    within `hash-line-drift-tolerance` of it        -> follow it.
     3. line wins - `hash` is AMBIGUOUS (matches several lines, at least one
                    near the stated line): the hash can't choose, but the model
                    addressed an EXPLICIT line, so use it. Duplicate hashes do
                    NOT make a `lineno:hash` anchor ambiguous.
     4. misplaced - `hash` matches only line(s) FAR from the stated line: a
                    strong line-vs-content contradiction -> refuse (WRONG-LINE
                    guard; this is what stops an edit landing on the wrong line).
     5. not-found - `hash` matches no live line (content is gone)   -> refuse.
   Plus: a malformed anchor (no `<lineno>:` prefix) or a line outside the file
   is refused — those genuinely cannot be located."
  [lines which {:keys [line hash malformed raw]}]
  (if malformed
    {:error {:reason :hashline-malformed :which which :anchor raw}}
    (let
      [idx0
       (dec (long line))

       n
       (long (count lines))]

      (cond (or (neg? idx0) (>= idx0 n))
            {:error {:reason :hashline-line-out-of-range :which which :line line :lines n}}
            ;; 1. exact — content at the stated line verifies the hash
            (= hash (line-hash (nth lines idx0))) {:index idx0}
            :else (let [matches (indices-matching-hash lines hash)]
                    (if (empty? matches)
                      ;; 5. content is gone — refuse, re-read. Carry the CURRENT anchor at
                      ;;    the stated line so the caller can recover in ONE step (the
                      ;;    common stale-after-`write` case) instead of a separate `cat`.
                      {:error {:reason :hashline-not-found
                               :which which
                               :hash hash
                               :stated-line line
                               :current-anchor (line-anchor line (nth lines idx0))
                               :current-text (nth lines idx0)}}
                      (let
                        [tol (long hash-line-drift-tolerance)
                         in-win (filterv (fn [i]
                                           (<= (Math/abs (- (inc (long i)) (long line))) tol))
                                  matches)]

                        (cond
                          ;; 2. drifted — one nearby match, follow the content
                          (= 1 (long (count in-win))) {:index (first in-win)}
                          ;; 4. hash matches only FAR from the stated line — WRONG-LINE guard
                          (empty? in-win) {:error {:reason :hashline-misplaced
                                                   :which which
                                                   :hash hash
                                                   :stated-line line
                                                   :found-lines (mapv #(inc (long %)) matches)}}
                          ;; 3. several nearby matches — hash can't disambiguate; the
                          ;;    explicit line wins (the user's `lineno:dup-hash` case)
                          :else {:index idx0}))))))))

(defn resolve-anchor-range
  "Resolve `from_anchor` (and `to_anchor`, defaulting to `from_anchor` for a single
   line) against LIVE `current`. Each is a `<line-number>:<hash>` anchor: the
   line number LOCATES it, the hash VERIFIES the content still matches AND sits
   near the stated line (else `:hashline-misplaced` — the wrong-line guard). Both
   coordinates are REQUIRED; a bare hash with no line number is refused
   (`:hashline-malformed`). Returns `{:from-line N :to-line N}` (1-based,
   INCLUSIVE) or `{:error {:reason KW …}}`.

   Shared by `resolve-anchor-edit-span` (WRITE — patch :from_anchor) and the cat
   `:anchor` READ path so both address lines identically."
  [^String current from_anchor to_anchor]
  (let
    [lines
     (split-content-lines current)

     from-a
     (parse-anchor from_anchor)

     to-a
     (if (or (nil? to_anchor) (= (str to_anchor) (str from_anchor)))
       from-a
       (parse-anchor to_anchor))

     fr
     (resolve-one-anchor lines :from from-a)]

    (if (:error fr)
      fr
      (let [tr (if (identical? from-a to-a) fr (resolve-one-anchor lines :to to-a))]
        (if (:error tr)
          tr
          (let
            [fi (long (:index fr))
             ti (long (:index tr))]

            (if (< ti fi)
              {:error {:reason :hashline-range-inverted :from-line (inc fi) :to-line (inc ti)}}
              {:from-line (inc fi) :to-line (inc ti)})))))))

(defn resolve-anchor-range-read
  "READ-tolerant twin of `resolve-anchor-range` for the `cat :anchor` path. A read
   is NON-DESTRUCTIVE, so a stale/missing hash must not block the look the way it
   (correctly) blocks a WRITE. Each anchor still resolves by CONTENT first —
   following small drift exactly like the write path — but when its hash matches no
   live line the anchor's LINE NUMBER is used as a fallback (a read can safely show
   whatever now sits at that line). Returns `{:from-line N :to-line N :stale? BOOL}`
   (1-based, INCLUSIVE; lines are ordered so the window never inverts; `:stale?` is
   true when any hash was missing/misplaced and its line number was used) — or
   `{:error …}` ONLY for a genuinely unlocatable anchor (`:hashline-malformed` — no
   line number — or `:hashline-line-out-of-range` — a line outside the file)."
  [^String current from_anchor to_anchor]
  (let
    [lines
     (split-content-lines current)

     n
     (long (count lines))

     resolve-read
     (fn [which anchor]
       (let
         [a
          (parse-anchor anchor)

          r
          (resolve-one-anchor lines which a)]

         (cond (:index r) (assoc r :stale? false)
               ;; Hash gone (content changed) or matches only far lines, but the
               ;; anchor still names an in-range line — fall back to it for the READ.
               (and (:line a)
                    (contains? #{:hashline-not-found :hashline-misplaced}
                               (get-in r [:error :reason]))
                    (<= 1 (long (:line a)) n))
               {:index (dec (long (:line a))) :stale? true}
               :else r)))

     fr
     (resolve-read :from from_anchor)]

    (if (:error fr)
      fr
      (let
        [tr (if (or (nil? to_anchor) (= (str to_anchor) (str from_anchor)))
              fr
              (resolve-read :to to_anchor))]
        (if (:error tr)
          tr
          (let
            [fi (long (:index fr))
             ti (long (:index tr))]

            {:from-line (inc (min fi ti))
             :to-line (inc (max fi ti))
             :stale? (boolean (or (:stale? fr) (:stale? tr)))}))))))

(defn resolve-anchor-edit-span
  "Resolve a content-addressed line-range edit to a CHAR SPAN against `current`,
   WITHOUT building new content: `{:start S :end E :replacement R :applied-line N}`
   or `{:error {:reason KW …}}`. Lets a multi-edit batch resolve every anchor
   against the ORIGINAL snapshot and splice all spans together atomically, so an
   earlier edit can't drift a later edit's hash/ordinal. `to_anchor` defaults to
   `from_anchor` (single line). The stated line is tried first and only then
   small drift is considered; duplicate hashes at other lines do not make an
   exact `lineno:hash` anchor ambiguous."
  [^String current from_anchor to_anchor ^String replace]
  (let [res (resolve-anchor-range current from_anchor to_anchor)]
    (if (:error res)
      res
      (let
        [line-start (dec (long (:from-line res)))
         line-end (long (:to-line res))]

        (if (= "" (str replace))
          ;; DELETION (empty replace): take the WHOLE physical line(s) including
          ;; the trailing newline, so the line is actually removed. The
          ;; `line-span->char-span` rule below deliberately keeps a matched
          ;; region's trailing `\n` OUTSIDE the span (so a REPLACE never doubles
          ;; the newline) — but for an empty replace that rule makes a single
          ;; blank-line delete a ZERO-WIDTH no-op and a multi-line delete leave
          ;; one line behind. Consuming the trailing newline here makes
          ;; `replace ""` mean "delete these lines" with no leftover blank.
          (let
            [char-start (char-offset-at-line current line-start)
             char-end (char-offset-at-line current line-end)]

            {:start char-start :end char-end :replacement "" :applied-line (inc line-start)})
          (let
            [[char-start char-end] (line-span->char-span current line-start line-end)
             ;; Only a NON-EMPTY span can end in a newline. An empty-line span is
             ;; zero-width (char-start == char-end); without this guard the check
             ;; reads the PREVIOUS line's `\n` (just before char-end) and wrongly
             ;; pads the replacement with a `\n`, inserting instead of replacing.
             matched-ends-nl? (and (< (long char-start) (long char-end))
                                   (= \newline (.charAt current (dec (long char-end)))))
             replace-ends-nl? (str/ends-with? replace "\n")
             rewritten
             (if (and matched-ends-nl? (not replace-ends-nl?)) (str replace "\n") replace)]

            {:start char-start
             :end char-end
             :replacement rewritten
             :applied-line (inc line-start)}))))))
