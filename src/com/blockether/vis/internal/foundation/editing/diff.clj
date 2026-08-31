(ns com.blockether.vis.internal.foundation.editing.diff
  "Unified-diff rendering and line accounting, for BOTH producers of a diff.

   `patch` has always had the two texts and rendered its own hunks here; the
   confined sandbox filesystem now has them too, because a plain
   `open(path, \"w\")` from Python is an edit with a before and an after just
   like an anchored patch is. One renderer, so a hand-written file and a patched
   one reach Activity — and every surface reading it — in the SAME vocabulary.

   A leaf on purpose: `internal.sandbox-fs` is loaded while the GraalPy context
   is being built, long before the tool namespaces exist, so this requires
   nothing of vis."
  (:require [clojure.string :as str])
  (:import (com.github.difflib DiffUtils UnifiedDiffUtils)
           (com.github.difflib.patch AbstractDelta Chunk Patch)))

(def ^:const context-lines 3)

(def ^:private ^:const max-render-lines 240)

(def ^:private ^:const java-max-work 20000000)

(defn- estimated-size
  "Cheap O(n) LOWER BOUND on the Myers edit-script length: the size of the
   symmetric multiset difference of the two line bags. Sparse edits in a huge
   file score tiny; a full rewrite scores about n+m."
  [a b]
  (let [counts (reduce (fn [m line]
                         (update m line (fnil dec 0)))
                       (frequencies a)
                       b)]
    (long (reduce + 0 (map #(abs (long %)) (vals counts))))))

(defn- affordable?
  "Whether `java-diff-utils` may diff this pair. Myers costs about O(n*d), so a
   flat line-count cap punished a huge file with a one-line edit (the cheapest
   case) while waving through an expensive full rewrite just under the cap.
   Budget the actual work instead."
  [a b]
  (let [size
        (long (max (count a) (count b)))

        edits
        (max 1 (long (estimated-size a b)))]

    (<= (* size edits) (long java-max-work))))

(def ^:private ^:const min-hunk-lines 14)

(defn head-tail-cap
  "Bound a line vector to `limit`, keeping a HEAD and a TAIL window rather than a
   plain head-cut. A pure head-cut let a deletion-heavy preview fill the whole
   visible budget with `-` lines and bury the `+` replacement below the cut, so a
   correct edit read as a catastrophic deletion. `what`/`unit` name the truncation
   in the caller's own words: a diff loses lines, a patch report loses rows."
  ([lines ^long limit] (head-tail-cap lines limit "diff" "line"))
  ([lines ^long limit ^String what ^String unit]
   (let [lines
         (vec lines)

         n
         (long (count lines))]

     (if (<= n limit)
       lines
       (let [tail-n
             (quot limit 4)

             head-n
             (- limit tail-n)

             omitted
             (- n head-n tail-n)]

         (vec (concat (subvec lines 0 head-n)
                      [(str "... " what " truncated; " omitted " " unit "(s) omitted")]
                      (subvec lines (- n tail-n)))))))))

(defn- hunk-header? [line] (str/starts-with? (str line) "@@"))

(defn- split-diff-hunks
  "Split a unified diff into `[preamble hunks]`, each hunk a vector whose first
   element is its own `@@` header. A diff with no `@@` (pure add/delete preview)
   yields no hunks."
  [lines]
  (let [lines
        (vec lines)

        start
        (or (first (keep-indexed (fn [idx line]
                                   (when (hunk-header? line) idx))
                                 lines))
            (count lines))]

    [(subvec lines 0 start)
     (reduce (fn [hunks line]
               (if (hunk-header? line)
                 (conj hunks [line])
                 (cond-> hunks
                   (seq hunks)
                   (update (dec (count hunks)) conj line))))
             []
             (subvec lines start))]))

(defn- cap-hunk-lines
  "Bound ONE hunk to `budget` lines, ALWAYS keeping its `@@` header plus a head
   and a tail of its own body, so the hunk still reads as one connected region
   instead of two unrelated fragments."
  [hunk ^long budget]
  (let [n (long (count hunk))]
    (if (<= n budget)
      (vec hunk)
      (let [body (max 2 (dec budget))
            tail-n (max 1 (quot body 3))
            head-n (max 1 (- body tail-n))
            omitted (- n 1 head-n tail-n)]

        (if (pos? omitted)
          (vec (concat (subvec hunk 0 (inc head-n))
                       [(str "... " omitted " line(s) omitted in this hunk")]
                       (subvec hunk (- n tail-n))))
          (vec hunk))))))

(defn- cap-diff-lines
  "Bound a rendered diff to `max-render-lines` HUNK-WISE. Cutting the
   diff as one flat line list sliced through the middle of a hunk, so on a narrow
   screen the surviving head and tail looked like edits to two unrelated places.
   Each hunk keeps its own header, head and tail; whole hunks past the budget are
   dropped with an explicit count rather than half-shown."
  [lines]
  (let [lines
        (vec lines)

        n
        (long (count lines))]

    (if (<= n (long max-render-lines))
      lines
      (let [[preamble hunks] (split-diff-hunks lines)]
        (if (empty? hunks)
          (head-tail-cap lines max-render-lines)
          (let [budget (max (long min-hunk-lines) (- (long max-render-lines) (count preamble) 1))
                ;; Fill the budget hunk by hunk: a hunk is shown whole when it
                ;; fits, capped in place when a usable remainder is left, and the
                ;; rest are reported as a count instead of being half-rendered.
                [kept dropped]
                (loop [pending hunks
                       used 0
                       kept []]

                  (if-let [hunk (first pending)]
                    (let [remaining (- budget used)]
                      (cond (<= (count hunk) remaining)
                            (recur (next pending) (+ used (count hunk)) (into kept hunk))
                            (>= remaining (long min-hunk-lines))
                            (let [capped (cap-hunk-lines hunk remaining)]
                              (recur (next pending) (+ used (count capped)) (into kept capped)))
                            :else [kept (count pending)]))
                    [kept 0]))]

            (vec (concat preamble
                         kept
                         (when (pos? (long dropped))
                           [(str "... diff truncated; " dropped " more hunk(s) omitted")])))))))))

(defn- common-prefix-count
  [a b]
  (let [limit (long (min (count a) (count b)))]
    (loop [i 0]
      (if (and (< i limit) (= (a i) (b i))) (recur (inc i)) i))))

(defn- common-suffix-count
  [a b ^long prefix-count]
  (let [a-count
        (long (count a))

        b-count
        (long (count b))

        limit
        (- (min a-count b-count) prefix-count)]

    (loop [i 0]
      (if (and (< i limit) (= (a (- a-count i 1)) (b (- b-count i 1)))) (recur (inc i)) i))))

(defn- prefixed-diff-lines
  [prefix lines]
  (let [lines
        (vec lines)

        n
        (long (count lines))

        shown-n
        (min n (long max-render-lines))

        shown
        (subvec lines 0 shown-n)

        omitted
        (- n shown-n)]

    (cond-> (mapv #(str prefix %) shown)
      (pos? omitted)
      (conj (str prefix "... (" omitted " line(s) omitted)")))))

(defn- compact-diff-lines
  "Linear fallback for very large files. It is a bounded preview, not a
   minimal diff: for normal-sized files `java-diff-utils` renders real
   unified hunks."
  [a b]
  (let [prefix-count
        (long (common-prefix-count a b))

        suffix-count
        (long (common-suffix-count a b prefix-count))

        a-count
        (long (count a))

        b-count
        (long (count b))

        a-change-end
        (- a-count suffix-count)

        b-change-end
        (- b-count suffix-count)

        pre-start
        (max 0 (- prefix-count (long context-lines)))

        post-end
        (min a-count (+ a-change-end (long context-lines)))

        pre-lines
        (subvec a pre-start prefix-count)

        del-lines
        (subvec a prefix-count a-change-end)

        add-lines
        (subvec b prefix-count b-change-end)

        post-lines
        (subvec a a-change-end post-end)

        before-skip
        pre-start

        after-skip
        (- a-count post-end)]

    (vec (concat (when (pos? before-skip) [(str "... " before-skip " unchanged line(s) before")])
                 (map #(str " " %) pre-lines)
                 (prefixed-diff-lines "-" del-lines)
                 (prefixed-diff-lines "+" add-lines)
                 (map #(str " " %) post-lines)
                 (when (pos? after-skip) [(str "... " after-skip " unchanged line(s) after")])))))

(defn- java-unified-diff-lines
  "Unified hunks WITHOUT the `--- before` / `+++ after` file header pair: every
   renderer (TUI, companion app) already shows the path and colours each line by
   its `-`/`+` prefix, so the two header lines only ate screen space."
  [a b]
  (let [patch
        (DiffUtils/diff a b)

        lines
        (vec (UnifiedDiffUtils/generateUnifiedDiff "before" "after" a patch context-lines))]

    (if (and (>= (count lines) 2)
             (str/starts-with? (str (nth lines 0)) "---")
             (str/starts-with? (str (nth lines 1)) "+++"))
      (subvec lines 2)
      lines)))

(def ^:private diff-hunk-header-re #"^@@ -(\d+)(,\d+)? \+(\d+)(,\d+)? @@(.*)$")

(defn- shift-hunk-headers
  "Renumber `@@` headers produced from a WINDOW of the file back to real file
   line numbers."
  [lines ^long offset]
  (mapv (fn [line]
          (if-let [[_ a a-len b b-len trailing] (re-matches diff-hunk-header-re (str line))]
            (str "@@ -"
                 (+ offset (long (parse-long a)))
                 a-len
                 " +"
                 (+ offset (long (parse-long b)))
                 b-len
                 " @@"
                 trailing)
            line))
        lines))

(defn- windowed-unified-diff-lines
  "REAL unified hunks, at real file line numbers, for ANY file size. A huge file
   with a few small edits used to fall back to one flat delete-block plus one
   add-block spanning everything between the first and the last change, so a
   two-line edit rendered as hundreds of `-` lines of untouched code —
   disconnected nonsense on a narrow screen. Trim the shared prefix/suffix (minus
   context), diff only the changed window, and renumber the `@@` headers back to
   file lines. nil when even that window is too expensive to diff."
  [a b]
  (let [prefix-count
        (long (common-prefix-count a b))

        suffix-count
        (long (common-suffix-count a b prefix-count))

        start
        (max 0 (- prefix-count (long context-lines)))

        a-end
        (min (count a) (+ (- (count a) suffix-count) (long context-lines)))

        b-end
        (min (count b) (+ (- (count b) suffix-count) (long context-lines)))]

    (when (and (<= start a-end) (<= start b-end))
      (let [a-win
            (subvec a start a-end)

            b-win
            (subvec b start b-end)]

        (when (affordable? a-win b-win)
          (shift-hunk-headers (java-unified-diff-lines a-win b-win) start))))))

(defn- whole-file-rewrite?
  "True when NOTHING of the old content survives: no shared leading line and no
   shared trailing line. A real unified diff then degenerates into every old
   line as `-` immediately followed by every new line as `+` — the same file
   twice, with zero signal about what changed, because everything changed."
  [a b]
  (and (seq a)
       (seq b)
       (zero? (long (common-prefix-count a b)))
       (zero? (long (common-suffix-count a b 0)))))

(defn unified-diff-text
  "Unified diff preview for two file blobs: real `@@` hunks at real file line
   numbers, bounded hunk-wise. Only a change too expensive to diff (a full
   rewrite of a very large file) drops to the linear bounded preview.

   A WHOLE-FILE REWRITE (a Python whole-file write —
   nothing of the old content survives) renders ONE side only: the new content
   as `+` lines under a `--- (replaced, N line(s))` marker. Both sides there
   printed the file twice on every renderer (TUI and companion app read this
   same `\"diff\"` string), so the fix belongs here, not in the renderers."
  [before after]
  (cond (= before after) nil
        (nil? before) (str/join "\n" (prefixed-diff-lines "+" (str/split-lines (or after ""))))
        (nil? after) (str "--- (deleted, " (count (str/split-lines (or before ""))) " lines)")
        :else (let [a
                    (vec (str/split-lines before))

                    b
                    (vec (str/split-lines after))]

                (if (whole-file-rewrite? a b)
                  (str/join "\n"
                            (into [(str "--- (replaced, " (count a) " line(s))")]
                                  (prefixed-diff-lines "+" b)))
                  (str/join "\n"
                            (cap-diff-lines (or (windowed-unified-diff-lines a b)
                                                (compact-diff-lines a b))))))))

(defn- window-line-counts
  "Line counts from the common prefix/suffix WINDOW alone — the fallback for a
   pair too expensive for a real Myers diff. The overlapping part of the window
   counts as modified, the surplus on either side as added or removed."
  [a b]
  (let [pre
        (long (common-prefix-count a b))

        suf
        (long (common-suffix-count a b pre))

        removed
        (- (long (count a)) pre suf)

        added
        (- (long (count b)) pre suf)

        both
        (min removed added)]

    {"added" (- added both) "removed" (- removed both) "modified" both}))

(defn- delta-line-counts
  "Line counts from the real edit script: every delta is a source chunk replaced
   by a target chunk, so the overlapping lines are MODIFIED and only the surplus
   on one side is a pure addition or deletion."
  [a b]
  (let [^Patch patch (DiffUtils/diff a b)]
    (reduce (fn [acc ^AbstractDelta delta]
              (let [src (long (count (.getLines ^Chunk (.getSource delta))))
                    tgt (long (count (.getLines ^Chunk (.getTarget delta))))
                    both (min src tgt)]

                (-> acc
                    (update "modified" + both)
                    (update "removed" + (- src both))
                    (update "added" + (- tgt both)))))
            {"added" 0 "removed" 0 "modified" 0}
            (.getDeltas patch))))

(defn line-change-counts
  "`{\"added\" a \"removed\" r \"modified\" m}` for one file's before→after, or nil
   when nothing changed. The `:diff` is capped hunk-wise for rendering, so these
   counts are computed from the content itself and stay exact for the whole file
   even when the rendered diff is truncated. A new file is all additions, a
   deleted one all removals."
  [before after]
  (cond (= before after) nil
        (nil? before)
        {"added" (long (count (str/split-lines (or after "")))) "removed" 0 "modified" 0}
        (nil? after) {"added" 0 "removed" (long (count (str/split-lines before))) "modified" 0}
        :else (let [a
                    (vec (str/split-lines before))

                    b
                    (vec (str/split-lines after))]

                (if (affordable? a b) (delta-line-counts a b) (window-line-counts a b)))))

