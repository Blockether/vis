(ns com.blockether.vis.internal.foundation.editing.balance
  "Delimiter rebalancing for an edit whose spliced result would NOT parse.

   An editor refusal is INFORMATION: the caller learns the replacement did not fit
   where it aimed it, and re-reads. A repair that runs on the REPLACEMENT ALONE
   cannot know that — it balances a fragment against nothing, so a partial line
   (`[{:keys [a b]}`, deliberately open because its enclosing form closes it) comes
   back \"repaired\" into a complete form, the splice accepts it, and the file parses
   while meaning something else. That trade is never worth it: it turns one honest
   refusal into a silent semantic rewrite the caller has to discover later.

   So the repair runs on the WHOLE spliced file, where the enclosing forms and the
   caller's own indentation decide where a delimiter belongs, and it is accepted
   only when it is provably confined to the lines the caller wrote:

     1. the repaired file parses clean;
     2. it keeps the same number of lines and the same final newline;
     3. every line it changes lies inside an edited span;
     4. it moved delimiters and whitespace only — every other character, in order,
        is still the caller's.

   Fail any one and the edit is REFUSED with its parse error intact. A repair that
   has to reach outside the edit is guessing about code nobody in this call wrote,
   and guessing is what produced the corruption above.

   The repair itself belongs to a language pack, not here: an `:ext/language-tools`
   entry registers `:balance-fn`, `String -> String | nil` (nil = unrepairable).
   This namespace owns only the decision to trust its answer, and it is handed the
   function — the LOOKUP is the editing tools' policy (`editing.core`), so the
   structural layer under them stays deterministic and pack-free."
  (:require [clojure.string :as str]))

(def ^:private delimiter?
  "The characters a rebalance is allowed to move. Anything else it touches is a
   rewrite, not a repair."
  #{\( \) \[ \] \{ \}})

(defn- skeleton
  "`s` with every delimiter and every whitespace character removed — what the
   caller actually WROTE. Two sources with the same skeleton differ only in
   delimiters and layout, which is the whole licence a repair has."
  ^String [^String s]
  (str/replace s #"[\s(){}\[\]]" ""))

(defn changed-span
  "The 1-based, inclusive `[from to]` line range of `after` that differs from
   `before` — shared leading and trailing LINES are dropped, so it is exactly the
   region one splice touched, whatever op produced it. nil when the two are equal.
   The comparison is per LINE, not per character: a character-level diff ends inside
   the first surviving line and would hand a repair one line of licence the edit
   never wrote. A caller that already knows which lines it wrote passes those
   instead; this is for the structural editors, which splice a byte range and count
   lines afterwards."
  [^String before ^String after]
  (when (not= before after)
    (let
      [b
       (str/split-lines before)

       a
       (str/split-lines after)

       ;; shared leading lines
       p
       (long (count (take-while true? (map = b a))))

       ;; shared trailing lines, never eating into the shared prefix
       s
       (long (count (take-while true? (map = (reverse (drop p b)) (reverse (drop p a))))))

       to
       (max p (- (count a) s))

       from
       (min (inc p) (max 1 to))]

      [from to])))

(defn- inside-spans?
  "True when 1-based `line` falls inside one of the `[from to]` line ranges the
   caller's own edits occupy in the NEW content."
  [^long line spans]
  (boolean (some (fn [[from to]]
                   (<= (long from) line (long to)))
                 spans)))

(defn- delimiter-note
  "What the repair did to ONE line, as the caller reads it: ``line 5193 added `]` ``. The
   note exists so a repair is never a silent footnote — the caller can see the
   exact character that was added and where, and refuse it with the next edit."
  [line-no ^String before ^String after]
  (let
    [tally
     (fn [^String s]
       (frequencies (filter delimiter? s)))

     b
     (tally before)

     a
     (tally after)

     runs
     (fn [from to]
       (apply str
         (for
           [[c n]
            (sort-by key from)

            :let [d
                  (- (long n) (long (get to c 0)))]
            :when (pos? d)]

           (apply str (repeat d c)))))

     added
     (runs a b)

     removed
     (runs b a)]

    (str "line "
         line-no
         (when (seq added) (str " added `" added "`"))
         (when (seq removed) (str " removed `" removed "`")))))

(defn rebalance
  "Try to make `source` — the content an edit WOULD have written, which does not
   parse — parse, by repairing its delimiters WITHOUT letting the repair reach past
   the caller's own lines. `spans` are `[from-line to-line]` pairs, 1-based and
   inclusive, in `source`'s own coordinates; `parses-clean?` re-parses a candidate;
   `balancer` is the language pack's `:balance-fn`.

   Answers nil when there is no balancer to ask, `{:ok? true :content S :notes [..]}`
   for a repair that may be written, and `{:ok? false :why msg}` for one that was
   found and REJECTED — the caller puts `why` in its refusal, because \"a repair
   exists but it reaches outside your edit\" is exactly what tells the caller to
   re-read the region instead of retrying the same replacement."
  [{:keys [balancer parses-clean? ^String source spans]}]
  (when (ifn? balancer)
    (let [candidate (try (balancer source) (catch Throwable _ nil))]
      (cond (or (not (string? candidate)) (= candidate source))
            {:ok? false :why "no delimiter repair was found"}
            (not (parses-clean? candidate))
            {:ok? false :why "a delimiter repair was found but it still would not parse"}
            (not= (skeleton source) (skeleton candidate))
            {:ok? false :why "the delimiter repair would rewrite code, not delimiters"}
            (not= (str/ends-with? source "\n") (str/ends-with? ^String candidate "\n"))
            {:ok? false :why "the delimiter repair would change the file's final newline"}
            :else (let
                    [before (str/split-lines source)
                     after (str/split-lines candidate)
                     changed (when (= (count before) (count after))
                               (into []
                                     (keep-indexed (fn [i l]
                                                     (when (not= l (nth after i)) (inc (long i)))))
                                     before))
                     outside (remove #(inside-spans? % spans) changed)]

                    (cond (not= (count before) (count after))
                          {:ok? false :why "the delimiter repair would add or drop lines"}
                          (seq outside) {:ok? false
                                         :why (str "a delimiter repair exists but it changes line "
                                                   (first outside)
                                                   ", outside the lines this call edited")}
                          :else {:ok? true
                                 :content candidate
                                 :notes (mapv #(delimiter-note %
                                                               (nth before (dec (long %)))
                                                               (nth after (dec (long %))))
                                              changed)}))))))
