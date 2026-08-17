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
      4. it only ADDED delimiters the caller omitted — one they WROTE is never
         deleted, moved or retyped — and every other character, whitespace and line
         endings included, is theirs, in order.

    Fail any one and the edit is REFUSED with its parse error intact. A repair that
    has to reach outside the edit is guessing about code nobody in this call wrote,
    and guessing is what produced the corruption above. DELETING a delimiter is the
    same guess from the other side: `(-> s str/trim)` whose opening paren was lost
    reads as `-> s str/trim)`, and dropping that surplus `)` — character for character
    the same mistake as an honest `)` too many — writes a body of three loose symbols
    that parses. The caller is told which of the two to look for instead.

   Add-only still leaves WHERE to guess: a balancer has only the caller's indentation
   to go on, so a closer omitted in the MIDDLE of a line comes back at that line's END
   and regroups the arguments between. The text the edit REPLACED settles it. Where a
   line survived the edit as the same code, every delimiter it dropped goes back
   exactly where that text had it: `(map? x) (str …)` retyped as `(map? x (str …)` is
   restored, not closed at the end, and a LOST OPENER stops being indistinguishable
   from a surplus closer, because the replaced text says which of the two it was. Only
   where the code around it survived — code the caller DELETED takes its own
   delimiters with it, and they are never resurrected inside what they wrote.

   That witness binds both ways. A repair may not ADD a delimiter to a line whose code
   survived either: the text it replaced already says which delimiters that code had, so
   a balancer closing an untouched line — because the line UNDER it dropped a closer of
   its own — is guessing against evidence, and `(if (seq names)` closed a second time
   parses while the branches it was meant to guard move into a form nobody wrote. That is
   refused too, and the caller is told which line still carries the omission.

    Both witnesses answer WHERE. When neither can — the balancer's answer reaches past the
    edit, or retypes something of the caller's — one candidate is left: the closers the text
    never wrote, appended at the END of the last line this call wrote. It is tried last, and
    only for a call that wrote ONE region, because appending closes everything between the
    omission and that seat into the form those closers close, and with a single edited region
    that is code THIS call wrote, handed back to it in the note. Two edits in one call have
    untouched lines between them, and those are never regrouped. How MANY of them is read off
    the lines AFTER that seat: they close what they close, and only what they cannot supply is
    added — save for a call with no replaced text to bound its own lines, where nothing short of
    closing everything they left open is a claim this namespace will make.

    A dropped \" is the same mistake one character over, and no balancer can see it: a repair
    only puts back `()[]{}`, while every string after the missing quote is inside out. Where
    the text a region REPLACED ended with a quote and the line standing there now does not,
    that one quote goes back at the same seat, under its own rule — never a general balancing
    of strings, whose misplacement parses just as happily as the omission.

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

(def ^:private opener?
  "The half of `delimiter?` that OPENS. An opener belongs before the code it opens and
   a closer after the code it closes, which is the only thing that says where a
   delimiter goes back when the edit also wrote new code beside it."
  #{\( \[ \{})

(defn- delimiter-char?
  "`delimiter?` asked of one PRIMITIVE character. Every scan here runs a whole file
   through this question, and a set membership test boxes one Character per character to
   answer what a jump table answers for nothing."
  [c]
  (case c
    (\( \) \[ \] \{ \})
    true

    false))

(defn- space-char?
  "The six characters Java's `\\s` matches — space, tab, newline, vertical tab, form feed
   and carriage return — so `skeleton` drops exactly what the regex it replaced dropped,
   and no Unicode separator that regex never saw."
  [c]
  (case c
    (\space \tab \newline \u000B \formfeed \return)
    true

    false))

(defn- skeleton
  "`s` with every delimiter and every whitespace character removed — what the
   caller actually WROTE. Two sources with the same skeleton differ only in
   delimiters and layout, which is the whole licence a repair has."
  ^String [^String s]
  (let
    [n
     (.length s)

     sb
     (StringBuilder. n)]

    (dotimes [i n]
      (let [c (.charAt s i)]
        (when-not (or (space-char? c) (delimiter-char? c)) (.append sb c))))
    (.toString sb)))

(defn- delimiters
  "Every delimiter character of `s`, in order — the SHAPE the caller wrote, with the
   code between them dropped."
  [^String s]
  (let [n (.length s)]
    (loop
      [i 0
       acc (transient [])]

      (if (< i n)
        (let [c (.charAt s i)]
          (recur (inc i) (if (delimiter-char? c) (conj! acc c) acc)))
        (persistent! acc)))))

(defn- unterminated-string
  "The 1-based line where a string literal OPENS and is never closed, or nil when every
   string in `s` closes. Scanned the way the reader reads: a backslash escapes the next
   character inside a string and makes a character literal outside one, so the character
   double-quote opens nothing, and a semicolon starts a comment only where no string is
   open."
  [^String s]
  (let [n (long (count s))]
    (loop
      [i (long 0)
       line (long 1)
       opened nil
       in-string? false
       in-comment? false
       escaped? false]

      (if (>= i n)
        (when in-string? opened)
        (let
          [c (.charAt s (int i))
           next-line (if (= c \newline) (inc line) line)]

          (cond escaped? (recur (inc i) next-line opened in-string? in-comment? false)
                in-string? (cond (= c \\) (recur (inc i) line opened true false true)
                                 (= c \") (recur (inc i) line nil false false false)
                                 :else (recur (inc i) next-line opened true false false))
                in-comment? (recur (inc i) next-line opened false (not= c \newline) false)
                (= c \\) (recur (inc i) line opened false false true)
                (= c \") (recur (inc i) line line true false false)
                (= c \;) (recur (inc i) line opened false true false)
                :else (recur (inc i) next-line opened false false false)))))))

(defn- open-string-why
  "Why nothing can be repaired when the text leaves a STRING open — the message, or nil
   when every string closes. A repair only puts back `()[]{}`, so a dropped quote is a
   different mistake with a different fix: a caller told only that no repair was found
   goes hunting for a bracket that is not missing, while the quote is on the line named
   here."
  ^String [^String s]
  (when-let [line (unterminated-string s)]
    (str "no delimiter repair is possible: line "
         line
         " opens a string that is never closed, and a repair only puts back `()[]{}`")))
(defn- open-stack
  "The delimiters `s` leaves open, outermost first — `[]` for a text that closes everything it
   opened. nil when a closer arrives that closes nothing or closes the wrong opener, and when a
   string is left open, because a quote swallows delimiters this scan may not count.

   Read the way `unterminated-string` reads, and on the same terms: a delimiter inside a
   string, a comment or a character literal is text, not structure."
  [^String s]
  (let [n (long (count s))]
    (loop
      [i (long 0)
       stack []
       in-string? false
       in-comment? false
       escaped? false]

      (if (>= i n)
        (when-not in-string? stack)
        (let [c (.charAt s (int i))]
          (cond escaped? (recur (inc i) stack in-string? in-comment? false)
                in-string? (cond (= c \\) (recur (inc i) stack true false true)
                                 (= c \") (recur (inc i) stack false false false)
                                 :else (recur (inc i) stack true false false))
                in-comment? (recur (inc i) stack false (not= c \newline) false)
                (= c \\) (recur (inc i) stack false false true)
                (= c \") (recur (inc i) stack true false false)
                (= c \;) (recur (inc i) stack false true false)
                (= c \() (recur (inc i) (conj stack \)) false false false)
                (= c \[) (recur (inc i) (conj stack \]) false false false)
                (= c \{) (recur (inc i) (conj stack \}) false false false)
                (delimiter-char? c) (let [^Character top (peek stack)]
                                      (when (and top (= c (.charValue top)))
                                        (recur (inc i) (pop stack) false false false)))
                :else (recur (inc i) stack false false false)))))))

(defn- subsequence?
  "True when every element of `a` appears in `b`, in order — `b` is `a` with things
   INSERTED and nothing else."
  [a b]
  (loop
    [a
     (seq a)

     b
     (seq b)]

    (cond (nil? a) true
          (nil? b) false
          (= (first a) (first b)) (recur (next a) (next b))
          :else (recur a (next b)))))

(defn- additions-only?
  "True when the repair only ADDED delimiters: `before` still appears in `after`, in
   order, so every character the caller wrote survives and the repair supplied what
   they omitted. Deleting one they wrote is refused — not because it is always wrong,
   but because it is never distinguishable. `(f a))` with one `)` too many and `f a))`
   with its opener lost are the same string to a balancer, and dropping the surplus
   repairs the first while quietly turning the second into loose symbols that parse.
   Adding is safe in a way dropping is not: the caller's own indentation says where an
   omitted closer belongs, and nothing of theirs is overwritten to put it there.

   A repair that is neither — one that MOVED or RETYPED a delimiter — fails this too:
   `(foo [1 2] 3)` mistyped as `(foo (1 2] 3)` comes back as `(foo (1 2 3))`, a vector
   turned into a call that swallowed the argument after it, with the skeleton and the
   line count untouched."
  [before after]
  (subsequence? before after))

(defn- undelimited
  "`s` with every delimiter removed — every OTHER character, whitespace and line endings
   included, exactly as it stands. Two texts that agree here differ in delimiters alone.

   `skeleton` drops whitespace as well, so nothing else in this decision can see the two
   rewrites that keep the code, the delimiters, the line count and the final newline and
   rewrite the file anyway: a repair that RE-INDENTS the lines it closed, and one that
   normalizes a file's CRLF line endings to LF — every line of a file the call asked to
   put ONE delimiter back into."
  ^String [^String s]
  (let
    [n
     (.length s)

     sb
     (StringBuilder. n)]

    (dotimes [i n]
      (let [c (.charAt s i)]
        (when-not (delimiter-char? c) (.append sb c))))
    (.toString sb)))
(defn- surplus
  "The delimiters `a` has and `b` does not, as a string in delimiter order: `((a))`
   over `(a)` answers `()`."
  ^String [a b]
  (let
    [ta
     (frequencies a)

     tb
     (frequencies b)]

    (apply str
      (for
        [[ch n]
         (sort-by key ta)

         :let [d
               (- (long n) (long (get tb ch 0)))]
         :when (pos? d)]

        (apply str (repeat d ch))))))

(defn- direction-why
  "Why a repair that is not additions-only is refused, told apart so the caller knows
    which mistake of theirs to go and look for. `subject` names WHOSE delimiters these
    are — the replacement an edit wrote, or a whole file a formatter was handed."
  [^String source ^String candidate ^String subject]
  (let
    [s
     (delimiters source)

     c
     (delimiters candidate)]

    (if (subsequence? c s)
      (str "the delimiter repair would delete `"
           (surplus s c)
           "` "
           subject
           ": it closes more than it opens, or an opener was lost")
      (str "the delimiter repair would move or retype a delimiter " subject))))
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

(defn- excerpt
  "One line as the caller should re-read it: indentation dropped, long lines cut,
   because the note rides on the status line."
  ^String [^String line]
  (let [t (str/trim line)]
    (if (> (count t) 56) (str (subs t 0 55) "…") t)))

(defn- delimiter-note
  "What the repair did to ONE line, as the caller reads it:
   ``line 5193 added `]` → `(let [{:keys [a b]}]` ``. The note exists so a repair is
   never a silent footnote, and it carries the RESULTING line and not just the
   character. With the replaced text to seat it from, that line is the one the caller
   meant; without it — a formatter, or a line this edit rewrote — a closer omitted in
   the middle comes back at the line's END, and `(count names \"at\" stamp` closing as
   `(count names \"at\" stamp)` regroups the arguments between. Reading the line is how
   the caller catches that in one glance."
  [line-no ^String before ^String after]
  (let
    [b
     (delimiters before)

     a
     (delimiters after)

     added
     (surplus a b)

     removed
     (surplus b a)]

    (str "line "
         line-no
         (when (seq added) (str " added `" added "`"))
         (when (seq removed) (str " removed `" removed "`"))
         " → `"
         (excerpt after)
         "`")))

(def ^:private ^:const align-max-cells
  "The largest table `align` will fill. Past it the two texts are too far apart to be one
   edited into the other, and the repair falls back to the balancer's own answer."
  1000000)

(defn- element-keys
  "One `int` key per element of `xs` — the character itself for a string, the element's
   hash for a vector of lines. Keys are what makes `align`'s table primitive: a cell
   compares two ints instead of two boxed characters or two whole lines."
  ^ints [xs]
  (if (string? xs)
    (let
      [^String s
       xs

       n
       (.length s)

       out
       (int-array n)]

      (dotimes [i n]
        (aset out i (int (.charAt s i))))
      out)
    (let
      [^objects a
       (to-array xs)

       n
       (alength a)

       out
       (int-array n)]

      (dotimes [i n]
        (aset out i (int (hash (aget a i)))))
      out)))

(defn- align
  "A longest-common-subsequence alignment of `a` and `b`: the `[i j]` index pairs of
   the elements they share, in order. Answers nil when the two are too large to align
   in one table, and the repair falls back to the balancer's own answer.

   Two lines (characters) or two files' lines (strings) — either way the table is filled
   over `element-keys` and every key match is CONFIRMED against the elements themselves,
   because two different lines can hash alike and a repair may not pair them."
  [a b]
  (let
    [m
     (long (count a))

     n
     (long (count b))]

    (when (and (pos? m) (pos? n) (<= (* m n) (long align-max-cells)))
      (let
        [^ints ka
         (element-keys a)

         ^ints kb
         (element-keys b)

         ^objects av
         (when-not (string? a) (to-array a))

         ^objects bv
         (when-not (string? b) (to-array b))

         same?
         (fn [^long i ^long j]
           (and (== (aget ka i) (aget kb j)) (or (nil? av) (= (aget av i) (aget bv j)))))

         w
         (inc n)

         ;; t[i][j] is the length of the longest common subsequence of the suffixes
         ;; a[i..] and b[j..], so the walk below can read it forwards.
         t
         (int-array (* (inc m) w))]

        (dotimes [ii m]
          (let [i (- m 1 (long ii))]
            (dotimes [jj n]
              (let [j (- n 1 (long jj))]
                (aset t
                      (+ (* i w) j)
                      (if (same? i j)
                        (inc (aget t (+ (* (inc i) w) (inc j))))
                        (max (aget t (+ (* (inc i) w) j)) (aget t (+ (* i w) (inc j))))))))))
        (loop
          [i
           0

           j
           0

           acc
           []]

          (cond (or (== i m) (== j n)) acc
                (same? i j) (recur (inc i) (inc j) (conj acc [i j]))
                (>= (aget t (+ (* (inc (long i)) w) (long j)))
                    (aget t (+ (* (long i) w) (inc (long j)))))
                (recur (inc i) j acc)
                :else (recur i (inc j) acc)))))))

(defn- lcs-length
  "How many characters `a` and `b` share, in order — the LENGTH of what `align` would
   answer, without the table it answers from. `similar?` asks this of every candidate pair
   of lines in an edit and needs the number alone, so one rolling row replaces a table of
   `a` by `b` cells and the walk back through it. 0 when the two are too large to compare,
   which is `align` answering nothing."
  ^long [^String a ^String b]
  (let
    [m
     (long (.length a))

     n
     (long (.length b))]

    (if (or (zero? m) (zero? n) (> (* m n) (long align-max-cells)))
      0
      (let [row (int-array (inc n))]
        (dotimes [ii m]
          (let [i (- m 1 (long ii))]
            (loop
              [j (dec n)
               diag 0]

              (when (>= j 0)
                (let
                  [prev (aget row j)
                   v (if (== (int (.charAt a (int i))) (int (.charAt b (int j))))
                       (inc (long diag))
                       (max (aget row (inc j)) prev))]

                  (aset row j v)
                  (recur (dec j) prev))))))
        (long (aget row 0))))))

(defn- splice
  "`s` with each `[index char]` of `seats` inserted BEFORE that index — the only
   mutation a reseat performs, so nothing the caller wrote can be overwritten."
  ^String [seats ^String s]
  (let [sb (StringBuilder.)]
    (loop
      [i 0
       remaining (seq seats)]

      (cond (and remaining (= (long (first (first remaining))) (long i)))
            (do (.append sb ^char (second (first remaining))) (recur i (next remaining)))
            (< (long i) (count s)) (do (.append sb (.charAt s i)) (recur (inc i) remaining))
            :else nil))
    (.toString sb)))

(defn- reseat-line
  "`wrote` with every delimiter it dropped from `replaced` put back WHERE that line
   had it. A delimiter is put back only when everything else the edit did not keep
   around it is delimiters and whitespace: code the caller deleted takes its own
   delimiters with it. nil when nothing was dropped."
  ^String [^String replaced ^String wrote]
  (when-let [pairs (align replaced wrote)]
    (let
      [seats (loop
               [remaining (concat pairs [[(count replaced) (count wrote)]])
                ;; the next unmatched character of `replaced`, and the place in `wrote` just
                ;; after the previous match — the two edges of the same hole.
                was-from 0
                wrote-from 0
                acc []]

               (if-let [[was-at wrote-at] (first remaining)]
                 (let
                   [gap (subs replaced was-from (long was-at))
                    dropped (filterv delimiter? gap)
                    seat
                    (if (and (seq dropped) (every? opener? dropped)) wrote-from (long wrote-at))]

                   (recur (next remaining)
                          (inc (long was-at))
                          (inc (long wrote-at))
                          (if (every? #(or (delimiter? %) (Character/isWhitespace ^char %)) gap)
                            (into acc
                                  (map (fn [ch]
                                         [seat ch]))
                                  dropped)
                            acc)))
                 acc))]
      (when (seq seats) (splice seats wrote)))))

(defn- similar?
  "True when two lines are the SAME line, edited: their code shares more than half of
   the longer one's characters. Below that they are different lines that happen to fall
   at the same offset, and nothing the older one had can be trusted onto the newer."
  [^String was ^String now]
  (let
    [a
     (skeleton was)

     b
     (skeleton now)

     longer
     (max (count a) (count b))]

    (and (pos? longer) (> (* 2 (lcs-length a b)) longer))))

(defn- middles
  "The lines `original` and `source` do NOT share at either end, as `[head was now]`: `head`
   is how many leading lines they have in common, `was` the lines this edit replaced and
   `now` the lines it wrote in their place. A shared line at either end is the file AROUND
   the edit, and nothing about it is evidence of what the edit did."
  [^String original ^String source]
  (let
    [was
     (vec (str/split-lines original))

     now
     (vec (str/split-lines source))

     head
     (long (count (take-while true? (map = was now))))

     tail
     (long (count (take-while true?
                              (map = (reverse (subvec was head)) (reverse (subvec now head))))))]

    [head (subvec was head (- (count was) tail)) (subvec now head (- (count now) tail))]))

(defn- paired-lines
  "Which line of `original` each line of `source` IS, as `{:line :replaced :wrote
   :same-code?}` maps whose `:line` is 1-based in `source`.

   Two passes, because two different questions are being asked. Lines whose `skeleton`
   matches are anchors: their code is untouched, so the only difference is delimiters
   and whitespace, and what the replaced text says about them is fact — `:same-code?`.
   Between two anchors, when the edit left as many lines as it found, each is paired
   with the line it stands in for as long as the two are still `similar?`: the edit
   REWROTE that line, so its delimiters are its own business, but the text it replaced
   still says where a delimiter it KEPT used to sit. A line inserted or deleted has no
   pair at all, and nothing here can speak for it. Blank lines pair with anything and
   say nothing, so they are dropped."
  [^String original ^String source]
  (let
    [[head was-mid now-mid]
     (middles original source)

     anchors
     (when (and (seq was-mid) (seq now-mid))
       (align (mapv skeleton was-mid) (mapv skeleton now-mid)))

     indexes
     (loop
       [remaining
        (concat anchors [[(count was-mid) (count now-mid)]])

        was-at
        0

        now-at
        0

        acc
        []]

       (if-let [[was-to now-to] (first remaining)]
         (let
           [gap (- (long was-to) (long was-at))
            filled (if (and (pos? gap) (= gap (- (long now-to) (long now-at))))
                     (into acc
                           (map (fn [k]
                                  [(+ (long was-at) (long k)) (+ (long now-at) (long k)) false]))
                           (range gap))
                     acc)]

           (recur (next remaining)
                  (inc (long was-to))
                  (inc (long now-to))
                  (cond-> filled
                    (< (long was-to) (count was-mid))
                    (conj [(long was-to) (long now-to) true]))))
         acc))]

    (into []
          (comp (map (fn [[was-at now-at same?]]
                       {:line (inc (+ (long head) (long now-at)))
                        :replaced (nth was-mid was-at)
                        :wrote (nth now-mid now-at)
                        :same-code? same?}))
                (remove (fn [{:keys [replaced wrote same-code?]}]
                          (or (str/blank? (skeleton replaced))
                              (and (not same-code?) (not (similar? replaced wrote)))))))
          indexes)))

(defn- terminated-lines
  "`s` cut into lines that KEEP their own ending. `str/split-lines` drops the `\r` of a
   CRLF file, so a file rebuilt by joining its answer comes back with every line ending
   in the file normalized — a whole-file rewrite from a call that asked to put back ONE
   delimiter, and one no rule that compares code, delimiters or line counts can see."
  [^String s]
  (vec (re-seq #"[^\n]*\n|[^\n]+" s)))

(defn- line-ending
  "The terminator `line` carries — `\r\n`, `\n`, or nothing for a last line that ends the
   file without one."
  ^String [^String line]
  (cond (str/ends-with? line "\r\n") "\r\n"
        (str/ends-with? line "\n") "\n"
        :else ""))
(defn- reseat
  "`source` with every delimiter its `paired-lines` dropped from the text they REPLACED
   put back WHERE that text had it — the only way a closer omitted in the MIDDLE of a
   line goes back in the middle instead of at its end. nil when nothing was dropped."
  ^String [pairs ^String source]
  (let
    [now
     (terminated-lines source)

     reseated
     (reduce (fn [acc {:keys [line replaced wrote]}]
               (if (= replaced wrote)
                 acc
                 (if-let [seated (reseat-line replaced wrote)]
                   (assoc acc (dec (long line)) seated)
                   acc)))
             {}
             pairs)]

    (when (seq reseated)
      (apply str
        (reduce-kv (fn [acc i seated]
                     (assoc acc i (str seated (line-ending (nth acc i)))))
                   now
                   reseated)))))

(defn- written-lines
  "The 1-based, inclusive `[from to]` lines of `source` this call actually WROTE, from the
   text it replaced. `[1 n]` when there is no replaced text to compare against — every line
   is then the edit's own as far as anything here can tell — and an empty range when the
   edit only deleted, which leaves no line of its own to put anything on."
  [^String original ^String source lines]
  (if (string? original)
    (let [[head _ now-mid] (middles original source)]
      [(inc (long head)) (+ (long head) (count now-mid))])
    [1 (count lines)]))

(defn- span-seat
  "Where a delimiter this call omitted may be APPENDED: the index into `lines` of the last
   line carrying anything inside `[from to]` and inside the ONE span this call wrote, or nil.

   One span, because appending closes everything between the omission and the seat into the
   form those closers close. With a single edited region that is code THIS call wrote, and
   the note hands the caller the line it produced. Two edits in one call have untouched
   lines between them, and a closer appended after the second would regroup code nobody
   here wrote — the same guess this namespace refuses everywhere else."
  [spans lines [from to]]
  (when (= 1 (count spans))
    (last (keep-indexed (fn [i ^String l]
                          (let [n (inc (long i))]
                            (when (and (<= (long from) n (long to))
                                       (inside-spans? n spans)
                                       (not (str/blank? l)))
                              i)))
                        lines))))

(defn- append-at
  "`lines` rebuilt with `s` appended to line `idx`, after its last non-blank character and
   before whatever ends the line — so nothing already on it moves, its trailing whitespace
   is untouched and a CRLF file keeps its endings."
  ^String [lines ^long idx ^String s]
  (let
    [^String line
     (nth lines idx)

     ending
     (line-ending line)

     body
     (subs line 0 (- (count line) (count ending)))

     code
     (str/trimr body)]

    (apply str (assoc (vec lines) idx (str code s (subs body (count code)) ending)))))

(defn- seat-closers
  "The closers to APPEND at line `idx` so the file closes again, or nil.

   Taken from the top of what stands open at the seat — innermost first — and never more than
   this call opened after line `from`, so a form the edit found already open is never closed on
   its behalf. HOW MANY of them depends on what stands behind the call:

     with a witness  the text this call replaced bounds its own lines, so the tail is read as it
                     is written and as FEW closers are added as make the file close — the lines
                     after the seat carry some of them already, and a block dropped into the
                     middle of a `{…}` leaves those lines closing in an order the file no longer
                     has;
     without one     nothing says which forms are this call's, so the only claim left is that
                     its lines close everything they opened — all of it, or no repair at all.

   nil when that run of closers does not close the file, which is also the answer to one closer
   too MANY: a surplus and a lost opener are the same string here, and nothing tells them apart."
  ^String [lines ^long from ^long idx witness?]
  (let
    [before
     (open-stack (apply str (take (dec from) lines)))

     through
     (open-stack (apply str (take (inc idx) lines)))

     closes
     (fn [k]
       (let [closers (apply str (reverse (take-last k through)))]
         (when-let [remaining (open-stack (append-at lines idx closers))]
           (when (empty? remaining) closers))))]

    (when (and before
               through
               (< (count before) (count through))
               (= (vec before) (vec (take (count before) through))))
      (let [opened (- (count through) (count before))]
        (if witness? (first (keep closes (range 1 (inc opened)))) (closes opened))))))

(defn- closed-at-tail
  "`source` with the closers the lines this call WROTE left open appended to the last of them —
   the candidate of last resort, when neither the replaced text nor the balancer's indentation
   could say where they belong. nil when the call closed everything it opened, when it closes
   more than it opens, or when it wrote no single line to put them on."
  ^String [spans ^String original ^String source]
  (let
    [lines
     (terminated-lines source)

     written
     (written-lines original source lines)]

    (when-let [idx (span-seat spans lines written)]
      (when-let [missing (seat-closers lines (first written) idx (string? original))]
        (append-at lines idx missing)))))

(defn- quoted-tail?
  "True when the last line of `lines` that carries anything ends with a `\"` — the text a
   docstring or a string literal ends with."
  [lines]
  (boolean (when-let [^String l (last (remove str/blank? lines))]
             (str/ends-with? (str/trimr l) "\""))))

(defn- requoted
  "`source` with the `\"` this edit dropped put back at the end of the last line it wrote —
   nil unless a string is left open AND the text this edit replaced ended with a quote where
   the line standing there now does not.

   A dropped quote is not a missing bracket: every string after it is inside out, the reader
   stops somewhere else entirely, and no balancer can help because a repair only puts back
   `()[]{}`. The replaced text is the only witness that a quote ended that region, and it is
   the whole licence for this candidate — which is why it is one quote, at that one seat,
   and never a general balancing of strings."
  ^String [spans ^String original ^String source]
  (when (and (string? original) (unterminated-string source))
    (let
      [lines
       (terminated-lines source)

       [_ was-mid _]
       (middles original source)

       idx
       (span-seat spans lines (written-lines original source lines))]

      (when (and idx
                 (quoted-tail? was-mid)
                 (not (str/ends-with? (str/trimr ^String (nth lines idx)) "\"")))
        (append-at lines idx "\"")))))

(defn- substitution
  "The first delimiter `wrote` has that `replaced` did not, as `[typed had]`: the
   character the edit typed and the one it stood in for, `had` nil when the line simply
   has one delimiter more. nil when `wrote` only OMITTED delimiters `replaced` had —
   the one mistake a repair can put right.

   A RETYPED delimiter is the third way to break a line, and the only one nothing
   downstream can see: `[a b]` typed as `[a b(` is balanced by closing the `(`, and
   `[a b ()]` parses. Every rule holds — code untouched, delimiters only added, inside
   the edited lines — and the meaning is still gone. The text that line replaced is the
   only witness that the `(` was never meant, and it is why a repair that touches such
   a line is refused instead of guessed at."
  [^String replaced ^String wrote]
  (loop
    [a
     (seq (delimiters wrote))

     b
     (seq (delimiters replaced))

     skipped
     nil]

    (cond (nil? a) nil
          (nil? b) [(first a) skipped]
          (= (first a) (first b)) (recur (next a) (next b) nil)
          :else (recur a (next b) (or skipped (first b))))))

(defn- substitution-why
  "Why a repair is refused on a line whose code the edit KEPT: the line holds a
   delimiter the text it replaced did not, so the repair would close something the
   caller never opened. Both characters are named, and so is the line the repair would
   have written — the caller reads it and sees at once that it is not what they meant."
  [line-no [typed had] ^String repaired ^String subject]
  (str "the delimiter repair would close `"
       typed
       "` "
       subject
       " on line "
       line-no
       (if had
         (str ", where the text it replaced had `" had "`")
         ", one more than the text it replaced has")
       ": that delimiter was retyped or added, not omitted, and closing it regroups the"
       " line into `"
       (excerpt repaired)
       "`"))

(defn- invention
  "The delimiter a repair would add to a line whose CODE the edit left alone and whose
   replaced text never had it, as a string — nil when the repair only puts back
   delimiters that text proves were omitted.

   A line the edit did not rewrite carries its own witness: the text it replaced says
   exactly which delimiters that code had. Adding one it did not have is the balancer
   reading INDENTATION against that witness — `(if (seq names)` closed a second time
   because the line under it dropped a closer of its own — and the result parses, so
   nothing downstream catches it. The omission is real; it is on another line."
  ^String [^String replaced ^String repaired]
  (let
    [had
     (delimiters replaced)

     now
     (delimiters repaired)]

    (when-not (subsequence? now had) (surplus now had))))

(defn- invention-why
  "Why a repair is refused for adding a delimiter to a line the edit KEPT. The replaced
   text is quoted because it is the evidence: that code never carried this delimiter, so
   the caller's omission is on a line they actually wrote, and that is where to look."
  [line-no ^String added ^String replaced]
  (str "a delimiter repair exists but it adds `"
       added
       "` to line "
       line-no
       ", whose code this edit did not change — the text it replaced was `"
       (excerpt replaced)
       "` and never had that delimiter, so what this call omitted is on another line"))

(defn- changed-lines
  "The 1-based numbers of the lines where `after` differs from `before`, two line vectors of
   the same length — nil when they are not, because a repair that changed the line COUNT is a
   different mistake and is refused as one."
  [before after]
  (when (= (count before) (count after))
    (into []
          (keep-indexed (fn [i l]
                          (when (not= l (nth after i)) (inc (long i)))))
          before)))

(defn- verdict
  "Whether `candidate` may be written in place of `source`. Every rule is a licence the
   caller gave: their code untouched (`skeleton`), their delimiters untouched
   (`additions-only?`), their lines only (`spans`), their file's shape (line count and
   final newline). Where the text a line REPLACED is known it is the strictest licence of
   all: on a line whose code the edit kept, a repair may only put back delimiters that
   text had — one the edit typed instead (`substitution`) and one the balancer invents
   from indentation (`invention`) both regroup code nobody in this call rewrote. Fail one
   and the repair is REJECTED, naming the mistake to look for."
  [{:keys [parses-clean? ^String source spans subject pairs]} candidate]
  (cond (or (not (string? candidate)) (= candidate source))
        {:ok? false :why (or (open-string-why source) "no delimiter repair was found")}
        (not (parses-clean? candidate))
        {:ok? false
         :why (or (open-string-why source)
                  "a delimiter repair was found but it still would not parse")}
        (not= (skeleton source) (skeleton candidate))
        {:ok? false :why "the delimiter repair would rewrite code, not delimiters"}
        (not (additions-only? (delimiters source) (delimiters candidate)))
        {:ok? false :why (direction-why source candidate (or subject "this edit wrote"))}
        (not= (str/ends-with? source "\n") (str/ends-with? ^String candidate "\n"))
        {:ok? false :why "the delimiter repair would change the file's final newline"}
        :else
        (let
          [before
           (str/split-lines source)

           after
           (str/split-lines candidate)

           changed
           (changed-lines before after)

           outside
           (remove #(inside-spans? % spans) changed)

           paired
           (into {}
                 (comp (filter :same-code?)
                       (map (fn [{:keys [line replaced wrote]}]
                              [(long line) [replaced wrote]])))
                 pairs)

           typed
           (some (fn [n]
                   (when-let [[replaced wrote] (get paired (long n))]
                     (when-let [s (substitution replaced wrote)]
                       [n s])))
                 changed)

           invented
           (some (fn [n]
                   (when-let [[replaced _] (get paired (long n))]
                     (when-let [c (invention replaced (nth after (dec (long n))))]
                       [n c replaced])))
                 changed)]

          (cond (not= (count before) (count after))
                {:ok? false :why "the delimiter repair would add or drop lines"}
                (seq outside) {:ok? false
                               :why (str "a delimiter repair exists but it changes line "
                                         (first outside)
                                         ", outside the lines this call edited")}
                (not= (undelimited source) (undelimited candidate))
                {:ok? false
                 :why (str "the delimiter repair would change whitespace " (or subject
                                                                               "this edit wrote")
                           ": it re-indents or re-ends lines instead of only putting back "
                           "the delimiters that were omitted")}
                typed {:ok? false
                       :why (substitution-why (first typed)
                                              (second typed)
                                              (nth after (dec (long (first typed))))
                                              (or subject "this edit wrote"))}
                invented {:ok? false
                          :why (invention-why (first invented) (second invented) (nth invented 2))}
                :else {:ok? true
                       :content candidate
                       :notes (mapv #(delimiter-note %
                                                     (nth before (dec (long %)))
                                                     (nth after (dec (long %))))
                                    changed)}))))

(defn- quote-added?
  "True when `after` is `before` with exactly ONE `\"` inserted and nothing else — every other
   character theirs, in order, which is the whole change a requote may make to a line."
  [^String before ^String after]
  (let
    [strip
     (fn [^String s]
       (str/replace s "\"" ""))

     quotes
     (fn [^String s]
       (- (count s) (count (strip s))))]

    (and (= (strip before) (strip after)) (= (inc (long (quotes before))) (long (quotes after))))))

(defn- quote-note
  "What a requote did to ONE line, in `delimiter-note`'s words: the character it put back, and
   the line the caller now reads to check it."
  [line-no ^String after]
  (str "line " line-no " added `\"` → `" (excerpt after) "`"))

(defn- requote-verdict
  "Whether `candidate` — `source` with one `\"` put back where the text this edit replaced
   ended with one — may be written. The parse gate every other candidate passes, and then the
   two rules that make a quote different from a bracket: the string that was left OPEN now
   closes, and exactly one quote was added, on ONE line, inside the lines this call wrote.
   Answers nil when any of it fails, so the refusal the caller reads still describes the
   balancer's own answer."
  [{:keys [parses-clean? ^String source spans]} candidate]
  (when (and (string? candidate)
             (unterminated-string source)
             (parses-clean? candidate)
             (nil? (unterminated-string candidate)))
    (let
      [before
       (str/split-lines source)

       after
       (str/split-lines ^String candidate)

       changed
       (changed-lines before after)

       line
       (when (= 1 (count changed)) (long (first changed)))]

      (when (and line
                 (= (str/ends-with? source "\n") (str/ends-with? ^String candidate "\n"))
                 (inside-spans? line spans)
                 (quote-added? (nth before (dec (long line))) (nth after (dec (long line)))))
        {:ok? true :content candidate :notes [(quote-note line (nth after (dec (long line))))]}))))

(def ^:private window-tries
  "How many form starts `balancer-window` reads back through before it gives up and hands the
   balancer the whole text. Every candidate cut is confirmed by scanning what it would remove,
   so an unbounded search across a file whose lines start at the left margin inside a form
   costs more than the window saves."
  32)

(defn- line-offsets
  "The character index each of `lines` begins at, and last the length of them all — so the text
   of lines `[i j)` is `(subs s (nth offs i) (nth offs j))`."
  [lines]
  (vec (reductions + 0 (map count lines))))

(defn- self-contained?
  "True when `s` closes everything it opens and closes nothing it did not open — a text that can
   be cut away from what surrounds it without changing how the rest of the file nests."
  [^String s]
  (= [] (open-stack s)))

(defn- balancer-window
  "The `[from end)` line indices of `source` a balancer has to see to answer for an edit on
   `spans`: the form the edit sits in, plus one whole form on either side, so the balancer still
   reads the line above the edit and the dedent below it — the two things indent mode closes a
   form by. What is cut away is `self-contained?`, so the answer inside the window is the answer
   the balancer would give for the whole text, and a repair that reaches past the window is one
   `verdict` refuses anyway for reaching outside the edit.

   Candidates are lines that start at the left margin; each is confirmed by reading the text it
   would cut away, and the search gives up after `window-tries` of them. nil when the edit has
   no such region, which leaves the whole text as its own window."
  [^String source lines offs spans]
  (when (seq spans)
    (let
      [n
       (count lines)

       from
       (dec (long (reduce min (map first spans))))

       to
       (dec (long (reduce max (map second spans))))

       own-form?
       (fn [i]
         (let [^String l (nth lines i)]
           (and (pos? (count l)) (not (space-char? (.charAt l (int 0)))))))

       heads
       (->> (range (min from (dec n)) -1 -1)
            (filter own-form?)
            (take window-tries)
            (filter #(self-contained? (subs source 0 (long (nth offs %)))))
            (take 2))

       ends
       (->> (range (inc to) (inc n))
            (filter #(or (= % n) (own-form? %)))
            (take window-tries)
            (filter #(self-contained? (subs source (long (nth offs %)))))
            (take 2))

       head
       (last heads)

       end
       (last ends)]

      (when (and head end (< (- (long end) (long head)) n)) [head end]))))

(defn- balancer-answer
  "`source` repaired by `balancer`, which is shown no more of it than an accepted repair could
   touch. A whole-file balancer is quadratic in practice — parinfer re-reads the text it has
   already placed — so asking it about a 12k-line file costs seconds, while the edit's own
   region costs milliseconds whatever the file's size: the difference between a repair the
   caller waits for and one it never notices. nil when the balancer has no answer or throws —
   it is a language pack's code, and a rebalance that cannot repair still has to refuse."
  ^String [balancer ^String source spans]
  (let
    [lines
     (terminated-lines source)

     offs
     (line-offsets lines)

     answer
     (fn [^String s]
       (try (balancer s) (catch Throwable _ nil)))]

    (if-let [[from end] (balancer-window source lines offs spans)]
      (let
        [a (long (nth offs from))
         b (long (nth offs end))]

        (when-let [fixed (answer (subs source a b))]
          (str (subs source 0 a) fixed (subs source b))))
      (answer source))))
(defn rebalance
  "Try to make `source` — the content an edit WOULD have written, which does not
   parse — parse, by repairing its delimiters WITHOUT letting the repair reach past
   the caller's own lines. `spans` are `[from-line to-line]` pairs, 1-based and
   inclusive, in `source`'s own coordinates; `parses-clean?` re-parses a candidate;
   `balancer` is the language pack's `:balance-fn`; `subject` names whose delimiters a
   refusal is about and defaults to the edit that produced `source` — a formatter
   handed a WHOLE file passes its own.

   `original` is the content this edit REPLACED, when there is one. It is the better
   evidence and is tried FIRST: a delimiter dropped from a line whose code survived
   goes back where that line had it, which is the only way to put a closer back in the
   MIDDLE of a line, or to tell a lost opener from one closer too many. The balancer's
   own answer — indentation, and nothing else — is the fallback.

   Two candidates follow it, and both put what the text is MISSING at the end of the last
   line this call wrote: `closed-at-tail` appends the closers nothing else could place, for
   a call that wrote one region, and `requoted` puts back a `\"` the replaced text proves
   ended that region, which no balancer can supply. Neither ever speaks in a refusal — that
   still describes the BALANCER's answer, because it is the repair the caller can reason
   about from what they wrote.

   Answers nil when there is no balancer to ask, `{:ok? true :content S :notes [..]}`
   for a repair that may be written, and `{:ok? false :why msg}` for one that was
   found and REJECTED — the caller puts `why` in its refusal, because \"a repair
   exists but it reaches outside your edit\" is exactly what tells the caller to
   re-read the region instead of retrying the same replacement."
  [{:keys [balancer ^String source ^String original spans] :as request}]
  (when (ifn? balancer)
    (let
      [pairs
       (when (string? original) (paired-lines original source))

       request
       (assoc request :pairs pairs)

       seated
       (when (seq pairs) (verdict request (reseat pairs source)))

       asked
       (when-not (:ok? seated) (verdict request (balancer-answer balancer source spans)))

       tailed
       (when-not (or (:ok? seated) (:ok? asked))
         (verdict request (closed-at-tail spans original source)))]

      (cond (:ok? seated) seated
            (:ok? asked) asked
            (:ok? tailed) tailed
            :else (or (requote-verdict request (requoted spans original source)) asked)))))
