(ns com.blockether.vis.test-prose
  "Prose limits for reader-facing text: the paragraph size limit, the ASD-STE100
   sentence and paragraph limits and the semicolon rule. The documentation page
   canon and the symbol documentation guard share these rules."
  (:require [clojure.string :as str]))

(def max-unit-chars
  "Characters one paragraph — or one list item with its continuation lines — may
   carry before it stops being prose and becomes a table nobody drew. Roughly 120
   words: past that a reader scans instead of reading, and the structure is
   already inside the sentence."
  800)

(defn text-units
  "PURE: `[[line text] …]` — every prose paragraph of `md`, plus every list item
   with the lines that continue it, joined into one string. Fenced blocks,
   headings, tables, quotes and markup-only HTML lines are skipped."
  [^String md]
  (let [close (fn [acc {:keys [line buf]}]
                (if (seq buf) (conj acc [line (str/join " " buf)]) acc))]
    (loop [ls (str/split-lines md)
           n 1
           in-fence? false
           cur {:line 0 :buf []}
           acc []]

      (if (empty? ls)
        (close acc cur)
        (let [l (str/trim (str (first ls)))
              item? (boolean (re-matches #"(?s)([-*+]|\d+[.)])\s.*" l))
              skip? (or (str/blank? l)
                        (str/starts-with? l "#")
                        (str/starts-with? l "|")
                        (str/starts-with? l ">")
                        (boolean (re-matches #"(?:<[^>]+>\s*)+" l)))]

          (cond (str/starts-with? l "```")
                (recur (rest ls) (inc n) (not in-fence?) {:line 0 :buf []} (close acc cur))
                in-fence? (recur (rest ls) (inc n) in-fence? cur acc)
                skip? (recur (rest ls) (inc n) in-fence? {:line 0 :buf []} (close acc cur))
                item? (recur (rest ls) (inc n) in-fence? {:line n :buf [l]} (close acc cur))
                :else (recur (rest ls)
                             (inc n)
                             in-fence?
                             (if (seq (:buf cur)) (update cur :buf conj l) {:line n :buf [l]})
                             acc)))))))

(def max-sentence-words
  "Words one prose sentence may carry: the ASD-STE100 Simplified Technical English
   limit for descriptive writing. Instructions keep to 20 by review, not by test."
  25)

(def max-unit-sentences
  "Sentences one paragraph, or one list item, may carry: the ASD-STE100 limit."
  6)

(defn sentences
  "PURE: the sentences of one text unit as plain text. The list marker, link targets,
   images, tags and emphasis are dropped, and a code span becomes one word, the way
   ASD-STE100 counts a technical name."
  [^String unit]
  (let [plain (-> unit
                  (str/replace #"^(?:[-*+]|\d+[.)])\s+" "")
                  (str/replace #"`[^`]*`" "code")
                  (str/replace #"!\[[^\]]*\]\([^)]*\)" "")
                  (str/replace #"\[([^\]]*)\]\([^)]*\)" "$1")
                  (str/replace #"<[^>]+>" "")
                  (str/replace "*" ""))]
    (remove str/blank? (str/split plain #"(?<=[.!?])[\"'”)\]]*\s+"))))

(defn word-count
  "PURE: the words of `sentence`, its spaced tokens that carry a letter or a digit."
  [^String sentence]
  (count (filter #(re-find #"[\p{L}\p{N}]" %) (str/split sentence #"\s+"))))

(defn semicolon?
  "PURE: whether `unit` joins clauses with a semicolon outside code spans, link
   targets and HTML entities."
  [^String unit]
  (str/includes? (-> unit
                     (str/replace #"`[^`]*`" "")
                     (str/replace #"\]\([^)]*\)" "]")
                     (str/replace #"&#?\w+;" ""))
                 ";"))

(defn breaks
  "PURE: every way `md` breaks a prose limit, as reader-facing lines that name the
   line of the paragraph."
  [^String md]
  (let [units (text-units md)]
    (concat (for [[line text] units
                  :when (> (count text) (long max-unit-chars))]

              (str "the paragraph on line "
                   line
                   " runs "
                   (count text)
                   " characters — over "
                   max-unit-chars
                   ", so it is a list or a table wearing prose"))
            (for [[line text] units
                  sentence (sentences text)
                  :let [n (word-count sentence)]
                  :when (> (long n) (long max-sentence-words))]

              (str "a sentence in the paragraph on line " line
                   " runs " n
                   " words — over " max-sentence-words
                   ", so split it: " (pr-str sentence)))
            (for [[line text] units
                  :let [n (count (sentences text))]
                  :when (> (long n) (long max-unit-sentences))]

              (str "the paragraph on line "
                   line
                   " holds "
                   n
                   " sentences — over "
                   max-unit-sentences
                   ", so it covers more than one topic"))
            (for [[line text] units
                  :when (semicolon? text)]

              (str "the paragraph on line "
                   line
                   " joins clauses with a semicolon — end the first clause with a full stop")))))
