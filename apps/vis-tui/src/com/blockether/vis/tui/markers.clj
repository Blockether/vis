(ns com.blockether.vis.tui.markers
  "Zero-width markers of the painter lines. This namespace is their one owner.

   A BLOCK MARKER is the first char of a line. It selects the row style of the
   chat-bubble painter (code band, heading, table row and so on). An INLINE
   SPAN sentinel can be at each position of a line. It opens or closes a style
   for the text after it; the Lanterna fork owns its range (`InlineSpans`) and
   gives it no columns.

   Derive each test and each strip from the definitions here. Do not write a
   codepoint range in another namespace."
  (:require [com.blockether.vis.tui.primitives :as p])
  (:import [com.googlecode.lanterna InlineSpans]
           [java.util BitSet]
           [java.util.regex Pattern]))

;;; ── Block markers ───────────────────────────────────────────────────────────

(def MARKER_THINKING "\u200B")  ;; zero-width space       -> italic, dim (reasoning)

(def MARKER_CODE "\u200C")  ;; zero-width non-joiner  -> code style

(def MARKER_RESULT "\u200D")  ;; zero-width joiner      -> result/return value (success)

(def MARKER_SEP "\u2060")  ;; word-joiner            -> separator line

(def MARKER_CODE_OK "\u2061")  ;; function application   -> code with success status

(def MARKER_CODE_ERR "\u2062")  ;; invisible times        -> code with error status

(def MARKER_ERR_RESULT "\u2063")  ;; invisible separator    -> error result line

(def MARKER_ITERATION_HDR "\u2066")  ;; LRI                    -> iteration header with bg

(def MARKER_ANSWER_SEP "\u2069")  ;; PDI                    -> answer separator (trace->answer break)

(def MARKER_CODE_PAD "\u206A")  ;; ISS                    -> running/neutral code block padding line

(def MARKER_CODE_ERR_PAD "\u206B") ;; ASS                   -> error code block padding line

(def MARKER_CODE_OK_PAD "\uE000") ;; PUA                    -> successful code block padding line

(def MARKER_RECAP "\uE00E") ;; PUA                    -> iteration recap line (header-bg, bold+italic)

(def MARKER_ITERATION_PAD "\u206C")  ;; IAFS                   -> iteration zone padding (margin between blocks)

(def MARKER_ANSWER_HDR "\u206D")  ;; AAFS                   -> final answer header

(def MARKER_ANSWER_TXT "\u206E")  ;; NADS                   -> answer text line (with answer bg)

(def MARKER_ANSWER_PAD "\u206F")  ;; NODS                   -> answer padding line
;; Markdown markers (PUA \uE000+) - guaranteed unique, never collide
;; with the iteration/answer markers above. Two parallel sets:
;;   - MARKER_MD_*    -> answer-zone markdown (answer-bg)
;;   - MARKER_TH_MD_* -> thinking-zone markdown (iteration-header-bg, italic)
(def MARKER_MD_H1 "\uE001") ;; markdown heading 1 (answer)

(def MARKER_MD_H2 "\uE002") ;; markdown heading 2 (answer)

(def MARKER_MD_H3 "\uE003") ;; markdown heading 3 (answer)

(def MARKER_MD_BOLD "\uE004") ;; markdown bold line (answer)

(def MARKER_MD_CODE "\uE005") ;; markdown fenced code (answer)

(def MARKER_MD_BULLET "\uE006") ;; markdown bullet list item (answer)

(def MARKER_MD_TABLE_HEAD "\uE007") ;; markdown table header row (answer)

(def MARKER_MD_TABLE_SEP "\uE008") ;; markdown table border / separator (answer)

(def MARKER_MD_TABLE_ROW "\uE009") ;; markdown table data row (answer)

(def MARKER_MD_QUOTE "\uE00A") ;; markdown blockquote (answer)

(def MARKER_MD_HR "\uE00B") ;; markdown horizontal rule (answer)

(def MARKER_MD_SUMMARY "\uE00C") ;; markdown <summary> disclosure label (answer)

(def MARKER_HINT "\uE010") ;; affordance hint (e.g. "↑ to edit") -> accent fg on regular terminal bg, NOT the queue band

(def MARKER_STEP_DIGEST "\uE014") ;; digest of the steps after a progress note, on transcript paper

(def MARKER_USER_INPUT "\uE016") ;; queued message delivered into the running turn (`→ Send now`) -> the user's words on the user bubble paper, like a normal user message

(def MARKER_USER_INPUT_HDR "\uE017") ;; `You · Queued · …` header row of a delivered message -> bold user role "You", bold accent "Queued", then the delivery meta in the hint color

(def MARKER_ACTIVITY "\uE015") ;; compact Activity timeline surface

(def MARKER_ATTACHMENT_CARD "\uE018") ;; attachment card row -> bordered card paper with file rows, Open buttons and an optional fold title in the top border

(def MARKER_TH_MD_H1 "\uE021") ;; markdown heading 1 (thinking)

(def MARKER_TH_MD_H2 "\uE022") ;; markdown heading 2 (thinking)

(def MARKER_TH_MD_H3 "\uE023") ;; markdown heading 3 (thinking)

(def MARKER_TH_MD_BOLD "\uE024") ;; markdown bold line (thinking)

(def MARKER_TH_MD_CODE "\uE025") ;; markdown fenced code (thinking)

(def MARKER_TH_MD_BULLET "\uE026") ;; markdown bullet list item (thinking)

(def MARKER_TH_MD_TABLE_HEAD "\uE027") ;; markdown table header row (thinking)

(def MARKER_TH_MD_TABLE_SEP "\uE028") ;; markdown table border (thinking)

(def MARKER_TH_MD_TABLE_ROW "\uE029") ;; markdown table data row (thinking)

(def MARKER_TH_MD_QUOTE "\uE02A") ;; markdown blockquote (thinking)

(def MARKER_TH_MD_HR "\uE02B") ;; markdown horizontal rule (thinking)

(def MARKER_TH_MD_SUMMARY "\uE02C") ;; markdown <summary> disclosure label (thinking)

(def ^:private block-marker-chars
  "Each block marker, as one string. Read from the `MARKER_*` vars above, so a
   new marker needs no second registration."
  (->> (ns-publics 'com.blockether.vis.tui.markers)
       (keep (fn [[sym v]]
               (when (.startsWith (name sym) "MARKER_") @v)))
       sort
       (apply str)))

(defn block-marker?
  "True when `ch` is a block marker."
  [ch]
  (>= (.indexOf ^String block-marker-chars (int (char ch))) 0))

(defn split-block-marker
  "`[marker body]` when `line` starts with a block marker, else nil."
  [line]
  (when (and (string? line) (pos? (.length ^String line)) (block-marker? (.charAt ^String line 0)))
    [(subs line 0 1) (subs line 1)]))

(defn unmarked
  "`line` without its leading block marker. A value that is not a string comes
   back as is."
  [line]
  (if (and (string? line) (pos? (.length ^String line)) (block-marker? (.charAt ^String line 0)))
    (subs line 1)
    line))

;;; ── Inline spans ────────────────────────────────────────────────────────────

(def INLINE_BOLD_ON (str InlineSpans/BOLD_ON))

(def INLINE_BOLD_OFF (str InlineSpans/BOLD_OFF))

(def INLINE_ITALIC_ON (str InlineSpans/ITALIC_ON))

(def INLINE_ITALIC_OFF (str InlineSpans/ITALIC_OFF))

(def INLINE_STRIKE_ON (str InlineSpans/STRIKE_ON))

(def INLINE_STRIKE_OFF (str InlineSpans/STRIKE_OFF))

(def INLINE_CODE_ON (str InlineSpans/CODE_ON))

(def INLINE_CODE_OFF (str InlineSpans/CODE_OFF))

(def INLINE_LINK_ON (str InlineSpans/LINK_ON))

(def INLINE_LINK_OFF (str InlineSpans/LINK_OFF))

(def INLINE_ERR_ON (str InlineSpans/ERROR_ON))

(def INLINE_ERR_OFF (str InlineSpans/ERROR_OFF))

(defn strip-inline-spans
  "`s` without inline-span sentinels. A string that has none comes back as is."
  ^String [s]
  (InlineSpans/strip (str (or s ""))))

;;; ── Text that a person reads ────────────────────────────────────────────────

(def ^Pattern invisible-pattern
  "Matches each block marker, each inline-span sentinel and the byte-order mark,
   at each position. For text that leaves the painter, such as a copy."
  (Pattern/compile
    (str "[" block-marker-chars "\uFEFF" InlineSpans/BOLD_ON "-" InlineSpans/ERROR_OFF "]")))

(def ^:private ^BitSet invisible-chars
  "The chars that `invisible-pattern` matches, as a set for a char loop."
  (let [bits (BitSet. 65536)]
    (doseq [c (str block-marker-chars "\uFEFF")]
      (.set bits (int c)))
    (.set bits (int InlineSpans/BOLD_ON) (inc (int InlineSpans/ERROR_OFF)))
    bits))

(defn strip-invisible
  "`s` without the chars that `invisible-pattern` matches. A string that has none comes
   back as is. The pattern tests each marker in turn for each char of the text; this
   reads one bit for each char."
  ^String [^String s]
  (let [^BitSet invisible
        invisible-chars

        n
        (.length s)

        hit
        (long (loop [i 0]
                (cond (>= i n) -1
                      (.get invisible (int (.charAt s i))) i
                      :else (recur (inc i)))))]

    (if (neg? hit)
      s
      (let [sb (StringBuilder. n)]
        (.append sb s 0 (int hit))
        (loop [i (inc hit)]
          (when (< i n)
            (let [c (.charAt s i)]
              (when-not (.get invisible (int c)) (.append sb c)))
            (recur (inc i))))
        (.toString sb)))))

(defn plain-text
  "The text that a person reads in the painter line `line`: no SGR escape, no
   leading block marker or other leading format char, no inline-span sentinel.
   A line that has none of them comes back as is."
  ^String [line]
  (let [^String s
        (p/strip-sgr (str (or line "")))

        ^String s
        (if (and (pos? (.length s))
                 (let [c (.charAt s 0)]
                   (or (block-marker? c) (= (int Character/FORMAT) (Character/getType c)))))
          (subs s 1)
          s)]

    (InlineSpans/strip s)))
