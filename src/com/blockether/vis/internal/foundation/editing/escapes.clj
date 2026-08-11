(ns com.blockether.vis.internal.foundation.editing.escapes
  "Unicode-escape hygiene for model-authored edit TEXT.

   Public surface: `decode-unicode-escapes` — undo the `\\uXXXX` drift a model
   writes when it means the character itself, and nothing else."
  (:require [clojure.string :as str]))

;; =============================================================================
;; Drifted `\uXXXX` escapes in edit TEXT
;;
;; A model sometimes emits the six characters `\u2014` where it means an em
;; dash. JSON decoding is not the culprit: the escape arrives already escaped,
;; so those six characters ARE the text the tool was asked to write, and they
;; used to land on disk exactly like that.
;;
;; Decoding is therefore narrow enough that it can only ever undo drift: the
;; backslash must be unescaped, and the escape must name a VISIBLE assigned
;; character -- judged on the code point, so a surrogate pair is judged on the
;; character it builds. Real source legitimately contains an ASCII or control
;; escape, a doubled escape inside prose about escapes, and private-use code
;; points in icon fonts; nothing decodes into invisible ink (bidi overrides,
;; zero width, a space that is not a space) or into an unassigned point. Every
;; one of those is written through verbatim.
;; =============================================================================

(defn- hex-digit-value
  "Value of the ASCII hex digit code unit `c`, or -1.

   Deliberately ASCII-only, and deliberately not `Character/digit`: that also
   accepts a non-ASCII digit (U+0664 ARABIC-INDIC FOUR, U+FF14 FULLWIDTH FOUR),
   and a quad of those is not an escape any model drifted into."
  ^long [^long c]
  (cond (and (<= 48 c) (<= c 57)) (- c 48)
        (and (<= 97 c) (<= c 102)) (- c 87)
        (and (<= 65 c) (<= c 70)) (- c 55)
        :else -1))

(defn- unicode-escape-unit
  "The 16-bit value of the `\\uXXXX` escape whose backslash sits at `i` in `s`,
   or -1 when no complete escape starts there.

   Primitive and allocation-free: the quad is folded digit by digit, so a
   decode never cuts a substring, compiles a matcher, or boxes a code unit."
  ^long [^String s ^long i]
  ;; An escape is exactly six characters wide: \ u and four hex digits.
  (if (or (> (+ i 6) (.length s))
          (not (== (int \\) (int (.charAt s i))))
          (not (== (int \u0075) (int (.charAt s (inc i))))))
    -1
    (let [end (+ i 6)]
      (loop
        [k (+ i 2)
         acc 0]

        (if (== k end)
          acc
          (let [d (hex-digit-value (int (.charAt s k)))]
            (if (neg? d) -1 (recur (inc k) (+ (* acc 16) d)))))))))

(def ^:private undecodable-categories
  "Unicode general categories an escape may never be decoded INTO. Cn/Co/Cs are
   unassigned points, private use and half characters; Cc/Cf, Zl/Zp and Zs are
   controls, invisible formatting (bidi overrides, zero width, soft hyphen),
   line separators and spaces that do not look like spaces. Writing one of those
   into source as a real character is strictly worse than leaving six visible
   characters a human can see and fix."
  [Character/UNASSIGNED Character/PRIVATE_USE Character/SURROGATE Character/CONTROL Character/FORMAT
   Character/LINE_SEPARATOR Character/PARAGRAPH_SEPARATOR Character/SPACE_SEPARATOR])

(def ^:private undecodable-category-bits
  "`undecodable-categories` folded into one bit per `Character/getType` value, so
   the judgement below is a shift and a mask rather than a boxed set lookup once
   per escape."
  (reduce (fn [^long acc t]
            (bit-or acc (bit-shift-left 1 (int t))))
          0
          undecodable-categories))

(defn- decodable-code-point?
  "True when `cp` names a VISIBLE assigned non-ASCII character -- the only kind
   of escape that is decoded, and the same question for one BMP unit as for the
   code point a surrogate pair builds. Below U+00A0 sit ASCII and the C0/C1
   controls, where the escape is load-bearing: `\\n`, `\\u001b` inside an ANSI
   sequence, `\\u0022` inside JSON."
  [^long cp]
  (and (>= cp 0xA0)
       ;; An unassigned point IS category Cn, so one `getType` answers both
       ;; "does this character exist" and "may it be written".
       (zero? (bit-and (long undecodable-category-bits)
                       (bit-shift-left 1 (Character/getType (int cp)))))))

(defn decode-unicode-escapes
  "Decode the `\\uXXXX` escapes in edit text that can only be model drift.

   A literal `\\u2014` handed to `struct_patch` `code` used
   to be written to disk as those six characters, so edited files grew
   `\\u2014` where an em dash belonged. Here it becomes the em dash, while every
   escape a real file may legitimately carry is returned untouched: a doubled
   `\\\\uXXXX`, an ASCII or control escape, a lone surrogate, and anything that
   would decode into invisible or unreal ink -- private use, an unassigned
   point, a bidi override, a zero-width joiner, a line separator, a space that
   does not look like one. A surrogate PAIR is judged by the code point it
   builds, so emoji survive the trip and an invisible U+E0020 tag character
   cannot sneak in as one.

   Pure and total: a non-string, or a string with no escape in it, is returned
   as-is. Text between backslashes is never walked character by character --
   `indexOf` finds the next candidate and the span is bulk-copied -- so the
   common edit, which carries no escape at all or one, costs a scan."
  [s]
  (if-not (and (string? s) (str/includes? s "\\u"))
    s
    (let
      [^String text
       s

       n
       (.length text)

       sb
       (StringBuilder. n)]

      (loop [i 0]
        (let [b (.indexOf text (int \\) i)]
          (if (neg? b)
            ;; No backslash left in the tail: copy it whole and stop.
            (do (.append sb text i n) (.toString sb))
            (let
              [_ (.append sb text i b)
               run-end
               (long (loop [j b]
                       (if (and (< j n) (== (int \\) (int (.charAt text j)))) (recur (inc j)) j)))]

              (if (even? (- run-end b))
                ;; Every backslash is itself escaped: this is text ABOUT an escape.
                (do (.append sb text b run-end) (recur run-end))
                (let
                  [start (dec run-end)
                   unit (unicode-escape-unit text start)
                   low (if (and (<= 0xD800 unit) (<= unit 0xDBFF))
                         (unicode-escape-unit text (+ start 6))
                         -1)
                   pair (if (and (<= 0xDC00 low) (<= low 0xDFFF))
                          (long (Character/toCodePoint (char unit) (char low)))
                          -1)]

                  (.append sb text b start)
                  (cond (and (not (neg? pair)) (decodable-code-point? pair))
                        (do (.append sb (char unit))
                            (.append sb (char low))
                            ;; Both halves consumed: two escapes, one character.
                            (recur (+ start 12)))
                        (and (not (neg? unit)) (decodable-code-point? unit))
                        (do (.append sb (char unit)) (recur (+ start 6)))
                        :else (do (.append sb \\) (recur run-end))))))))))))
