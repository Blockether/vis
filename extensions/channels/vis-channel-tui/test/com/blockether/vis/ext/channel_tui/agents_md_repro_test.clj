(ns com.blockether.vis.ext.channel-tui.agents-md-repro-test
  "End-to-end repro: take the WHOLE repo-root `AGENTS.md` as a single
   string, run it through `markdown->lines`, and dump the resulting
   marker-prefixed lines into `.verification/agents-md-render.txt`
   in a human-readable form (input line N, output line N, marker
   label, body with inline sentinels stripped).

   The point is to expose newline / blank-line / list / heading
   boundary regressions on a real-world document instead of pin-
   pointed minimal cases. Inspect the dump (`cat
   .verification/agents-md-render.txt`) to see exactly where the
   parser swallows / duplicates / re-orders newlines.

   Asserts:
     1. parser emits a non-empty vector,
     2. every blank line in the input -> at least one corresponding
        blank line in the output (otherwise paragraph breaks are
        being eaten - the canonical `rozjebane newline` symptom),
     3. every `#`/`##`/`###`+ heading in the input maps to an H*
        marker in the output (no heading silently demoted to plain),
     4. consecutive bullet items in the input never get merged into
        one output line."
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str]
   [com.blockether.vis.ext.channel-tui.primitives :as p]
   [com.blockether.vis.ext.channel-tui.render :as render]
   [lazytest.core :refer [defdescribe describe expect it]]))

(set! *warn-on-reflection* true)

(def ^:private md->lines @#'render/markdown->lines)

(def ^:private dump-path
  ".verification/agents-md-render.txt")

(def ^:private wrap-w 100)

;; ── marker -> label table ──────────────────────────────────────────

(def ^:private marker->label
  {p/MARKER_MD_H1         "H1     "
   p/MARKER_MD_H2         "H2     "
   p/MARKER_MD_H3         "H3+    "
   p/MARKER_MD_BOLD       "BOLD   "
   p/MARKER_MD_CODE       "CODE   "
   p/MARKER_MD_BULLET     "BULLET "
   p/MARKER_MD_TABLE_HEAD "T-HEAD "
   p/MARKER_MD_TABLE_SEP  "T-SEP  "
   p/MARKER_MD_TABLE_ROW  "T-ROW  "
   p/MARKER_MD_QUOTE      "QUOTE  "
   p/MARKER_MD_HR         "HR     "
   p/MARKER_MD_SUMMARY    "SUMMARY"})

(defn- marker-of
  "First codepoint of `s` if it sits in the PUA marker range, else nil."
  [^String s]
  (when (and (string? s) (pos? (count s)))
    (let [c (.charAt s 0)
          n (int c)]
      (when (and (>= n 0xE001) (<= n 0xE00F))
        (subs s 0 1)))))

(defn- label-of [^String s]
  (or (marker->label (marker-of s)) "PLAIN  "))

(defn- body-of [^String s]
  (if (marker-of s)
    (subs s 1)
    s))

(defn- strip-inline-sentinels
  "Replace inline span sentinels (\\uE110...\\uE117) with visible
   `[B]/[/B]/[I]/[/I]/[S]/[/S]/[C]/[/C]` so the dump reads like
   pseudo-HTML instead of raw PUA noise."
  ^String [^String s]
  (-> s
    (str/replace p/INLINE_BOLD_ON    "[B]")
    (str/replace p/INLINE_BOLD_OFF   "[/B]")
    (str/replace p/INLINE_ITALIC_ON  "[I]")
    (str/replace p/INLINE_ITALIC_OFF "[/I]")
    (str/replace p/INLINE_STRIKE_ON  "[S]")
    (str/replace p/INLINE_STRIKE_OFF "[/S]")
    (str/replace p/INLINE_CODE_ON    "[C]")
    (str/replace p/INLINE_CODE_OFF   "[/C]")))

(defn- visible
  "Drop the leading line marker, render inline sentinels as readable
   tags, render trailing whitespace as `/` so we can see hard breaks."
  ^String [^String s]
  (let [body (strip-inline-sentinels (body-of s))]
    (cond
      (zero? (count body)) "(blank)"
      :else                (str/replace body #"\s+$"
                             (fn [m] (str/join (repeat (count m) \/)))))))

;; ── dump writer ──────────────────────────────────────────────────

(defn- write-dump!
  [src out-lines]
  (io/make-parents dump-path)
  (let [in-lines  (str/split-lines src)
        in-count  (count in-lines)
        out-count (count out-lines)
        max-w     50
        clip      (fn [^String s]
                    (if (> (count s) max-w)
                      (str (subs s 0 (- max-w 1)) "...")
                      s))
        sb        (StringBuilder.)]
    (.append sb (str "AGENTS.md -> markdown->lines repro\n"
                  "================================\n"
                  "wrap width : " wrap-w "\n"
                  "input lines: " in-count "\n"
                  "output rows: " out-count "\n"
                  (when (not= in-count out-count)
                    (str "DELTA      : " (- out-count in-count)
                      " (positive = wrap/split, negative = lost rows)\n"))
                  "\n"
                  ;; Side-by-side header.
                  "  IN# │ INPUT                                              │ OUT# │ MARKER  │ OUTPUT\n"
                  "──────┼────────────────────────────────────────────────────┼──────┼─────────┼"
                  (apply str (repeat 80 \─)) "\n"))
    ;; Walk the longer of the two so we don't truncate either side.
    (dotimes [i (max in-count out-count)]
      (let [in-line  (when (< i in-count) (nth in-lines i))
            out-line (when (< i out-count) (nth out-lines i))
            in-disp  (cond
                       (nil? in-line)         "/"
                       (str/blank? in-line)   "(blank)"
                       :else                  (clip in-line))
            out-disp (if out-line (clip (visible out-line)) "/")
            label    (if out-line (label-of out-line) "       ")]
        (.append sb (format " %4d │ %-50s │ %4d │ %-7s │ %s%n"
                      (inc i)
                      in-disp
                      (inc i)
                      label
                      out-disp))))
    (spit dump-path (.toString sb))))

;; ── invariants extracted from the input ──────────────────────────

(defn- count-blank-runs
  "Count maximal runs of blank lines in `lines` - that's what we
   should observe roughly as blank rows in the output."
  [lines]
  (->> lines
    (partition-by str/blank?)
    (filter #(str/blank? (first %)))
    count))

(defn- visually-blank? [^String l]
  ;; A row is visually blank when, after stripping the leading marker
  ;; (if any), nothing remains - even if the row carries a non-blank
  ;; PUA marker char like `MARKER_MD_CODE`. The marker is invisible
  ;; (zero-width PUA) and only paints background colour, so the row
  ;; reads to the user as a blank visual line.
  (let [b (body-of l)]
    (or (nil? b) (str/blank? b))))

(defn- count-blank [lines]
  (count (filter visually-blank? lines)))

(defn- count-headings [lines]
  (count (filter #(re-matches #"^#{1,6} .*" %) lines)))

(defn- count-bullets [lines]
  (count (filter #(re-matches #"^\s*[-*+]\s+.*" %) lines)))

;; ── tests ────────────────────────────────────────────────────────

(defdescribe agents-md-render-repro-test
  (describe "AGENTS.md round-trips through markdown->lines"
    (let [agents-md  (slurp (io/file (System/getProperty "user.dir") "AGENTS.md"))
          out-lines  (md->lines agents-md wrap-w :answer)
          in-lines   (str/split-lines agents-md)]
      ;; Always write the dump first so even an assertion failure
      ;; leaves a readable artefact for inspection.
      (write-dump! agents-md (or out-lines []))

      (it (str "produces a non-empty vector (dump at " dump-path ")")
        (expect (seq out-lines)))

      (it "every input heading lands on an H* marker"
        (let [in-headings  (count-headings in-lines)
              out-headings (count
                             (filter (fn [^String l]
                                       (#{p/MARKER_MD_H1
                                          p/MARKER_MD_H2
                                          p/MARKER_MD_H3}
                                        (str (when (pos? (count l))
                                               (.charAt l 0)))))
                               out-lines))]
          (expect (= in-headings out-headings)
            (str "expected " in-headings " heading rows, got "
              out-headings " - see " dump-path))))

      (it "every blank-line RUN in the input survives as >=1 blank row in the output"
        ;; The canonical "rozjebane newline" smell: `md/join` emits
        ;; `\n\n` between blocks, the parser collapses every blank
        ;; line into nothing, and bullets / paragraphs visually fuse.
        ;; We allow the output to have MORE blanks (wrap can create
        ;; them inside code blocks) but never fewer than the input
        ;; carried.
        (let [in-blank-runs (count-blank-runs in-lines)
              out-blanks    (count-blank out-lines)]
          (expect (>= out-blanks in-blank-runs)
            (str "input had " in-blank-runs " blank-line runs, "
              "output has only " out-blanks " blank rows - paragraph "
              "breaks are being swallowed; see " dump-path))))

      (it "every input bullet maps to a bullet row in the output"
        ;; If two consecutive bullets fuse into one output line,
        ;; this count drops below the input's bullet count.
        (let [in-bullets  (count-bullets in-lines)
              out-bullets (count
                            (filter (fn [^String l]
                                      (= p/MARKER_MD_BULLET
                                        (str (when (pos? (count l))
                                               (.charAt l 0)))))
                              out-lines))]
          (expect (>= out-bullets in-bullets)
            (str "input has " in-bullets " bullet items, output only "
              out-bullets " bullet rows - bullets are fusing; see "
              dump-path)))))))
