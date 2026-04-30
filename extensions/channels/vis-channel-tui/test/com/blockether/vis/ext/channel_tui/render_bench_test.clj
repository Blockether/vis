(ns com.blockether.vis.ext.channel-tui.render-bench-test
  "Microbenchmarks for the inline Markdown rendering hot path.

   Why this file exists: conversation 954bf315 (40 iterations across 5
   queries, ~2 MB of stored prompt+blocks payload) wouldn't open in
   the TUI — `markdown->inline` ran a `str/replace` regex pre-pass
   over EVERY line of EVERY assistant bubble on EVERY redraw, which
   pegged the input thread before the first frame ever made it to the
   terminal. The pre-pass got rewritten as a hand-rolled
   StringBuilder scanner (`strip-md-links`) and the whole tokenizer
   got wrapped in the existing identity-keyed LRU. This file exists
   to PROVE the rewrite is faster, on a workload modelled after the
   real conversation that broke in the first place, and to keep us
   honest if anyone touches the inline path again.

   Two surfaces:

   1. `equivalence-test` — a real lazytest `defdescribe` that runs in
      the default suite. Checks that the new `strip-md-links` and
      `markdown->inline` produce byte-identical output to the old
      regex-based versions on a representative corpus. This is the
      regression net.

   2. `run-bench!` — invokes `criterium.core/quick-benchmark` for each
      variant and prints the raw stats. NOT a deftest. NOT auto-run.
      Invoke explicitly:

          clojure -M:test -e \"(require '[com.blockether.vis.ext.channel-tui.render-bench-test :as b]) (b/run-bench!)\"

      Skipped by lazytest discovery because `defn` is not a test.
      Ratchet target: NEW must beat OLD on every workload class. If
      it doesn't, don't merge."
  (:require
   [clojure.string :as str]
   [com.blockether.vis.ext.channel-tui.primitives :as p]
   [com.blockether.vis.ext.channel-tui.render :as render]
   [criterium.core :as crit]
   [lazytest.core :refer [defdescribe describe expect it]]))

;;; ── Privates we want to bench ──────────────────────────────────────────────

(def ^:private strip-md-links     @#'render/strip-md-links)
(def ^:private find-inline-close  @#'render/find-inline-close)
(def ^:private word-char-at?      @#'render/word-char-at?)
(def ^:private markdown->inline   @#'render/markdown->inline)

;;; ── OLD implementation ─────────────────────────────────────────────────────
;;
;; Lifted verbatim from git HEAD before the StringBuilder rewrite (commit
;; 545fd16's body of `markdown->inline`, plus the standalone regex
;; pre-pass). Kept here forever as the baseline. If the comparison ever
;; gets less interesting (because someone "improves" the OLD copy), it
;; stops being a fair fight — so DO NOT touch these unless you're
;; deliberately establishing a new baseline.

(defn- strip-OLD
  "Pre-StringBuilder regex pre-pass. Allocates a `Pattern` match,
   a `Matcher`, and a per-match callback closure on every call."
  ^String [^String s]
  (str/replace s
    #"(!)?\[([^\]]*?)\]\(([^)\s]+)(?:\s+\"[^\"]*\")?\)"
    (fn [[_ image? text]] (if image? "" text))))

(defn- markdown->inline-OLD
  "Pre-cache, regex-prepass body. Recurses to itself (NOT the cached
   `markdown->inline`) so the comparison stays apples-to-apples."
  ^String [^String s]
  (if (or (nil? s) (zero? (.length s)))
    (or s "")
    (let [s (strip-OLD s)
          n  (.length s)
          sb (StringBuilder.)
          tokens [["**" "**" p/INLINE_BOLD_ON   p/INLINE_BOLD_OFF   true]
                  ["__" "__" p/INLINE_BOLD_ON   p/INLINE_BOLD_OFF   true]
                  ["~~" "~~" p/INLINE_STRIKE_ON p/INLINE_STRIKE_OFF true]
                  ["*"  "*"  p/INLINE_ITALIC_ON p/INLINE_ITALIC_OFF true]
                  ["_"  "_"  p/INLINE_ITALIC_ON p/INLINE_ITALIC_OFF true]
                  ["`"  "`"  p/INLINE_CODE_ON   p/INLINE_CODE_OFF   false]]
          opener-allowed?
          (fn [^String op ^long i]
            (or (not= \_ (.charAt op 0))
              (not (word-char-at? s (dec i)))))
          match-opener (fn [^long i]
                         (some (fn [[op _ _ _ _ :as t]]
                                 (let [op-len (.length ^String op)]
                                   (when (and (<= (+ i op-len) n)
                                           (.regionMatches s i ^String op 0 op-len)
                                           (opener-allowed? op i))
                                     t)))
                           tokens))]
      (loop [i 0]
        (if (>= i n)
          (.toString sb)
          (if-let [[opener closer on off recurse?] (match-opener i)]
            (let [content-from (+ i (.length ^String opener))
                  close-at     (find-inline-close s closer content-from)]
              (if (neg? close-at)
                (do (.append sb (.charAt s i))
                  (recur (inc i)))
                (let [content (subs s content-from close-at)]
                  (.append sb ^String on)
                  (.append sb ^String (if recurse?
                                        (markdown->inline-OLD content)
                                        content))
                  (.append sb ^String off)
                  (recur (+ close-at (.length ^String closer))))))
            (do (.append sb (.charAt s i))
              (recur (inc i)))))))))

;;; ── Workload corpus ────────────────────────────────────────────────────────
;;
;; Modelled after the answers in conversation 954bf315: dense markdown
;; link payloads (`[path](path)` repeated, file:line refs, README/test
;; cross-references) interleaved with plain prose, headings, bullets,
;; quotes, and inline code. Mix is roughly representative of what an
;; assistant emits when describing a code change.

(def ^:private corpus-link-heavy
  ["The bug was in [extensions/channels/vis-channel-tui/src/com/blockether/vis/ext/channel_tui/render.clj:1490](extensions/channels/vis-channel-tui/src/com/blockether/vis/ext/channel_tui/render.clj:1490) — the inline tokenizer was emitting the URL after the anchor text."
   "Edited [render.clj:1505](extensions/.../render.clj:1505) and added a regex pre-pass that strips `[text](url)` to just `text` before the sentinel scan."
   "**Files touched**: [render.clj](src/render.clj), [render_test.clj](test/render_test.clj), [README.md](README.md), [AGENTS.md](AGENTS.md), [deps.edn](deps.edn)."
   "See also [`docs/src/architecture.md`](docs/src/architecture.md) and [`docs/src/SUMMARY.md`](docs/src/SUMMARY.md)."])

(def ^:private corpus-mixed
  ["### Root cause found & fixed"
   "A line with `inline code` and **bold** and a [link](http://example.com) and ~~strike~~ all together."
   "- bullet with [a](b) and [c](d)"
   "> quoted with [link](url) inside"
   "Just a single [foo](bar)."])

(def ^:private corpus-plain
  ["Plain prose with no markup at all — the common case for code lines and stdout snippets."
   "Another plain line with **bold** and *italic* and `code` but no links."
   "VARIABLE_LIKE_THIS_WITH_UNDERSCORES inside text."
   "Verified with bin/vis run --conversation-id 954bf315 — all 14 iterations render cleanly."
   ""
   "    indented code line, four spaces"])

(def ^:private corpus-all
  (vec (concat corpus-link-heavy corpus-mixed corpus-plain)))

;;; ── Equivalence test (runs in the default suite) ───────────────────────────

(defdescribe equivalence-test
  (describe "strip-md-links matches the regex pre-pass byte-for-byte"
    (it "on link-heavy lines"
      (doseq [s corpus-link-heavy]
        (expect (= (strip-OLD s) (strip-md-links s)))))
    (it "on mixed-markup lines"
      (doseq [s corpus-mixed]
        (expect (= (strip-OLD s) (strip-md-links s)))))
    (it "on plain prose"
      (doseq [s corpus-plain]
        (expect (= (strip-OLD s) (strip-md-links s)))))
    (it "preserves identity when no markup is present (no-alloc path)"
      (let [s "no markup at all here, just words and code chars"]
        (expect (identical? s (strip-md-links s)))))
    (it "drops images entirely"
      (expect (= "alt: " (strip-md-links "alt: ![logo](http://x)"))))
    (it "leaves orphan brackets alone"
      (expect (= "[oops]"   (strip-md-links "[oops]")))
      (expect (= "[a]()"    (strip-md-links "[a]()")))
      (expect (= "[a]( )"   (strip-md-links "[a]( )"))))
    (it "honours optional title"
      (expect (= "go go" (strip-md-links "go [go](http://x \"title\")")))))
  (describe "markdown->inline output matches the old uncached regex impl"
    (it "on the full corpus"
      (doseq [s corpus-all]
        (render/invalidate-cache!)
        (expect (= (markdown->inline-OLD s) (markdown->inline s)))))))

;;; ── Benchmarks (manual) ────────────────────────────────────────────────────

(defn- fmt-time
  "Pretty-print `seconds` using whichever unit best fits its
   magnitude (ns / µs / ms / s). Locale-pinned to ROOT so the output
   is the same on macOS-PL, en_US, CI, etc. — comma-vs-dot drift
   used to make these numbers unparseable in scripts."
  ^String [^double seconds]
  (cond
    (< seconds 1e-6)  (String/format java.util.Locale/ROOT "%7.1f ns" (object-array [(* seconds 1e9)]))
    (< seconds 1e-3)  (String/format java.util.Locale/ROOT "%7.2f µs" (object-array [(* seconds 1e6)]))
    (< seconds 1.0)   (String/format java.util.Locale/ROOT "%7.3f ms" (object-array [(* seconds 1e3)]))
    :else             (String/format java.util.Locale/ROOT "%7.3f s " (object-array [seconds]))))

(defn- bench-quick
  "Run criterium's quick-benchmark on `f` over the workload `xs` and
   print one line of raw stats. `quick-benchmark*` does its own JIT
   warm-up and statistical sampling — no hand-rolled timing loops.
   Returns the stats map so callers can compute speedup ratios."
  [label f xs]
  (let [body  #(run! f xs)
        stats (crit/quick-benchmark* body {})
        mean  (first (:mean stats))
        sd    (Math/sqrt (first (:sample-variance stats)))
        lower (first (:lower-q stats))
        upper (first (:upper-q stats))
        sample-count    (:sample-count stats)
        execution-count (:execution-count stats)]
    (println (format "  %-46s mean=%s  sd=%s  q25=%s  q75=%s   (%d samples × %d execs)"
               label
               (fmt-time mean)
               (fmt-time sd)
               (fmt-time lower)
               (fmt-time upper)
               sample-count
               execution-count))
    stats))

(defn- speedup-line
  "Print 'NEW is N× faster than OLD' (or '× slower' if we regressed)
   for two stats maps returned by `bench-quick`."
  [label-old stats-old label-new stats-new]
  (let [m-old (first (:mean stats-old))
        m-new (first (:mean stats-new))
        ratio (/ m-old m-new)
        verb  (if (>= ratio 1.0) "FASTER" "SLOWER")
        x     (if (>= ratio 1.0) ratio (/ 1.0 ratio))]
    (println (String/format java.util.Locale/ROOT
               "  → %s vs %s: %.2f× %s"
               (object-array [label-new label-old x verb])))))

(defn run-bench!
  "Print before/after numbers for the inline-markdown hot path.

   Three phases. Phases 1 and 2 isolate the *algorithmic* win
   (StringBuilder pre-pass vs `str/replace` regex). Phase 3 reports
   the *cache* win separately, because comparing OLD-without-cache
   to NEW-with-cache would hand the cache layer credit it doesn't
   deserve and bury the algorithmic delta. Both wins ship together
   in the user-visible TUI; we just attribute them honestly.

       1. strip-only             — link/image pre-pass in isolation
       2. markdown->inline cold  — full strip + tokenize, cache cleared
                                   → isolates the algorithmic win
       3. markdown->inline hot   — finalized bubble re-renders;
                                   compares NEW-hot vs NEW-cold so
                                   the cache layer's contribution is
                                   measured against itself, not
                                   against an OLD that never had one

   Run with:

       clojure -M:bench-micro -e \"(require '[com.blockether.vis.ext.channel-tui.render-bench-test :as b]) (b/run-bench!)\""
  []
  (let [workload corpus-all
        chars    (reduce + (map count workload))]
    (println)
    (println (format "Workload: %d unique lines, %d chars total — measured per workload pass"
               (count workload) chars))
    (println "(JIT warm-up + sampling handled by criterium/quick-benchmark*)")

    (println)
    (println "── 1. Link / image pre-pass in isolation ──")
    (let [old (bench-quick "strip-OLD (str/replace + regex)"  strip-OLD       workload)
          new (bench-quick "strip-NEW (StringBuilder)"         strip-md-links workload)]
      (speedup-line "strip-OLD" old "strip-NEW" new))

    (println)
    (println "── 2. Full markdown->inline, cold cache (every call pays full cost) ──")
    (let [old (bench-quick "markdown->inline OLD (regex, no cache)"
                markdown->inline-OLD workload)
          new (bench-quick "markdown->inline NEW (SB strip, cleared cache)"
                (fn [s] (render/invalidate-cache!) (markdown->inline s))
                workload)]
      (speedup-line "markdown->inline OLD" old "markdown->inline NEW (cold)" new))

    (println)
    (println "── 3. Cache layer alone: NEW-hot vs NEW-cold (NOT vs OLD) ──")
    (println "     Phase 2 already measured strip+tokenize without cache help.")
    (println "     Phase 3 holds the algorithm fixed (NEW on both sides) and")
    (println "     varies only the cache state, so the speedup attributes")
    (println "     cleanly to the LRU and not to the StringBuilder rewrite.")
    (let [cold (bench-quick "markdown->inline NEW (cold cache, cleared each call)"
                 (fn [s] (render/invalidate-cache!) (markdown->inline s))
                 workload)
          _    (run! markdown->inline workload) ;; warm
          hot  (bench-quick "markdown->inline NEW (hot cache)"
                 markdown->inline workload)]
      (speedup-line "NEW cold" cold "NEW hot" hot))

    (println)
    (println "Done. Lower = faster. Phase 1 + 2 = algorithm; Phase 3 = cache.")
    nil))
