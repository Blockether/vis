(ns com.blockether.vis.internal.util
  "The engine's one shared leaf: the primitives every namespace kept re-rolling —
   a millisecond clock, the two blank-string idioms, a trimmed environment read,
   UTF-8 bytes, SHA-256 and the hex fold.

   It requires NOTHING from the rest of vis and never will. That is the whole
   contract: a leaf can be required from anywhere — specs that load during
   namespace initialization, the gateway, the sandbox — without a cycle to
   reason about. Everything here is a pure function of its arguments (or of one
   process-wide reading), so nothing here may become a top-level value: a `def`
   that CALLS one of these freezes the BUILDER's answer into the native image
   (`native-image-env-capture-test` is the gate).

   A name earns a place here when a THIRD namespace needs it. A helper with one
   caller belongs beside its caller, not in this file."
  (:require [clojure.string :as str])
  (:import (java.nio.charset StandardCharsets)
           (java.security MessageDigest)))

(defn now-ms
  "Milliseconds since the epoch — the engine's one wall clock."
  ^long []
  (System/currentTimeMillis))

(defn non-blank-string?
  "True when `x` is a string carrying something other than whitespace."
  [x]
  (and (string? x) (not (str/blank? x))))

(defn non-blank
  "`x` trimmed to a string, or nil when it is nil, empty or all whitespace."
  ^String [x]
  (let [s (some-> x
                  str
                  str/trim)]
    (when-not (str/blank? s) s)))

(defn env-val
  "Environment variable `k`, trimmed, or nil when it is unset or blank."
  ^String [^String k]
  (non-blank (System/getenv k)))

(defn utf8
  "`s` as UTF-8 bytes — the one charset every vis wire format names."
  ^bytes [^String s]
  (.getBytes s StandardCharsets/UTF_8))

(defn bytes->hex
  "Lowercase hex of `b`, two characters per byte and no separators."
  ^String [^bytes b]
  (let [sb (StringBuilder. (* 2 (alength b)))]
    (dotimes [i (alength b)]
      (let [v (bit-and (aget b i) 0xff)]
        (when (< v 16) (.append sb \0))
        (.append sb (Integer/toHexString v))))
    (.toString sb)))

(defn sha256-digest
  "A fresh SHA-256 `MessageDigest` — for the streaming case, where the content
   arrives in chunks and `sha256` would need it all in memory at once."
  ^MessageDigest []
  (MessageDigest/getInstance "SHA-256"))

(defn sha256 "SHA-256 digest bytes of `b`." ^bytes [^bytes b] (.digest (sha256-digest) b))

(defn sha256-hex
  "Lowercase-hex SHA-256 of bytes, or of a string's UTF-8 bytes — the ONE
   content-identity fold in the engine: cache keys, replay dedup, source
   markers and pairing all read the same digits for the same input."
  ^String [x]
  (bytes->hex (sha256 (if (bytes? x) x (utf8 (str x))))))

;; ── Strings ──────────────────────────────────────────────────────────────

(defn truncate
  "Head-clip `s` to at most `n` chars (no ellipsis) — the engine's ONE head-clip,
   so it is also the one place that must never hand a lone surrogate to a UTF-8
   consumer downstream (JSON escape, SQLite, the mobile client): a cut that would
   land inside an astral char takes one char less instead of splitting the pair."
  [s ^long n]
  (let [^String s
        (str s)

        c
        (long (count s))]

    (if (> c n)
      (let [cut (if (and (pos? n) (Character/isHighSurrogate (.charAt s (dec n)))) (dec n) n)]
        (subs s 0 (max 0 cut)))
      s)))

(defn fence-delimiter
  "Markdown fence delimiter (a backtick run) longer than any backtick run in
   `body`. Arbitrary content — a file being read, a diff of a Markdown file, a
   tool's stdout — carries Markdown fences of its own; a fixed triple-backtick
   wrapper is then ambiguous and the INNER fence closes the outer block early,
   so everything after it renders as prose instead of code. CommonMark permits
   longer fences: pick the shortest safe one."
  [body]
  (let [max-run (->> (re-seq #"`+" (str body))
                     (map count)
                     (reduce max 0))]
    (apply str (repeat (max 3 (inc (long max-run))) "`"))))

(defn fenced
  "Wrap `body` in a fenced code block `body` itself cannot close early, with an
   optional `lang` info string. Callers keep their own blank/nil guards."
  ([body] (fenced body nil))
  ([body lang]
   (let [body
         (str body)

         delimiter
         (fence-delimiter body)]

     (str delimiter (or lang "") "\n" body "\n" delimiter))))

(def ^:private elision-marker-pattern
  "Trailing summary-elision marker on provider reasoning text.

   With summarized extended thinking, Anthropic ENDS a thinking block with a
   `thinking_delta` whose entire payload is the single character `…` — traced
   straight off the wire (`-Dsvar.stream.trace=true`: 16 such SSE lines in one
   run) and visible in the session journals, where 100 of 218 non-empty
   reasoning blocks carry it as their last delta. It is PROTOCOL — the summary
   says it elided the rest — not text the model wrote, and on a two-word summary
   (`I need…`) it reads as if Vis truncated the thought.

   Only a TRAILING `…` matches, so an ellipsis inside a sentence survives, and
   `...` is never touched: the marker is always the one code point."
  #"\s*…\s*$")

(defn strip-elision-marker
  "Reasoning `text` without its trailing provider elision marker.

   Suffix-only, therefore MONOTONE: a live stream may strip on every tick and
   the cumulative-minus-emitted-length delta math still never loses a
   character."
  [text]
  (some-> text
          str
          (str/replace elision-marker-pattern "")))

(defn normalize-thinking-text
  "Canonical thinking text for every surface — gateway SSE, poll/replay, session
   transcript and the TUI timeline all normalize HERE so no consumer is the
   first place a difference appears.

   Reasoning streams arrive with paragraph-style blank-line runs and
   whitespace-padded blank rows; collapse those, then drop the provider's
   elision marker. nil when nothing is left, so a blank tick falls back to the
   text already on screen instead of wiping it."
  [text]
  (when-let [s (some-> text
                       str)]
    (not-empty (-> s
                   (str/replace #"[ \t\r\f\v]+\r?\n" "\n")
                   (str/replace #"(?:\r?\n){2,}" "\n")
                   str/trim
                   strip-elision-marker
                   str/trim))))

(def sentence-boundary-pattern
  "One CLOSED sentence or clause end in model text — `.`, `!`, `?` or `…` with
   any trailing quotes/brackets, at whitespace or end of string, or a newline.

   Owned here because the gateway uses it to flush the live stream one sentence
   at a time (`gateway.state/sentence-closed-in-suffix?`). Settled reasoning also
   treats a syntactically closed Markdown heading as a complete thought."
  #"[.!?…][\"')\]]*(?:\s|$)|\n")

(def ^:private markdown-heading-pattern
  "A whole ATX or strong-emphasis line: provider summaries use both as headings."
  #"(?m)^(?:#{1,6}[ \t]+\S[^\n]*|\*\*\S(?:[^\n]*\S)?\*\*|__\S(?:[^\n]*\S)?__)$")

(defn- last-match-end
  "Index just past the last match of `pattern` in `s`, or nil."
  ^Long [pattern ^String s]
  (let [m (re-matcher pattern s)]
    (loop [end nil]
      (if (.find m) (recur (.end m)) end))))

(defn- last-closed-thought-end
  "Index after the last closed sentence, line, or Markdown heading in `s`."
  ^Long [^String s]
  (let [sentence-end
        (last-match-end sentence-boundary-pattern s)

        heading-end
        (last-match-end markdown-heading-pattern s)]

    (cond (and sentence-end heading-end) (max (long sentence-end) (long heading-end))
          sentence-end sentence-end
          :else heading-end)))

(defn settled-thinking-text
  "Canonical thinking text for a SETTLED iteration — what a transcript, a
   timeline entry or a replayed row keeps — or nil when the provider showed
   nothing usable.

   `normalize-thinking-text`, CLIPPED to the last complete thought: a closed
   sentence/line or a syntactically closed Markdown heading; nil when it closed
   none. Anthropic writes the thinking summary with a SECOND model that streams
   alongside the thinking block; when the block closes first the summary stops
   wherever it stood — mid-word — and the wire terminates it with the `…` marker.
   So prose AFTER the last complete boundary is exactly what the summarizer was
   writing when it was cut. A closed heading, however, is a complete structural
   unit and remains useful without terminal punctuation.

   LIVE ticks keep `normalize-thinking-text`: mid-stream every summary is still
   a fragment, and the stream must paint as it arrives. This decision belongs at
   the shared progress boundary; presentation clients render its settled value
   exactly rather than maintaining their own filtering vocabulary."
  [text]
  (when-let [s (normalize-thinking-text text)]
    (when-let [end (last-closed-thought-end s)]
      (not-empty (str/trimr (subs s 0 end))))))
