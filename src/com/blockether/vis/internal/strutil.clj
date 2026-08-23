(ns com.blockether.vis.internal.strutil
  "Shared tiny string helpers. A dependency-free leaf so any namespace can use it
   without risking a cycle."
  (:require [clojure.string :as str]))

(defn truncate
  "Head-clip `s` to at most `n` chars (no ellipsis)."
  [s ^long n]
  (let [s
        (str s)

        c
        (long (count s))]

    (if (> c n) (subs s 0 n) s)))

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
