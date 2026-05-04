(ns com.blockether.vis.internal.markdown
  "Conversation -> Markdown exporter.

   Single-purpose host helper: take a conversation-id (or a `[:id uuid]`
   ref / a conversation map) and a `db-info`, return a complete Markdown
   document covering every persisted turn (user prompt + final answer +
   optional metadata line).

   Lives in the host runtime so EVERY channel (TUI, Telegram, CLI agent,
   third-party) can offer a `Copy as Markdown` / `Export conversation`
   affordance without re-implementing the projection. The shape is
   deterministic and intentionally boring: human-readable, paste-into-
   GitHub-issue-friendly Markdown, no HTML, no tables, no fenced-code
   wrapping of the user's text (which is itself often Markdown already
   and would double-render under fences).

   Pure: depends only on `internal.format` for date / cost / duration
   helpers and on `internal.persistance` for the two read fns. Zero
   knowledge of any specific channel."
  (:require
   [clojure.string :as str]
   [com.blockether.vis.internal.format :as fmt]
   [com.blockether.vis.internal.persistance :as persistance]))

;; =============================================================================
;; Pure Markdown builders
;; =============================================================================

(defn- ->str
  "Coerce x to String. nil -> \"\". Sequential collections are rejected so
   callers do not accidentally stringify lazy seqs into garbage. Splice one
   level up with `join` / `lines` / the variadic helpers instead."
  ^String [x]
  (cond
    (nil? x)        ""
    (string? x)     x
    (sequential? x) (throw (ex-info
                             (str "markdown helper got a sequential collection where a string was expected. "
                               "Splice with (join ...) / (lines ...), or build eagerly.")
                             {:value-class (.getName (class x))
                              :sample      (->> x (take 3) (mapv #(if (string? %) % (pr-str %))))}))
    :else           (str x)))

(declare expand-parts)

(defn- compose-text
  "Variadic-friendly text composer used by inline / heading / block helpers.
   nil dropped, one-level seq splice, caller-owned whitespace."
  ^String [parts]
  (cond
    (and (= 1 (count parts)) (string? (first parts)))
    (first parts)

    (and (= 1 (count parts)) (not (sequential? (first parts))))
    (->str (first parts))

    :else
    (->> (expand-parts parts)
      (mapv ->str)
      (apply str))))

(defn h
  "Heading at level n (clamped [1, 6])."
  ^String [n & parts]
  (let [lvl (max 1 (min 6 (long n)))]
    (str (apply str (repeat lvl "#")) " " (compose-text parts))))

(defn h1 ^String [& parts] (str "# "      (compose-text parts)))
(defn h2 ^String [& parts] (str "## "     (compose-text parts)))
(defn h3 ^String [& parts] (str "### "    (compose-text parts)))
(defn h4 ^String [& parts] (str "#### "   (compose-text parts)))
(defn h5 ^String [& parts] (str "##### "  (compose-text parts)))
(defn h6 ^String [& parts] (str "###### " (compose-text parts)))

(defn p
  "Paragraph. Joins parts with single spaces. nil dropped, seqs spliced one
   level deep."
  ^String [& parts]
  (let [joined (str/join " " (keep (fn [p] (when-not (str/blank? p) (str/trim p)))
                               (mapv ->str (expand-parts parts))))]
    (loop [s joined]
      (let [next (str/replace s "  " " ")]
        (if (str/includes? next "  ")
          (recur next)
          next)))))

(defn bold        ^String [& parts] (str "**"     (compose-text parts) "**"))
(def strong bold)
(defn italic      ^String [& parts] (str "*"      (compose-text parts) "*"))
(def em italic)
(defn bold-italic ^String [& parts] (str "***"    (compose-text parts) "***"))
(defn strike      ^String [& parts] (str "~~"     (compose-text parts) "~~"))
(defn code        ^String [& parts] (str "`"      (compose-text parts) "`"))
(defn kbd         ^String [& parts] (str "<kbd>"  (compose-text parts) "</kbd>"))

(defn- escape-title-attr
  ^String [title]
  (str/replace (->str title) "\"" "\\\""))

(defn link
  (^String [text url] (link text url nil))
  (^String [text url title]
   (let [t (->str title)]
     (if (str/blank? t)
       (str "[" (->str text) "](" (->str url) ")")
       (str "[" (->str text) "](" (->str url)
         " \"" (escape-title-attr t) "\")")))))

(defn image
  (^String [alt url] (image alt url nil))
  (^String [alt url title]
   (str "!" (link alt url title))))

(defn file-link
  (^String [path] (file-link path nil))
  (^String [path line]
   (let [p (->str path)]
     (if line
       (link (str p ":" line) (str p "#L" line))
       (link p p)))))

(defn anchor
  (^String [text] (anchor text nil))
  (^String [text slug]
   (let [raw  (->str text)
         slug (or slug
                (-> raw
                  str/lower-case
                  (str/replace #"[^a-z0-9\s-]" "")
                  (str/replace #"\s+" "-")
                  (str/replace #"-+" "-")
                  (str/replace #"^-|-$" "")))]
     (link raw (str "#" slug)))))

(defn- fence-lang-str [lang]
  (let [s (cond
            (nil? lang) nil
            (or (keyword? lang) (symbol? lang)) (name lang)
            :else (->str lang))
        s (some-> s str/trim)]
    (when-not (str/blank? s) s)))

(defn- code-block-body
  ^String [body]
  (when (nil? body)
    (throw (ex-info
             "code-block requires a non-nil body. Tool envelopes keep output under :result, e.g. (get-in run [:result :stdout])."
             {:body body})))
  (let [body (->str body)]
    (if (str/ends-with? body "\n") body (str body "\n"))))

(defn code-block
  (^String [body]
   (let [body (code-block-body body)]
     (str "```\n" body "```")))
  (^String [lang body]
   (let [body (code-block-body body)]
     (str "```" (fence-lang-str lang) "\n" body "```"))))

(defn blockquote
  ^String [& parts]
  (let [text (compose-text parts)]
    (if (str/blank? text)
      ">"
      (->> (str/split-lines text)
        (map #(if (str/blank? %) ">" (str "> " %)))
        (str/join "\n")))))

(def hr "---")

(def ^{:doc "Hard line break suffix. Stringifies to two trailing spaces and is also callable as a zero-arg fn."}
  br
  (reify
    CharSequence
    (toString [_] "  ")
    clojure.lang.IFn
    (invoke [_] "  ")
    (applyTo [_ _] "  ")))

(defn summary
  ^String [& parts]
  (str "<summary>" (compose-text parts) "</summary>"))

(defn- summary-tagged? [s]
  (and (string? s)
    (str/starts-with? s "<summary>")
    (str/ends-with? s "</summary>")))

(defn details
  ^String [& parts]
  (let [parts (expand-parts parts)
        strs  (mapv ->str parts)
        {sums true bodies false} (group-by summary-tagged? strs)
        sum   (first sums)
        body  (when (seq bodies) (str/join "\n\n" bodies))]
    (when (> (count sums) 1)
      (throw (ex-info
               (str "details got " (count sums) " <summary>…</summary> parts — at most one is allowed.")
               {:summary-count (count sums)})))
    (cond
      (and sum body) (str "<details>\n" sum "\n\n" body "\n\n</details>")
      sum            (str "<details>\n" sum "\n\n</details>")
      body           (str "<details>\n" body "\n\n</details>")
      :else          "<details>\n\n</details>")))

(defn- normalize-list-items [items]
  (let [unwrapped (if (and (= 1 (count items)) (sequential? (first items)))
                    (first items)
                    items)]
    (remove nil? unwrapped)))

(defn- item-text [x]
  (if (sequential? x)
    (compose-text x)
    (->str x)))

(defn li [& parts]
  (str "- " (compose-text parts)))

(defn- unordered-marker? [s]
  (or (str/starts-with? s "- ")
    (str/starts-with? s "* ")
    (str/starts-with? s "+ ")))

(defn- ordered-marker? [s]
  (boolean (re-find #"^\d+[.)]\s+.*" s)))

(defn- list-marker? [s]
  (or (unordered-marker? s)
    (ordered-marker? s)))

(defn- text-lines [text]
  (str/split text #"\n" -1))

(defn- root-list-block? [text]
  (let [lines (remove str/blank? (text-lines text))]
    (and (< 1 (count lines))
      (every? list-marker? lines))))

(defn- continuation-prefix? [s]
  (boolean (re-matches #"^(?:\s+|[,;:.)\]}]|[—–-]).*" s)))

(defn- dangling-suffix? [s]
  (boolean (re-find #"(?:\s|[({\[:;,—–-])$" s)))

(defn- fragmentable-list-item? [x]
  (and (string? x)
    (not (list-marker? x))
    (not (root-list-block? x))))

(defn- attach-inline-fragment? [current x]
  (and (fragmentable-list-item? current)
    (fragmentable-list-item? x)
    (or (dangling-suffix? current)
      (continuation-prefix? x))))

(defn- coalesce-inline-list-fragments [items]
  (loop [remaining (seq items)
         current   nil
         acc       []]
    (cond
      (nil? remaining)
      (cond-> acc current (conj current))

      (nil? current)
      (recur (next remaining) (first remaining) acc)

      (attach-inline-fragment? current (first remaining))
      (recur (next remaining) (str current (first remaining)) acc)

      :else
      (recur (next remaining) (first remaining) (conj acc current)))))

(defn- indent-lines [indent lines]
  (let [pad (apply str (repeat indent " "))]
    (map #(str pad %) lines)))

(defn- render-prefixed-item [prefix text]
  (let [[head & tail] (text-lines text)]
    (if (seq tail)
      (str prefix head "\n" (str/join "\n" (indent-lines (count prefix) tail)))
      (str prefix head))))

(defn- render-nested-list-block [marker-prefix text]
  (str (str/trimr marker-prefix)
    "\n"
    (str/join "\n" (indent-lines (count marker-prefix) (text-lines text)))))

(defn- render-unordered-item [x]
  (let [text (item-text x)]
    (cond
      (root-list-block? text) (render-nested-list-block "- " text)
      (unordered-marker? text) (render-prefixed-item "" text)
      :else (render-prefixed-item "- " text))))

(defn ul [& items]
  (->> (normalize-list-items items)
    coalesce-inline-list-fragments
    (map render-unordered-item)
    (str/join "\n")))

(defn ol [& items]
  (->> (normalize-list-items items)
    coalesce-inline-list-fragments
    (map-indexed (fn [i x]
                   (let [marker-prefix (str (inc i) ". ")
                         text          (item-text x)
                         text          (if (and (not (root-list-block? text))
                                             (unordered-marker? text))
                                         (subs text 2)
                                         text)]
                     (if (root-list-block? text)
                       (render-nested-list-block marker-prefix text)
                       (render-prefixed-item marker-prefix text)))))
    (str/join "\n")))

(defn checklist [& items]
  (->> (normalize-list-items items)
    (map (fn [it]
           (let [[t d?] (cond
                          (map? it)        [(:text it) (:done? it)]
                          (sequential? it) [(first it) (second it)]
                          :else            [it false])]
             (str "- [" (if d? "x" " ") "] " (item-text t)))))
    (str/join "\n")))

(defn- pipe-escape
  ^String [s]
  (-> (->str s)
    (str/replace "\\" "\\\\")
    (str/replace "|" "\\|")
    (str/replace "\n" " ")))

(defn- align-spec
  ^String [a]
  (case a
    :center " :---: "
    :right  " ---: "
    :left   " :--- "
    " --- "))

(defn table
  (^String [headers rows] (table headers rows nil))
  (^String [headers rows {:keys [align]}]
   (let [n       (count headers)
         pad-row (fn [r]
                   (let [v (vec r)]
                     (vec (for [i (range n)] (nth v i nil)))))
         hdr     (str "| " (str/join " | " (map pipe-escape headers)) " |")
         sep     (str "|"
                   (str/join "|" (for [i (range n)] (align-spec (nth (or align []) i :default))))
                   "|")
         body    (->> (or rows [])
                   (map (fn [r]
                          (str "| " (str/join " | " (map pipe-escape (pad-row r))) " |")))
                   (str/join "\n"))]
     (if (str/blank? body)
       (str hdr "\n" sep)
       (str hdr "\n" sep "\n" body)))))

(defn expand-parts
  "Flatten one level of seqs so callers can mix variadic args with seq-producing
   forms without lazy-seq stringification leaks."
  [parts]
  (persistent!
    (reduce
      (fn [acc p]
        (cond
          (nil? p)        acc
          (sequential? p) (reduce conj! acc (remove nil? p))
          :else           (conj! acc p)))
      (transient [])
      parts)))

(defn join ^String [& parts]
  (->> (expand-parts parts)
    (mapv ->str)
    (str/join "\n\n")))

(defn lines ^String [& parts]
  (->> (expand-parts parts)
    (mapv ->str)
    (str/join "\n")))

(defn section
  (^String [title body] (section 2 title body))
  (^String [level title body]
   (str (h level title) "\n\n" (->str body))))

(defn escape
  ^String [s]
  (str/replace (->str s) #"([\\`*_{}\[\]()#+\-!|>])" "\\\\$1"))

(defn needs-input
  [ask-or-opts]
  (let [{:keys [ask missing] :as opts} (if (map? ask-or-opts)
                                         ask-or-opts
                                         {:ask ask-or-opts})
        ask-text (str/trim (->str ask))]
    (when (str/blank? ask-text)
      (throw (ex-info "needs-input requires a non-blank :ask"
               {:opts opts})))
    (cond-> {:vis/answer-mode :needs-input
             :answer/text ask-text}
      (some? missing) (assoc :missing (->str missing))
      (seq (dissoc opts :ask :missing)) (assoc :metadata (dissoc opts :ask :missing)))))

(def ^:private DEFAULT_OPTS
  "Defaults applied via `merge` over caller `opts`.

   - `:include-meta?`    (default true) emit a one-line italic meta
     suffix on the assistant header (provider, model, iteration count,
     wall duration, cost, tokens) when those fields are present.
   - `:include-system?`  (default false) emit the conversation's
     stored system prompt as a blockquote section between the header
     and the first turn. Off by default because the prompt is usually
     verbose + duplicated across exports of the same conversation.
   - `:turn-separator`   (default `\"---\"`) horizontal rule emitted
     between adjacent turns. Set to `\"\"` (empty string) to drop
     the rules entirely.
   - `:user-label` / `:assistant-label` cosmetic labels. Override for
     non-English exports."
  {:include-meta?   true
   :include-system? false
   :turn-separator  "---"
   :user-label      "You"
   :assistant-label "Assistant"})

(def ^:private SHORT_ID_CHARS
  "How many leading characters of a UUID to surface in the header.
   Matches the convention `vis conversations`, the TUI header, and
   `v/inspect` conversation data \u2014 keeps copy-pasted exports
   trivially correlatable with other tooling."
  8)

(defn- short-id [uuid-or-string]
  (when uuid-or-string
    (let [s (str uuid-or-string)]
      (subs s 0 (min SHORT_ID_CHARS (count s))))))

(defn- normalize-glued-fences
  [text]
  (if-not (str/includes? text "```")
    text
    (letfn [(split-glued-opening-fence [line]
              (when-let [[_ before fence]
                         (and (string? line)
                           (re-matches #"^(.*\S)(```[A-Za-z0-9_+-]*)\s*$" line))]
                [before fence]))
            (split-glued-closing-fence [line]
              (when-let [trimmed (some-> line str/trim)]
                (when (str/starts-with? trimmed "```")
                  (let [tail (subs trimmed 3)]
                    (when (and (not (str/blank? tail))
                            (or (re-find #"\s" tail)
                              (str/includes? tail ":")))
                      ["```" tail])))))]
      (->> (loop [remaining (seq (str/split-lines text))
                  in-code?  false
                  acc       []]
             (if-not remaining
               acc
               (let [line (first remaining)
                     rst  (next remaining)]
                 (cond
                   (and (not in-code?)
                     (split-glued-opening-fence line))
                   (let [[before fence] (split-glued-opening-fence line)]
                     (recur (cons fence rst) in-code? (conj acc before)))

                   (and in-code?
                     (split-glued-closing-fence line))
                   (let [[fence tail] (split-glued-closing-fence line)]
                     (recur (cons tail rst) false (conj acc fence)))

                   :else
                   (let [trimmed (str/trim line)]
                     (if (str/starts-with? trimmed "```")
                       (recur rst (not in-code?) (conj acc line))
                       (recur rst in-code? (conj acc line))))))))
        (str/join "\n")))))

(defn- section-bullet-line?
  "True for a top-level bullet whose whole body is one bold label,
   e.g. `- **Verification**`. These are common LLM final-summary
   section headers; sibling bullets that follow are evidence for the
   section, not peers of the section header."
  [line]
  (boolean
    (and (string? line)
      (re-matches #"^[-*+]\s+\*\*.+\*\*\s*$" line))))

(defn- top-level-list-line? [line]
  (boolean
    (and (string? line)
      (re-matches #"^(?:[-*+]|\d+[.)])\s+.*" line))))

(defn- indented-list-line? [line]
  (boolean
    (and (string? line)
      (re-matches #"^\s+(?:[-*+]|\d+[.)])\s+.*" line))))

(defn- lone-top-level-bullet? [line]
  (boolean
    (and (string? line)
      (re-matches #"^[-*+]\s*$" line))))

(defn- fence-line? [line]
  (boolean
    (and (string? line)
      (str/starts-with? (str/trim line) "```"))))

(defn- normalize-summary-section-bullets
  "Repair malformed final-answer summaries where a section bullet is
   followed by sibling evidence bullets:

       - **Verification**
       - All cases verified via nREPL:
       ```clojure
       ...
       ```

   The renderer has a visual hierarchy, so normalize this to nested
   evidence before rendering/exporting. Existing nested bullets are
   preserved; a lone `-` directly under a section is treated as LLM
   noise and dropped."
  [text]
  (->> (loop [remaining   (seq (str/split-lines text))
              in-section? false
              in-child?   false
              in-code?    false
              code-prefix nil
              acc         []]
         (if-not remaining
           acc
           (let [line (first remaining)
                 rst  (next remaining)]
             (cond
               in-code?
               (let [out      (str code-prefix line)
                     closing? (fence-line? line)]
                 (recur rst in-section? in-child? (not closing?) (when-not closing? code-prefix) (conj acc out)))

               (section-bullet-line? line)
               (recur rst true false false nil (conj acc line))

               (and in-section? (lone-top-level-bullet? line))
               (recur rst in-section? in-child? false nil acc)

               (and in-section? in-child? (fence-line? line))
               (let [prefix "      "]
                 (recur rst in-section? in-child? true prefix (conj acc (str prefix line))))

               (and in-section? (top-level-list-line? line))
               (recur rst true true false nil (conj acc (str "  " line)))

               (and in-section? (indented-list-line? line))
               (recur rst true true false nil (conj acc line))

               (and in-section? (str/blank? line))
               (recur rst in-section? in-child? false nil (conj acc line))

               (and in-section? (not (str/blank? line)))
               (recur rst false false false nil (conj acc line))

               :else
               (recur rst false false false nil (conj acc line))))))
    (str/join "\n")))

(defn normalize-chat-markdown
  "Repair narrow classes of malformed Markdown emitted by LLM answers.

   High-confidence fixes only:

   1. Opening fence glued to preceding prose:
        `Intro:```text` -> `Intro:\n```text`

   2. Closing fence glued to following prose while already inside a
      fenced block:
        ` ```Done.` -> ` ```\nDone.`

   3. Final-summary section bullets (`- **Verification**`) followed by
      sibling evidence bullets are normalized into nested bullets, with
      a following fenced code block nested under the evidence bullet.

   This preserves the visible prose while making the answer renderable
   in chat/export surfaces. Forensic transcript views intentionally
   keep the stored raw text and do NOT call this helper."
  [text]
  (if-not (string? text)
    text
    (-> text
      normalize-glued-fences
      normalize-summary-section-bullets)))

(defn- comma-int
  "Group-3 thousands separator for integers, US locale-style. Used by
   the token line so a 12,345-prompt-token export reads naturally.
   Returns nil for nil / non-numeric input so the caller can skip it."
  [n]
  (when (number? n)
    (String/format java.util.Locale/US "%,d" (object-array [(long n)]))))

(defn- format-tokens-line
  "Compact token summary like `1,234 in / 567 out` if either side is
   present, else nil. Each non-positive / nil component is skipped so
   the line never reads `0 think /  cached` for turns that didn't
   exercise reasoning or cache."
  [{:keys [input-tokens output-tokens reasoning-tokens cached-tokens]}]
  (let [emit  (fn [n suffix]
                (when (and (number? n) (pos? (long n)))
                  (str (comma-int n) " " suffix)))
        bits  (keep identity
                [(emit input-tokens     "in")
                 (emit output-tokens    "out")
                 (emit reasoning-tokens "think")
                 (emit cached-tokens    "cached")])]
    (when (seq bits)
      (str/join " / " bits))))

(defn- format-turn-meta
  "Italic meta blob appended to the `**Assistant:**` line when
   `:include-meta?` is on. Returns nil when nothing useful to surface."
  [turn]
  (let [parts (cond-> []
                (:provider turn)        (conj (name (:provider turn)))
                (:model turn)           (conj (:model turn))
                (:iteration-count turn) (conj (str (:iteration-count turn)
                                                " iter"
                                                (when (> (long (:iteration-count turn)) 1) "s")))
                (:duration-ms turn)     (conj (fmt/format-duration (:duration-ms turn)))
                (:total-cost turn)      (conj (fmt/format-cost (:total-cost turn))))
        tokens (format-tokens-line turn)
        parts (if tokens (conj parts tokens) parts)]
    (when (seq parts)
      (str/join " \u00b7 " parts))))

(defn- export-blockquote
  "Wrap each line of `text` with `> ` so the whole thing renders as a
   Markdown blockquote. Empty input -> nil (caller drops the section)."
  [text]
  (when (and (string? text) (not (str/blank? text)))
    (->> (str/split-lines text)
      (map #(if (str/blank? %) ">" (str "> " %)))
      (str/join "\n"))))

(defn- render-header
  "Title line + one-line metadata blockquote describing the
   conversation as a whole. Always at least the title; the meta
   blockquote is dropped when no fact is known."
  [conversation turn-count]
  (let [title (or (:title conversation) "Untitled conversation")
        meta  (cond-> []
                (:id conversation)         (conj (str "id `" (short-id (:id conversation)) "`"))
                (:channel conversation)    (conj (str "channel `" (name (:channel conversation)) "`"))
                (pos? turn-count)          (conj (str turn-count " turn"
                                                   (when (> turn-count 1) "s")))
                (:provider conversation)   (conj (name (:provider conversation)))
                (:model conversation)      (conj (:model conversation))
                (:created-at conversation) (conj (fmt/format-date (:created-at conversation))))
        meta-line (when (seq meta)
                    (str "> " (str/join " \u00b7 " meta)))]
    (str/join "\n\n"
      (cond-> [(str "# " title)]
        meta-line (conj meta-line)))))

(defn- render-system-block
  "Optional system-prompt section (blockquoted). Only emitted when
   `:include-system?` is on AND the conversation has a stored prompt."
  [conversation]
  (when-let [prompt (:system-prompt conversation)]
    (str "## System prompt\n\n" (export-blockquote prompt))))

(defn- render-turn-body
  "Render one turn as: `## Turn N` heading, blockquoted user request,
   `**Assistant:**` line (with italic meta when enabled), and the
   answer body verbatim. Skips the answer paragraph when the turn
   has no `:answer` (in-flight or errored), substituting a short
   placeholder so the export stays diff-friendly across re-exports."
  [{:keys [user-label assistant-label include-meta?] :as _opts} index turn]
  (let [meta        (when include-meta? (format-turn-meta turn))
        user-block  (or (export-blockquote (:user-request turn)) "> *(empty user request)*")
        answer-text (cond
                      (:answer turn) (normalize-chat-markdown (:answer turn))
                      (= :error (:status turn)) "*(turn errored \u2014 no answer recorded)*"
                      :else "*(no answer recorded yet)*")
        sections    [(str "## Turn " (inc index))
                     (str "**" user-label ":**")
                     user-block
                     (str "**" assistant-label ":**"
                       (when meta (str " *\u2014 " meta "*")))
                     answer-text]]
    (str/join "\n\n" sections)))

(defn- render-turns
  "Join every turn with a blank-padded horizontal rule (or just blank
   lines when `:turn-separator` is `\"\"`) so the exported document
   is readable both raw and after a Markdown render pass."
  [{:keys [turn-separator] :as opts} turns]
  (let [bodies (map-indexed (partial render-turn-body opts) turns)
        sep    (if (str/blank? turn-separator)
                 "\n\n"
                 (str "\n\n" turn-separator "\n\n"))]
    (str/join sep bodies)))

(defn conversation->markdown
  "Project a full conversation as Markdown.

   `db-info` is the persistance handle (typically `(:db-info env)` or
   the singleton from `lp/get-db-info`).
   `conversation-ref` accepts the same shapes `persistance/db-get-conversation`
   does: a UUID, a `[:id uuid]` vector, a string, or a map carrying
   `:id`. Resolves to `nil` cleanly when the conversation is unknown.

   Returns a Markdown string, or `nil` when the conversation can't be
   located (so callers can pattern-match instead of catching).

   See `DEFAULT_OPTS` for tunables. The returned string never ends in
   a trailing newline \u2014 callers append one (or not) per their
   destination's convention (clipboard usually doesn't want one)."
  ([db-info conversation-ref]
   (conversation->markdown db-info conversation-ref nil))
  ([db-info conversation-ref opts]
   (when (and db-info conversation-ref)
     (let [opts         (merge DEFAULT_OPTS opts)
           conversation (persistance/db-get-conversation db-info conversation-ref)
           turns        (vec (or (persistance/db-list-conversation-turns db-info conversation-ref) []))]
       (when conversation
         (let [chunks (cond-> [(render-header conversation (count turns))]
                        (:include-system? opts) (conj (render-system-block conversation))
                        (seq turns)             (conj (render-turns opts turns)))]
           (->> chunks
             (remove nil?)
             (str/join "\n\n"))))))))
