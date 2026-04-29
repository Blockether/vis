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
   `(vis/conversation)` already use \u2014 keeps copy-pasted exports
   trivially correlatable with other tooling."
  8)

(defn- short-id [uuid-or-string]
  (when uuid-or-string
    (let [s (str uuid-or-string)]
      (subs s 0 (min SHORT_ID_CHARS (count s))))))

(defn- format-tokens-line
  "Compact token summary like `1,234 in / 567 out` if either side is
   present, else nil. Mirrors what the TUI footer surfaces so the two
   channels can't disagree on the wording."
  [{:keys [input-tokens output-tokens reasoning-tokens cached-tokens]}]
  (let [bits (cond-> []
               input-tokens     (conj (str (fmt/format-tokens input-tokens) " in"))
               output-tokens    (conj (str (fmt/format-tokens output-tokens) " out"))
               reasoning-tokens (conj (str (fmt/format-tokens reasoning-tokens) " think"))
               cached-tokens    (conj (str (fmt/format-tokens cached-tokens) " cached")))]
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

(defn- blockquote
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
    (str "## System prompt\n\n" (blockquote prompt))))

(defn- render-turn-body
  "Render one turn as: `## Turn N` heading, blockquoted user request,
   `**Assistant:**` line (with italic meta when enabled), and the
   answer body verbatim. Skips the answer paragraph when the turn
   has no `:answer` (in-flight or errored), substituting a short
   placeholder so the export stays diff-friendly across re-exports."
  [{:keys [user-label assistant-label include-meta?] :as _opts} index turn]
  (let [meta        (when include-meta? (format-turn-meta turn))
        user-block  (or (blockquote (:text turn)) "> *(empty user request)*")
        answer-text (cond
                      (:answer turn) (:answer turn)
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
           turns        (vec (or (persistance/db-list-conversation-queries db-info conversation-ref) []))]
       (when conversation
         (let [chunks (cond-> [(render-header conversation (count turns))]
                        (:include-system? opts) (conj (render-system-block conversation))
                        (seq turns)             (conj (render-turns opts turns)))]
           (->> chunks
             (remove nil?)
             (str/join "\n\n"))))))))
