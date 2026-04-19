(ns com.blockether.vis.loop.runtime.formatters
  "Pure `:format-result` formatters for built-in RLM tools.

   Every fn in this namespace:
   - Takes EXACTLY one arg (the tool's raw return value).
   - Is pure — no env, no dynamic vars, no other iteration state.
   - Returns a plain string, safe to embed inside a mustache template.
   - Handles `nil` (the validator probes the formatter with nil).

   Tools that return pre-formatted markdown / patch strings (search-documents,
   search-batch, git-commit-diff) intentionally use the default formatter:
   strings can't carry `:rlm/format` metadata, so a custom formatter here
   would be inert for the LLM serializer and for the sandbox println override.
   Their callers already format for human consumption upstream.

   Formatters here target IObj returns (maps / vectors / sets) where the
   default pr-str dump is a token tax."
  (:require [clojure.string :as str]))

;; =============================================================================
;; Helpers
;; =============================================================================

(defn- fmt-inst
  "Short ISO-ish date (YYYY-MM-DD) for any inst-like value, or '-'."
  [x]
  (cond
    (instance? java.util.Date x) (subs (.toString (.toInstant ^java.util.Date x)) 0 10)
    (instance? java.time.Instant x) (subs (.toString ^java.time.Instant x) 0 10)
    (inst? x) (let [s (str x)] (if (>= (count s) 10) (subs s 0 10) s))
    (string? x) (if (>= (count x) 10) (subs x 0 10) x)
    :else "-"))

(defn- short-sha [sha] (if (and (string? sha) (>= (count sha) 7)) (subs sha 0 7) (str sha)))

(defn- name-or-str [x] (if (keyword? x) (name x) (str x)))

(defn- truncate-str
  [s n]
  (let [s (str s)]
    (if (> (count s) n) (str (subs s 0 n) "…") s)))

(defn- one-liner
  "Collapse whitespace + trim to at most n chars, suitable for summary rows."
  [s n]
  (truncate-str (str/trim (str/replace (or s "") #"\s+" " ")) n))

(defn- lines-with-limit
  "Join rows with newlines. If rows exceeds `max`, keeps `max` and appends
   a trailing '... N more' marker."
  [rows max]
  (let [shown (take max rows)
        more  (- (count rows) (count shown))]
    (str (str/join "\n" shown)
      (when (pos? more) (str "\n... " more " more")))))

;; =============================================================================
;; Conversation history / code / results
;; =============================================================================

(defn format-conversation-history
  "Vec of {:query-pos :query-id :text :answer-preview :status :iterations
   :key-vars :created-at} → compact table."
  [rows]
  (if (empty? rows)
    "no history"
    (str "conversation history — " (count rows) " query/ies\n"
      (lines-with-limit
        (map (fn [{:keys [query-pos text status iterations key-vars created-at]}]
               (str "  #" query-pos " [" (fmt-inst created-at) "] "
                 (when status (str status " "))
                 (when iterations (str "(" iterations "it) "))
                 (one-liner text 60)
                 (when (seq key-vars) (str "  vars: " (str/join "," key-vars)))))
          rows)
        30))))

(defn format-conversation-code
  "Vec of {:iteration-pos :created-at :code :answer} → iteration summaries."
  [rows]
  (if (empty? rows)
    "no code"
    (str "query code — " (count rows) " iteration(s)\n"
      (lines-with-limit
        (map (fn [{:keys [iteration-pos created-at code answer]}]
               (str "  it#" iteration-pos " [" (fmt-inst created-at) "]"
                 (when (seq code)
                   (str " — " (count code) " block(s): "
                     (one-liner (str/join " | " (map pr-str (take 3 code))) 120)))
                 (when answer (str "\n    → " (one-liner answer 140)))))
          rows)
        20))))

(defn format-conversation-results
  "Vec of {:iteration-pos :created-at :results :vars :answer} → summaries."
  [rows]
  (if (empty? rows)
    "no results"
    (str "query results — " (count rows) " iteration(s)\n"
      (lines-with-limit
        (map (fn [{:keys [iteration-pos results vars answer]}]
               (str "  it#" iteration-pos
                 " — " (count results) " result(s), " (count vars) " var(s)"
                 (when (seq vars)
                   (str " [" (str/join "," (map :name (take 8 vars))) "]"))
                 (when answer (str "\n    → " (one-liner answer 140)))))
          rows)
        20))))

;; =============================================================================
;; Restore
;; =============================================================================

(defn format-restore-vars
  "Map sym → value-or-error → summary of successes vs failures."
  [m]
  (if (empty? m)
    "no vars restored"
    (let [entries (seq m)
          ok   (filter (fn [[_ v]] (not (and (map? v) (:error v)))) entries)
          bad  (filter (fn [[_ v]] (and (map? v) (:error v))) entries)]
      (str "restored " (count ok) "/" (count entries) " var(s)"
        (when (seq ok)
          (str "\n  ok: " (str/join ", " (map (comp str first) ok))))
        (when (seq bad)
          (str "\n  failed:"
            (lines-with-limit
              (map (fn [[sym v]]
                     (str "\n    " sym " — "
                       (or (get-in v [:error :message]) "unknown error")))
                bad)
              10)))))))

;; =============================================================================
;; Var history / diff
;; =============================================================================

(defn format-var-history
  "Vec of {:version :value :code :diffable? :query-id :created-at}."
  [rows]
  (if (empty? rows)
    "no versions"
    (str "var history — " (count rows) " version(s)\n"
      (lines-with-limit
        (map (fn [{:keys [version created-at diffable? value]}]
               (str "  v" version " [" (fmt-inst created-at) "]"
                 (when-not diffable? " [non-diffable]")
                 " — " (one-liner (pr-str value) 120)))
          rows)
        20))))

(defn format-var-diff
  "Map dispatched on :type. Pre-formatted patch text for :string-diff;
   compact summary for others."
  [{:keys [type from-version to-version unified edit-count edits from to delta pct-change] :as result}]
  (if (nil? result)
    ""
    (case type
      :string-diff
      (str "diff v" from-version " → v" to-version " (" edit-count " edit(s))\n"
        (truncate-str unified 4000))

      :number-delta
      (str "diff v" from-version " → v" to-version ": " from " → " to
        " (Δ " delta
        (when pct-change
          ;; Force US locale — %.1f honors JVM default locale (',' in many EU
          ;; locales), breaking both deterministic output AND common numeric
          ;; regex assumptions upstream.
          (String/format java.util.Locale/US " / %.1f%%" (into-array Object [(double pct-change)])))
        ")")

      :replacement
      (str "diff v" from-version " → v" to-version ": replace\n"
        "  from: " (one-liner (pr-str from) 120) "\n"
        "    to: " (one-liner (pr-str to) 120))

      :structural
      (str "diff v" from-version " → v" to-version " — " edit-count " structural edit(s)\n"
        (lines-with-limit
          (map (fn [{:keys [op path value]}]
                 (str "  " (name op) " " (pr-str path)
                   (when (contains? #{:added :changed} op)
                     (str " = " (one-liner (pr-str value) 80)))))
            edits)
          15))

      ;; Unknown :type — fallback to pr-str (short).
      (one-liner (pr-str result) 500))))

;; =============================================================================
;; Git
;; =============================================================================

(defn- fmt-commit-row
  "Single commit as a compact row. Works for both DB-backed and JGit-backed
   commit maps — picks whatever author field is present."
  [c]
  (let [{commit-title :name :keys [sha date category author author-email ticket-refs file-paths subject]} c
        title (or commit-title subject "")
        who   (or author author-email "?")
        cat   (when category (str "[" (name-or-str category) "] "))
        tix   (when (seq ticket-refs) (str " (" (str/join "," ticket-refs) ")"))
        fc    (when (seq file-paths) (str " " (count file-paths) "f"))]
    (str "  " (short-sha sha) " " (fmt-inst date) " " who " " (or cat "")
      (one-liner title 80) (or tix "") (or fc ""))))

(defn format-commit-list
  "Shared formatter for git-search-commits, git-commit-history,
   git-commits-by-ticket, git-file-history. Same vec-of-commit-maps shape."
  [rows]
  (cond
    (nil? rows) ""
    (empty? rows) "no commits"
    :else (str (count rows) " commit(s)\n"
            (lines-with-limit (map fmt-commit-row rows) 30))))

(defn format-commit-parents
  "Vec of SHA strings."
  [shas]
  (cond
    (nil? shas) ""
    (empty? shas) "no parents (root)"
    :else (str (count shas) " parent(s): " (str/join " " (map short-sha shas)))))

(defn format-blame
  "Vec of {:line :sha :short :author :email :date :content}."
  [rows]
  (if (empty? rows)
    "no blame"
    (str "blame — " (count rows) " line(s)\n"
      (lines-with-limit
        (map (fn [{:keys [line short author date content]}]
               (str "  " line ": " (or short "?") " " (fmt-inst date) " "
                 (one-liner (or author "?") 20) " │ "
                 (one-liner content 100)))
          rows)
        50))))

;; =============================================================================
;; Concepts
;; =============================================================================

(defn format-concept-info
  "Map {:term :definition :group :status :aliases :sources} or nil."
  [c]
  (cond
    (nil? c) "concept not found"
    :else (let [{:keys [term definition group status aliases sources]} c]
            (str term
              (when group (str " [" group "]"))
              (when status (str " (" status ")"))
              (when (seq aliases) (str " a.k.a. " (str/join ", " aliases)))
              (when definition (str "\n  " (one-liner definition 400)))
              (when (seq sources)
                (str "\n  " (count sources) " source(s)"))))))

(defn format-concept-mutation
  "Small map — {:removed X :rationale Y} or {:edited X :updates {...}}."
  [m]
  (cond
    (nil? m) ""
    (:removed m) (str "removed " (:removed m)
                   (when-let [r (:rationale m)] (str " — " (one-liner r 200))))
    (:edited m) (str "edited " (:edited m)
                  (when-let [u (:updates m)]
                    (str " " (one-liner (pr-str u) 200))))
    :else (one-liner (pr-str m) 200)))
