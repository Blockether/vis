(ns com.blockether.vis.ext.channel-tui.file-suggest
  "Inline `@` file-mention suggestions for the TUI composer — the SAME
   affordance the web composer already has, so both channels share one
   behaviour instead of a modal on one side and an inline picker on the
   other.

   Ranking is powered by fff (`internal.file-picker/fuzzy-file-rows`) — the
   very same engine behind the `find_files` tool and the gateway
   `/v1/sessions/:sid/suggest` service — so `@fpick` fuzzily finds
   `file_picker.clj` (typo-tolerant subsequence match ranked by frecency),
   not just a literal substring.

   The trigger rules mirror the web/JS verbatim so writing a literal `@`
   is never endangered:

   - the `@` must begin a word (start of input or right after whitespace),
     so `foo@bar`, `user@host`, decorators never pop the picker;
   - `@@` escapes to a literal `@` and suppresses the popup;
   - selection is advisory — nothing is rewritten unless the user picks.

   fff owns a FRESH in-memory index instance that is opened OFF the render
   thread and cached for the session; per-keystroke `search` on the open
   instance is sub-millisecond, so filtering stays instant. The instance is
   NEVER closed while a search may run (fff's native search SIGSEGVs on a
   closed handle); a periodic rebuild swaps a new instance in and closes the
   superseded one only after a grace period."
  (:require [clojure.string :as str]
            [com.blockether.vis.ext.channel-tui.input :as input]
            [com.blockether.vis.internal.file-picker :as picker]))

(def ^:private index-ttl-ms 30000)
(def ^:private max-rows 20)

;; {:idx <Closeable fff> :built-at <ms> :building? <bool>}
(defonce ^:private index-cache (atom {:idx nil :built-at 0 :building? false}))

(defn- status-glyph
  "Collapse a git status word (\"modified\", \"untracked\", …) to the single
   letter git itself uses, so the picker meta stays one column, not a word."
  [status]
  (case status
    "modified"
    "M"

    "untracked"
    "?"

    ("added" "created" "new" "staged")
    "A"

    "deleted"
    "D"

    "renamed"
    "R"

    "copied"
    "C"

    ("conflict" "conflicted" "unmerged")
    "U"

    "typechange"
    "T"

    "ignored"
    "I"

    (some-> status
            not-empty
            (subs 0 1)
            str/upper-case)))

(defn- fresh?
  [{:keys [idx built-at]}]
  (and idx (< (- (System/currentTimeMillis) (long built-at)) index-ttl-ms)))

(defn- close-later!
  "Close a superseded fff instance, but only after a grace period so no
   in-flight synchronous search (sub-ms) touches a freed native handle."
  [^java.io.Closeable idx]
  (when idx (future (Thread/sleep 2000) (try (.close idx) (catch Throwable _ nil)))))

(defn- kick-refresh!
  "Rebuild the fff index in the background unless a build is already in
   flight. Non-blocking: the render thread never waits on the scan."
  []
  (let [[old _] (swap-vals! index-cache
                            (fn [c]
                              (if (:building? c) c (assoc c :building? true))))]
    (when-not (:building? old)
      (future (let [new-idx (try (picker/open-fuzzy-index) (catch Throwable _ nil))
                    [prev _] (swap-vals! index-cache
                                         (fn [c]
                                           (assoc c
                                             :idx (or new-idx (:idx c))
                                             :built-at (System/currentTimeMillis)
                                             :building? false)))]

                (when (and new-idx (:idx prev) (not (identical? new-idx (:idx prev))))
                  (close-later! (:idx prev))))))))

(defn- ensure-index!
  "Return the cached OPEN fff instance, kicking a background refresh when the
   cache is missing or stale. Returns nil until the first scan lands (callers
   simply show nothing until then)."
  []
  (let [c @index-cache]
    (when-not (fresh? c) (kick-refresh!))
    (:idx c)))

(def ^:private trigger-regex #"(?:^|\s)@(?!@)(\S*)$")

(defn- head-text
  "Input text up to the caret — the region the trigger looks back over."
  [{:keys [lines crow ccol]}]
  (let [lines
        (vec lines)

        crow
        (long crow)]

    (str/join "\n" (conj (subvec lines 0 crow) (subs (nth lines crow) 0 (long ccol))))))

(defn mention-at
  "Return `{:query q :at start}` for an active `@` file mention ending at
   the caret, or nil. `head` is the input text up to the caret. `start` is
   the index of the `@` within `head`."
  [head]
  (when-let [m (re-find trigger-regex head)]
    (let [q (nth m 1)]
      {:query q :at (- (count head) (count q) 1)})))

(defn suggestions
  "File-mention suggestions for `input-state`, shaped to ride the SAME
   overlay + key handling as slash suggestions: `:slash/usage` is the
   `path` chip, `:label` the size · age · status meta, `:slash/selected?`
   marks the cursor row. Returns nil when there is no active `@` mention
   at the caret (so the slash path stays in charge)."
  [input-state selected-index]
  (when-let [{:keys [query]} (mention-at (head-text input-state))]
    (when-let [idx (ensure-index!)]
      (let [rows (try (picker/fuzzy-file-rows idx query {:limit max-rows}) (catch Throwable _ nil))
            n (count rows)
            sel (max 0 (min (dec n) (long (or selected-index 0))))]

        (when (seq rows)
          (map-indexed (fn [idx it]
                         (let [status (:status-label it)
                               meta (->> [(:size-label it) (:age-label it)
                                          (when (and status (not= "clean" status))
                                            (status-glyph status))]
                                         (remove str/blank?)
                                         (str/join " · "))]

                           {:file/mention? true
                            :file/path (:path it)
                            :slash/name (:path it)
                            :slash/usage (:path it)
                            :label meta
                            :slash/selected? (= idx sel)}))
                       rows))))))

(defn apply-mention
  "Splice the picked `path` into `input-state`, replacing the active `@token`
   at the caret with a visible file mention (`input/format-file-mention`) plus
   a trailing space. Returns the input unchanged when no mention is active."
  [{:keys [lines crow ccol] :as st} path]
  (let [head (head-text st)]
    (if-let [{:keys [at]} (mention-at head)]
      (let [lines (vec lines)
            crow (long crow)
            ccol (long ccol)
            line (nth lines crow)
            line-start (- (count head) ccol) ; head offset where the current line begins
            col (max 0 (- at line-start)) ; column of the `@` on the current line
            before (subs line 0 col)
            after (subs line ccol)
            mention (str (input/format-file-mention path) " ")
            new-line (str before mention after)]

        {:lines (assoc lines crow new-line) :crow crow :ccol (+ (count before) (count mention))})
      st)))
