(ns com.blockether.vis.internal.file-picker
  "Backend for file-picking UIs (the `@` mention picker, TUI + web).

   Everything here rides the ONE canonical pooled fff index
   (`internal.fff-index`) that the `grep` / `find_files` tools use: fff owns the
   tree walk, the gitignore policy, the git-status metadata and the
   frecency-ranked fuzzy match. This namespace only leases that index and turns
   fff rows into display rows.

   There is deliberately NO Clojure-side directory walk, git-status subprocess,
   ignore matcher or scoring heuristic left in here — reintroducing one means the
   picker and the search tools would rank and see different files."
  (:require [clojure.string :as str]
            [com.blockether.vis.internal.workspace :as workspace]
            [com.blockether.fff :as fff]
            [com.blockether.vis.internal.fff-index :as fff-index])
  (:import [java.nio.file Path]
           [java.util Locale]))

(def ^:const max-results 200)

(defn cwd-path
  "Current explicit workspace cwd as a Path. Indirected for tests."
  ^Path []
  (.toPath (workspace/cwd)))

(defn format-bytes
  "Human-ish byte string for picker rows."
  [^long n]
  (cond (< n 1024) (str n "B")
        (< n (* 1024 1024))
        (String/format Locale/US "%.1fK" (into-array Object [(/ (double n) 1024.0)]))
        :else (String/format Locale/US "%.1fM" (into-array Object [(/ (double n) 1048576.0)]))))

(defn format-relative-age
  "Compact relative age for picker rows."
  [^long now-ms ^long mtime-ms]
  (let
    [delta-ms
     (long (max 0 (- now-ms mtime-ms)))

     minutes
     (quot delta-ms 60000)

     hours
     (quot delta-ms 3600000)

     days
     (quot delta-ms 86400000)]

    (cond (< minutes 1) "now"
          (< minutes 60) (str minutes "m")
          (< hours 24) (str hours "h")
          :else (str days "d"))))

;; ── fff-backed fuzzy search (shared with the `grep` tool) ───────────────
;; The `@`-mention pickers (TUI + gateway/web) rank files with the SAME engine
;; the `grep` tool uses — real typo-tolerant subsequence matching ranked
;; by frecency — over the same pooled index.

(defn cwd-lease
  "The canonical pooled-fff lease for the current workspace cwd: gitignore
   respected, exactly like the `@` picker and the `find_files` tool. Every
   picker search goes through this so the UI shares ONE index with the search
   tools instead of scanning the tree again per popup."
  []
  (fff-index/lease (.toFile (cwd-path)) true))

(defn index-warm?
  "True when the picker's pooled index is already built, so `fuzzy-file-rows`
   costs only a search."
  []
  (fff-index/warm? (cwd-lease)))

(defn prewarm-index!
  "Kick the picker's pooled index build off the caller's thread (no-op when it
   is already warm)."
  []
  (fff-index/prewarm! (cwd-lease)))

(defn fuzzy-file-rows
  "Frecency-ranked, typo-tolerant fuzzy file search via fff — the SAME pooled
   index (and therefore the same ranking) the `grep`/`find_files` tools use.
   Returns display rows (`:path :label :status-label :size-label :age-label`),
   capped at `limit`.

   The index is leased from `internal.fff-index` for the call only: callers own
   nothing and must not close anything. The FIRST call on a cold pool blocks for
   the tree scan — a render thread should gate on `index-warm?` / `prewarm-index!`.

   A blank `query` yields fff's default frecency/recency ordering."
  ([query] (fuzzy-file-rows query {}))
  ([query {:keys [now-ms limit] :or {now-ms (System/currentTimeMillis) limit max-results}}]
   (fff-index/with-index
     [idx (cwd-lease)]
     (->> (:items (fff/search idx {:query (or query "") :page-size limit}))
          (mapv (fn [{:keys [relative-path git-status size modified]}]
                  (let
                    [status (when (and (string? git-status)
                                       (not (str/blank? git-status))
                                       (not= "clean" git-status))
                              git-status)]
                    {:path relative-path
                     :label relative-path
                     :status-label (or status "clean")
                     :size-label (format-bytes (or size 0))
                     ;; fff `:modified` is epoch SECONDS; the age helper wants ms.
                     :age-label (format-relative-age now-ms (* 1000 (long (or modified 0))))})))))))

(defn ->wire
  "Project ONE rich fuzzy/picker row (`:path :size-label :age-label
   :status-label`, the shape `fuzzy-file-rows` yields) into the channel-agnostic
   WIRE shape `{:name :size :age :status}` the gateway `/v1/sessions/:sid/suggest`
   service serves to the web composer.

   This is the SINGLE web-specific step: both the web and the TUI start from the
   SAME rich rows; the TUI renders them in-process (richer `size · age · status`
   chip), the web projects them last through here."
  [{:keys [path size-label age-label status-label]}]
  {:name (or (some-> path
                     str)
             "")
   :size (or size-label "")
   :age (or age-label "")
   :status (or status-label "")})

(defn suggest-file-rows
  "Self-contained fuzzy file suggestion for the shared `@`/suggest surface: the
   gateway `/v1/sessions/:sid/suggest` service and any other caller project the
   SAME `fuzzy-file-rows` engine the TUI picker uses through `->wire`, so web and
   TUI never diverge on ranking or field derivation — only on the final shape.

   Returns the channel-agnostic WIRE rows `{:name :size :age :status}` (bare
   relative path in `:name`). Never throws; on any error yields `[]`."
  ([query] (suggest-file-rows query {}))
  ([query {:keys [limit] :or {limit max-results}}]
   (try (into [] (map ->wire) (fuzzy-file-rows query {:limit limit})) (catch Throwable _ []))))
