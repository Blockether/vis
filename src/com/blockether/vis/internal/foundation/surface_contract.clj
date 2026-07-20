(ns com.blockether.vis.internal.foundation.surface-contract
  "clojure.spec CONTRACT for the language-surface tool RESULTS (`format_code`,
   `lint_code`).

   Every language pack that registers a `:format-fn` / `:lint-fn` under
   `:ext/language-tools` returns a result map that MUST conform to these specs,
   so the shape is UNIFORM across packs (clojure, and a future python / js) and
   can never silently drift. Both results share the directory-nested `by-dir`
   grouping (`{<dir> {<basename> <payload>}}`) that writes each long directory
   prefix ONCE.

   The result maps cross the strings-only Python boundary, so their keys are
   STRINGS (\"op\", \"findings\", \"by-dir\", ...). clojure.spec's `s/keys` only
   speaks keyword keys, so the map specs here are plain predicates over the
   string keys, composed from `s/map-of` / `s/coll-of` for the nested pieces.

   `check` validates a result and returns it UNCHANGED, throwing ex-info with
   `s/explain-data` when it violates the contract — the schema check the packs
   run on every format/lint result before handing it back through the surface.
   `capability->spec` is the single source of truth mapping a capability keyword
   to its result spec."
  (:require [clojure.spec.alpha :as s]))

;; =============================================================================
;; Shared: the directory-nested grouping BOTH format and lint expose
;; =============================================================================

;; `{<dir> {<basename> <payload-map>}}` — the long directory prefix is written
;; ONCE per group (dir), then each file's basename under it. `<payload>` is a
;; map: for lint the level->findings map, for format the per-file flag map.
(s/def ::by-dir (s/map-of string? (s/map-of string? map?)))

(defn- opt
  "A predicate over string key `k`: true when `m` lacks `k`, else `pred` holds
   on its value. Optional keys never fail merely by being absent."
  [k pred]
  (fn [m]
    (or (not (contains? m k)) (pred (get m k)))))

;; =============================================================================
;; format_code result
;; =============================================================================

(s/def ::format-file
  (s/and map?
         #(string? (get % "path"))
         #(contains? % "changed")))

(s/def ::format-result
  (s/and map?
         #(string? (get % "op"))
         (opt "changed" #(or (boolean? %) (nat-int? %)))
         (opt "files" #(s/valid? (s/coll-of ::format-file) %))
         (opt "by-dir" #(s/valid? ::by-dir %))))

;; =============================================================================
;; lint_code result
;; =============================================================================

(s/def ::finding
  (s/and map?
         #(string? (get % "level"))
         #(string? (get % "message"))))

(defn- count-key
  "The value at string key `k` is a non-negative count (or absent, i.e. 0)."
  [k]
  (fn [m]
    (let [v (get m k)]
      (or (nil? v) (and (number? v) (not (neg? (long v))))))))

(s/def ::lint-result
  (s/and map?
         (count-key "error")
         (count-key "warning")
         (count-key "info")
         #(s/valid? (s/coll-of ::finding) (get % "findings"))
         (opt "providers" #(s/valid? (s/coll-of string?) %))
         (opt "by-dir" #(s/valid? ::by-dir %))))

;; =============================================================================
;; Capability -> spec + the check the packs run
;; =============================================================================

(def capability->spec
  "Maps a language-tool capability keyword to the spec its result must satisfy.
   Capabilities absent here (`:test-fn`, `:repl-eval-fn`, ...) are unconstrained."
  {:format-fn ::format-result :lint-fn ::lint-result})

(defn valid?
  "True when `result` conforms to the contract for `capability` (or the
   capability has no registered spec)."
  [capability result]
  (if-let [spec (get capability->spec capability)]
    (s/valid? spec result)
    true))

(defn explain
  "The human explain string for a non-conforming `result`, or nil when valid /
   the capability has no registered spec."
  [capability result]
  (when-let [spec (get capability->spec capability)]
    (when-not (s/valid? spec result) (s/explain-str spec result))))

(defn check
  "Validate `result` against the language-surface contract for `capability`,
   returning it UNCHANGED when it conforms. Throws ex-info carrying the spec
   `explain-data` when it violates the contract. Unknown capabilities pass
   through unchecked, so this is a no-op for tools without a registered spec."
  [capability result]
  (if-let [spec (get capability->spec capability)]
    (if (s/valid? spec result)
      result
      (throw (ex-info (str "language-surface contract violation for " capability)
                      {:type :surface/contract-violation
                       :capability capability
                       :explain-data (s/explain-data spec result)})))
    result))
