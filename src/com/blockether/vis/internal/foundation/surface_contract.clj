(ns com.blockether.vis.internal.foundation.surface-contract
  "clojure.spec CONTRACT for the language-surface tool RESULTS (`format_code`,
   `lint_code`, `run_tests`).

   Every language pack that registers a `:format-fn` / `:lint-fn` / `:test-fn`
   under
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
  (:require [clojure.spec.alpha :as s]
            [clojure.string :as str]))

;; =============================================================================
;; Shared: the directory-nested grouping BOTH format and lint expose
;; =============================================================================

;; `{<dir> {<basename> <payload-map>}}` — the long directory prefix is written
;; ONCE per group (dir), then each file's basename under it. `<payload>` is a
;; map: for lint the level->findings map, for format the per-file flag map.
(s/def ::by-dir (s/map-of string? (s/map-of string? map?)))

(defn- opt
  "A predicate over string key `k`: true when `m` lacks `k` OR holds nil there,
   else `pred` holds on its value. Optional keys never fail merely by being
   absent — nor by being PRESENT-but-nil, which is what a total key set (see
   `test-result-base`) makes of \"the runner did not report this\"."
  [k pred]
  (fn [m]
    (or (not (contains? m k)) (nil? (get m k)) (pred (get m k)))))

;; =============================================================================
;; format_code result
;; =============================================================================

(s/def ::format-file
  (s/and map?
         #(string? (get % "path"))
         #(contains? % "changed")
         ;; the backend that formatted THIS file ("zprint" | "cljfmt"), when reported
         (opt "formatter" string?)))

(s/def ::format-result
  (s/and map?
         #(string? (get % "op"))
         (opt "changed" #(or (boolean? %) (nat-int? %)))
         (opt "files" #(s/valid? (s/coll-of ::format-file) %))
         (opt "by-dir" #(s/valid? ::by-dir %))
         ;; which backend(s) ran: "formatter" on a single-file/code result, the
         ;; distinct "formatters" set on a batch — so the result NAMES the provider
         (opt "formatter" string?)
         (opt "formatters" #(s/valid? (s/coll-of string?) %))))

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
;; run_tests result
;; =============================================================================

;; One failing/erroring test. The map carries \"ns\"/\"test\"/\"message\"/\"file\"/
;; \"line\". Like `::finding`, every present field is type-pinned (strings for
;; ns/test/message/file, a non-negative line), but all stay OPTIONAL so a branch
;; that omits a field never rejects — close parity with lint's `::finding`.
(s/def ::test-failure
  (s/and map?
         (opt "message" string?)
         (opt "ns" string?)
         (opt "test" string?)
         (opt "file" string?)
         (opt "line" nat-int?)))

;; The uniform run_tests result. \"mode\" (repl|cli) and \"language\" are the two
;; invariants EVERY branch returns; counts / exit / flags are per-branch optional.
;; "by-dir" is the SAME directory-nested grouping format + lint expose — here
;; `{<dir> {<basename> {\"failures\" [...] \"errors\" [...]}}}` off each fault's file.
(s/def ::test-result
  (s/and map?
         #(contains? #{"repl" "cli"} (get % "mode"))
         #(string? (get % "language"))
         (opt "ns" string?)
         (opt "framework" string?)
         (opt "tool" string?)
         (opt "port" nat-int?)
         (opt "exit" int?)
         (opt "is_pass" boolean?)
         (count-key "total")
         (count-key "pass")
         (count-key "fail")
         (count-key "selected")
         (count-key "skipped")
         (opt "failures" #(s/valid? (s/coll-of ::test-failure) %))
         (opt "errors" #(s/valid? (s/coll-of ::test-failure) %))
         (opt "by-dir" #(s/valid? ::by-dir %))))

;; =============================================================================
;; run_tests: the TOTAL key set
;; =============================================================================

(def test-result-base
  "TOTAL key set of EVERY `run_tests` result — one tool, ONE result shape across
   every language pack. A pack fills what its runner measured; the keys it does
   NOT fill keep these neutral values instead of VANISHING, so ordinary model
   Python (`r[\"failures\"][:3]`, `r[\"total\"]`) can never KeyError, and never
   reads None where a collection belongs.

   Counts stay nil when the runner reported none — UNKNOWN is not zero — while
   collections default empty and flags default false. Applied ONCE at the
   language surface (`language-surface/run-tests`), AFTER the pack's own
   `check`, so packs keep returning only what they know."
  {"mode" nil
   "language" nil
   "framework" nil
   "runner" nil
   "tool" nil
   "command" nil
   "dir" nil
   "ns" nil
   "port" nil
   "exit" nil
   "ms" nil
   "is_pass" nil
   ;; counts — nil means "the runner did not report it"
   "total" nil
   "pass" nil
   "fail" nil
   "selected" nil
   "skipped" nil
   ;; structured faults + their directory-nested view
   "failures" []
   "errors" []
   "by-dir" {}
   ;; narrative
   "output" nil
   "note" nil
   "hint" nil
   "error" nil
   ;; flags
   "timed_out" false
   "repl_unusable" false
   "repl_wedged" false
   "recovered" false})

(defn- ->count
  "A reported count as a long, or nil when the runner reported nothing."
  [v]
  (when (number? v) (long v)))

(defn- ->faults
  "A fault collection as a vector; anything else (nil included) is no faults."
  [v]
  (if (coll? v) (vec v) []))

(defn complete-test-result
  "One pack's raw run_tests `result` onto `test-result-base` — the SINGLE place
   the uniform shape is made true.

   Per-pack key VOCABULARY is folded onto the canonical names, so the caller
   reads `pass`/`fail` whatever ran: pytest/bun `passed`/`failed`/`errored` ->
   `pass`/`fail` (errored counts as failed), an argv `cmd` -> a `command`
   string. `total`, `is_pass` and `language` are DERIVED only when the pack
   reported none — nothing a pack said is ever overwritten.

   Non-map results (a pack that returned something else) pass through."
  [language result]
  (if-not (map? result)
    result
    (let
      [pass
       (or (->count (get result "pass")) (->count (get result "passed")))

       errored
       (->count (get result "errored"))

       fail
       (or (->count (get result "fail"))
           (when-let [f (->count (get result "failed"))]
             (+ (long f) (long (or errored 0))))
           errored)

       skipped
       (->count (get result "skipped"))

       total
       (or (->count (get result "total"))
           (when (and pass fail) (+ (long pass) (long fail) (long (or skipped 0)))))

       exit
       (->count (get result "exit"))

       is-pass
       (cond (some? (get result "is_pass")) (boolean (get result "is_pass"))
             (some? (get result "ok")) (boolean (get result "ok"))
             (seq (str (get result "error"))) false
             (some? fail) (zero? (long fail))
             (some? exit) (zero? (long exit))
             :else nil)

       cmd
       (get result "cmd")

       command
       (or (get result "command")
           (cond (coll? cmd) (str/join " " (map str cmd))
                 (some? cmd) (str cmd)))]

      (assoc (merge test-result-base result)
        "language" (or (get result "language") language)
        "pass" pass
        "fail" fail
        "total" total
        "skipped" skipped
        "is_pass" is-pass
        "command" command
        "failures" (->faults (get result "failures"))
        "errors" (->faults (get result "errors"))
        "by-dir" (or (get result "by-dir") {})))))

;; =============================================================================
;; Capability -> spec + the check the packs run
;; =============================================================================

(def capability->spec
  "Maps a language-tool capability keyword to the spec its result must satisfy.
   Capabilities absent here (`:repl-eval-fn`, ...) are unconstrained."
  {:format-fn ::format-result :lint-fn ::lint-result :test-fn ::test-result})

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
