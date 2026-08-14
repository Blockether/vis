(ns com.blockether.vis.internal.foundation.surface-contract
  "clojure.spec CONTRACT for the language-surface tool RESULTS (`format_code`,
   `lint_code`, `run_tests`).

   Every language pack that registers a `:format-fn` / `:lint-fn` / `:test-fn`
   under
   `:ext/language-tools` returns a result map that MUST conform to these specs,
   so the shape is UNIFORM across packs (clojure, and a future python / js) and
   can never silently drift. Both results share the directory-nested `by-cwd`
   grouping (`{<dir> {<basename> <payload>}}`) that writes each long directory
   prefix ONCE.

   The result maps cross the strings-only Python boundary, so their keys are
   STRINGS (\"op\", \"findings\", \"by-cwd\", ...). clojure.spec's `s/keys` only
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
(s/def ::by-cwd (s/map-of string? (s/map-of string? map?)))

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
         (opt "formatter" string?)
         ;; the add-only delimiter repair: which lines it completed, and why a file
         ;; whose repair would have DELETED a delimiter was left unformatted instead
         (opt "repairs" #(s/valid? (s/coll-of string?) %))
         (opt "unbalanced" string?)))

(s/def ::format-result
  (s/and map?
         #(string? (get % "op"))
         (opt "changed" #(or (boolean? %) (nat-int? %)))
         (opt "files" #(s/valid? (s/coll-of ::format-file) %))
         (opt "by-cwd" #(s/valid? ::by-cwd %))
         ;; which backend(s) ran: "formatter" on a single-file/code result, the
         ;; distinct "formatters" set on a batch — so the result NAMES the provider
         (opt "formatter" string?)
         (opt "formatters" #(s/valid? (s/coll-of string?) %))
         ;; same two on a single-file / code result
         (opt "repairs" #(s/valid? (s/coll-of string?) %))
         (opt "unbalanced" string?)))

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
         (opt "snippet" string?)
         (opt "by-cwd" #(s/valid? ::by-cwd %))))

;; =============================================================================
;; run_tests result
;; =============================================================================

;; One failing test. The map carries \"ns\"/\"test\"/\"type\"/\"message\"/\"file\"/
;; \"line\". Like `::finding`, every present field is type-pinned (strings for
;; ns/test/message/file, a non-negative line), but all stay OPTIONAL so a branch
;; that omits a field never rejects — close parity with lint's `::finding`.
(s/def ::test-failure
  (s/and map?
         (opt "message" string?)
         (opt "ns" string?)
         (opt "test" string?)
         ;; WHY the test is in `failures`: "fail" (an assertion came back false)
         ;; or "error" (the test threw). ONE fault list, every fault saying which
         ;; — never a second `errors` collection restating the same maps.
         (opt "type" #{"fail" "error"})
         (opt "file" string?)
         (opt "line" nat-int?)))

;; The uniform run_tests result. \"mode\" (repl|cli) and \"language\" are the two
;; invariants EVERY branch returns; counts / exit / flags are per-branch optional.
;; "by-cwd" is the SAME directory-nested grouping format + lint expose — here
;; `{<dir> {<basename> {\"failures\" [...]}}}` off each fault's file.
(s/def ::test-result
  (s/and map?
         #(contains? #{"repl" "cli"} (get % "mode"))
         #(string? (get % "language"))
         (opt "ns" string?)
         (opt "target" string?)
         (opt "framework" string?)
         (opt "tool" string?)
         (opt "port" nat-int?)
         (opt "exit" int?)
         (opt "is_pass" boolean?)
         (count-key "total")
         (count-key "pass")
         (count-key "fail")
         (count-key "errored")
         (count-key "selected")
         (count-key "skipped")
         (opt "failures" #(s/valid? (s/coll-of ::test-failure) %))
         (opt "by-cwd" #(s/valid? ::by-cwd %))))

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
   "cwd" nil
   "ns" nil
   ;; WHAT THE CALL SELECTED (the `paths` entries, else the whole suite) — a
   ;; runner reports what it RAN, never what was ASKED FOR, so without this two
   ;; different selections render the same headline.
   "target" nil
   "port" nil
   "exit" nil
   "ms" nil
   "is_pass" nil
   ;; counts — nil means "the runner did not report it"
   "total" nil
   "pass" nil
   "fail" nil
   ;; The erroring SUBSET of "fail" — the tests that THREW rather than asserting
   ;; false. Already counted in "fail" and "total" (total = pass + fail +
   ;; skipped), so it is NEVER added to either again. It exists as a count
   ;; because a runner can report a tally with no per-test detail, where the
   ;; faults' "type" cannot answer how many threw.
   "errored" nil
   "selected" nil
   "skipped" nil
   ;; structured faults + their directory-nested view — ONE list, where an
   ;; erroring test is a fault with "type" "error"
   "failures" []
   "by-cwd" {}
   ;; narrative
   "output" nil
   "note" nil
   "hint" nil
   ;; Why the RUN could not produce results (nREPL down, timed out, no project,
   ;; an unparseable report) — never a failing test, which rides `failures`.
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

   NOTHING is translated here: every pack speaks the contract's OWN words.
   A runner that counts in other ones — pytest's `passed`/`failed`/`errored`,
   bun's `N pass` / `N fail` lines — is folded onto `pass`/`fail`/`errored`
   INSIDE its pack, where what those words mean is known (pytest's `failed`
   and `errors` are DISJOINT; lazytest's are not). A completed result
   therefore names each fact once because it only ever carried one name.

   What is filled in here is what a pack could not know: `total`, `errored`,
   `is_pass` and `language` are DERIVED only when the pack reported none —
   nothing a pack said is ever overwritten.

   `errored` is a count BESIDE `fail`, not inside a second list, because it
   names a different fact (how many threw), stays a SUBSET of `fail`, and
   survives where the typed fault list cannot: a runner that reported counts
   and no per-test detail lists no faults to type.

   Non-map results (a pack that returned something else) pass through."
  [language result]
  (if-not (map? result)
    result
    (let
      [pass
       (->count (get result "pass"))

       faults
       (->faults (get result "failures"))

       reported-errored
       (->count (get result "errored"))

       fail
       (->count (get result "fail"))

       ;; Unreported: every listed fault carries its "type", but only a fault
       ;; list that accounts for EVERY failure may be counted — pytest's summary
       ;; line reports `3 failed` and names none of them, and 0 faults typed
       ;; "error" out of 0 listed is UNKNOWN, not zero.
       errored
       (or reported-errored
           (when (and fail (= (count faults) fail))
             (count (filter #(= "error" (get % "type")) faults))))

       skipped
       (->count (get result "skipped"))

       total
       (or (->count (get result "total"))
           (when (and pass fail) (+ (long pass) (long fail) (long (or skipped 0)))))

       exit
       (->count (get result "exit"))

       is-pass
       (cond (some? (get result "is_pass")) (boolean (get result "is_pass"))
             (seq (str (get result "error"))) false
             (some? fail) (zero? (long fail))
             (some? exit) (zero? (long exit))
             :else nil)]

      (-> (merge test-result-base result)
          (assoc "language" (or (get result "language") language)
                 "pass" pass
                 "fail" fail
                 "errored" errored
                 "total" total
                 "skipped" skipped
                 "is_pass" is-pass
                 "failures" faults
                 "by-cwd" (or (get result "by-cwd") {}))))))

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
