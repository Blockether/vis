(ns com.blockether.vis.internal.test-contract
  "Language-neutral test-runner CONTRACT shared across vis language packs.

   ONE vocabulary for selecting and reporting tests, modeled on lazytest's CLI
   (NoahTheDuke/lazytest): single test, many namespaces, ignore by name or by
   metadata tag. A future python / js language pack returns the SAME shaped
   result map and accepts the SAME selector keys, so the agent learns the
   words once and they carry across languages.

   The selector vocabulary and the result shape are DEFINED with clojure.spec
   (`::selectors`, `::result`). `selector-keys` / `result-keys` are DERIVED from
   those specs (via `s/form`) so the spec is the single source of truth - a key
   never drifts out of sync with its documentation.

   SELECTOR keys (all optional; the Python dict the tool receives):
     :paths    vector of file / directory PATHS - WHERE the tests are. The ONE
               way a call names what to run, in every language: a pack resolves
               a path the way its own runner discovers tests under it (clojure
               reads each *_test.clj for its ns and maps a SOURCE file to its
               *-test ns; python/bun hand the paths to pytest / bun test). No
               namespace, module or package key rides beside it.
     :only     vector of test-name strings - run ONLY these tests/vars within
               the selected paths. (lazytest -v / source :focus)
     :include  vector of metadata-tag strings - run only tests carrying one of
               these tags, e.g. \"integration\". (lazytest -i)
     :exclude  vector of metadata-tag strings - skip tests carrying one of
               these tags, e.g. \"slow\". (lazytest -e)

   PRECEDENCE (copied verbatim from lazytest):
     - :exclude OVERRIDES :include (a test tagged both is skipped).
     - source-level :skip OVERRIDES :focus.

   RESULT keys (the uniform map every pack returns):
     :language  \"clojure\" | \"python\" | ...
     :mode      \"repl\" | \"cli\"        - which execution path ran
     :framework \"clojure.test\" | \"lazytest\" | ... (repl path)
     :tool      \"clj\" | \"lein\" | \"bb\" | ... (cli path)
     :ns        the namespace(s) that RAN - what a pack reports back, never
                what the call selected (that is :paths)
     :total     test count actually run
     :pass      passing count
     :fail      count that did NOT pass - assertion failures AND errors
     :errored   the SUBSET of :fail that THREW instead of asserting false
                (:fail minus :errored is the assertion failures). Already
                inside :fail and :total - never add it to either again
     :selected  count chosen by the selectors (before skips)
     :skipped   count filtered out by :exclude / source :skip
     :failures  [{:ns :test :type :message :file :line} ...] - EVERY fault, in
                ONE list; :type is \"fail\" (an assertion came back false) or
                \"error\" (the test threw), so nothing is restated in a second
                parallel collection
     :output    captured run log (framework report + error/exception traces)"
  (:require [clojure.spec.alpha :as s]
            [clojure.string :as str]))

;; =============================================================================
;; Selector specs
;; =============================================================================

;; WHERE the tests are - files or directories. The only way in.
(s/def ::paths (s/coll-of string?))

(s/def ::only (s/coll-of string?))

(s/def ::include (s/coll-of string?))

(s/def ::exclude (s/coll-of string?))

;; The selector map a runner tool accepts on its opts dict (all keys optional).
(s/def ::selectors (s/keys :opt-un [::paths ::only ::include ::exclude]))

;; =============================================================================
;; Result specs
;; =============================================================================

(s/def ::language string?)

;; RESULT side only: the namespace(s) a pack RAN (and a fault's own namespace).
;; What the call ASKED FOR is ::paths, and the two are never the same fact.
(s/def ::ns string?)

(s/def ::mode #{"repl" "cli"})

(s/def ::framework string?)

(s/def ::tool string?)

(s/def ::test (s/nilable string?))

(s/def ::message (s/nilable string?))

(s/def ::file (s/nilable string?))

(s/def ::line (s/nilable int?))

;; WHY a test is in :failures - an assertion came back false, or it threw.
(s/def ::type #{"fail" "error"})

(s/def ::failure (s/keys :opt-un [::ns ::test ::type ::message ::file ::line]))

(s/def ::total nat-int?)

(s/def ::pass nat-int?)

(s/def ::fail nat-int?)

;; The erroring SUBSET of ::fail, not a count beside it: a runner that reports
;; counts without per-test detail (pytest's summary line) is the only witness
;; that a fault THREW, so the tally has to carry what ::type carries per fault.
(s/def ::errored nat-int?)

(s/def ::selected nat-int?)

(s/def ::skipped nat-int?)

(s/def ::failures (s/coll-of ::failure))

(s/def ::output string?)

;; The uniform result map every language pack's runner returns. :output is the
;; captured run log (the framework's own printed report plus any error /
;; exception stacktraces written to *out* / *err*); :failures is the ONE fault
;; list, each fault carrying its :type.
(s/def ::result
  (s/keys :opt-un [::language ::mode ::framework ::tool ::ns ::total ::pass ::fail ::errored
                   ::selected ::skipped ::failures ::output]))

;; =============================================================================
;; Key vectors DERIVED from the specs (spec is the single source of truth)
;; =============================================================================

(defn- keys-spec-un-keys
  "Extract the unqualified :req-un + :opt-un keys, in order, from a registered
   `(s/keys ...)` spec - so the published key vectors can never drift from the
   spec definition. `(s/form spec)` yields `(s/keys :opt-un [::a ::b ...])`; we
   strip each fully-qualified key down to its bare keyword."
  [spec]
  (let [opts (apply hash-map (rest (s/form spec)))]
    (->> (concat (:req-un opts) (:opt-un opts))
         (mapv (comp keyword name)))))

(def selector-keys
  "The optional selector keys a runner tool accepts on its opts dict. Derived
   from the `::selectors` spec."
  (keys-spec-un-keys ::selectors))

(def result-keys
  "The uniform result-map keys every language pack's runner returns. Derived
   from the `::result` spec."
  (keys-spec-un-keys ::result))

;; =============================================================================
;; Normalization + selection (the shared runtime helpers)
;; =============================================================================

(defn- ->str-vec
  "Coerce nil / a single scalar / a sequential into a vec of trimmed,
   non-blank strings. Selector values arrive as strings (strings-only
   boundary), so `str` is total - no keyword branch."
  [x]
  (let
    [xs (cond (nil? x) []
              (sequential? x) x
              :else [x])]
    (->> xs
         (map str)
         (map str/trim)
         (remove str/blank?)
         vec)))

(defn normalize-selectors
  "Normalize a raw selector map (the Python dict the tool received) into the
   canonical shape `{:paths [str] :only [str] :include [str] :exclude [str]}`.
   ONE selection vocabulary - paths in, whatever the pack's runner discovers
   under them out. A pack that runs namespaces (clojure) resolves :paths to
   them itself and carries the result in its OWN key, so no second selector
   spelling ever reaches this map."
  [m]
  (let [m (or m {})]
    {:paths (->str-vec (:paths m))
     :only (->str-vec (:only m))
     :include (->str-vec (:include m))
     :exclude (->str-vec (:exclude m))}))

(defn selected?
  "Apply the lazytest precedence to one test, given normalized selectors.
   `test-name` is the test/var name string; `tags` the set of metadata-tag
   strings on it. Returns true when the test should RUN. exclude wins over
   include/only; only narrows by name; include gates by tag when present."
  [{:keys [only include exclude]} test-name tags]
  (let
    [tags
     (set tags)

     only
     (set only)

     inc*
     (set include)

     exc*
     (set exclude)]

    (cond (some exc* tags) false
          (and (seq only) (not (only test-name))) false
          (and (seq inc*) (not (some inc* tags))) false
          :else true)))
