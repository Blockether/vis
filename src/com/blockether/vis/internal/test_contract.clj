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
     :ns       string OR vector of namespace strings - which namespace(s) to
               run. One string = a single ns; a vector = many. (lazytest -n)
     :only     vector of test-name strings - run ONLY these tests/vars within
               the selected namespace(s). (lazytest -v / source :focus)
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
     :ns        the namespace(s) run
     :total     test count actually run
     :pass      passing count
     :fail      failing + erroring count
     :selected  count chosen by the selectors (before skips)
     :skipped   count filtered out by :exclude / source :skip
     :failures  [{:ns :test :message :file :line} ...]
     :errors    the erroring-test subset of :failures
     :output    captured run log (framework report + error/exception traces)"
  (:require [clojure.spec.alpha :as s]
            [clojure.string :as str]))

;; =============================================================================
;; Selector specs
;; =============================================================================

(s/def ::ns
  (s/or :one string?
        :many (s/coll-of string?)))
(s/def ::only (s/coll-of string?))
(s/def ::include (s/coll-of string?))
(s/def ::exclude (s/coll-of string?))

;; The selector map a runner tool accepts on its opts dict (all keys optional).
(s/def ::selectors (s/keys :opt-un [::ns ::only ::include ::exclude]))

;; =============================================================================
;; Result specs
;; =============================================================================

(s/def ::language string?)
(s/def ::mode #{"repl" "cli"})
(s/def ::framework string?)
(s/def ::tool string?)
(s/def ::test (s/nilable string?))
(s/def ::message (s/nilable string?))
(s/def ::file (s/nilable string?))
(s/def ::line (s/nilable int?))

(s/def ::failure (s/keys :opt-un [::ns ::test ::message ::file ::line]))

(s/def ::total nat-int?)
(s/def ::pass nat-int?)
(s/def ::fail nat-int?)
(s/def ::selected nat-int?)
(s/def ::skipped nat-int?)
(s/def ::failures (s/coll-of ::failure))
(s/def ::errors (s/coll-of ::failure))
(s/def ::output string?)

;; The uniform result map every language pack's runner returns. :output is the
;; captured run log (the framework's own printed report plus any error /
;; exception stacktraces written to *out* / *err*); :errors is the erroring-test
;; subset of :failures.
(s/def ::result
  (s/keys :opt-un [::language ::mode ::framework ::tool ::ns ::total ::pass ::fail ::selected
                   ::skipped ::failures ::errors ::output]))

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
   canonical shape `{:nses [str] :only [str] :include [str] :exclude [str]}`.
   :ns accepts a string OR a vector; :namespace / :namespaces are aliases for :ns."
  [m]
  (let [m (or m {})]
    {:nses (->str-vec (or (:ns m) (:namespace m) (:namespaces m)))
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
