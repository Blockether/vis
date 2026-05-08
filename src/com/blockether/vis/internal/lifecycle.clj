(ns com.blockether.vis.internal.lifecycle
  "Iteration-loop lifecycle event bus.

   Four phases, fired by `internal.loop` at well-defined boundaries:

     :turn-start      - once, after the turn's SYSTEM_* sandbox vars
                        are bound and BEFORE the first LLM call.
     :iteration-start - once per iteration, immediately BEFORE
                        `svar/ask-code!` is invoked.
     :iteration-end   - once per iteration, AFTER `db-store-iteration!`
                        commits, on BOTH success and error paths. The
                        payload's :status discriminates.
     :turn-end        - once, at every exit edge of the loop:
                        successful answer, cancelled, error budget
                        exhausted. The payload's :status discriminates.

   Composition is the whole point. Two listener sources feed the same
   broadcast:

     (a) Extensions: each registered extension may declare any of
         `:ext/on-turn-start-fn`, `:ext/on-iteration-start-fn`,
         `:ext/on-iteration-end-fn`, `:ext/on-turn-end-fn`. All
         currently-active extensions get fan-out, in extension
         registration order.

     (b) Per-call hooks: the caller of `(send!)` / `(run!)` passes
         a `:hooks` map. Each phase key may be a single fn or a vec
         of fns. Per-call listeners fire BEFORE extension listeners
         so the channel UI updates immediately, with extension side
         effects (logging, billing, telemetry) trailing.

   Why a separate ns: the iteration loop is already 2.9k lines and
   this is a cross-cutting concern that needs its own surface +
   tests. Pure module, zero deps on `loop.clj` so the test suite can
   exercise composition without a live router.

   Public API (used from `internal.loop`):

     `(compose-listeners hooks active-extensions)` -> listeners map
     `(emit! listeners phase payload)`             -> nil

   Public API (used from extension authors):

     Their extension manifest carries `:ext/on-<phase>-fn`. The host
     handles the rest. See `docs/src/extensions/lifecycle.md`.

   Errors: any listener that throws is caught + logged via Telemere,
   the broadcast continues. A misbehaving extension MUST NOT take
   the loop down."
  (:require
   [com.blockether.vis.internal.extension :as extension]
   [taoensso.telemere :as tel]))

;; ---------------------------------------------------------------------------
;; Phase <-> manifest-key registry. Stays in one place so extension authors
;; have a single source of truth, and the spec list in `extension.clj`
;; can derive its specs from this map without drift.
;; ---------------------------------------------------------------------------

(def phase->manifest-key
  "Lifecycle phase keyword -> extension-manifest field that exposes a
   listener for that phase. Single source of truth used by both the
   broadcast composer here and the spec validation in
   `internal.extension`."
  {:turn-start      :ext/on-turn-start-fn
   :iteration-start :ext/on-iteration-start-fn
   :iteration-end   :ext/on-iteration-end-fn
   :turn-end        :ext/on-turn-end-fn})

(def phase->hook-key
  "Lifecycle phase keyword -> the matching `:hooks` map slot a caller
   uses to subscribe per-call (channels, tests, ad-hoc scripts).
   Mirrors the manifest map but in the `:on-<phase>` shape every
   loop ever has used."
  {:turn-start      :on-turn-start
   :iteration-start :on-iteration-start
   :iteration-end   :on-iteration-end
   :turn-end        :on-turn-end})

(def phases
  "Canonical ordered vector of lifecycle phases. Order is fire order
   over the lifetime of a turn."
  [:turn-start :iteration-start :iteration-end :turn-end])

;; ---------------------------------------------------------------------------
;; Composition.
;; ---------------------------------------------------------------------------

(defn- wrap-extension-listener
  [ext f]
  (fn [payload]
    (binding [extension/*current-extension* ext
              extension/*current-symbol* nil]
      (f payload))))

(defn- extension-listener
  [ext manifest-key]
  (when-let [f (get ext manifest-key)]
    (wrap-extension-listener ext f)))

(defn- coerce-listeners
  "A `:hooks` slot may carry nil, a single fn, or a coll of fns. Always
   produce a vec of fns (possibly empty). Non-fn entries are dropped
   with a warning so a typo doesn't crash the loop."
  [v]
  (cond
    (nil? v)        []
    (fn? v)         [v]
    (sequential? v) (let [{good true bad false} (group-by fn? (vec v))]
                      (when (seq bad)
                        (tel/log! {:level :warn :id ::non-fn-listener
                                   :data  {:dropped-count (count bad)}
                                   :msg   "lifecycle listener slot held non-fn entries; dropped"}))
                      (vec good))
    (ifn? v)        [v]
    :else           (do (tel/log! {:level :warn :id ::non-fn-listener
                                   :data  {:value-type (type v)}
                                   :msg   "lifecycle listener slot held a non-fn value; ignored"})
                      [])))

(defn compose-listeners
  "Build the `{phase -> [fn ...]}` map the loop broadcasts to.

   `hooks`             - the per-call hooks map (may be nil).
   `active-extensions` - vec of extension maps already filtered by
                         `:ext/activation-fn` for the current turn.

   Per-phase ordering: per-call listeners first (UI updates fire
   first), then extension listeners in registration order (side
   effects last). Empty vecs for unused phases - callers can `(seq
   listeners)` cheaply.

   Pure: returns plain data, no side effects."
  [hooks active-extensions]
  (into {}
    (map (fn [phase]
           (let [hook-key     (phase->hook-key phase)
                 manifest-key (phase->manifest-key phase)
                 from-hooks   (coerce-listeners (get hooks hook-key))
                 from-exts    (vec (keep #(extension-listener % manifest-key) active-extensions))]
             [phase (into from-hooks from-exts)]))
      phases)))

;; ---------------------------------------------------------------------------
;; Broadcast.
;; ---------------------------------------------------------------------------

(defn emit!
  "Broadcast `payload` to every listener registered for `phase`.
   Returns nil. A listener that throws is caught and logged via
   Telemere; the broadcast continues so one bad extension can't
   take the loop down or starve later listeners.

   `payload` is always a map. The loop guarantees at least
   `:phase` (the keyword), `:conversation-id`, and `:conversation-turn-id`
   are present; phase-specific extras (`:iteration`, `:status`,
   `:tokens`, `:cost-usd`, etc.) are documented per call site in
   `internal.loop` and `docs/src/extensions/lifecycle.md`."
  [listeners phase payload]
  (let [phase-listeners (get listeners phase)]
    (when (seq phase-listeners)
      (let [enriched (assoc payload :phase phase)]
        (doseq [f phase-listeners]
          (try
            (f enriched)
            (catch Throwable t
              (tel/log! {:level :warn :id ::listener-threw
                         :data  {:phase phase
                                 :error (ex-message t)
                                 :ex-class (.getName (class t))}
                         :msg   (str "lifecycle listener for " phase " threw; broadcast continues")}))))))
    nil))
