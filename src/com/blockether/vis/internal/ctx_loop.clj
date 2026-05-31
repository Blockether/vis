(ns com.blockether.vis.internal.ctx-loop
  "Loop integration layer for the CTX engine.

   The pure engine (`ctx-engine`) takes data, returns data. Real-world wiring
   needs side effects: a mutable CTX atom that lives across iters within a
   turn, a scope cursor derived from the loop's running counters, and SCI
   symbol bindings the model can call directly from inside a fence.

   This namespace is the thin adapter that ties the engine to the loop.

   **One atom**: `:ctx-atom` on the env map carries the entire engine state
   for a session. Mutators swap! it. Transient mutator output lives on the
   ephemeral `:engine/warnings` key on the ctx itself — collected per-iter,
   drained at render. Stripped via `eng/strip-ephemeral` before
   Nippy-snapshotting.

   **`build-sci-bindings`** — `{'symbol sci-fn}` ready to merge into the
   loop's `env-bindings`. Each mutator:
     - synthesises the current form scope from the loop counters
     - calls `eng/apply-mutator`
     - swap!s the ctx atom with new ctx + accumulated warnings
     - returns `:vis/silent` so the form result isn't echoed

   D12: hint-satisfaction surface (`satisfy-hint!`,
   `drain-and-apply-satisfies!`, `:engine/pending-satisfies`) was retired.
   Hook-emitted soft work items now live as hook-sourced tasks; the model
   satisfies them via `(task-set! id {:status :done})`. Done is
   self-asserted — the engine stamps `:done-born` and surfaces a soft
   terminal-dep warning, but does not verify or revert."
  (:require [clojure.string :as str]
            [com.blockether.vis.internal.ctx-engine :as eng]
            [com.blockether.vis.internal.env-digest :as env-digest]
            [com.blockether.vis.internal.extension :as extension]
            [com.blockether.vis.internal.persistance :as persistance]
            [com.blockether.vis.internal.prompt :as prompt]
            [taoensso.telemere :as tel]))

;; =============================================================================
;; Atom and constructor — ONE atom carries the entire engine state
;; =============================================================================

(defn make-ctx-atom
  "Initialize the CTX atom for a session. Uses the canonical empty scaffold.
   The scaffold carries an empty `:engine/warnings` vec so every swap!
   path can `update` it without nil-puncturing."
  ([] (atom (eng/empty-ctx)))
  ([session-id] (atom (eng/empty-ctx session-id))))

;; =============================================================================
;; Scope synthesis
;; =============================================================================

;; =============================================================================
;; Single turn-state atom
;; =============================================================================
;;
;; Replaces the six-atom soup
;;   :current-turn-position-atom
;;   :current-iteration-atom
;;   :current-form-idx-atom
;;   :current-iteration-id-atom
;;   :current-session-turn-id-atom
;;   :current-user-request-atom
;; with ONE `:turn-state-atom` holding a map of the same fields, plus
;; an `:iteration-shape` value that mirrors the legacy
;; `:current-iteration-atom`'s flexibility (number or {:position N}).
;;
;; Reads are `(@(:turn-state-atom env) :field)` or via helpers below.
;; Writes go through `swap-turn-state!`, `set-form-idx!`, etc. — same
;; atomic update path, single source of truth.

(defn make-turn-state-atom
  "Initialize the per-session turn-state atom. Holds every cursor +
   DB-id field the iteration loop and ctx-loop helpers consume."
  []
  (atom {:turn-position   nil
         :iteration       nil
         :form-idx        nil
         :iteration-id    nil
         :session-turn-id nil
         :user-request    nil}))

(defn swap-turn-state!
  "swap! the turn-state map. Returns the new state. No-op if no atom on env."
  [env f & args]
  (when-let [a (:turn-state-atom env)]
    (apply swap! a f args)))

(defn set-turn-state!
  "swap! `assoc` shortcut for one or more turn-state keys."
  [env & kvs]
  (when-let [a (:turn-state-atom env)]
    (swap! a #(apply assoc % kvs))))

(defn read-turn-state
  "Deref the turn-state map or {} when atom is missing."
  [env]
  (or (some-> (:turn-state-atom env) deref) {}))

(defn- normalize-iteration
  "Iteration field accepts a number, a {:position N} map, or nil. Returns N."
  [v]
  (cond
    (map? v)     (or (:position v) 1)
    (number? v)  v
    :else        1))

(defn synthesize-scope
  "Build the current form scope `tN/iM/fK` from `:turn-state-atom`.
   Defaults each field to 1 (form-idx defaults to 0 → next-form 1) so
   the helper is safe to call before the loop has initialised the
   atom (e.g. early hooks)."
  [env]
  (let [{:keys [turn-position iteration form-idx]} (read-turn-state env)]
    (str "t" (or turn-position 1)
      "/i" (normalize-iteration iteration)
      "/f" (inc (or form-idx 0)))))

(defn cursor-snapshot
  "Build a `:session/scope` map from `:turn-state-atom`. Mirrors the
   engine's `{:turn :iter :next-form}` shape used by `classify-scope`
   and the renderer."
  [env]
  (let [{:keys [turn-position iteration form-idx]} (read-turn-state env)]
    {:turn      (or turn-position 1)
     :iter      (normalize-iteration iteration)
     :next-form (inc (or form-idx 0))}))

;; =============================================================================
;; Mutator bindings
;; =============================================================================

(defn apply-and-record!
  "Call the engine mutator and swap! the result onto ctx-atom in one shot.
   Engine warnings land on `:engine/warnings` directly. Returns
   `:vis/silent` so the form result is hidden from the model's eval echo
   (the mutation effect is visible on next render).

   Public so the loop can route foundation hook-task emissions through
   the same write path the model uses."
  [{:keys [ctx-atom] :as env} mutator args]
  (let [scope (synthesize-scope env)]
    (swap! ctx-atom
      (fn [c]
        (let [c+cursor (assoc c :session/scope (cursor-snapshot env))
              {:keys [ctx warnings stamped?]} (eng/apply-mutator c+cursor scope mutator args)
              base (if stamped? ctx c)]
          (cond-> base
            (seq warnings) (update :engine/warnings (fnil into []) warnings)))))
    :vis/silent))

(defn build-sci-bindings
  "Return `{'symbol bare-fn}` for every engine mutator. The model writes
   `(task-set! :K {:status :done})` or `(fact-set! :K {…})` directly inside
   a fence; we route the call through `apply-and-record!` against the
   single ctx-atom.

   All mutators return `:vis/silent` — engine mutations are 'effect-only',
   visible on next render but quiet in the form echo.

   D12: no `satisfy-hint!` binding. Hook-task satisfaction goes through
   the standard `task-set!` mutator with `{:status :done}`."
  [env]
  {'task-set!     (fn task-set!     [k partial]            (apply-and-record! env :task-set!     [k partial]))
   'fact-set!     (fn fact-set!     [k partial]            (apply-and-record! env :fact-set!     [k partial]))
   'task-depends! (fn task-depends! [k deps]               (apply-and-record! env :task-depends! [k deps]))
   'fact-depends! (fn fact-depends! [k deps]               (apply-and-record! env :fact-depends! [k deps]))
   'fact-contradicts!        (fn fact-contradicts!        [a b] (apply-and-record! env :fact-contradicts!        [a b]))
   'fact-contradicts-remove! (fn fact-contradicts-remove! [a b] (apply-and-record! env :fact-contradicts-remove! [a b]))})

;; =============================================================================
;; Per-iter helpers used by the loop
;; =============================================================================

(defn drain-warnings!
  "Atomically read and clear `:engine/warnings` on ctx-atom. Called by the
   renderer between iters."
  [{:keys [ctx-atom]}]
  (when ctx-atom
    (let [ws (atom nil)]
      (swap! ctx-atom
        (fn [c]
          (reset! ws (or (:engine/warnings c) []))
          (assoc c :engine/warnings [])))
      @ws)))

(defn title-gate-blocker
  "Title setup is host-owned. Kept as a public no-op helper for old tests and
   extension call sites; the main model should not spend forms on session-title
   maintenance."
  [_turn-pos _session-title]
  nil)

(defn apply-done!
  "Side-effecting wrapper around `eng/apply-done`.

   When the payload carries `:trailer-drop`, `:trailer-summarize`,
   or `:archive`, the engine applies them as part of the same swap.

   Returns the intent map plus warnings."
  [{:keys [ctx-atom] :as env} {:keys [answer answer-summary
                                      user-request turn-summary
                                      trailer-drop trailer-summarize archive]}]
  (let [start-ms      (System/nanoTime)
        cursor        (cursor-snapshot env)
        scope         (synthesize-scope env)
        warns         (atom [])
        blocked?      (atom false)]
    ;; Title-blocker is now computed FRESH at render time in
    ;; `build-and-render-ctx` (via `title-gate-blocker` over live
    ;; turn-state + session-title-atom), so apply-done! no longer
    ;; pushes blocker state. Drop any stale :missing-title /
    ;; :stale-title from previous renders before the engine swap so
    ;; nothing leaks into the persisted ctx.
    (when ctx-atom
      (swap! ctx-atom update :engine/blockers
        (fn [bs] (vec (remove #(#{:missing-title :stale-title} (:id %)) (or bs []))))))
    (when ctx-atom
      (swap! ctx-atom
        (fn [c]
          (let [c+cur (assoc c :session/scope cursor)
                {ctx' :ctx ws :warnings blocked :blocked?}
                (eng/apply-done c+cur scope
                  {:answer         answer
                   :answer-summary answer-summary
                   :user-request   user-request
                   :turn-summary   turn-summary
                   :trailer-drop   trailer-drop
                   :trailer-summarize trailer-summarize
                   :archive        archive})]
            (swap! warns into ws)
            (reset! blocked? (boolean blocked))
            (cond-> (dissoc ctx' :session/scope)
              (seq ws) (update :engine/warnings (fnil into []) ws)))))
      (tel/log! {:level :info :id ::apply-done
                 :data {:answer-present?   (boolean (not (clojure.string/blank? (str answer))))
                        :answer-summary?   (boolean (not (clojure.string/blank? (str answer-summary))))
                        :trailer-drop      trailer-drop
                        :trailer-summarize trailer-summarize
                        :archive           archive
                        :warnings          @warns
                        :blocked?          @blocked?
                        :duration-ms       (/ (- (System/nanoTime) start-ms) 1e6)}}
        "apply-done completed"))
    {:answer answer
     :answer-summary answer-summary
     :trailer-drop trailer-drop
     :trailer-summarize trailer-summarize
     :archive archive
     :blocked? @blocked?
     :warnings @warns}))

(defn stamp-cursor
  "Return a ctx map with both `:session/turn` and `:session/scope` synced
   from the loop's running counters. Render path + every engine derivation
   call goes through this so the model never sees a stale top-level
   `:session/turn` (e.g. after a resume that loaded turn N's snapshot but
   the current loop is on turn N+1)."
  [env ctx]
  (let [cursor (cursor-snapshot env)]
    (-> ctx
      (assoc :session/turn  (:turn cursor))
      (assoc :session/scope cursor))))

(defn current-ctx
  "Deref the CTX atom with both `:session/turn` and `:session/scope`
   stamped from the loop counters. This is the shape passed to the
   renderer + `derive-warnings`. Returns nil when ctx-atom is missing
   on env (defensive for partial test envs)."
  [{:keys [ctx-atom] :as env}]
  (when ctx-atom (stamp-cursor env @ctx-atom)))

(defn trailer->form-results
  "Flatten every trailer pin's :forms vec into a `{scope envelope}` map.
   Used at render time so engine's `classify-scope` pass can look up
   per-form `:result` / `:error` for any scope the model references."
  [trailer]
  (into {}
    (for [entry trailer
          :when (some? (:forms entry))
          form  (:forms entry)
          :when (and (some? form) (some? (:scope form)))]
      [(:scope form) form])))

(defn render-block!
  "Build the `;; ctx` block for the next user message. Pure data flow,
   single side effect (drain-warnings!) at the start. Returns the
   rendered string, or nil when ctx-atom is missing on env.

   Flow:
     deref ctx-atom → stamp cursor → attach `:session/env` digest +
     extension `:ext/ctx` contributions → build form-results map from
     trailer → build indexes → drain mutator warnings + derive
     render-time warnings → assoc `:session/warnings` → call renderer.

   `:session/env` lives on the rendered ctx but is NOT pushed back into
   `ctx-atom`: extension contributions are recomputed each iter so
   transient state (e.g. voice / git status) stays fresh."
  [env renderer-fn]
  (when-let [ctx (current-ctx env)]
    (let [active-exts    (try (prompt/active-extensions env)
                           (catch Throwable _ nil))
          ext-ctx        (try (extension/ctx-contributions env active-exts)
                           (catch Throwable t
                             (tel/log! {:level :warn :id ::ctx-contributions-failed
                                        :data  {:error (ex-message t)}})
                             {}))
          env-block      (try (env-digest/deep-merge
                                (env-digest/base-digest env)
                                (:session/env ext-ctx))
                           (catch Throwable t
                             (tel/log! {:level :warn :id ::env-digest-failed
                                        :data  {:error (ex-message t)}})
                             nil))
          ctx*           (cond-> (env-digest/deep-merge ctx (dissoc ext-ctx :session/env))
                           (seq env-block) (assoc :session/env env-block))
          fr             (trailer->form-results (:session/trailer ctx*))
          idx            (eng/build-indexes ctx*)
          drained-warns  (drain-warnings! env)
          derived-warns  (eng/derive-warnings ctx* idx fr)
          warns          (vec (concat drained-warns derived-warns))
          ;; Live title-blocker injection. Computed FRESH from
          ;; turn-state + session-title-atom every render — model sees
          ;; the blocker on iter 1 the moment the conditions hold,
          ;; without waiting for apply-done! to push state. Drops the
          ;; same iter the title gets set.
          live-turn-pos    (or (:turn-position (read-turn-state env))
                             (:session/turn ctx*))
          live-title       (some-> (:session-title-atom env) deref str
                             clojure.string/trim not-empty)
          title-blocker    (title-gate-blocker live-turn-pos live-title)
          ctx-with-blocker (cond-> ctx*
                             title-blocker
                             (update :engine/blockers
                               (fn [bs]
                                 (vec
                                   (concat [title-blocker]
                                     (remove #(#{:missing-title :stale-title} (:id %))
                                       (or bs [])))))))
          ;; `:session/warnings` is a render-only key: a vec of short
          ;; strings (structural warnings + drained mutator warnings)
          ;; surfaced to the model in the rendered ctx. NOT pushed back
          ;; into ctx-atom.
          ctx-rendered     (cond-> ctx-with-blocker
                             (seq warns) (assoc :session/warnings warns))]
      (renderer-fn {:ctx ctx-rendered :warnings warns}))))

;; =============================================================================
;; Introspect verbs — model-facing SCI bindings over engine history helpers
;; =============================================================================

(defn- with-live-history
  "Build the {turn → ctx} history map used by engine introspect helpers.
   Combines:
     - the LIVE ctx-atom value (assigned to the env's :session/turn so the
       current in-progress turn is reachable via `introspect-ctx-at` and the
       per-key introspect verbs)
     - the persisted snapshots loaded via the provided `history-loader`
   The live value lands LAST (highest turn) so per-key lookups walking
   reverse find it first."
  [env history-loader]
  (let [live  (some-> (:ctx-atom env) deref)
        live-turn (or (:turn (cursor-snapshot env)) (:session/turn live) 1)
        loaded (history-loader)
        history (into {} loaded)]
    (cond-> history
      live (assoc live-turn live))))

;; =============================================================================
;; Per-form / per-iter / per-turn introspection
;; —
;; Three drill layers, no overlap:
;;
;;   (introspect-turn-list)         — every turn in this session
;;   (introspect-iter "t<N>/i<M>")  — full :forms vec for one iter
;;   (introspect-form "t<N>/i<M>/f<K>") — one form envelope
;;
;; All DB-backed against `session_turn_iteration.forms` (the per-form
;; envelope BLOB — single canonical source for what every executed
;; form returned). The earlier `introspect-turn` / `introspect-iter-
;; heads` verbs were retired: `introspect-iter` already returns the
;; full forms vec (including heads), and `introspect-turn-list` covers
;; the turn-level meta.
;;
;; Scope grammar (matches the engine cursor):
;;   t<N>            — turn N
;;   t<N>/i<M>       — iteration M of turn N
;;   t<N>/i<M>/f<K>  — form K (1-based) of iteration M of turn N
;; =============================================================================

(defn- parse-iter-scope [s]
  (when (string? s)
    (when-let [[_ n m] (re-matches #"^t([1-9][0-9]*)/i([1-9][0-9]*)$" s)]
      {:turn (parse-long n) :iter (parse-long m)})))

(defn- parse-form-scope [s]
  (when (string? s)
    (when-let [[_ n m k] (re-matches #"^t([1-9][0-9]*)/i([1-9][0-9]*)/f([1-9][0-9]*)$" s)]
      {:turn (parse-long n) :iter (parse-long m) :form (parse-long k)})))

(defn- one-line
  "Compact `src` for the head-only views: trim, collapse internal
   whitespace, cap to ~100 chars so a `turn-list` reply stays scannable."
  [src]
  (let [s (-> (or src "") str str/trim (str/replace #"\s+" " "))]
    (if (> (count s) 100) (str (subs s 0 100) "…") s)))

(defn- turn-summary
  "Top-level row for `introspect-turn-list`: one entry per turn."
  [{:keys [position user-request status iteration-count]}]
  (cond-> {:scope (str "t" position)
           :turn  position
           :status status
           :iteration-count iteration-count}
    user-request (assoc :user-request (one-line user-request))))

(defn- turn-by-pos
  "Resolve turn position to the persisted turn-soul map (or nil)."
  [{:keys [db-info session-id]} turn-pos]
  (when (and db-info session-id turn-pos)
    (some #(when (= turn-pos (:position %)) %)
      (persistance/db-list-session-turns db-info session-id))))

(defn- iter-by-pos
  "Resolve iter position to the persisted iteration map for `turn-pos`."
  [env turn-pos iter-pos]
  (when-let [turn (turn-by-pos env turn-pos)]
    (some #(when (= iter-pos (:position %)) %)
      (persistance/db-list-session-turn-iterations (:db-info env) (:id turn)))))

(defn- unavailable-introspect-reason
  "Classify why a DB-backed iter/form scope cannot be read. The current
   iteration is not persisted until its whole fence finishes, so same-iter
   introspection is impossible and should be called out explicitly instead
   of returning silent nil."
  [env {:keys [turn iter]}]
  (let [{ct :turn ci :iter} (cursor-snapshot env)]
    (cond
      (or (> turn ct) (and (= turn ct) (> iter ci))) :future-scope
      (and (= turn ct) (= iter ci))                 :current-iteration-not-persisted
      :else                                        :scope-not-found)))

(defn- introspect-unavailable
  [kind scope reason & {:as extra}]
  (merge
    {:vis/error :introspect-scope-unavailable
     :kind      kind
     :scope     scope
     :reason    reason
     :hint      "Only completed iterations are DB-introspectable. Do not introspect the current/future iteration; use visible trailer head/tail first."}
    extra))

(defn build-introspect-bindings
  "Return `{'symbol bare-fn}` for the introspect-* verbs the model calls.
   `history-loader` is a 0-arity thunk that returns the persisted history
   vec (pairs of `[turn-n ctx]`). The loop passes a thunk that calls
   `persistance/db-load-ctx-history` against the env's db + session.

   The live ctx-atom is folded in on every call so introspection on the
   IN-PROGRESS turn also works (otherwise the model could not query its
   own just-defined tasks/facts from the next iter).

   Per-form / per-iter / per-turn verbs read `session_turn_iteration.forms`
   directly via `persistance/db-list-session-turn-iterations` against the
   env's `:db-info` + `:session-id` keys."
  [env history-loader]
  (let [history #(with-live-history env history-loader)]
    {'introspect-task     (fn introspect-task     [k]   (eng/introspect-task (history) k))
     'introspect-fact     (fn introspect-fact     [k]   (eng/introspect-fact (history) k))
     'introspect-changes  (fn introspect-changes  [turn-key] (eng/introspect-changes (history) turn-key))
     'introspect-archived (fn introspect-archived [kind] (eng/introspect-archived (history) kind))
     'introspect-ctx-at   (fn introspect-ctx-at   [turn-key]
                            (let [t (cond
                                      (string? turn-key)
                                      (when-let [[_ n] (re-matches #"^t([1-9][0-9]*)$" turn-key)]
                                        (parse-long n))
                                      (number? turn-key) (long turn-key)
                                      :else nil)]
                              (when t (eng/introspect-ctx-at (history) t))))
     'introspect-turn-list
     (fn introspect-turn-list []
       (mapv turn-summary
         (persistance/db-list-session-turns (:db-info env) (:session-id env))))
     'introspect-iter
     (fn introspect-iter [scope]
       (if-let [{:keys [turn iter] :as parsed} (parse-iter-scope scope)]
         (if-let [it (iter-by-pos env turn iter)]
           {:scope  scope
            :status (:status it)
            :code   (:code it)
            :forms  (vec (:forms it))}
           (introspect-unavailable :iter scope
             (unavailable-introspect-reason env parsed)
             :parsed parsed))
         (introspect-unavailable :iter scope :malformed-scope)))
     'trailer-find
     ;; Phase F: SQLite FTS5 search over indexed iteration `code`
     ;; (the whole fence body). Returns iteration scopes ranked by
     ;; FTS5 BM25 so the model can pull `(introspect-iter scope)`
     ;; for full forms when it spots a relevant hit.
     ;;
     ;; Opts:
     ;;   :src-matches    FTS5 MATCH query string (required; bare
     ;;                   tokens are AND-ed, supports prefix `foo*`,
     ;;                   `"foo OR bar"`, etc.)
     ;;   :scope-after    "tN/iM" post-filter (only hits strictly
     ;;                   after this scope are returned)
     ;;   :limit          int, default 20
     ;;
     ;; Returns: vec of `{:scope :preview :rank}` sorted best-first.
     (fn trailer-find [opts]
       (let [{:keys [src-matches scope-after limit]} (or opts {})]
         (when (and (string? src-matches) (not (str/blank? src-matches)))
           (let [hits   (persistance/db-search (:db-info env) src-matches
                          {:owner-table "session_turn_iteration"
                           :field       "code"
                           :limit       (max 1 (long (or limit 20)))})
                 turns  (or (persistance/db-list-session-turns
                              (:db-info env) (:session-id env)) [])
                 turn-by-soul   (into {} (map (juxt :id :position)) turns)
                 ;; cache iter rows per turn so a multi-hit query does
                 ;; not refetch the same iteration list.
                 turn-iter-rows (atom {})
                 iter-rows-for  (fn [turn-soul-id]
                                  (or (get @turn-iter-rows turn-soul-id)
                                    (let [rows (persistance/db-list-session-turn-iterations
                                                 (:db-info env) turn-soul-id)]
                                      (swap! turn-iter-rows assoc turn-soul-id rows)
                                      rows)))
                 scope-cursor    (when (string? scope-after)
                                   (parse-iter-scope scope-after))
                 strictly-after? (fn [{:keys [turn iter]}]
                                   (or (nil? scope-cursor)
                                     (> turn (:turn scope-cursor))
                                     (and (= turn (:turn scope-cursor))
                                       (> iter (:iter scope-cursor)))))]
             (vec
               (for [{:keys [owner-id snippet rank]} hits
                     :let [iter-row (some (fn [t]
                                            (some #(when (= owner-id (:id %)) [t %])
                                              (iter-rows-for (:id t))))
                                      turns)
                           [turn-row iter] iter-row
                           turn-pos (turn-by-soul (:id turn-row))
                           iter-pos (:position iter)
                           scope    (when (and turn-pos iter-pos)
                                      {:turn turn-pos :iter iter-pos})]
                     :when (and scope (strictly-after? scope))]
                 {:scope   (str "t" (:turn scope) "/i" (:iter scope))
                  :preview snippet
                  :rank    rank}))))))
     'introspect-form
     (fn introspect-form [scope]
       (if-let [{:keys [turn iter form] :as parsed} (parse-form-scope scope)]
         (if-let [it (iter-by-pos env turn iter)]
           (let [forms (vec (:forms it))]
             (or (nth forms (dec form) nil)
               (introspect-unavailable :form scope :form-not-found
                 :parsed parsed
                 :available-forms (count forms))))
           (introspect-unavailable :form scope
             (unavailable-introspect-reason env parsed)
             :parsed parsed))
         (introspect-unavailable :form scope :malformed-scope)))}))
