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

   When the payload carries `:summarize` (a `{:trailer … :facts …
   :tasks …}` map), the engine applies it as part of the same swap.

   Returns the intent map plus warnings."
  [{:keys [ctx-atom] :as env} {:keys [answer answer-summary
                                      user-request turn-summary
                                      summarize]}]
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
                   :summarize      summarize})]
            (swap! warns into ws)
            (reset! blocked? (boolean blocked))
            (cond-> (dissoc ctx' :session/scope)
              (seq ws) (update :engine/warnings (fnil into []) ws)))))
      (tel/log! {:level :info :id ::apply-done
                 :data {:answer-present?   (boolean (not (clojure.string/blank? (str answer))))
                        :answer-summary?   (boolean (not (clojure.string/blank? (str answer-summary))))
                        :summarize         summarize
                        :warnings          @warns
                        :blocked?          @blocked?
                        :duration-ms       (/ (- (System/nanoTime) start-ms) 1e6)}}
        "apply-done completed"))
    {:answer answer
     :answer-summary answer-summary
     :summarize summarize
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
;; rewind / lens / find — model-facing recovery bindings
;;
;; Scope grammar (matches the engine cursor):
;;   t<N>/i<M>       — iteration M of turn N
;;   t<N>/i<M>/f<K>  — form K (1-based) of iteration M of turn N
;; All DB-backed against `session_turn_iteration.forms`.
;; =============================================================================

(defn- parse-iter-scope [s]
  (when (string? s)
    (when-let [[_ n m] (re-matches #"^t([1-9][0-9]*)/i([1-9][0-9]*)$" s)]
      {:turn (parse-long n) :iter (parse-long m)})))

(defn- parse-form-scope [s]
  (when (string? s)
    (when-let [[_ n m k] (re-matches #"^t([1-9][0-9]*)/i([1-9][0-9]*)/f([1-9][0-9]*)$" s)]
      {:turn (parse-long n) :iter (parse-long m) :form (parse-long k)})))

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

(defn build-introspect-bindings
  "Return `{'symbol bare-fn}` for the recovery surface: `summarize`
   (mid-turn compaction) + `recall` (the single recovery verb; the old
   `introspect-*` sprawl + rewind/lens/grep retired).

   `recall` dispatches on arg shape — by ADDRESS (form scope / entity
   key) it char-windows the stored value (scrollable, read-only); by
   CONTENT (`{:match …}`) it FTS5-searches the live (non-summarized)
   trace and returns scopes (`:match` required, `:limit` default 10).

   `_history-loader` is accepted for call-site compatibility and unused
   (cross-turn snapshot verbs were dropped)."
  [env _history-loader]
  (let [db   (:db-info env)
        sid  (:session-id env)
        live-ctx (fn [] (some-> (:ctx-atom env) deref))
        form-envelope (fn [scope]
                        (when-let [{:keys [turn iter form]} (parse-form-scope scope)]
                          (when-let [it (iter-by-pos env turn iter)]
                            (nth (vec (:forms it)) (dec form) nil))))
        addr-value (fn [addr]
                     (cond
                       (string? addr)  (some-> (form-envelope addr) :result)
                       (keyword? addr) (let [c (live-ctx)]
                                         (or (get-in c [:session/facts addr :content])
                                           (get-in c [:session/facts addr])
                                           (get-in c [:session/tasks addr])))
                       :else nil))
        iter-pin (fn [scope]
                   ;; build a trailer pin re-materialising ALL forms of an
                   ;; iter scope "tN/iM" (for recall {:scopes …}).
                   (when-let [{:keys [turn iter]} (parse-iter-scope scope)]
                     (when-let [it (iter-by-pos env turn iter)]
                       (let [forms (vec (:forms it))]
                         (when (seq forms)
                           {:scope (str "t" turn "/i" iter) :forms forms})))))]
    {'summarize
     ;; Mid-turn compression: collapse irrelevant trailer ranges /
     ;; settled facts+tasks NOW (same {:trailer :facts :tasks} shape and
     ;; engine fn as `(done {:summarize …})`) so stale forms don't ride
     ;; every prompt until close-of-turn.
     (fn summarize [spec]
       (let [scope (synthesize-scope env)
             out   (atom nil)]
         (when-let [ca (:ctx-atom env)]
           (swap! ca (fn [c]
                       (let [r (eng/apply-summarize c scope (or spec {}))]
                         (reset! out r)
                         (cond-> (:ctx r)
                           (seq (:warnings r))
                           (update :engine/warnings (fnil into []) (:warnings r)))))))
         {:summarized   (select-keys (or spec {}) [:trailer :facts :tasks])
          :warnings     (vec (:warnings @out))
          :trailer-size (count (:session/trailer (live-ctx)))}))
     'recall
     ;; ONE recovery verb. Pull something that already exists back from
     ;; execution memory — dispatched on arg shape:
     ;;   RESTORE  {:ids [:t3/auth …] :why "…"}   entity → live subtree
     ;;            {:scopes ["t4/i2" …] :why "…"} iter   → trailer
     ;;            (:why REQUIRED; stamps :recalled so it's clear WHY)
     ;;   SEARCH   {:match "…"}                    → [{:scope :preview :rank}]
     ;;   WINDOW   "tN/iM/fK" / :K [{:offset N}]   → scrollable slice
     (fn recall
       ([arg] (recall arg nil))
       ([arg opts]
        (cond
          ;; --- restore mode (entities → subtree, scopes → trailer) ----
          (and (map? arg) (or (contains? arg :ids) (contains? arg :scopes)))
          (let [{:keys [ids scopes why]} arg]
            (if (str/blank? (str why))
              {:vis/error :recall-requires-why
               :hint "(recall {:ids […] :why \"…\"}) — :why is REQUIRED to restore (say why it's back)"}
              (let [scope (synthesize-scope env)
                    ids-out    (atom [])
                    scopes-out (atom [])
                    ;; recall-entity resolves LIVE (same-turn :archived)
                    ;; OR :session/archived (GC'd in a past turn — final
                    ;; state captured AT gc, in-memory). One map lookup,
                    ;; no snapshot scan.
                    restore-id
                    (fn [id]
                      (let [out (atom nil)]
                        (when-let [ca (:ctx-atom env)]
                          (swap! ca (fn [c] (let [r (eng/recall-entity c id scope (str why))]
                                              (reset! out r) (:ctx r)))))
                        (if (:found? @out)
                          {:id id :restored (:kind @out) :key (:key @out)}
                          {:id id :vis/error :not-found})))]
                (doseq [id (or ids [])]
                  (swap! ids-out conj (restore-id id)))
                (doseq [sc (or scopes [])]
                  (if-let [pin (iter-pin sc)]
                    (do (when-let [ca (:ctx-atom env)]
                          (swap! ca update :session/trailer
                            (fn [tr] (eng/sort-trailer (conj (vec tr) pin)))))
                      (swap! scopes-out conj {:scope sc :forms (count (:forms pin))}))
                    (swap! scopes-out conj {:scope sc :vis/error :scope-not-found})))
                {:recalled {:ids @ids-out :scopes @scopes-out}
                 :why why
                 :trailer-size (count (:session/trailer (live-ctx)))})))
          ;; --- search mode -------------------------------------------
          (and (map? arg) (contains? arg :match))
          (let [{:keys [match scope-after limit]} arg]
            (if (str/blank? (str match))
              {:vis/error :recall-requires-match
               :hint "(recall {:match \"text\"}) — :match is REQUIRED for search"}
              (let [ranges (eng/summarized-iter-ranges (:session/trailer (live-ctx)))
                    hits   (persistance/db-search db (str match)
                             {:owner-table "session_turn_iteration"
                              :field       "code"
                              :limit       (max 1 (long (or limit 10)))})
                    turns  (or (persistance/db-list-session-turns db sid) [])
                    turn-by-soul  (into {} (map (juxt :id :position)) turns)
                    iter-cache    (atom {})
                    iter-rows-for (fn [tid]
                                    (or (get @iter-cache tid)
                                      (let [rows (persistance/db-list-session-turn-iterations db tid)]
                                        (swap! iter-cache assoc tid rows) rows)))
                    cursor (when (string? scope-after) (parse-iter-scope scope-after))
                    after? (fn [{:keys [turn iter]}]
                             (or (nil? cursor)
                               (> turn (:turn cursor))
                               (and (= turn (:turn cursor)) (> iter (:iter cursor)))))]
                (vec
                  (for [{:keys [owner-id snippet rank]} hits
                        :let [pair (some (fn [t] (some #(when (= owner-id (:id %)) [t %])
                                                   (iter-rows-for (:id t)))) turns)
                              [trow it] pair
                              tp (turn-by-soul (:id trow))
                              ip (:position it)
                              sc (when (and tp ip) (str "t" tp "/i" ip))]
                        :when (and sc
                                (after? {:turn tp :iter ip})
                                (not (eng/scope-in-summarized? ranges sc)))]
                    {:scope sc :preview snippet :rank rank})))))
          ;; --- window mode -------------------------------------------
          :else
          (let [{:keys [offset limit]} (or opts {})
                v (addr-value arg)]
            (if (nil? v)
              {:vis/error :recall-target-not-found :addr arg
               :hint "address is a form scope \"tN/iM/fK\" or an existing fact/task key"}
              (eng/recall-window (pr-str arg) v offset limit))))))}))

