(ns com.blockether.vis.internal.ctx-spec
  "Formal data model for the new CTX (per CTX_REDESIGN.md).

   `clojure.spec.alpha` definitions for every shape in the CTX blob.
   Engine consults these specs at write time (warn on shape mismatch),
   at render time (deterministic ordering), and at test time
   (`s/gen` generators). The specs are the canonical source of truth;
   the prose in CTX_REDESIGN.md describes RATIONALE around the specs,
   not the shape itself.

   Top-level entry point: `::ctx`.

   Subtrees:
     `:session/scope`     engine-rendered cursor {:turn :iter :next-form}
     `:session/specs`     formal requirements with validator-backed requirements
     `:session/tasks`     work items; each task carries proofs grouped by spec
     `:session/facts`     observations, decisions, rules, behavior
     `:session/workspace` engine-rendered git workspace state
     `:session/symbols`   engine-rendered live SCI symbol directory
     `:session/trailer`   pinned iter envelopes (verbatim or summarized)

   Foundation extension `:turn.iteration/start` hooks emit hook-sourced
   tasks (`:source :hook`, `:hook-id`, `:importance`, `:validator-fn`)
   under `:session/tasks`; the model satisfies them via
   `(task-set! id {:status :done :proof \"…\"})`. There is no separate
   `:session/hints` subtree (collapsed into tasks in D12).

   Scope coordinates:
     `::scope-form`  e.g. \"t3/i2/f1\"
     `::scope-iter`  e.g. \"t3/i2\"
     `::scope-turn`  e.g. \"t3\"

   Edge inventory across subtrees (validated as soft warnings, not by spec):
     spec requirement → facts via `:session.requirement/facts` (vec of `::entry-key`)
     requirement      → validator source via `:session.requirement/validator-fn`
     task             → specs via `:session.task/specs` map keys
     task             → tasks via `:session.task/depends-on` (vec of `::entry-key`)
     task proof       → requirement via `:session.task.proof/requirement`
     task proof       → scope via `:session.task.proof/proof`

   Proofs live on tasks, grouped by spec. Specs define requirements and
   optional validators. Facts are connected through requirements, not generic
   fact-to-anything edges.

   Scope cursor (`:session/scope`) is required so the model can pick proof
   scopes deterministically. Engine stamps it before render. User-supplied
   `::scope-form` values that resolve to the future relative to the cursor
   are soft-warned by the engine — not refused, since the model may legally
   reference scopes later in the same fence.

   Mutator API (upsert-only; consumed by `ctx-engine/apply-mutator`):

     Top-level (merge partials):
       :spec-set! :task-set! :fact-set!

     Per-requirement (on :session/specs/:K/:requirements):
       :req-add!    — collision on :id is soft-rejected (no write)
       :req-update! — :id immutable, other keys merged
       :req-remove! — cascade-warns orphaned task proofs

     Per-proof (on :session/tasks/:K/:specs/:spec-K):
       :proof-add! :proof-remove!

   See CTX_REDESIGN.md §Invariants for the full enforcement matrix and which
   invariants are HARD (cycle, malformed scope, partial-overlap trailer) vs
   SOFT (everything else, surfaced as `;; ⚠` annotations)."
  (:require [clojure.spec.alpha :as s]
            [clojure.spec.gen.alpha :as gen]))

;; =============================================================================
;; Scope coordinates
;; =============================================================================

(def ^:private scope-form-re #"^t[1-9][0-9]*/i[1-9][0-9]*/f[1-9][0-9]*$")
(def ^:private scope-iter-re #"^t[1-9][0-9]*/i[1-9][0-9]*$")
(def ^:private scope-turn-re #"^t[1-9][0-9]*$")

(defn- pos-int-gen
  "Bounded positive integer generator (1..1000) for scope segment indices."
  []
  (gen/large-integer* {:min 1 :max 1000}))

(s/def ::scope-form
  (s/with-gen
    (s/and string? #(re-matches scope-form-re %))
    #(gen/fmap (fn [[t i f]] (str "t" t "/i" i "/f" f))
       (gen/tuple (pos-int-gen) (pos-int-gen) (pos-int-gen)))))

(s/def ::scope-iter
  (s/with-gen
    (s/and string? #(re-matches scope-iter-re %))
    #(gen/fmap (fn [[t i]] (str "t" t "/i" i))
       (gen/tuple (pos-int-gen) (pos-int-gen)))))

(s/def ::scope-turn
  (s/with-gen
    (s/and string? #(re-matches scope-turn-re %))
    #(gen/fmap (fn [t] (str "t" t))
       (pos-int-gen))))

;; =============================================================================
;; Entry keys (model-chosen keywords)
;; =============================================================================

(s/def ::entry-key keyword?)

;; =============================================================================
;; Common fields
;; =============================================================================

(s/def ::content string?)
(s/def ::title   string?)
(s/def ::born    ::scope-form)
(s/def ::summary string?)

;; =============================================================================
;; Fact — observations, decisions, rules, behavior
;; =============================================================================
;;
;; Facts are not deleted. When a fact is superseded by newer knowledge the
;; model flips :status to :superseded; engine stamps :done-born and GCs from
;; live CTX after the superseded-TTL. :active facts live indefinitely.

(s/def :session.fact/status    #{:active :superseded})
(s/def :session.fact/done-born ::scope-form)

(s/def ::fact
  (s/keys :req-un [::content ::born]
    :opt-un [:session.fact/status
             :session.fact/done-born]))

;; Soft rules:
;;   - :status defaults to :active when omitted (engine assumes :active for legacy facts)
;;   - :status :superseded MUST have :done-born (engine auto-stamps)
;;   - facts referenced by NO active requirement :facts vec AND aged > 12 turns
;;     emit a render hint suggesting supersede, but engine never auto-archives :active facts

;; =============================================================================
;; Task — work items
;; =============================================================================
;;
;; Model never deletes tasks. To abandon: flip :status to :cancelled. Engine
;; stamps :done-born when status enters a terminal value (:done / :cancelled)
;; and garbage-collects the entry from live CTX after a status-specific TTL
;; (see versioning + GC section in CTX_REDESIGN.md). Snapshots in
;; per-turn ctx snapshots on session_turn_state make every archived
;; entry replayable through the soul/state chain.

(s/def :session.task/depends-on (s/coll-of ::entry-key :kind vector?))
(s/def :session.task/status     #{:todo :doing :done :cancelled})
(s/def :session.task/done-born  ::scope-form)

(s/def :session.task.proof/requirement keyword?)
(s/def :session.task.proof/proof       ::scope-form)
;; Entry inside a task's `:specs` proofs map (`:session.task/specs`).
;; Renamed from `:session.task/proof` to `:session.task/proof-entry`
;; in D12 so the task-level `:proof` field (hook-task done scope) can
;; own the unqualified `:proof` key without collision.
(s/def :session.task/proof-entry
  (s/keys :req-un [:session.task.proof/requirement
                   :session.task.proof/proof]))

(s/def :session.task/specs
  (s/map-of ::entry-key (s/coll-of :session.task/proof-entry :kind vector?)))

;; Hook-emitted task fields (D12: hints collapsed into tasks).
;;
;; `:source` discriminates ownership:
;;   :user   — model-created via `(task-set! …)`
;;   :hook   — foundation extension `:turn.iteration/start` hook emitted
;;   :engine — reserved for engine-synthesised tasks
;;
;; Hook-tasks additionally carry `:hook-id` (the keyword the hook
;; identified itself with, also used as the task key so dedup is
;; trivial), `:importance` for renderer sort order, and `:validator-fn`
;; — a SCI source string the engine runs against the form envelope at
;; `:proof` when the model writes
;; `(task-set! :hook-id {:status :done :proof \"tN/iM/fK\"})`. Pass
;; sticks the :done; fail reverts to :todo and emits a warning.
(s/def :session.task/source       #{:user :hook :engine})
(s/def :session.task/hook-id      keyword?)
(s/def :session.task/importance   #{:info :warn :critical})
;; `:validator-fn` accepts any of three shapes (see
;; `ctx-engine/resolve-validator`):
;;   - plain `fn?` value             (in-memory only)
;;   - `{:fn <fn> :src "…"}` map       (host-defined, persist-safe)
;;   - SCI source string             (model-emitted)
;; Pure spec check stays loose; structural validity is enforced by
;; `ctx-engine/validator-fn?`.
(s/def :session.task/validator-fn
  (s/or :fn      fn?
    :map     (s/keys :opt-un [::validator-fn-fn ::validator-fn-src])
    :src     string?))
(s/def :session.task/proof        ::scope-form)
;; Engine-stamped `:validated? true` once `reconcile-done-hook-tasks`
;; runs the task's `:validator-fn` against the form envelope at `:proof`
;; and the validator returns truthy. Subsequent reconcile passes skip
;; already-validated `:done` tasks so an unrelated `(done {:trailer-drop
;; […]})` that wipes the proof envelope cannot retro-actively revert a
;; legitimately satisfied hook-task. Cleared when `:status` transitions
;; away from `:done` (any non-:done write).
(s/def :session.task/validated?   boolean?)

(s/def ::task
  (s/keys :req-un [::title
                   :session.task/specs
                   :session.task/status
                   ::born]
    :opt-un [:session.task/depends-on
             :session.task/done-born
             :session.task/source
             :session.task/hook-id
             :session.task/importance
             :session.task/validator-fn
             :session.task/proof
             :session.task/validated?]))

;; Soft rules (engine-side validators; not enforced by spec):
;;   - :specs keys must point to existing keys in :session/specs
;;   - :depends-on entries must point to existing keys in :session/tasks
;;   - proof :requirement ids must exist on the referenced spec
;;   - when requirement has :validator-fn, proof scope result must satisfy it
;;   - :status :done | :cancelled MUST have :done-born (engine auto-stamps)
;;   - :status :done requires every :depends-on target to be :done or :cancelled
;;   - task-level :validator-fn (D12 hook-tasks): when the model writes
;;       (task-set! id {:status :done :proof "tN/iM/fK"})
;;     engine runs the validator-fn against the form envelope at :proof.
;;     Pass → :done sticks. Fail → status reverts (or stays :todo) and the
;;     engine emits `:task-done-validator-fail`. No :proof + :validator-fn
;;     present → `:task-done-no-proof` warn + status reverted.
;;   - status transitions are reconstructable from :session/trailer mutation pins;
;;     no per-task journal is stored

;; =============================================================================
;; Spec — formal requirements
;; =============================================================================

(s/def :session.requirement/id           keyword?)
(s/def :session.requirement/title        string?)
;; Same shape contract as `:session.task/validator-fn`.
(s/def :session.requirement/validator-fn
  (s/or :fn      fn?
    :map     map?
    :src     string?))
(s/def :session.requirement/facts
  (s/coll-of ::entry-key :kind vector?))

(s/def :session.spec/requirement
  (s/keys :req-un [:session.requirement/id
                   :session.requirement/title]
    :opt-un [:session.requirement/facts
             :session.requirement/validator-fn]))

(s/def :session.spec/requirements
  (s/coll-of :session.spec/requirement :kind vector? :min-count 1))

(s/def :session.spec/status
  #{:draft :doing :done :cancelled})

(s/def :session.spec/done-born ::scope-form)

(s/def ::spec
  (s/keys :req-un [::title
                   :session.spec/requirements
                   :session.spec/status
                   ::born]
    :opt-un [:session.spec/done-born]))

;; Soft rules:
;;   - :status :done | :cancelled MUST have :done-born (engine auto-stamps)
;;   - requirement :facts entries must point to existing keys in :session/facts
;;   - :status :done requires every requirement to have at least one valid task proof
;;   - :validator-fn is an SCI fn source string evaluated by the engine in a
;;     bounded sandbox; parse / runtime failures surface as render warnings

;; =============================================================================
;; Trailer entries — verbatim pin OR summary
;; =============================================================================

(s/def :session.trailer.form/scope   ::scope-form)
(s/def :session.trailer.form/tag     #{:observation :mutation})
(s/def :session.trailer.form/src     string?)
(s/def :session.trailer.form/result  any?)

(s/def :session.trailer.form.error/message string?)
(s/def :session.trailer.form.error/data    map?)
(s/def :session.trailer.form/error
  (s/keys :req-un [:session.trailer.form.error/message]
    :opt-un [:session.trailer.form.error/data]))

(s/def ::trailer-form
  (s/keys :req-un [:session.trailer.form/scope
                   :session.trailer.form/tag
                   :session.trailer.form/src]
    :opt-un [:session.trailer.form/result
             :session.trailer.form/error]))

(s/def :session.trailer.pin/scope ::scope-iter)
(s/def :session.trailer.pin/forms
  (s/coll-of ::trailer-form :kind vector? :min-count 1))

(s/def ::trailer-pin
  (s/keys :req-un [:session.trailer.pin/scope
                   :session.trailer.pin/forms]))

(s/def :session.trailer.summary/scope-start  ::scope-iter)
(s/def :session.trailer.summary/scope-end    ::scope-iter)
(s/def :session.trailer.summary/summary      string?)
(s/def :session.trailer.summary/born         ::scope-form)

(s/def ::trailer-summary
  (s/keys :req-un [:session.trailer.summary/scope-start
                   :session.trailer.summary/scope-end
                   :session.trailer.summary/summary
                   :session.trailer.summary/born]))

;; Soft rules:
;;   :scope-start must be ≤ :scope-end per scope comparator.
;;   New summary must NOT partially overlap an existing summary;
;;   partial overlap is rejected with an explain message at validation time.
;;   Engine stamps :born to the scope of the (done {:trailer-summarize …}) form.

(s/def ::trailer-entry
  (s/or :pin     ::trailer-pin
    :summary ::trailer-summary))

;; =============================================================================
;; Workspace — engine-rendered. VCS-agnostic by design: the
;; `:vcs/kind` discriminator tells consumers which namespaced subset of
;; keys (`:git/*`, `:hg/*`, `:jj/*`, etc.) the workspace map carries.
;; Every VCS-specific key is OPTIONAL at the spec level; the detector
;; for whatever VCS is in use stamps the right ones. An empty `{}` is
;; a valid workspace (used by `empty-ctx` and any non-VCS session like
;; a scratch REPL).
;;
;; Why no `:req`: hardcoding `:git/branch` forced Mercurial / Jujutsu /
;; non-VCS workspaces to either fake git keys or fail spec. Permissive
;; shape lets the detector own the contract; consumers (renderer,
;; prompt) read defensively.
;; =============================================================================

(s/def :vcs/kind #{:git :hg :jj :fossil :none})

;; Generic VCS keys — shared semantics across VCS kinds, unprefixed for
;; the common case. Detectors may also emit kind-specific aliases.
(s/def :vcs/branch  string?)
(s/def :vcs/trunk   string?)
(s/def :vcs/head    string?)
(s/def :vcs/dirty?  boolean?)

;; Per-file added/removed line counts — the only VCS-namespaced sub-spec.
;; Used by `:vcs/stats {path → file-stats}`. Common to every detector
;; (git diff --numstat, hg diff --stat, jj diff …).
(s/def :vcs.file/added    nat-int?)
(s/def :vcs.file/removed  nat-int?)

(s/def :vcs/file-stats
  (s/keys :req-un [:vcs.file/added :vcs.file/removed]))

(s/def :vcs/stats
  (s/map-of string? :vcs/file-stats))

(s/def ::workspace
  (s/keys :opt [:vcs/kind
                :vcs/branch :vcs/trunk :vcs/head :vcs/dirty? :vcs/stats]))

;; =============================================================================
;; Symbol directory — engine-rendered from SCI introspection + engine-side :born index
;; =============================================================================
;; `:doc` is OMITTED when the underlying var has no docstring. Engine never
;; emits `:doc nil` — model treats absence as "no docstring" without ambiguity.

(s/def :session.symbol/arglists  (s/coll-of seq?))
(s/def :session.symbol/doc       string?)
(s/def :session.symbol/born      ::scope-form)

(s/def ::symbol-info
  (s/keys :req-un [:session.symbol/born]
    :opt-un [:session.symbol/arglists
             :session.symbol/doc]))

;; =============================================================================
;; Hints — RETIRED (D12)
;; =============================================================================
;; What used to be `:session/hints` is now expressed as tasks with
;; `:source :hook`, `:hook-id`, `:importance`, and `:validator-fn`.
;; Foundation extension hooks emit task shape directly; the model
;; satisfies via `(task-set! :hook-id {:status :done :proof "…"})`.
;; One concept, one mutator, one validator path.

;; =============================================================================
;; Scope cursor — engine-rendered current position inside the turn
;; =============================================================================
;;
;; `:session/scope` tells the model where it is RIGHT NOW. Engine-stamped per
;; iter, before render. Without it the model cannot honestly pick a `:proof`
;; scope: `:session/turn` lives at top level but iter and form counters do not.
;;
;; `:next-form` is 1-based — the index that the model's first form in this
;; iter's fence WILL receive. Subsequent forms in the same fence increment
;; from there.
;;
;; Soft rule: any user-provided `::scope-form` (e.g. task proof :proof) is
;; classified by the engine relative to this cursor:
;;   :ok | :unknown | :errored | :future-form | :future-iter | :future-turn
;; Future-scope refs and unknown scopes warn but never refuse.

(s/def :session.scope/turn      pos-int?)
(s/def :session.scope/iter      pos-int?)
(s/def :session.scope/next-form pos-int?)

(s/def :session/scope
  (s/keys :req-un [:session.scope/turn
                   :session.scope/iter
                   :session.scope/next-form]))

;; =============================================================================
;; Top-level CTX
;; =============================================================================

(s/def :session/id        string?)
(s/def :session/turn      pos-int?)
(s/def :session/workspace ::workspace)
(s/def :session/symbols   (s/map-of symbol?      ::symbol-info))
(s/def :session/specs     (s/map-of ::entry-key  ::spec))
(s/def :session/tasks     (s/map-of ::entry-key  ::task))
(s/def :session/facts     (s/map-of ::entry-key  ::fact))
(s/def :session/trailer   (s/coll-of ::trailer-entry :kind vector?))

;; `:session/env` is an open map keyed by domain-named bare keys
;; (`:host`, `:project`, `:extensions`, plus whatever extensions
;; deep-merge in via `:ext/ctx`). Each value is itself a small map; the
;; engine treats the section as opaque data and only enforces that the
;; top-level slot is a map. Domain specs live with their owners
;; (foundation-core/environment/digest, voice ext, …) — keeping the
;; engine free of provider-specific knowledge.
(s/def :session/env (s/map-of keyword? (s/nilable map?)))

(s/def ::ctx
  (s/keys :req [:session/id
                :session/turn
                :session/scope
                :session/workspace
                :session/symbols
                :session/specs
                :session/tasks
                :session/facts
                :session/trailer]
    :opt [:session/env]))

;; =============================================================================
;; Convenience: spec-keyed validators by subtree path
;; =============================================================================

(def ^{:doc "Maps a `:session/X` subtree key to the spec for one of its entries.
   Used by the engine for write-time validation: when model calls e.g.
   `(task-set! :K v)`, engine looks up `(get subtree->entry-spec :session/tasks)`
   and runs `(s/explain-data spec v)` for soft warnings."}
  subtree->entry-spec
  {:session/specs  ::spec
   :session/tasks  ::task
   :session/facts  ::fact})

(def ^{:doc "Maps a `:session/X` subtree key to the spec for the whole subtree map.
   Used at render and persistence boundaries."}
  subtree->container-spec
  {:session/specs    :session/specs
   :session/tasks    :session/tasks
   :session/facts    :session/facts
   :session/workspace :session/workspace
   :session/symbols  :session/symbols
   :session/trailer  :session/trailer})
