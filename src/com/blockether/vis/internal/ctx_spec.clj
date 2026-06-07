(ns com.blockether.vis.internal.ctx-spec
  "Formal data model for the CTX — tasks + facts only.

   `clojure.spec.alpha` definitions for every shape in the CTX blob.
   The engine consults these specs at write time (warn on shape
   mismatch), at render time (deterministic ordering), and at test time
   (`s/gen` generators). The specs are the canonical source of truth for
   the SHAPE; the engine adds dependency / contradiction / FSM
   invariants on top.

   Top-level entry point: `::ctx`.

   Subtrees:
     `:session/scope`     engine-rendered cursor {:turn :iter :next-form}
     `:session/tasks`     work items; status FSM, deps, hook metadata
     `:session/facts`     observations, decisions, rules, behavior
     `:session/workspace` engine-rendered git workspace state
     `:session/symbols`   engine-rendered live SCI symbol directory
     `:session/trailer`   pinned iter envelopes (verbatim or summarized)

   Foundation extension `:turn.iteration/start` hooks emit hook-sourced
   tasks (`:source :hook`, `:hook-id`, `:importance`) under
   `:session/tasks`; the model satisfies them via
   `(task-set! id {:status :done})`. Done is self-asserted — the engine
   stamps `:done-born` and does NOT verify the claim.

   Scope coordinates:
     `::scope-form`  e.g. \"t3/i2/f1\"
     `::scope-iter`  e.g. \"t3/i2\"
     `::scope-turn`  e.g. \"t3\"

   Universal `:depends_on` edge inventory across subtrees:
     task → task | fact via `:session.task/depends-on`
     fact → task | fact via `:session.fact/depends-on`
   Refs are bare keys (same-kind shorthand) or typed `[:task :K]` /
   `[:fact :K]` vectors. The engine HARD-rejects writes that would
   introduce a `:depends_on` cycle across kinds; dangling refs are a
   soft `:session/hints` entry.

   Scope cursor (`:session/scope`) is engine-stamped before render so
   the model always knows where it is. User-supplied `::scope-form`
   values are accepted verbatim; the engine never refuses a write on a
   future scope.

   Mutator API (upsert-only; consumed by `ctx-engine/apply-mutator`):

     Top-level (merge partials):
       :task-set! :fact-set!

     Dependency edges (replace the full vec):
       :task-depends! :fact-depends!

     Fact contradictions (symmetric):
       :fact-contradicts! :fact-contradicts-remove!

   Invariants the engine enforces are split HARD (cycle, malformed scope,
   partial-overlap trailer-summarize) vs SOFT (everything else, surfaced
   as `:session/hints` short strings in rendered ctx)."
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
;;
;; Facts carry the universal `:depends_on` graph and symmetric
;; `:contradicts` links (declared via `fact-contradicts!`).

(s/def :session.fact/status     #{:active :superseded})
(s/def :session.fact/done-born  ::scope-form)
(s/def :session.fact/depends-on (s/coll-of ::entry-key :kind vector?))
(s/def :session.fact/contradicts (s/coll-of ::entry-key :kind set?))
;; W2 — file knowledge is a FACT (durable), not just a trailer stub. A fact MAY
;; carry the SAME structured `:files` regions as a trailer summary (full path +
;; verbatim `:src` + hashline `:from-hash`/`:to-hash`), so a located/edited
;; region survives cross-turn as immortal knowledge and stays directly
;; re-patchable — instead of being lost when the trailer is re-summarized. Shares
;; the region shape with the trailer summary (`::trailer-summary-file`, defined
;; below; the forward ref resolves at validation time).
(s/def :session.fact/files (s/coll-of ::trailer-summary-file :kind vector?))

(s/def ::fact
  (s/keys :req-un [::content ::born]
    :opt-un [:session.fact/status
             :session.fact/done-born
             :session.fact/depends-on
             :session.fact/contradicts
             :session.fact/files]))

;; Soft rules:
;;   - :status defaults to :active when omitted (engine assumes :active for legacy facts)
;;   - :status :superseded MUST have :done-born (engine auto-stamps)
;;   - :depends_on entries must point to existing tasks/facts (engine warns)
;;   - two :active facts that declared symmetric :contradicts emit a warning

;; =============================================================================
;; Task — work items
;; =============================================================================
;;
;; Model never deletes tasks. To abandon: flip :status to :cancelled. Engine
;; stamps :done-born when status enters a terminal value (:done / :cancelled)
;; and garbage-collects the entry from live CTX after a status-specific TTL.
;; Snapshots in per-turn ctx snapshots on session_turn_state make every
;; archived entry replayable through the soul/state chain.
;;
;; Done is SELF-ASSERTED. `(task-set! :K {:status :done})` is accepted as-is;
;; the engine stamps :done-born and does NOT verify the claim. There is no
;; proof, validator, or reversion.

(s/def :session.task/depends-on (s/coll-of ::entry-key :kind vector?))
(s/def :session.task/status     #{:todo :doing :done :cancelled})
(s/def :session.task/done-born  ::scope-form)

;; Hook-emitted task fields (hints collapsed into tasks).
;;
;; `:source` discriminates ownership:
;;   :user   — model-created via `(task-set! …)`
;;   :hook   — foundation extension `:turn.iteration/start` hook emitted
;;   :engine — reserved for engine-synthesised tasks
;;
;; Hook-tasks additionally carry `:hook-id` (the keyword the hook
;; identified itself with, also used as the task key so dedup is
;; trivial) and `:importance` for renderer sort order. The model
;; satisfies a hook task by `(task-set! :hook-id {:status :done})`.
(s/def :session.task/source     #{:user :hook :engine})
(s/def :session.task/hook-id    keyword?)
(s/def :session.task/importance #{:info :warn :critical})

;; W3 — acceptance + verification. A task SHOULD carry `:acceptance` (the
;; concrete criterion that means it's truly done) and, once met, `:verified?
;; true` (the model asserts it checked — engine does NOT verify the claim). The
;; engine emits a SOFT `:session/hints` nudge when a task is closed `:done`
;; with an `:acceptance` but `:verified? not true` — so "done" isn't claimed
;; without a stated, checked criterion.
(s/def :session.task/acceptance string?)
(s/def :session.task/verified?  boolean?)

(s/def ::task
  (s/keys :req-un [::title
                   :session.task/status
                   ::born]
    :opt-un [:session.task/depends-on
             :session.task/done-born
             :session.task/source
             :session.task/hook-id
             :session.task/importance
             :session.task/acceptance
             :session.task/verified?]))

;; Soft rules (engine-side; not enforced by spec):
;;   - :depends_on entries must point to existing tasks/facts
;;   - :status :done | :cancelled MUST have :done-born (engine auto-stamps)
;;   - :status :done while a :depends_on target is non-terminal emits a soft
;;     warning (does not block or revert — done is self-asserted)
;;   - status transitions are reconstructable from :session/trailer mutation pins;
;;     no per-task journal is stored

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

;; W1 — actionable summaries. A model-produced trailer summary MAY carry a
;; structured `:files` record so the interesting REGION of a touched file
;; survives compaction WITH ITS VERBATIM CONTENT — letting the big full-file
;; read pin be dropped from the prompt while the surgical regions stay
;; directly editable (no re-cat needed). Each file lists `:regions`; each
;; region is {:src "<verbatim text>" :note "<what/why>" :lines [start end]?}.
;;
;; Aligned with vis's NATIVE hashline editing (foundation-core editing/patch):
;; vis edits by CONTENT HASH, not line number. `cat` shows `<ln> <hash>│ text`
;; and `patch {:from-hash H1 :to-hash H2 :replace R}` resolves those per-line
;; hashes against LIVE content. So a region carries:
;;   :src        verbatim text — the MEMORY (what's there, for reasoning) and a
;;               `:search`-patch fallback; required (a 4-hex hash alone is opaque).
;;   :from-hash  the line-hash of the region's first line, copied from the cat
;;   :to-hash    gutter — the native, drift-resistant, token-cheap edit address
;;               so the model patches the region from memory (no re-cat). :to-hash
;;               defaults to :from-hash (single line). Optional.
;;   :lines      [start end] — an ADVISORY navigation hint (where in the file the
;;               region roughly sits). NOT an edit address (drifts after edits;
;;               the gutter uses hashes). Optional; `(s/tuple nat-int? nat-int?)`
;;               for a clean generator.
;;   :note       what/why it matters. Optional.
;; No file content hash (a stale region just fails to patch — `:src`/`:from-hash`
;; are self-validating, the re-read signal).
(s/def :session.trailer.summary.file/path        string?)
(s/def :session.trailer.summary.region/src       string?)
(s/def :session.trailer.summary.region/note      string?)
(s/def :session.trailer.summary.region/from-hash string?)
(s/def :session.trailer.summary.region/to-hash   string?)
(s/def :session.trailer.summary.region/lines     (s/tuple nat-int? nat-int?))
(s/def ::trailer-summary-region
  (s/keys :req-un [:session.trailer.summary.region/src]
    :opt-un [:session.trailer.summary.region/note
             :session.trailer.summary.region/from-hash
             :session.trailer.summary.region/to-hash
             :session.trailer.summary.region/lines]))
(s/def :session.trailer.summary.file/regions
  (s/coll-of ::trailer-summary-region :kind vector?))
(s/def ::trailer-summary-file
  (s/keys :req-un [:session.trailer.summary.file/path]
    :opt-un [:session.trailer.summary.file/regions]))
(s/def :session.trailer.summary/files
  (s/coll-of ::trailer-summary-file :kind vector?))

(s/def ::trailer-summary
  (s/keys :req-un [:session.trailer.summary/scope-start
                   :session.trailer.summary/scope-end
                   :session.trailer.summary/summary
                   :session.trailer.summary/born]
    :opt-un [:session.trailer.summary/files]))

;; Soft rules:
;;   :scope-start must be ≤ :scope-end per scope comparator.
;;   New summary must NOT partially overlap an existing summary;
;;   partial overlap is rejected with an explain message at validation time.
;;   Engine stamps :born to the scope of the (summarize {:trailer …}) form.

(s/def ::trailer-entry
  (s/or :pin     ::trailer-pin
    :summary ::trailer-summary))

;; =============================================================================
;; Workspace — engine-rendered. Workspace identity and VCS capability are
;; separate axes:
;;   workspace = root/place where tools operate
;;   sandbox   = isolated/disposable workspace flag
;;   VCS       = optional capability on that root
;;
;; `:vcs/kind :none` means a real workspace with no supported VCS. It does
;; NOT mean missing workspace. Generic names only: no git-era
;; `branch/trunk` aliases in CTX; use `:vcs/ref` and `:vcs/mainline`.
;; =============================================================================

(s/def :vcs/kind #{:git :hg :jj :fossil :none})

(s/def :workspace/id any?)
(s/def :workspace/root string?)
(s/def :workspace/sandbox? boolean?)
(s/def :workspace/parent-id any?)
(s/def :workspace/exists? boolean?)
(s/def :workspace/error string?)

;; Generic VCS keys — shared semantics across VCS kinds.
(s/def :vcs/ref       string?)
(s/def :vcs/mainline  string?)
(s/def :vcs/head      string?)
(s/def :vcs/dirty?    boolean?)
(s/def :vcs/integrable? (s/or :bool boolean? :unknown #{:unknown}))

;; Per-file added/removed line counts — the only VCS-namespaced sub-spec.
;; Used by `:vcs/stats {path → file-stats}`. Common to every detector
;; (git diff --numstat, hg diff --stat, jj diff …).
(s/def :vcs.file/added    nat-int?)
(s/def :vcs.file/removed  nat-int?)

(s/def :vcs/file-stats
  (s/keys :req-un [:vcs.file/added :vcs.file/removed]))

(s/def :vcs/stats
  (s/map-of string? :vcs/file-stats))

(s/def :vcs/commit
  (s/keys :opt-un [:vcs.commit/sha :vcs.commit/message]))
(s/def :vcs.commit/sha string?)
(s/def :vcs.commit/message string?)
(s/def :vcs/unmerged-commits (s/coll-of :vcs/commit :kind vector?))

(s/def ::workspace
  (s/and
    (s/keys :opt [:workspace/id :workspace/root :workspace/sandbox?
                  :workspace/parent-id :workspace/exists? :workspace/error
                  :vcs/kind :vcs/ref :vcs/mainline :vcs/head :vcs/dirty?
                  :vcs/stats :vcs/unmerged-commits :vcs/integrable?])
    ;; If VCS has been detected/rendered at all (including :none), the
    ;; workspace root must be present. Bare `{:vcs/kind :none}` is legacy
    ;; placeholder shape and must not validate.
    #(or (nil? (:vcs/kind %)) (string? (:workspace/root %)))))

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
;; Scope cursor — engine-rendered current position inside the turn
;; =============================================================================
;;
;; `:session/scope` tells the model where it is RIGHT NOW. Engine-stamped per
;; iter, before render. `:next-form` is 1-based — the index that the model's
;; first form in this iter's fence WILL receive. Subsequent forms in the same
;; fence increment from there.

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
  {:session/tasks  ::task
   :session/facts  ::fact})

(def ^{:doc "Maps a `:session/X` subtree key to the spec for the whole subtree map.
   Used at render and persistence boundaries."}
  subtree->container-spec
  {:session/tasks    :session/tasks
   :session/facts    :session/facts
   :session/workspace :session/workspace
   :session/symbols  :session/symbols
   :session/trailer  :session/trailer})
