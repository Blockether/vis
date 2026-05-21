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
     `:session/specs`     formal requirements with acceptance criteria
     `:session/tasks`     work items; each task serves exactly one spec
     `:session/facts`     observations, decisions, rules, behavior
     `:session/workspace` engine-rendered git workspace state
     `:session/symbols`   engine-rendered live SCI symbol directory
     `:session/hints`     engine-rendered one-shot instructions
     `:session/trailer`   pinned iter envelopes (verbatim or summarized)

   Scope coordinates:
     `::scope-form`  e.g. \"t3/i2/f1\"
     `::scope-iter`  e.g. \"t3/i2\"
     `::scope-turn`  e.g. \"t3\"

   Edge inventory across subtrees (validated as soft warnings, not by spec):
     spec  → facts  via `:session.spec/facts`   (vec of `::entry-key`)
     spec  → tasks  via `:session.spec/tasks`   (vec of `::entry-key`)
     task  → spec   via `:session.task/spec`    (single `::entry-key`)
     task  → tasks  via `:session.task/depends-on` (DAG of `::entry-key`)
     task  → facts  via `:session.task/facts`   (OWNED — cascade-removed on task-remove!)
     fact  → *      via `:session.fact/connections` (any `::entry-key`)"
  (:require [clojure.spec.alpha :as s]))

;; =============================================================================
;; Scope coordinates
;; =============================================================================

(def ^:private scope-form-re #"^t[1-9][0-9]*/i[1-9][0-9]*/f[1-9][0-9]*$")
(def ^:private scope-iter-re #"^t[1-9][0-9]*/i[1-9][0-9]*$")
(def ^:private scope-turn-re #"^t[1-9][0-9]*$")

(s/def ::scope-form
  (s/and string? #(re-matches scope-form-re %)))

(s/def ::scope-iter
  (s/and string? #(re-matches scope-iter-re %)))

(s/def ::scope-turn
  (s/and string? #(re-matches scope-turn-re %)))

;; =============================================================================
;; Entry keys (model-chosen keywords)
;; =============================================================================

(s/def ::entry-key keyword?)

;; =============================================================================
;; Common fields
;; =============================================================================

(s/def ::content string?)
(s/def ::title   string?)
(s/def ::tags    (s/coll-of keyword? :kind set?))
(s/def ::born    ::scope-form)
(s/def ::summary string?)

;; =============================================================================
;; Fact — observations, decisions, rules, behavior
;; =============================================================================

(s/def :session.fact/connections
  (s/coll-of ::entry-key :kind vector?))

(s/def ::fact
  (s/keys :req-un [::content ::born]
    :opt-un [::tags :session.fact/connections]))

;; =============================================================================
;; Task — work items
;; =============================================================================

(s/def :session.task/spec        ::entry-key)
(s/def :session.task/depends-on  (s/coll-of ::entry-key :kind vector?))
(s/def :session.task/facts       (s/coll-of ::entry-key :kind vector?))
(s/def :session.task/status      #{:todo :doing :done :cancelled})
(s/def :session.task/evidence    (s/coll-of ::scope-form :kind vector?))

(s/def :session.task.journal-entry/scope  ::scope-form)
(s/def :session.task.journal-entry/status :session.task/status)

(s/def :session.task/journal-entry
  (s/keys :req-un [:session.task.journal-entry/status
                   :session.task.journal-entry/scope]))

(s/def :session.task/journal
  (s/coll-of :session.task/journal-entry :kind vector?))

(s/def ::task
  (s/keys :req-un [::title
                   :session.task/spec
                   :session.task/status
                   ::born]
    :opt-un [:session.task/depends-on
             :session.task/facts
             :session.task/evidence
             :session.task/journal]))

;; Soft rules (engine-side validators; not enforced by spec):
;;   - :status :done MUST have non-empty :evidence
;;   - :facts entries must not appear in any OTHER task's :facts (exclusive ownership)
;;   - :spec must point to an existing key in :session/specs
;;   - :depends-on entries must point to existing keys in :session/tasks

;; =============================================================================
;; Spec — formal requirements
;; =============================================================================

(s/def :session.spec/acceptance
  (s/coll-of string? :kind vector? :min-count 1))

(s/def :session.spec/facts
  (s/coll-of ::entry-key :kind vector?))

(s/def :session.spec/tasks
  (s/coll-of ::entry-key :kind vector?))

(s/def :session.spec/status
  #{:draft :doing :done :cancelled})

(s/def :session.spec/done-born ::scope-form)

(s/def ::spec
  (s/keys :req-un [::title
                   :session.spec/acceptance
                   :session.spec/status
                   ::born]
    :opt-un [:session.spec/facts
             :session.spec/tasks
             :session.spec/done-born]))

;; Soft rules:
;;   - :status :done | :cancelled MUST have :done-born (engine auto-stamps)
;;   - :facts entries must point to existing keys in :session/facts
;;   - :tasks entries must point to existing keys in :session/tasks

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
;; Workspace — engine-rendered. Keys are :git/* (VCS-discriminator
;; namespace). Future hg/jj/etc support will slot in as :hg/branch
;; :hg/head etc without colliding.
;; =============================================================================

(s/def :git/branch  string?)
(s/def :git/trunk   string?)
(s/def :git/head    string?)
(s/def :git/dirty?  boolean?)

(s/def :git.file/added    nat-int?)
(s/def :git.file/removed  nat-int?)

(s/def :git/file-stats
  (s/keys :req-un [:git.file/added :git.file/removed]))

(s/def :git/stats
  (s/map-of string? :git/file-stats))

(s/def ::workspace
  (s/keys :req [:git/branch :git/trunk :git/head :git/dirty? :git/stats]))

;; =============================================================================
;; Symbol directory — engine-rendered from SCI introspection + engine-side :born index
;; =============================================================================

(s/def :session.symbol/arglists  (s/coll-of seq?))
(s/def :session.symbol/doc       (s/nilable string?))
(s/def :session.symbol/born      ::scope-form)

(s/def ::symbol-info
  (s/keys :req-un [:session.symbol/born]
    :opt-un [:session.symbol/arglists
             :session.symbol/doc]))

;; =============================================================================
;; Hints — engine-rendered one-shot instructions
;; =============================================================================

(s/def :session.hint/body         string?)
(s/def :session.hint/importance   #{:info :warn :critical})
(s/def :session.hint/satisfy-with string?)

(s/def ::hint
  (s/keys :req-un [:session.hint/body]
    :opt-un [:session.hint/importance
             :session.hint/satisfy-with]))

;; =============================================================================
;; Top-level CTX
;; =============================================================================

(s/def :session/id        string?)
(s/def :session/turn      pos-int?)
(s/def :session/workspace ::workspace)
(s/def :session/symbols   (s/map-of symbol?      ::symbol-info))
(s/def :session/hints     (s/map-of keyword?     ::hint))
(s/def :session/specs     (s/map-of ::entry-key  ::spec))
(s/def :session/tasks     (s/map-of ::entry-key  ::task))
(s/def :session/facts     (s/map-of ::entry-key  ::fact))
(s/def :session/trailer   (s/coll-of ::trailer-entry :kind vector?))

(s/def ::ctx
  (s/keys :req [:session/id
                :session/turn
                :session/workspace
                :session/symbols
                :session/hints
                :session/specs
                :session/tasks
                :session/facts
                :session/trailer]))

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
   :session/hints    :session/hints
   :session/trailer  :session/trailer})
