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
     `:session/hints`     engine-rendered one-shot instructions
     `:session/trailer`   pinned iter envelopes (verbatim or summarized)

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
   reference scopes later in the same fence."
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

(s/def ::fact
  (s/keys :req-un [::content ::born]))

;; =============================================================================
;; Task — work items
;; =============================================================================

(s/def :session.task/depends-on (s/coll-of ::entry-key :kind vector?))
(s/def :session.task/status     #{:todo :doing :done :cancelled})

(s/def :session.task.proof/requirement keyword?)
(s/def :session.task.proof/proof       ::scope-form)
(s/def :session.task/proof
  (s/keys :req-un [:session.task.proof/requirement
                   :session.task.proof/proof]))

(s/def :session.task/specs
  (s/map-of ::entry-key (s/coll-of :session.task/proof :kind vector?)))

(s/def ::task
  (s/keys :req-un [::title
                   :session.task/specs
                   :session.task/status
                   ::born]
    :opt-un [:session.task/depends-on]))

;; Soft rules (engine-side validators; not enforced by spec):
;;   - :specs keys must point to existing keys in :session/specs
;;   - :depends-on entries must point to existing keys in :session/tasks
;;   - proof :requirement ids must exist on the referenced spec
;;   - when requirement has :validator-fn, proof scope result must satisfy it
;;   - status transitions are reconstructable from :session/trailer mutation pins;
;;     no per-task journal is stored

;; =============================================================================
;; Spec — formal requirements
;; =============================================================================

(s/def :session.requirement/id           keyword?)
(s/def :session.requirement/title        string?)
(s/def :session.requirement/validator-fn string?)
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
(s/def :session/hints     (s/map-of keyword?     ::hint))
(s/def :session/specs     (s/map-of ::entry-key  ::spec))
(s/def :session/tasks     (s/map-of ::entry-key  ::task))
(s/def :session/facts     (s/map-of ::entry-key  ::fact))
(s/def :session/trailer   (s/coll-of ::trailer-entry :kind vector?))

(s/def ::ctx
  (s/keys :req [:session/id
                :session/turn
                :session/scope
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
