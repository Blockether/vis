(ns com.blockether.vis.internal.improve.core
  "Project-scoped improvement workflow. Intake is independent of UI and automation settings.
   Source content is evidence, not permission to execute it; review only edits workflow records."
  (:require [com.blockether.vis.contract.improve :as contract]
            [com.blockether.vis.internal.persistance.core :as ps]))

(defn list-records
  "Records ordered by id. Omitted project means all; explicit nil means Unassigned.
   Keyset options: after (exclusive, default 0), limit (default 100, maximum 200), status."
  [db opts]
  (ps/db-improve-list db (contract/validate! :list opts)))

(defn get-record
  "Record including original source, or nil when absent."
  [db id]
  (ps/db-improve-get db id))

(defn create!
  "Create a manual record or group. Original Council source cannot be supplied by a client."
  [db attrs]
  (ps/db-improve-create! db (contract/validate! :create attrs)))

(defn update!
  "Edit analysis or hierarchy. Close cascades down; reopen opens ancestors; project moves
   include descendants. Optional expected_version rejects stale edits with HTTP status 409."
  [db id attrs]
  (ps/db-improve-update! db id (contract/validate! :update attrs)))

(defn project-ids
  "Distinct projects (including nil Unassigned) with open records."
  [db]
  (ps/db-improve-project-ids db))

(defn apply-review!
  "Atomically apply {:expected {id version}, :updates [{:id id ...attrs}],
   :groups [{:title ... :project_id ... :content ... :children [id ...]}]}.
   Every touched existing record, including cascade targets, must have an expected version.
   A quick, side-effect-free still-current? callback gates writes and commit. Conflict aborts
   the entire proposal. Returns {:records [...]}, including groups and cascade targets."
  [db proposal still-current?]
  (ps/db-improve-apply-review! db proposal still-current?))
