(ns com.blockether.vis.loop.knowledge.git-ingestion-test
  "Pending SQLite port — original SQLite-based git-ingestion storage tests
   removed during the SQLite cutover. Rewrite against rlm-db/store-commit-entity!
   + rlm-db/db-search-commits when reinstating coverage. Pure parser tests
   (commit-message parsing, ticket-ref extraction) can be reintroduced without
   DB dependencies first."
  (:require
   [lazytest.core :refer [defdescribe it expect]]))

(defdescribe git-ingestion-pending-sqlite-port
  (it "pending SQLite port" (expect true)))
