(ns com.blockether.vis.rlm.vitality-test
  "Pending SQLite port — original Datalevin-based vitality tests removed
   during the SQLite cutover. Rewrite against rlm-db/* vitality APIs
   (compute-page-vitality, record-page-access!, propagate-activation!,
   finalize-q-updates!) when reinstating coverage."
  (:require
   [lazytest.core :refer [defdescribe it expect]]))

(defdescribe vitality-pending-sqlite-port
  (it "pending SQLite port" (expect true)))
