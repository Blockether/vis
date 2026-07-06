(ns com.blockether.vis.internal.foundation.environment.host-test
  "Smoke tests for host-side fact extraction. The values come from
   the running JVM, so we assert *shape* rather than exact values."
  (:require [com.blockether.vis.internal.foundation.environment.host :as host]
            [lazytest.core :refer [defdescribe expect it]]))

(defdescribe host-snapshot-test
             (it "returns the documented keys"
                 (let [snapshot (host/snapshot)]
                   (expect (string? (:cwd snapshot)))
                   (expect (string? (:user snapshot)))
                   (expect (string? (:home snapshot)))
                   (expect (string? (:os-name snapshot)))
                   (expect (string? (:os-arch snapshot)))
                   (expect (string? (:os-version snapshot)))
                   (expect (string? (:shell snapshot)))
                   (expect (string? (:locale snapshot)))
                   (expect (string? (:time snapshot)))
                   (expect (string? (:timezone snapshot)))
                   (expect (string? (:jvm snapshot)))))
             (it "shell falls back to 'unknown' rather than nil"
                 (let [snapshot (host/snapshot)]
                   (expect (some? (:shell snapshot))))))
