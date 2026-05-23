(ns com.blockether.vis.internal.agents-test
  "Smoke tests for the internal AGENTS.md / CLAUDE.md reader.

   The exhaustive truncation / fallback / cache coverage lives in the
   foundation-core test (it predates the move and exercises the same
   logic via the shim). This namespace pins the internal entry points
   so a future shim removal can lean on a direct test surface."
  (:require
   [babashka.fs :as fs]
   [clojure.string :as str]
   [com.blockether.vis.internal.agents :as agents]
   [lazytest.core :refer [defdescribe expect it]]))

(defn- with-tmp-root* [f]
  (let [tmp (fs/create-temp-dir {:prefix "vis-internal-agents-"})]
    (try (f (.toFile tmp))
      (finally (fs/delete-tree tmp)))))

(defdescribe scan-in-test
  (it "AGENTS.md present -> :found? true, :source :repo"
    (with-tmp-root*
      (fn [^java.io.File root]
        (spit (java.io.File. root "AGENTS.md") "# rules\n")
        (let [{:keys [result warnings]} (agents/scan-in root)]
          (expect (true? (:found? result)))
          (expect (= :repo (:source result)))
          (expect (str/ends-with? (:path result) "AGENTS.md"))
          (expect (empty? warnings))))))

  (it "AGENTS.md absent + CLAUDE.md present -> :repo:claude-md-fallback"
    (with-tmp-root*
      (fn [^java.io.File root]
        (spit (java.io.File. root "CLAUDE.md") "# claude rules\n")
        (let [{:keys [result]} (agents/scan-in root)]
          (expect (true? (:found? result)))
          (expect (= :repo:claude-md-fallback (:source result)))))))

  (it "neither file present -> :found? false"
    (with-tmp-root*
      (fn [^java.io.File root]
        (let [{:keys [result warnings]} (agents/scan-in root)]
          (expect (false? (:found? result)))
          (expect (empty? warnings)))))))
