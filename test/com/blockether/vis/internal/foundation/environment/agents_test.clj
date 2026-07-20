(ns com.blockether.vis.internal.foundation.environment.agents-test
  "Unit tests for AGENTS.md / CLAUDE.md discovery + cwd-keyed caching.

   The old byte-truncate boundary suite was removed when project rules
   were promoted to a real system block: AGENTS.md / CLAUDE.md are now
   inlined verbatim, no MAX_BYTES cap, no `[TRUNCATED …]` marker."
  (:require [babashka.fs :as fs]
            [clojure.string :as str]
            [com.blockether.vis.internal.foundation.environment.agents :as agents]
            [com.blockether.vis.internal.workspace :as workspace]
            [lazytest.core :refer [defdescribe expect it]]))

(defn- write-bytes!
  [^java.io.File f ^long n ^Character ch]
  (with-open [out (java.io.FileOutputStream. f)]
    (let [b (byte (int ch))]
      (dotimes [_ n]
        (.write out (int b)))))
  f)

(defn- with-tmp*
  "Run `f` with a freshly-created temp dir (as `java.io.File`); always
   delete the tree on exit. Function-based to keep clj-kondo happy."
  [f]
  (let [^java.nio.file.Path tmp (fs/create-temp-dir {:prefix "vis-agents-test-"})]
    (try (f (.toFile tmp)) (finally (fs/delete-tree tmp)))))

(defdescribe scan-in-test
             (it "AGENTS.md present -> :found? true, :source :repo"
                 (with-tmp* (fn [^java.io.File root]
                              (spit (java.io.File. root "AGENTS.md") "# rules\nuse honeysql\n")
                              (let [{:keys [result warnings]} (agents/scan-in root)]
                                (expect (:found? result))
                                (expect (= :repo (:source result)))
                                (expect (str/ends-with? (:path result) "AGENTS.md"))
                                (expect (= "# rules\nuse honeysql\n" (:content result)))
                                (expect (empty? warnings))))))
             (it "AGENTS.md absent + CLAUDE.md present -> :repo:claude-md-fallback"
                 (with-tmp* (fn [^java.io.File root]
                              (spit (java.io.File. root "CLAUDE.md") "# claude rules\n")
                              (let [{:keys [result]} (agents/scan-in root)]
                                (expect (:found? result))
                                (expect (= :repo:claude-md-fallback (:source result)))
                                (expect (str/ends-with? (:path result) "CLAUDE.md"))
                                (expect (= "# claude rules\n" (:content result)))))))
             (it "AGENTS.md wins over CLAUDE.md when both exist"
                 (with-tmp* (fn [^java.io.File root]
                              (spit (java.io.File. root "AGENTS.md") "# agents wins\n")
                              (spit (java.io.File. root "CLAUDE.md") "# claude loses\n")
                              (let [{:keys [result]} (agents/scan-in root)]
                                (expect (= :repo (:source result)))
                                (expect (= "# agents wins\n" (:content result)))))))
             (it "neither file present -> :found? false, no warnings"
                 (with-tmp* (fn [^java.io.File root]
                              (let [{:keys [result warnings]} (agents/scan-in root)]
                                (expect (false? (:found? result)))
                                (expect (empty? warnings))))))
             (it "large file is inlined verbatim — no truncation, no marker"
                 ;; AGENTS.md content rides in the PROJECT-INSTRUCTIONS system
                 ;; block; provider prompt caching covers the cost. The reader
                 ;; must never silently drop bytes.
                 (with-tmp*
                   (fn [^java.io.File root]
                     (let
                       [f
                        (java.io.File. root "AGENTS.md")

                        n
                        (* 64 1024)]

                       ;; 64 KB ≫ old 16 KB cap
                       (write-bytes! f n \a)
                       (let [{:keys [result]} (agents/scan-in root)]
                         (expect (:found? result))
                         (expect (= n (:bytes result)))
                         (expect (= n (count (:content result))))
                         (expect (not (contains? result :truncated?)))
                         (expect (not (contains? result :original-bytes)))
                         (expect (not (str/includes? (:content result) "[TRUNCATED")))))))))

(defdescribe instructions-shape-test
             (it "instructions uses active workspace root instead of JVM cwd"
                 (with-tmp* (fn [^java.io.File root]
                              (spit (java.io.File. root "AGENTS.md") "# workspace rules\n")
                              (binding [workspace/*workspace-root* (.getCanonicalPath root)]
                                (let [result (:result (agents/reload!))]
                                  (expect (:found? result))
                                  (expect (= "# workspace rules\n" (:content result)))
                                  (expect (str/ends-with? (:path result) "AGENTS.md"))))
                              (binding [workspace/*workspace-root* nil]
                                (agents/reload!)))))
             (it "instructions returns a map even when nothing found"
                 (with-tmp* (fn [^java.io.File root]
                              (let [{:keys [result]} (agents/scan-in root)]
                                (expect (map? result))
                                (expect (false? (:found? result)))
                                (expect (some? result))))))
             (it "conditional reload rereads only when guidance file marker changes"
                 (with-tmp* (fn [^java.io.File root]
                              (let [file (java.io.File. root "AGENTS.md")]
                                (spit file "# first\n")
                                (binding [workspace/*workspace-root* (.getCanonicalPath root)]
                                  (let
                                    [first-result (:result (agents/reload!))
                                     second-result (:result (agents/reload!))]

                                    (expect (= "# first\n" (:content first-result)))
                                    (expect (identical? first-result second-result)))
                                  (Thread/sleep 5)
                                  (spit file "# second\n")
                                  (.setLastModified file (+ 10000 (.lastModified file)))
                                  (let [changed-result (:result (agents/reload!))]
                                    (expect (= "# second\n" (:content changed-result)))))))))
             (it "placeholder for render-prompt-block coverage - covered via render_test.clj"
                 (expect true)))
