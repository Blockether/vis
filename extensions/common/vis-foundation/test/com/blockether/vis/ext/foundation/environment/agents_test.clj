(ns com.blockether.vis.ext.foundation.environment.agents-test
  "Unit tests for AGENTS.md / CLAUDE.md discovery, byte-truncate
   boundary, and cwd-keyed caching. See plan §6 / Q4 / Q5 / Q10."
  (:require
   [babashka.fs :as fs]
   [clojure.string :as str]
   [com.blockether.vis.ext.foundation.environment.agents :as agents]
   [com.blockether.vis.internal.workspace-context :as workspace-context]
   [lazytest.core :refer [defdescribe expect it]]))

(defn- write-bytes! [^java.io.File f ^long n ^Character ch]
  (with-open [out (java.io.FileOutputStream. f)]
    (let [b (byte (int ch))]
      (dotimes [_ n] (.write out (int b)))))
  f)

(defn- with-tmp*
  "Run `f` with a freshly-created temp dir (as `java.io.File`); always
   delete the tree on exit. Function-based to keep clj-kondo happy."
  [f]
  (let [tmp (fs/create-temp-dir {:prefix "vis-agents-test-"})]
    (try (f (.toFile tmp))
      (finally (fs/delete-tree tmp)))))

(defdescribe scan-in-test
  (it "AGENTS.md present -> :found? true, :source :repo"
    (with-tmp* (fn [root]
                 (spit (java.io.File. root "AGENTS.md") "# rules\nuse honeysql\n")
                 (let [{:keys [result warnings]} (agents/scan-in root)]
                   (expect (:found? result))
                   (expect (= :repo (:source result)))
                   (expect (str/ends-with? (:path result) "AGENTS.md"))
                   (expect (= "# rules\nuse honeysql\n" (:content result)))
                   (expect (false? (:truncated? result)))
                   (expect (empty? warnings))))))

  (it "AGENTS.md absent + CLAUDE.md present -> :repo:claude-md-fallback"
    (with-tmp* (fn [root]
                 (spit (java.io.File. root "CLAUDE.md") "# claude rules\n")
                 (let [{:keys [result]} (agents/scan-in root)]
                   (expect (:found? result))
                   (expect (= :repo:claude-md-fallback (:source result)))
                   (expect (str/ends-with? (:path result) "CLAUDE.md"))
                   (expect (= "# claude rules\n" (:content result)))))))

  (it "AGENTS.md wins over CLAUDE.md when both exist"
    (with-tmp* (fn [root]
                 (spit (java.io.File. root "AGENTS.md") "# agents wins\n")
                 (spit (java.io.File. root "CLAUDE.md") "# claude loses\n")
                 (let [{:keys [result]} (agents/scan-in root)]
                   (expect (= :repo (:source result)))
                   (expect (= "# agents wins\n" (:content result)))))))

  (it "neither file present -> :found? false, no warnings"
    (with-tmp* (fn [root]
                 (let [{:keys [result warnings]} (agents/scan-in root)]
                   (expect (false? (:found? result)))
                   (expect (empty? warnings))))))

  (it "exactly MAX_BYTES (16384): no truncation"
    (with-tmp* (fn [root]
                 (let [f (java.io.File. root "AGENTS.md")]
                   (write-bytes! f agents/MAX_BYTES \a)
                   (let [{:keys [result]} (agents/scan-in root)]
                     (expect (:found? result))
                     (expect (false? (:truncated? result)))
                     (expect (= agents/MAX_BYTES (:bytes result)))
                     (expect (= agents/MAX_BYTES (count (:content result)))))))))

  (it "MAX_BYTES + 1 (16385): truncates with marker"
    (with-tmp* (fn [root]
                 (let [f (java.io.File. root "AGENTS.md")]
                   (write-bytes! f (inc agents/MAX_BYTES) \a)
                   (let [{:keys [result]} (agents/scan-in root)
                         content (:content result)]
                     (expect (:found? result))
                     (expect (true? (:truncated? result)))
                     (expect (= (inc agents/MAX_BYTES) (:original-bytes result)))
                     (expect (str/includes? content "[TRUNCATED - 1 more bytes."))
                     (expect (str/includes? content "(vis/main-agent-instructions)")))))))

  (it "much larger file (32KB): truncates, original-bytes preserved"
    (with-tmp* (fn [root]
                 (let [f (java.io.File. root "AGENTS.md")
                       n (* 2 agents/MAX_BYTES)]
                   (write-bytes! f n \b)
                   (let [{:keys [result]} (agents/scan-in root)]
                     (expect (true? (:truncated? result)))
                     (expect (= n (:original-bytes result)))
                     (expect (str/includes? (:content result)
                               (str "[TRUNCATED - " agents/MAX_BYTES " more bytes.")))))))))

(defdescribe instructions-shape-test
  (it "instructions uses active workspace root instead of JVM cwd"
    (with-tmp* (fn [root]
                 (spit (java.io.File. root "AGENTS.md") "# workspace rules\n")
                 (binding [workspace-context/*workspace-root* (.getCanonicalPath root)]
                   (let [result (:result (agents/reload!))]
                     (expect (:found? result))
                     (expect (= "# workspace rules\n" (:content result)))
                     (expect (str/ends-with? (:path result) "AGENTS.md"))))
                 (binding [workspace-context/*workspace-root* nil]
                   (agents/reload!)))))

  (it "instructions returns a map even when nothing found"
    (with-tmp* (fn [root]
                 (let [{:keys [result]} (agents/scan-in root)]
                   (expect (map? result))
                   (expect (false? (:found? result)))
                   (expect (some? result))))))

  (it "placeholder for render-prompt-block coverage - covered via render_test.clj"
    (expect true)))

(defdescribe truncation-marker-format-test
  (it "marker text matches plan Q5 spec exactly"
    (with-tmp* (fn [root]
                 (let [f (java.io.File. root "AGENTS.md")]
                   (write-bytes! f (+ agents/MAX_BYTES 999) \c)
                   (let [{:keys [result]} (agents/scan-in root)
                         content (:content result)]
                     (expect (str/includes? content "[TRUNCATED - 999 more bytes. Read full content via (vis/main-agent-instructions).]"))))))))
