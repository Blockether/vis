(ns com.blockether.vis.internal.agents-test
  "Smoke tests for the internal AGENTS.md / CLAUDE.md reader.

   The exhaustive truncation / fallback / cache coverage lives in the
   foundation-core test (it predates the move and exercises the same
   logic via the shim). This namespace pins the internal entry points
   so a future shim removal can lean on a direct test surface."
  (:require [babashka.fs :as fs]
            [clojure.string :as str]
            [com.blockether.vis.internal.agents :as agents]
            [lazytest.core :refer [defdescribe expect it]]))

(defn- with-tmp-root*
  [f]
  (let [tmp (fs/create-temp-dir {:prefix "vis-internal-agents-"})]
    (try (f (.toFile tmp)) (finally (fs/delete-tree tmp)))))

(defdescribe scan-in-test
             (it "AGENTS.md present -> :found? true, :source :repo"
                 (with-tmp-root* (fn [^java.io.File root]
                                   (spit (java.io.File. root "AGENTS.md") "# rules\n")
                                   (let [{:keys [result warnings]} (agents/scan-in root)]
                                     (expect (true? (:found? result)))
                                     (expect (= :repo (:source result)))
                                     (expect (str/ends-with? (:path result) "AGENTS.md"))
                                     (expect (empty? warnings))))))
             (it "AGENTS.md absent + CLAUDE.md present -> :repo:claude-md-fallback"
                 (with-tmp-root* (fn [^java.io.File root]
                                   (spit (java.io.File. root "CLAUDE.md") "# claude rules\n")
                                   (let [{:keys [result]} (agents/scan-in root)]
                                     (expect (true? (:found? result)))
                                     (expect (= :repo:claude-md-fallback (:source result)))))))
             (it "neither file present -> :found? false"
                 (with-tmp-root* (fn [^java.io.File root]
                                   (let [{:keys [result warnings]} (agents/scan-in root)]
                                     (expect (false? (:found? result)))
                                     (expect (empty? warnings)))))))

(defdescribe scan-roots-test
             (it
               "stacks global → ancestor → workspace root, outermost first"
               (with-tmp-root*
                 (fn [^java.io.File root]
                   (let [global
                         (doto (java.io.File. root "fake-home-vis") .mkdirs)

                         parent
                         (doto (java.io.File. root "repo") .mkdirs)

                         ws
                         (doto (java.io.File. parent "sub") .mkdirs)]

                     (spit (java.io.File. global "AGENTS.md") "GLOBAL-RULE")
                     (spit (java.io.File. parent "AGENTS.md") "PARENT-RULE")
                     (spit (java.io.File. ws "AGENTS.md") "PROJECT-RULE")
                     (let [{:keys [result warnings]}
                           (agents/scan-roots global ws)

                           files
                           (:files result)

                           scoped
                           (mapv (juxt :scope :content) files)]

                       (expect (true? (:found? result)))
                       (expect (empty? warnings))
                       ;; global first, workspace root last; the ancestor rides between
                       (expect (= [:global "GLOBAL-RULE"] (first scoped)))
                       (expect (= [:project "PROJECT-RULE"] (peek scoped)))
                       (expect (some #(= [:ancestor "PARENT-RULE"] %) scoped))
                       ;; legacy view: innermost file + combined origin-headed content
                       (expect (= :repo (:source result)))
                       (expect (str/ends-with? (:path result) "AGENTS.md"))
                       (expect (str/includes? (:content result) "GLOBAL-RULE"))
                       (expect (str/includes? (:content result) "PARENT-RULE"))
                       (expect (str/includes? (:content result) "PROJECT-RULE"))
                       (expect (< (str/index-of (:content result) "GLOBAL-RULE")
                                  (str/index-of (:content result) "PROJECT-RULE"))))))))
             (it "per-directory AGENTS.md beats CLAUDE.md; other dirs still fall back"
                 (with-tmp-root*
                   (fn [^java.io.File root]
                     (let [parent
                           (doto (java.io.File. root "repo") .mkdirs)

                           ws
                           (doto (java.io.File. parent "sub") .mkdirs)]

                       (spit (java.io.File. parent "AGENTS.md") "PARENT-AGENTS")
                       (spit (java.io.File. parent "CLAUDE.md") "PARENT-CLAUDE")
                       (spit (java.io.File. ws "CLAUDE.md") "WS-CLAUDE")
                       (let [{:keys [result]}
                             (agents/scan-roots nil ws)

                             files
                             (:files result)]

                         (expect (not-any? #(= "PARENT-CLAUDE" (:content %)) files))
                         (expect (some #(= "PARENT-AGENTS" (:content %)) files))
                         (expect (= [:project :claude-md] ((juxt :scope :source) (peek files))))
                         ;; legacy source keyword preserved for workspace-root CLAUDE.md
                         (expect (= :repo:claude-md-fallback (:source result))))))))
             (it "single file -> content verbatim, no origin headers"
                 (with-tmp-root* (fn [^java.io.File root]
                                   (let [ws (doto (java.io.File. root "solo") .mkdirs)]
                                     (spit (java.io.File. ws "AGENTS.md") "ONLY-RULE")
                                     (let [{:keys [result]} (agents/scan-roots nil ws)]
                                       (expect (= "ONLY-RULE" (:content result)))
                                       (expect (= 1 (count (:files result)))))))))
             (it "nothing anywhere -> :found? false"
                 (with-tmp-root* (fn [^java.io.File root]
                                   (let [ws
                                         (doto (java.io.File. root "empty") .mkdirs)

                                         {:keys [result warnings]}
                                         (agents/scan-roots nil ws)]

                                     (expect (false? (:found? result)))
                                     (expect (empty? warnings)))))))
