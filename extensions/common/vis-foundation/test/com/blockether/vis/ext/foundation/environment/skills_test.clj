(ns com.blockether.vis.ext.foundation.environment.skills-test
  "Unit tests for skills frontmatter discovery, parsing, malformed
   handling, collision policy, and alphabetical ordering. Covers
   plan §6 + Q6 + Q7 + Q10."
  (:require
   [babashka.fs :as fs]
   [clojure.string :as str]
   [com.blockether.vis.internal.skills :as skills]
   [com.blockether.vis.internal.workspace-context :as workspace-context]
   [lazytest.core :refer [defdescribe expect it]]))

(defn- with-tmp*
  "Run `f` with a freshly-created temp dir (as `java.io.File`); always
   delete the tree on exit. Function-based to keep clj-kondo happy."
  [f]
  (let [tmp (fs/create-temp-dir {:prefix "vis-skills-test-"})]
    (try (f (.toFile tmp))
      (finally (fs/delete-tree tmp)))))

(defn- with-two-tmps*
  "Run `f` with two freshly-created temp dirs (repo-root, user-root)."
  [f]
  (with-tmp* (fn [repo-root]
               (with-tmp* (fn [user-root]
                            (f repo-root user-root))))))

(defn- write-skill!
  "Write a SKILL.md under `<root>/.agents/skills/<name>/SKILL.md`.
   `frontmatter` may be a YAML string (without the `---` fences)
   or already include them; we only fence when the caller passed
   raw YAML content."
  [^java.io.File root ^String dir-name ^String content]
  (let [dir   (java.io.File. root (str ".agents/skills/" dir-name))]
    (.mkdirs dir)
    (spit (java.io.File. dir "SKILL.md") content)
    dir))

(def ^:private SAMPLE_SINGLE_LINE
  "---\nname: alpha\ndescription: Alpha skill description. Use when X happens.\n---\n\n# Alpha\n\nBody markdown.\n")

(def ^:private SAMPLE_FOLDED
  "---\nname: caveman\ndescription: >\n  Multi-line folded description that\n  collapses newlines into spaces. Use when token usage matters.\nother: passthrough\n---\n\n# Caveman body\n")

(def ^:private SAMPLE_DISABLE_INVOCATION
  "---\nname: grill-with-docs\ndescription: A grilling session.\ndisable-model-invocation: true\n---\n\nbody")

(def ^:private SAMPLE_MISSING_NAME
  "---\ndescription: This one has no name.\n---\n\nbody")

(def ^:private SAMPLE_MISSING_DESCRIPTION
  "---\nname: lonely\n---\n\nbody")

(def ^:private SAMPLE_BAD_YAML
  "---\nname: bad\ndescription: foo: bar:\n bad indent\n---\nbody")

(def ^:private SAMPLE_NO_FRONTMATTER
  "# Just markdown, no frontmatter at the top.\n")

(def ^:private SAMPLE_EMPTY_FRONTMATTER
  "---\n---\n\nbody")

(defdescribe scan-with-roots-test
  (it "finds repo skills and parses single-line descriptions"
    (with-two-tmps*
      (fn [root user-root]
        (write-skill! root "alpha" SAMPLE_SINGLE_LINE)
        (let [{:keys [loaded warnings]} (skills/scan-with-roots root user-root)]
          (expect (empty? warnings))
          (expect (= 1 (count loaded)))
          (let [s (first loaded)]
            (expect (= "alpha" (:name s)))
            (expect (= :repo (:source s)))
            (expect (str/starts-with? (:description s) "Alpha skill"))
            (expect (str/includes? (:body s) "# Alpha"))
            (expect (= {} (:extra s))))))))

  (it "parses folded scalar descriptions (caveman case)"
    (with-two-tmps*
      (fn [root user-root]
        (write-skill! root "caveman" SAMPLE_FOLDED)
        (let [{:keys [loaded]} (skills/scan-with-roots root user-root)
              s (first loaded)]
          (expect (= "caveman" (:name s)))
          (expect (str/includes? (:description s) "Multi-line folded"))
          (expect (str/includes? (:description s) "collapses newlines into spaces"))
          (expect (= "passthrough" (get-in s [:extra :other])))))))

  (it "preserves `disable-model-invocation` under :extra (grill-with-docs case)"
    (with-two-tmps*
      (fn [root user-root]
        (write-skill! root "grill-with-docs" SAMPLE_DISABLE_INVOCATION)
        (let [s (first (:loaded (skills/scan-with-roots root user-root)))]
          (expect (= true (get-in s [:extra :disable-model-invocation]))))))))

(defdescribe malformed-frontmatter-test
  (it "missing required `name` → drop with warning"
    (with-two-tmps*
      (fn [root user-root]
        (write-skill! root "no-name" SAMPLE_MISSING_NAME)
        (let [{:keys [loaded warnings]} (skills/scan-with-roots root user-root)]
          (expect (empty? loaded))
          (expect (= 1 (count warnings)))
          (let [w (first warnings)]
            (expect (= :skill-frontmatter (:source w)))
            (expect (str/includes? (:reason w) "name")))))))

  (it "missing required `description` → drop with warning"
    (with-two-tmps*
      (fn [root user-root]
        (write-skill! root "no-desc" SAMPLE_MISSING_DESCRIPTION)
        (let [{:keys [loaded warnings]} (skills/scan-with-roots root user-root)]
          (expect (empty? loaded))
          (expect (= 1 (count warnings)))
          (expect (str/includes? (:reason (first warnings)) "description"))))))

  (it "YAML parse error → drop with warning"
    (with-two-tmps*
      (fn [root user-root]
        (write-skill! root "bad-yaml" SAMPLE_BAD_YAML)
        (let [{:keys [loaded warnings]} (skills/scan-with-roots root user-root)]
          (expect (empty? loaded))
          (expect (= 1 (count warnings)))
          (expect (str/includes? (:reason (first warnings)) "YAML parse error"))))))

  (it "no frontmatter at all → drop with warning"
    (with-two-tmps*
      (fn [root user-root]
        (write-skill! root "no-front" SAMPLE_NO_FRONTMATTER)
        (let [{:keys [loaded warnings]} (skills/scan-with-roots root user-root)]
          (expect (empty? loaded))
          (expect (= 1 (count warnings)))
          (expect (str/includes? (:reason (first warnings)) "missing YAML frontmatter"))))))

  (it "empty frontmatter (`---\\n---\\n`) → drop with warning (missing required fields)"
    (with-two-tmps*
      (fn [root user-root]
        (write-skill! root "empty-front" SAMPLE_EMPTY_FRONTMATTER)
        (let [{:keys [loaded warnings]} (skills/scan-with-roots root user-root)]
          (expect (empty? loaded))
          (expect (= 1 (count warnings))))))))

(defdescribe collision-policy-test
  (it "repo wins silently when same `:name` exists in user-global"
    (with-two-tmps*
      (fn [repo-root user-root]
        (write-skill! repo-root "diagnose"
          "---\nname: diagnose\ndescription: REPO version of diagnose.\n---\n\nrepo body")
        (write-skill! user-root "diagnose"
          "---\nname: diagnose\ndescription: USER-GLOBAL version (should be hidden).\n---\n\nuser body")
        (let [{:keys [loaded warnings]} (skills/scan-with-roots repo-root user-root)]
          (expect (empty? warnings))
          (expect (= 1 (count loaded)))
          (let [s (first loaded)]
            (expect (= "diagnose" (:name s)))
            (expect (= :repo (:source s)))
            (expect (str/includes? (:description s) "REPO version"))))))))

(defdescribe alphabetical-order-test
  (it "merged catalog sorted alphabetically by :name"
    (with-two-tmps*
      (fn [repo-root user-root]
        (write-skill! repo-root "zebra"
          "---\nname: zebra\ndescription: Last alphabetically.\n---\n")
        (write-skill! repo-root "apple"
          "---\nname: apple\ndescription: First alphabetically.\n---\n")
        (write-skill! user-root "mango"
          "---\nname: mango\ndescription: Middle, from user-global.\n---\n")
        (let [{:keys [loaded]} (skills/scan-with-roots repo-root user-root)]
          (expect (= ["apple" "mango" "zebra"] (mapv :name loaded))))))))

(defdescribe scan-with-roots-empty-cases
  (it "no .agents/skills/ at all → empty catalog, no warnings"
    (with-two-tmps*
      (fn [repo-root user-root]
        (let [{:keys [loaded warnings]} (skills/scan-with-roots repo-root user-root)]
          (expect (empty? loaded))
          (expect (empty? warnings))))))

  (it "empty .agents/skills/ dir → empty catalog, no warnings"
    (with-two-tmps*
      (fn [repo-root user-root]
        (.mkdirs (java.io.File. repo-root ".agents/skills"))
        (let [{:keys [loaded warnings]} (skills/scan-with-roots repo-root user-root)]
          (expect (empty? loaded))
          (expect (empty? warnings)))))))

(defdescribe lookup-shape-test
  (it "lookup uses repo skills from active workspace root instead of JVM cwd"
    (with-tmp* (fn [root]
                 (write-skill! root "ws-only"
                   "---\nname: ws-only\ndescription: Workspace-only skill. Use when workspace root is bound.\n---\n\nbody")
                 (binding [workspace-context/*workspace-root* (.getCanonicalPath root)]
                   (skills/reload!)
                   (let [s (skills/lookup "ws-only")]
                     (expect (true? (:found? s)))
                     (expect (= :repo (:source s)))
                     (expect (str/includes? (:path s) "ws-only"))))
                 (binding [workspace-context/*workspace-root* nil]
                   (skills/reload!)))))

  (it "lookup returns map with :found? flag — present case"
    ;; Use the live cache against the actual repo; we know `caveman`
    ;; is one of the repo's skills.
    (let [s (skills/lookup "caveman")]
      (expect (true? (:found? s)))
      (expect (= "caveman" (:name s)))
      (expect (string? (:description s)))))

  (it "lookup returns {:found? false :name ...} for unknown name"
    (let [s (skills/lookup "this-skill-does-not-exist-anywhere")]
      (expect (false? (:found? s)))
      (expect (= "this-skill-does-not-exist-anywhere" (:name s))))))
