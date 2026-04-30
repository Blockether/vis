(ns com.blockether.vis.ext.foundation.environment.skills-test
  "Unit tests for skills frontmatter discovery, parsing, malformed
   handling, collision policy, and alphabetical ordering. Covers
   plan §6 + Q6 + Q7 + Q10."
  (:require
   [babashka.fs :as fs]
   [clojure.string :as str]
   [com.blockether.vis.ext.foundation.environment.skills :as skills]
   [lazytest.core :refer [defdescribe expect it]]))

(defmacro ^:private with-tmp [sym & body]
  `(let [tmp# (fs/create-temp-dir {:prefix "vis-skills-test-"})
         ~sym (.toFile tmp#)]
     (try ~@body
       (finally (fs/delete-tree tmp#)))))

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
    (with-tmp root
      (write-skill! root "alpha" SAMPLE_SINGLE_LINE)
      (with-tmp user-root
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
    (with-tmp root
      (write-skill! root "caveman" SAMPLE_FOLDED)
      (with-tmp user-root
        (let [{:keys [loaded]} (skills/scan-with-roots root user-root)
              s (first loaded)]
          (expect (= "caveman" (:name s)))
          ;; YAML folded scalar collapses single newlines into spaces.
          (expect (str/includes? (:description s) "Multi-line folded"))
          (expect (str/includes? (:description s) "collapses newlines into spaces"))
          ;; Non-required field passes through under :extra.
          (expect (= "passthrough" (get-in s [:extra :other])))))))

  (it "preserves `disable-model-invocation` under :extra (grill-with-docs case)"
    (with-tmp root
      (write-skill! root "grill-with-docs" SAMPLE_DISABLE_INVOCATION)
      (with-tmp user-root
        (let [s (first (:loaded (skills/scan-with-roots root user-root)))]
          (expect (= true (get-in s [:extra :disable-model-invocation]))))))))

(defdescribe malformed-frontmatter-test
  (it "missing required `name` → drop with warning"
    (with-tmp root
      (write-skill! root "no-name" SAMPLE_MISSING_NAME)
      (with-tmp user-root
        (let [{:keys [loaded warnings]} (skills/scan-with-roots root user-root)]
          (expect (empty? loaded))
          (expect (= 1 (count warnings)))
          (let [w (first warnings)]
            (expect (= :skill-frontmatter (:source w)))
            (expect (str/includes? (:reason w) "name")))))))

  (it "missing required `description` → drop with warning"
    (with-tmp root
      (write-skill! root "no-desc" SAMPLE_MISSING_DESCRIPTION)
      (with-tmp user-root
        (let [{:keys [loaded warnings]} (skills/scan-with-roots root user-root)]
          (expect (empty? loaded))
          (expect (= 1 (count warnings)))
          (expect (str/includes? (:reason (first warnings)) "description"))))))

  (it "YAML parse error → drop with warning"
    (with-tmp root
      (write-skill! root "bad-yaml" SAMPLE_BAD_YAML)
      (with-tmp user-root
        (let [{:keys [loaded warnings]} (skills/scan-with-roots root user-root)]
          (expect (empty? loaded))
          (expect (= 1 (count warnings)))
          (expect (str/includes? (:reason (first warnings)) "YAML parse error"))))))

  (it "no frontmatter at all → drop with warning"
    (with-tmp root
      (write-skill! root "no-front" SAMPLE_NO_FRONTMATTER)
      (with-tmp user-root
        (let [{:keys [loaded warnings]} (skills/scan-with-roots root user-root)]
          (expect (empty? loaded))
          (expect (= 1 (count warnings)))
          (expect (str/includes? (:reason (first warnings)) "missing YAML frontmatter"))))))

  (it "empty frontmatter (`---\\n---\\n`) → drop with warning (missing required fields)"
    (with-tmp root
      (write-skill! root "empty-front" SAMPLE_EMPTY_FRONTMATTER)
      (with-tmp user-root
        (let [{:keys [loaded warnings]} (skills/scan-with-roots root user-root)]
          (expect (empty? loaded))
          (expect (= 1 (count warnings))))))))

(defdescribe collision-policy-test
  (it "repo wins silently when same `:name` exists in user-global"
    (with-tmp repo-root
      (write-skill! repo-root "diagnose"
        "---\nname: diagnose\ndescription: REPO version of diagnose.\n---\n\nrepo body")
      (with-tmp user-root
        (write-skill! user-root "diagnose"
          "---\nname: diagnose\ndescription: USER-GLOBAL version (should be hidden).\n---\n\nuser body")
        (let [{:keys [loaded warnings]} (skills/scan-with-roots repo-root user-root)]
          ;; Plan Q6: collision is silent — no warning.
          (expect (empty? warnings))
          ;; Exactly one entry, sourced from :repo.
          (expect (= 1 (count loaded)))
          (let [s (first loaded)]
            (expect (= "diagnose" (:name s)))
            (expect (= :repo (:source s)))
            (expect (str/includes? (:description s) "REPO version"))))))))

(defdescribe alphabetical-order-test
  (it "merged catalog sorted alphabetically by :name"
    (with-tmp repo-root
      (write-skill! repo-root "zebra"
        "---\nname: zebra\ndescription: Last alphabetically.\n---\n")
      (write-skill! repo-root "apple"
        "---\nname: apple\ndescription: First alphabetically.\n---\n")
      (with-tmp user-root
        (write-skill! user-root "mango"
          "---\nname: mango\ndescription: Middle, from user-global.\n---\n")
        (let [{:keys [loaded]} (skills/scan-with-roots repo-root user-root)]
          (expect (= ["apple" "mango" "zebra"] (mapv :name loaded))))))))

(defdescribe scan-with-roots-empty-cases
  (it "no .agents/skills/ at all → empty catalog, no warnings"
    (with-tmp repo-root
      (with-tmp user-root
        (let [{:keys [loaded warnings]} (skills/scan-with-roots repo-root user-root)]
          (expect (empty? loaded))
          (expect (empty? warnings))))))

  (it "empty .agents/skills/ dir → empty catalog, no warnings"
    (with-tmp repo-root
      (.mkdirs (java.io.File. repo-root ".agents/skills"))
      (with-tmp user-root
        (let [{:keys [loaded warnings]} (skills/scan-with-roots repo-root user-root)]
          (expect (empty? loaded))
          (expect (empty? warnings)))))))

(defdescribe lookup-shape-test
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
