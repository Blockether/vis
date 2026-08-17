(ns com.blockether.vis.internal.foundation.harness.discovery-test
  (:require [clojure.java.io :as io]
            [com.blockether.vis.internal.foundation.harness.discovery :as d]
            [com.blockether.vis.internal.workspace :as workspace]
            [lazytest.core :refer [defdescribe it expect]])
  (:import [java.nio.file Files]
           [java.nio.file.attribute FileAttribute]))

(def ^:private agent-md
  (str "---\n" "name: code-reviewer\n"
       "description: Elite reviewer. Masters static analysis\n"
       "  and security scanning. Use PROACTIVELY.\n"
       "model: opus\n" "tools: Read, Grep\n"
       "---\n\n" "You are an elite code review expert.\n\n## Purpose\nReview code.\n"))

(defdescribe
  parse-frontmatter-test
  (it "splits the --- fenced head from the body"
      (let [{:keys [meta body]} (d/parse-frontmatter agent-md)]
        (expect (= "code-reviewer" (:name meta)))
        (expect (= "opus" (:model meta)))
        (expect (= "Read, Grep" (:tools meta)))
        (expect (re-find #"elite code review expert" body))
        (expect (not (re-find #"(?m)^---" body)))))
  (it "folds a continuation line into the previous value"
      (let [{:keys [meta]} (d/parse-frontmatter agent-md)]
        (expect (re-find #"static analysis and security scanning" (:description meta)))))
  (it "drops a YAML block-scalar indicator instead of rendering it"
      (let
        [{:keys [meta]} (d/parse-frontmatter
                          (str "---\n"
                               "name: ponytail\n" "description: >\n"
                               "  Forces the laziest solution that actually works,\n"
                               "  simplest and shortest.\n"
                               "---\n\n" "body\n"))]
        (expect (= "Forces the laziest solution that actually works, simplest and shortest."
                   (:description meta)))))
  (it "unwraps a quoted scalar after the fold is joined"
      (let
        [{:keys [meta]} (d/parse-frontmatter (str "---\n" "name: triage\n"
                                                  "description: \"Triage issues ONE AT A TIME,\n"
                                                  "  reproduction-first.\"\n"
                                                  "---\n\n" "body\n"))]
        (expect (= "Triage issues ONE AT A TIME, reproduction-first." (:description meta)))))
  (it "no frontmatter → empty meta, whole content is the body"
      (let [{:keys [meta body]} (d/parse-frontmatter "# Just a doc\nhello")]
        (expect (= {} meta))
        (expect (= "# Just a doc\nhello" body)))))

(defdescribe
  parse-agent-test
  (it "builds an agent entry from frontmatter + body"
      (let [a (d/parse-agent agent-md {:name-default "fallback" :tool :claude :path "/x.md"})]
        (expect (= "code-reviewer" (:name a)))
        (expect (= "opus" (:model a)))
        (expect (re-find #"Elite reviewer" (:description a)))
        (expect (re-find #"elite code review expert" (:body a)))
        (expect (= :claude (:tool a)))
        (expect (= "/x.md" (:path a)))))
  (it "falls back to the filename stem when frontmatter has no name"
      (let [a (d/parse-agent "no frontmatter here" {:name-default "my-agent"})]
        (expect (= "my-agent" (:name a)))
        (expect (= "" (:description a)))
        (expect (nil? (:model a)))))
  (it "nil when there is no usable name at all"
      (expect (nil? (d/parse-agent "body only" {:name-default "  "})))))

(defdescribe
  parse-skill-meta-test
  (it "builds a skill entry (no resources yet) from SKILL.md"
      (let
        [s (d/parse-skill-meta
             "---\nname: setup-pre-commit\ndescription: Set up hooks.\n---\n# Setup\nsteps"
             {:name-default "dir-name" :tool :claude :dir "/skills/x" :path "/skills/x/SKILL.md"})]
        (expect (= "setup-pre-commit" (:name s)))
        (expect (= "Set up hooks." (:description s)))
        (expect (re-find #"# Setup" (:body s)))
        (expect (= [] (:resources s)))))
  (it "falls back to the skill directory name"
      (expect (= "my-skill" (:name (d/parse-skill-meta "no fm" {:name-default "my-skill"}))))))

(defdescribe dedup-by-name-test
             (it "keeps the FIRST occurrence of each name (precedence = order)"
                 (let
                   [out (d/dedup-by-name [{:name "a" :tool :p} {:name "b"} {:name "a" :tool :u}])]
                   (expect (= ["a" "b"] (mapv :name out)))
                   (expect (= :p (:tool (first out)))))))

(defdescribe
  cross-tool-source-test
  (it "agent + skill sources span vis-local + every supported harness"
      (expect (= d/known-tools (set (map first d/agent-sources))))
      (expect (= d/known-tools (set (map first d/skill-sources)))))
  (it "vis project-local .vis/skills is the FIRST (highest-precedence) skill source"
      (let [[tool _kind & parts] (first d/skill-sources)]
        (expect (= :vis tool))
        (expect (= [".vis" "skills"] parts))))
  (it "skill sources include pi (~/.pi/agent/skills) and the agents standard"
      (expect (some (fn [[tool _ & parts]]
                      (and (= :pi tool) (= [".pi" "agent" "skills"] parts)))
                    d/skill-sources))
      (expect (some (fn [[tool _ & parts]]
                      (and (= :agents tool) (= [".agents" "skills"] parts)))
                    d/skill-sources)))
  (it "opencode sources include SPEL/generated plural agents and skills layouts"
      (expect (some (fn [[tool _ & parts]]
                      (and (= :opencode tool) (= [".opencode" "agents"] parts)))
                    d/agent-sources))
      (expect (some (fn [[tool _ & parts]]
                      (and (= :opencode tool) (= [".opencode" "skills"] parts)))
                    d/skill-sources)))
  (it "resolve-source tags an existing dir with its tool and drops a missing one"
      (let [home (System/getProperty "user.home")]
        (expect (= [:opencode] (map first ((deref #'d/resolve-source) [:opencode :rel home]))))
        (expect (empty? ((deref #'d/resolve-source) [:claude :rel "/no/such/dir/xyz-zzz"])))))
  (it "resolve-source expands a :home spec against the user home"
      (let [pairs ((deref #'d/resolve-source) [:pi :home "."])] ; ~/. always exists
        (expect (= [:pi] (map first pairs)))))
  (it "every discovered entry is tagged with a known harness tool"
      (let [tools (set (map :tool (concat (d/discover-agents) (d/discover-skills))))]
        (expect (every? d/known-tools tools)))))

(defdescribe
  skill-resources-test
  ;; `skill-resources` is what a resolved skill dir (e.g. .vis/skills/<name>)
  ;; hands the model: every bundled file EXCEPT SKILL.md, as `/`-relative paths.
  (it "lists bundled resource paths recursively, excluding SKILL.md, unix-slashed"
      (let [root (.toFile (Files/createTempDirectory "vis-skill" (make-array FileAttribute 0)))]
        (try (spit (io/file root "SKILL.md") "---\nname: demo\ndescription: d\n---\nbody")
             (io/make-parents (io/file root "scripts" "run.sh"))
             (spit (io/file root "scripts" "run.sh") "echo hi")
             (spit (io/file root "template.json") "{}")
             (let [rs ((deref #'d/skill-resources) root)]
               (expect (= ["scripts/run.sh" "template.json"] rs))
               (expect (not-any? #(= "SKILL.md" %) rs)))
             (finally (run! #(.delete ^java.io.File %) (reverse (file-seq root)))))))
  ;; Regression: the bundled-resource walk was Clojure `file-seq` instead of the
  ;; Rust engine every other traversal uses, and an ignore-aware walk would drop
  ;; exactly the files a nested project's (routinely gitignored) skill ships.
  (it "walks hidden and repository-ignored bundled files too"
      (let
        [root (.toFile (Files/createTempDirectory "vis-skill-hidden" (make-array FileAttribute 0)))]
        (try (spit (io/file root "SKILL.md") "---\nname: demo\ndescription: d\n---\nbody")
             (spit (io/file root ".gitignore") "ignored/\n")
             (io/make-parents (io/file root "ignored" "asset.txt"))
             (spit (io/file root "ignored" "asset.txt") "x")
             (io/make-parents (io/file root "refs" "deep" "note.md"))
             (spit (io/file root "refs" "deep" "note.md") "n")
             (expect (= [".gitignore" "ignored/asset.txt" "refs/deep/note.md"]
                        ((deref #'d/skill-resources) root)))
             (finally (run! #(.delete ^java.io.File %) (reverse (file-seq root))))))))

(defdescribe
  opencode-spel-layout-discovery-test
  (it "discovers SPEL skills from .opencode/skills/<name>/SKILL.md"
      (let
        [root
         (.toFile (Files/createTempDirectory "vis-opencode-skill" (make-array FileAttribute 0)))

         skill-md
         (io/file root ".opencode" "skills" "spel" "SKILL.md")]

        (try (io/make-parents skill-md)
             (spit skill-md "---\nname: spel\ndescription: Browser automation\n---\nBODY")
             (with-redefs-fn {#'d/project-root (fn []
                                                 root)
                              #'d/skill-sources [[:opencode :rel ".opencode" "skills"]]}
               (fn []
                 (let
                   [skills
                    (d/discover-skills)

                    spel
                    (first (filter #(= "spel" (:name %)) skills))]

                   (expect (= "spel" (:name spel)))
                   (expect (= :opencode (:tool spel)))
                   (expect (re-find #"BODY" (:body spel))))))
             (finally (run! #(.delete ^java.io.File %) (reverse (file-seq root))))))))

(defdescribe
  command-discovery-test
  (it
    "command sources span the cross-harness command dirs (no vis-local row — vis commands are .vis/prompts)"
    (expect (= #{:claude :pi :agents :opencode} (set (map first d/command-sources)))))
  (it
    "discovers a claude command from .claude/commands/<name>.md with its $ARGUMENTS body preserved"
    (let
      [root
       (.toFile (Files/createTempDirectory "vis-cmd" (make-array FileAttribute 0)))

       cmd-md
       (io/file root ".claude" "commands" "review.md")]

      (try (io/make-parents cmd-md)
           (spit cmd-md "---\ndescription: Review a PR\n---\nReview the diff for $ARGUMENTS.")
           (with-redefs-fn {#'d/project-root (fn []
                                               root)
                            #'d/command-sources [[:claude :rel ".claude" "commands"]]}
             (fn []
               (let [c (first (filter #(= "review" (:name %)) (d/discover-commands)))]
                 (expect (= "review" (:name c)))
                 (expect (= :claude (:tool c)))
                 (expect (re-find #"Review a PR" (:description c)))
                 (expect (re-find #"\$ARGUMENTS" (:body c))))))
           (finally (run! #(.delete ^java.io.File %) (reverse (file-seq root))))))))

(defdescribe discovery-smoke-test
             ;; Environment-agnostic: the scan must NEVER throw and always returns a
             ;; vector, whatever is (or isn't) on disk in ~/.claude.
             (it "discover-agents returns a vector and never throws"
                 (expect (vector? (vec (d/discover-agents)))))
             (it "discover-skills returns a vector and never throws"
                 (expect (vector? (vec (d/discover-skills)))))
             (it "discover-commands returns a vector and never throws"
                 (expect (vector? (vec (d/discover-commands)))))
             (it "every discovered entry has a non-blank name"
                 (expect (every? #(seq (:name %))
                                 (concat (d/discover-agents) (d/discover-skills))))))

(defdescribe
  nested-project-discovery-test
  ;; Regression: a session rooted in a monorepo app only saw the gateway's launch-root
  ;; skills, and gitignored harness directories were incorrectly assumed undiscoverable.
  (it
    "walks explicit project sources to the nearest git root, nearest first, regardless of gitignore"
    (let
      [root
       (.toFile (Files/createTempDirectory "vis-nested-project" (make-array FileAttribute 0)))

       app
       (io/file root "apps" "companion")

       root-skill
       (io/file root ".agents" "skills" "inherited" "SKILL.md")

       root-shadow
       (io/file root ".agents" "skills" "layered" "SKILL.md")

       app-shadow
       (io/file app ".agents" "skills" "layered" "SKILL.md")]

      (try (.mkdirs (io/file root ".git"))
           (spit (io/file root ".gitignore") ".agents/\n")
           (doseq [f [root-skill root-shadow app-shadow]]
             (io/make-parents f))
           (spit root-skill "---\nname: inherited\ndescription: root\n---\nROOT")
           (spit root-shadow "---\nname: layered\ndescription: root\n---\nROOT-SHADOW")
           (spit app-shadow "---\nname: layered\ndescription: app\n---\nAPP-SHADOW")
           (binding [workspace/*workspace-root* (.getCanonicalPath app)]
             (with-redefs [d/skill-sources [[:agents :rel-walk ".agents" "skills"]]]
               (let
                 [skills (d/discover-skills)
                  by-name (into {} (map (juxt :name identity)) skills)]

                 (expect (= #{"inherited" "layered"} (set (keys by-name))))
                 (expect (= "app" (:description (by-name "layered"))))
                 (expect (.startsWith ^String (:path (by-name "layered"))
                                      (.getCanonicalPath app))))))
           (finally (run! #(.delete ^java.io.File %) (reverse (file-seq root)))))))
  (it "uses ancestor walking for every project-local harness source"
      (doseq [sources [d/agent-sources d/skill-sources d/command-sources]]
        (expect (not-any? #(= :rel (second %)) sources))))
  ;; Regression: a repository-root session could not see a skill owned by a nested app,
  ;; and moving the skill to the repository root made its relative instructions run there.
  (it
    "discovers gitignored descendant-project skills from the repository root and records their owner"
    (let
      [root
       (.toFile (Files/createTempDirectory "vis-root-skill-discovery" (make-array FileAttribute 0)))

       app
       (io/file root "apps" "companion")

       skill-md
       (io/file app ".agents" "skills" "app-design" "SKILL.md")]

      (try (.mkdirs (io/file root ".git"))
           (io/make-parents skill-md)
           (spit (io/file root ".gitignore") ".agents/\n")
           (spit skill-md "---\nname: app-design\ndescription: nested\n---\nAPP")
           (binding [workspace/*workspace-root* (.getCanonicalPath root)]
             (with-redefs [d/skill-sources [[:agents :rel-walk ".agents" "skills"]]]
               (let [skill (first (filter #(= "app-design" (:name %)) (d/discover-skills)))]
                 (expect (some? skill))
                 (expect (= (.getCanonicalPath app) (:project-root skill)))
                 (expect (= (.getCanonicalPath skill-md)
                            (.getCanonicalPath (io/file (:path skill))))))))
           (finally (run! #(.delete ^java.io.File %) (reverse (file-seq root)))))))
  (it "treats skill resources as opaque and ignores implementation-specific command metadata"
      (let
        [root
         (.toFile (Files/createTempDirectory "vis-skill-resources" (make-array FileAttribute 0)))

         skill-md
         (io/file root ".agents" "skills" "demo" "SKILL.md")

         metadata
         (io/file root ".agents" "skills" "demo" "scripts" "command-metadata.json")]

        (try (.mkdirs (io/file root ".git"))
             (io/make-parents skill-md)
             (io/make-parents metadata)
             (spit skill-md "---\nname: demo\ndescription: Demo\n---\nBODY")
             (spit metadata "{\"audit\":{\"description\":\"Audit it\"}}")
             (binding [workspace/*workspace-root* (.getCanonicalPath root)]
               (with-redefs [d/skill-sources [[:agents :rel-walk ".agents" "skills"]]]
                 (let [skill (first (d/discover-skills))]
                   (expect (nil? (:commands skill)))
                   (expect (some #{"scripts/command-metadata.json"} (:resources skill))))))
             (finally (run! #(.delete ^java.io.File %) (reverse (file-seq root))))))))

;; Regression: a skill dropped into `.agents/skills` while a session was running
;; stayed unknown until the process restarted — the cache kept answering the set it
;; had read when the session started.
(defdescribe
  skills-cache-revalidates-test
  (it "sees a SKILL.md that appeared after the cache was warm, with no explicit reload"
      (let
        [root
         (.toFile (Files/createTempDirectory "vis-skill-midsession" (make-array FileAttribute 0)))

         early
         (io/file root ".agents" "skills" "already-here" "SKILL.md")

         late
         (io/file root ".agents" "skills" "arrived-later" "SKILL.md")]

        (try (.mkdirs (io/file root ".git"))
             (io/make-parents early)
             (spit early "---\nname: already-here\ndescription: First\n---\nFIRST")
             (binding [workspace/*workspace-root* (.getCanonicalPath root)]
               (with-redefs [d/skill-sources [[:agents :rel-walk ".agents" "skills"]]]
                 (let
                   [warm (set (map :name (d/skills)))
                    generation (d/generation)]

                   (expect (= #{"already-here"} warm))
                   (io/make-parents late)
                   (spit late "---\nname: arrived-later\ndescription: Second\n---\nSECOND")
                   (expect (= #{"already-here" "arrived-later"} (set (map :name (d/skills)))))
                   (expect (< (long generation) (long (d/generation)))))))
             (finally (run! #(.delete ^java.io.File %) (reverse (file-seq root))))))))
