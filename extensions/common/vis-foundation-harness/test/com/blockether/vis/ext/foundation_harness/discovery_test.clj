(ns com.blockether.vis.ext.foundation-harness.discovery-test
  (:require [clojure.java.io :as io]
            [com.blockether.vis.ext.foundation-harness.discovery :as d]
            [lazytest.core :refer [defdescribe it expect]])
  (:import [java.nio.file Files]
           [java.nio.file.attribute FileAttribute]))

(def ^:private agent-md
  (str "---\n" "name: code-reviewer\n"
       "description: Elite reviewer. Masters static analysis\n"
       "  and security scanning. Use PROACTIVELY.\n"
       "model: opus\n" "tools: Read, Grep\n"
       "---\n\n" "You are an elite code review expert.\n\n## Purpose\nReview code.\n"))

(defdescribe parse-frontmatter-test
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
             (it "no frontmatter → empty meta, whole content is the body"
                 (let [{:keys [meta body]} (d/parse-frontmatter "# Just a doc\nhello")]
                   (expect (= {} meta))
                   (expect (= "# Just a doc\nhello" body)))))

(defdescribe parse-agent-test
             (it "builds an agent entry from frontmatter + body"
                 (let [a (d/parse-agent agent-md
                                        {:name-default "fallback" :tool :claude :path "/x.md"})]
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
      (let [s
            (d/parse-skill-meta
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
                 (let [out (d/dedup-by-name [{:name "a" :tool :p} {:name "b"}
                                             {:name "a" :tool :u}])]
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
             (finally (run! #(.delete ^java.io.File %) (reverse (file-seq root))))))))

(defdescribe discovery-smoke-test
             ;; Environment-agnostic: the scan must NEVER throw and always returns a
             ;; vector, whatever is (or isn't) on disk in ~/.claude.
             (it "discover-agents returns a vector and never throws"
                 (expect (vector? (vec (d/discover-agents)))))
             (it "discover-skills returns a vector and never throws"
                 (expect (vector? (vec (d/discover-skills)))))
             (it "every discovered entry has a non-blank name"
                 (expect (every? #(seq (:name %))
                                 (concat (d/discover-agents) (d/discover-skills))))))
