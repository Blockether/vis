(ns com.blockether.vis.commit-convention-test
  "Every commit after `enforced-since` speaks the minimal conventional format:
   one `type(scope): imperative summary` line, a body of at most six WHY
   lines, and a Vis-Session trailer naming the session that made it. History
   before the cutoff is untouched; merge commits and bot authors are exempt."
  (:require [clojure.java.io :as io]
            [clojure.java.shell :as shell]
            [clojure.string :as str]
            [lazytest.core :refer [defdescribe expect it]]))

(def ^:private enforced-since
  "Committer-date cutoff: the first commit this convention covers. Pushed history
   is immutable, so when a commit that broke a rule is already on `main` the
   cutoff MOVES past it rather than staying red forever — the hook keeps new work
   honest, and this test keeps the hook from being skipped. It last moved past
   `0381c6b25`, whose subject ran to 73 characters."
  "2026-09-02T01:59:03+00:00")

(def ^:private max-subject-chars 72)

(def ^:private max-body-lines 6)

(def ^:private subject-re
  "`type`, an optional one-word `scope`, an optional breaking `!`, then an
   imperative summary."
  #"(feat|fix|docs|refactor|perf|test|build|ci|chore|revert)(\([a-z0-9./-]+\))?!?: \S.*")

(def ^:private session-trailer-re
  "The bare session id. The `Vis-Session:` key already says what the value
   names, so the clipboard marker (`header.clj`) has no work to do here."
  #"Vis-Session: [0-9a-f]{8}(-[0-9a-f]{4}){3}-[0-9a-f]{12}")

(def ^:private bot-author-re
  "Automation, not a person: GitHub app authors, and the deployer identity
   `.github/workflows/release.yml` commits the release notes as. A machine has no
   conversation to name in a trailer, so its messages are not ours to police."
  #"(?i)\[bot\]|^contact@blockether\.com$")

(def ^:private sample-trailer "Vis-Session: 123e4567-e89b-12d3-a456-426614174000")

(defn- git
  [& args]
  (let [{:keys [exit out err]} (apply shell/sh "git" args)]
    (when (pos? (int exit)) (throw (ex-info (str "git failed: " err) {:args (vec args)})))
    out))

(defn- ours?
  "A row this convention covers. A bot has no conversation to name in a trailer,
   and the shallow boundary is not a commit anyone wrote: GitHub builds the pull
   request's own merge commit and `actions/checkout` fetches that one alone, so
   git hides its parents and `--no-merges` never sees the merge it is."
  [[_ email parents _]]
  (and (not (re-find bot-author-re (str email))) (not (str/blank? parents))))

(defn- covered-commits
  "[hash email message] for every commit after the cutoff that is ours to police."
  []
  (->> (str/split (git "log" (str "--after=" enforced-since)
                       "--no-merges" "--format=%h%x1f%ae%x1f%P%x1f%B%x1e")
                  #"\x1e")
       (map str/trim)
       (remove str/blank?)
       (map #(str/split % #"\x1f" 4))
       (filter ours?)
       (map (fn [[hash email _ message]]
              [hash email message]))))

(defn- problems
  "Every rule this one commit message breaks, as `hash — rule` lines."
  [[hash _email message]]
  (let [lines
        (str/split-lines (str/trim message))

        subject
        (first lines)

        non-blank
        (remove str/blank? (rest lines))

        trailer
        (str (last non-blank))

        body
        (butlast non-blank)]

    (->
      []
      (cond->
        (not (re-matches subject-re subject))
        (conj (str hash " — subject must be `type(scope): imperative summary`"))

        (> (count subject) (long max-subject-chars))
        (conj (str hash " — subject over " max-subject-chars " chars"))

        (str/ends-with? subject ".")
        (conj (str hash " — subject must not end with a period"))

        (> (count body) (long max-body-lines))
        (conj
          (str hash " — body over " max-body-lines " lines: keep only the WHY, the diff says WHAT"))

        (not (re-matches session-trailer-re trailer))
        (conj (str hash " — last line must be `Vis-Session: <uuid>`"))))))

(def ^:private rule-cases
  "A message and the fragments its problems must mention."
  [[(str "fix(gateway): refuse an unknown speech engine with 400\n\n"
         "The route answered before the engine registry was consulted, so a\n"
         "typo in the id came back 501.\n\n" sample-trailer) []]
   [(str "docs: explain the speech engine registry\n\n" sample-trailer) []]
   [(str "feat(core)!: speech is an engine registry, not a hard-coded call\n\n" sample-trailer) []]
   [(str "A voice belongs to the machine, and the app can bring one\n\n"
         "Essay paragraph the diff already tells.\n\n"
         sample-trailer) ["subject must be"]]
   [(str "fix(companion): keep the heading row when the notch pushes it down\n\n"
         "One.\nTwo.\nThree.\nFour.\nFive.\nSix.\nSeven.\n\n"
         sample-trailer) ["body over"]] ["chore: bump deps\n" ["Vis-Session"]]
   [(str "chore: bump deps\n\n" "Vis-Session: vis_session_id#123e4567-e89b-12d3-a456-426614174000")
    ["Vis-Session"]] [(str "feat(api): add the voices route.\n\n" sample-trailer) ["period"]]
   [(str
      "refactor(tui): a subject that runs well past seventy-two characters and proves the length cap fires\n\n"
      sample-trailer) ["chars"]]])

(defdescribe
  commit-convention-test
  (it "accepts the minimal format and rejects each old vice by name"
      (doseq [[message expected-fragments] rule-cases]
        (let [found (problems ["abc1234" "dev@example.com" message])]
          (expect (= (count expected-fragments) (count found))
                  (str (pr-str message) " => " (pr-str found)))
          (doseq [fragment expected-fragments]
            (expect (some #(str/includes? % fragment) found)
                    (str fragment " not reported for " (pr-str message)))))))
  (it "leaves the bots and the shallow boundary out of the covered rows"
      (let [rows [["abc1234" "dev@example.com" "def5678" "fix(cli): ok"]
                  ["bbb2222" "dependabot[bot]@users.noreply.github.com" "def5678" "chore: bump"]
                  ["ccc3333" "dev@example.com" "" "Merge aca874e into db10c81"]]]
        (expect (= ["abc1234"] (mapv first (filter ours? rows))))))
  (it "every commit after the cutoff carries the line, the cap and the trailer"
      (let [found (if (.exists (io/file "deps.edn")) (mapcat problems (covered-commits)) [])]
        (expect (empty? found) (str "commits outside the convention:\n" (str/join "\n" found))))))
