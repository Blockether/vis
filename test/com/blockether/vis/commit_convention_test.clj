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
  "Committer-date cutoff: the first commit this convention covers. It moved
   the day the trailer dropped its `vis_session_id#` marker: commits before
   it speak the marked form, and one format is enforced here, never two."
  "2026-08-15T20:00:00+00:00")

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

(def ^:private bot-author-re #"(?i)\[bot\]")

(def ^:private sample-trailer "Vis-Session: 123e4567-e89b-12d3-a456-426614174000")

(defn- git
  [& args]
  (let [{:keys [exit out err]} (apply shell/sh "git" args)]
    (when (pos? (int exit)) (throw (ex-info (str "git failed: " err) {:args (vec args)})))
    out))

(defn- covered-commits
  "[hash email message] for every commit after the cutoff; merges and bots
   are not ours to police."
  []
  (->> (str/split (git "log" (str "--after=" enforced-since)
                       "--no-merges" "--format=%h%x1f%ae%x1f%B%x1e")
                  #"\x1e")
       (map str/trim)
       (remove str/blank?)
       (map #(str/split % #"\x1f" 3))
       (remove (fn [[_ email _]]
                 (re-find bot-author-re (str email))))))

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
  (it "every commit after the cutoff carries the line, the cap and the trailer"
      (let [found (if (.exists (io/file "deps.edn")) (mapcat problems (covered-commits)) [])]
        (expect (empty? found) (str "commits outside the convention:\n" (str/join "\n" found))))))
