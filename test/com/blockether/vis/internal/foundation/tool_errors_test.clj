(ns com.blockether.vis.internal.foundation.tool-errors-test
  "Public tool refusals stay short, actionable and distinct from Python failures."
  (:require [clojure.string :as str]
            [com.blockether.vis.internal.extension.core :as extension]
            [com.blockether.vis.internal.foundation.core]
            [com.blockether.vis.internal.python.env :as ep]
            [com.blockether.vis.test-python-context :as tpc]
            [lazytest.core :refer [defdescribe expect it]]))

(defdescribe
  compact-tool-refusals-test
  (it
    "compact tool refusals"
    (tpc/with-own
      [ctx (extension/builtin-sandbox-bindings (constantly {}))]
      (doseq [[code expected]
              [["cat({})" "cat: use cat(path, start?, end?); not an options map."]
               ["patch({})" "patch: wrong number of arguments; see doc(\"patch\")."]
               ["patch({}, [])" "patch: use patch(path, edits); see doc(\"patch\") for edit keys."]
               ["grep({'query': 'x', 'paths': ['resources'], 'wat': True})"
                "grep: unknown keys: wat. See doc(\"grep\")."]
               ["await shell('')" "shell: command required; use shell(command)."]
               ["await _shell_logs('missing-handle')"
                "shell: unknown id 'missing-handle'; use the handle returned by shell()."]
               ["await draft_approve('test')"
                "draft_approve(): not in a draft; use draft_create(\"name\") first."]
               ["await council.read()"
                "Council requires a persisted session with a project or workspace"]]]
        (let [out (ep/run-python-block ctx (str code "\nprint('unreachable')") "t1/i1")]
          (expect (= expected (get-in out [:error :message])) code)
          (expect (= :python/host (get-in out [:error :data :phase])) code)
          (expect (not (str/includes? (str (:stdout out)) "unreachable")) code)))
      ;; CI checkout paths differ in length across operating systems.
      (doseq [missing-path
              ["resources/__vis_missing_file__"
               (str "resources/" (apply str (repeat 160 "x")) "/__vis_missing_file__")]

              tool
              ["cat" "patch"]]

        (let [code
              (str tool "('" missing-path "'" (when (= tool "patch") ", []") ")")

              out
              (ep/run-python-block ctx code "t1/i1")

              message
              (get-in out [:error :message])

              reported-path
              (second (re-matches #"File not found: ([^\r\n]+); use grep to find the path\."
                                  message))]

          (expect (= :python/host (get-in out [:error :data :phase]))
                  (str tool " missing file: " missing-path))
          (expect (and reported-path (str/ends-with? reported-path missing-path))
                  (str tool " missing file: " missing-path "\n" message)))))))
