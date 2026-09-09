(ns com.blockether.vis.internal.foundation.tool-errors-test
  "Public tool refusals stay short, actionable and distinct from Python failures."
  (:require [clojure.string :as str]
            [com.blockether.vis.internal.extension.core :as extension]
            [com.blockether.vis.internal.foundation.core]
            [com.blockether.vis.internal.python.env :as ep]
            [com.blockether.vis.test-python-context :as tpc]
            [lazytest.experimental.interfaces.clojure-test :refer [deftest is testing]]))

(deftest compact-tool-refusals-test
  (tpc/with-own
    [ctx (extension/builtin-sandbox-bindings (constantly {}))]
    (doseq [[code expected]
            [["cat({})" "cat: use cat(path, start?, end?); not an options map."]
             ["patch({})" "patch: wrong number of arguments; see doc(\"patch\")."]
             ["patch({}, [])" "patch: use patch(path, edits); see doc(\"patch\") for edit keys."]
             ["repl_start('clojure', 'x')"
              "REPL: options must be a map; use {'cwd': ...}, {'id': ...} or {'port': ...}."]
             ["grep({'query': 'x', 'paths': ['resources'], 'wat': True})"
              "grep: unknown keys: wat. See doc(\"grep\")."]
             ["await shell('')" "shell: command required; use shell(command)."]
             ["await _shell_logs('missing-handle')"
              "shell: unknown id 'missing-handle'; use the handle returned by shell()."]
             ["await draft_approve('test')"
              "draft_approve(): not in a draft; use draft_create(\"name\") first."]
             ["await council.read()"
              "Council requires a persisted session with a project or workspace"]]]
      (testing code
        (let [out (ep/run-python-block ctx (str code "\nprint('unreachable')") "t1/i1")]
          (is (= expected (get-in out [:error :message])))
          (is (= :python/host (get-in out [:error :data :phase])))
          (is (not (str/includes? (str (:stdout out)) "unreachable"))))))
    (doseq [tool ["cat" "patch"]]
      (testing (str tool " missing file")
        (let [code (str tool "('resources/__vis_missing_file__'" (when (= tool "patch") ", []") ")")
              out (ep/run-python-block ctx code "t1/i1")
              message (get-in out [:error :message])]

          (is (= :python/host (get-in out [:error :data :phase])))
          (is (str/includes? message "__vis_missing_file__"))
          (is (str/ends-with? message "; use grep to find the path."))
          (is (< (count message) 110)))))
    (doseq [tool ["format_code" "lint_code" "run_tests" "repl_eval"]]
      (testing (str tool " unknown language")
        (let [out (ep/run-python-block ctx (str tool "('unknown', {})") "t1/i1")
              message (get-in out [:error :message])]

          (is (= :python/host (get-in out [:error :data :phase])))
          (is (str/starts-with? message (str tool ": no handler for 'unknown'; available: ")))
          (is (not (str/includes? message "-fn"))))))))
