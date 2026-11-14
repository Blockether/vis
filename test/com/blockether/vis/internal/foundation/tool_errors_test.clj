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
  (it "compact tool refusals"
      (tpc/with-own
        [ctx (extension/builtin-sandbox-bindings (constantly {}))]
        (doseq [[code expected]
                [["grep({'query': 'x', 'paths': ['resources'], 'wat': True})"
                  "grep: unknown keys: wat. See doc(\"grep\")."]
                 ["await shell('')" "shell: command required; use shell(command)."]
                 ["await _shell_logs('missing-handle')"
                  "shell: unknown id 'missing-handle'; use the handle returned by shell()."]
                 ["await council.read()"
                  "Council requires a persisted session with a project or workspace"]]]
          (let [out (ep/run-python-block ctx (str code "\nprint('unreachable')") "t1/i1")]
            (expect (= expected (get-in out [:error :message])) code)
            (expect (= :python/host (get-in out [:error :data :phase])) code)
            (expect (not (str/includes? (str (:stdout out)) "unreachable")) code))))))
