(ns com.blockether.vis.internal.python.sdk-bootstrap-test
  "Issue #253: the host loader uses the real pinned runtime, not the SDK wheel."
  (:require [charred.api :as json]
            [clojure.java.io :as io]
            [com.blockether.vis.internal.python.worker :as worker]
            [lazytest.core :refer [defdescribe expect it]]))

(defn- check-bootstrap!
  [call]
  (let [key
        (worker/extension-worker-key (str "sdk-bootstrap-" (random-uuid)) nil)

        session
        (str "sdk-bootstrap-" (random-uuid))

        source
        (slurp (io/resource "blockether/vis/extension.py"))

        fixture
        (slurp (io/file "test/resources/sdk_bootstrap.py"))

        sdk-root
        (.getCanonicalPath (io/file "packages/vis-agent/src"))]

    (try (worker/trust! key session true)
         (worker/exec!
           key
           session
           (str "import sys, json
sys.path.insert(0, json.loads("
                (pr-str (json/write-json-str sdk-root))
                "))
"
                "import blockether.vis.extension
sys.modules.pop('blockether.vis._outside', None)
"
                "SOURCE = json.loads("
                (pr-str (json/write-json-str source))
                ")
"
                fixture
                "
"
                call
                "
bootstrap_checked = True
"))
         (expect (= "True" (worker/eval-str key session "bootstrap_checked")))
         (finally (worker/stop-worker! key)))))

(defdescribe sdk-bootstrap-test
             (it "replaces the installed SDK and preserves its packages"
                 (check-bootstrap!
                   "test_bundled_module_replaces_installed_sdk_but_preserves_packages(SOURCE)"))
             (it "keeps an explicit host out of the sandbox SDK"
                 (check-bootstrap! "test_explicit_extension_host_is_not_reused_by_sandbox(SOURCE)"))
             (it "shares a sandbox module only for the same source"
                 (check-bootstrap! "test_sandbox_installs_share_one_module_for_one_source(SOURCE)"))
             (it "restores the previous module after failed source"
                 (check-bootstrap! "test_failed_load_restores_previous_module(SOURCE, True)"))
             (it "leaves no module after failed source without a previous module"
                 (check-bootstrap! "test_failed_load_restores_previous_module(SOURCE, False)")))
