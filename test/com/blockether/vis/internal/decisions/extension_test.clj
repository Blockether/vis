(ns com.blockether.vis.internal.decisions.extension-test
  "Real vis-decisions registration and immutable results across the Python host boundary."
  (:require [clojure.string :as str]
            [com.blockether.vis.internal.extension.core :as extension]
            [com.blockether.vis.internal.loop.environment :as loop-env]
            [com.blockether.vis.internal.persistance.core :as ps]
            [com.blockether.vis.internal.python.env :as ep]
            [com.blockether.vis.internal.python.extensions :as pyx]
            [com.blockether.vis.internal.workspace.core :as workspace]
            [lazytest.core :refer [defdescribe expect it]]))

(defdescribe
  unified-extension-host-boundary-test
  (it
    "registers every decision tool and returns a verified immutable checkpoint record"
    ;; Opt in with a prepared checkout and a verified checkpoint; never download model weights here.
    (when-let [source (System/getProperty "vis.test.decisions.extension.dir")]
      (let [checkpoint (System/getProperty "vis.test.laya.training.dir")
            store (ps/db-create-connection! :memory)]

        (expect (some? checkpoint))
        (binding [extension/*current-environment* {:db-info store :workspace/root source}
                  workspace/*workspace-root* source]

          (try
            (let [loaded (pyx/reload-python-extensions! {:dirs [source] :project-root source})
                  ext (some #(when (= "vis-decisions" (:ext/name %)) %)
                            (extension/registered-extensions))]

              (expect (= 1 (:loaded loaded)) (pr-str loaded))
              (expect (zero? (:failed loaded)) (pr-str loaded))
              (expect (= #{"decisions.models" "decisions.fetch" "decisions.inspect_checkpoint"
                           "decisions.train" "decisions.prepare" "decisions.package"}
                         (set (map #(get-in % [:ext.symbol/contract "name"])
                                   (get-in ext [:ext/engine :ext.engine/symbols])))))
              (let [made (ep/create-python-context {} nil {:worker? true} nil)
                    ctx (:python-context made)
                    env {:python-context ctx :extensions (atom [ext]) :active-extensions (atom [])}]

                (try (loop-env/sync-active-extension-symbols! env [ext])
                     (let
                       [answer
                        (ep/run-python-block
                          ctx
                          (str
                            "value = await decisions.inspect_checkpoint("
                            (pr-str checkpoint)
                            ")\n"
                            "assert value.model_id == value['model_id'] == 'laya-typed-decisions'\n"
                            "assert value.path == value['path']\n"
                            "assert value.revision and value.step == value.max_steps == 0\n"
                            "assert decisions.inspect_checkpoint.contract['tag'] == 'observation'\n"
                            "try:\n    value.step = 10\n" "except AttributeError:\n    pass\n"
                            "else:\n    raise AssertionError('mutable checkpoint record')\n"
                            "print('Verified decision extension boundary')"))]
                       (expect (nil? (:error answer)) (pr-str answer))
                       (expect (str/includes? (or (:stdout answer) "")
                                              "Verified decision extension boundary")))
                     (finally (ep/dispose-python-context! ctx)))))
            (finally (pyx/reload-python-extensions! {:dirs [] :project-root source})
                     (ps/db-dispose-connection! store))))))))
