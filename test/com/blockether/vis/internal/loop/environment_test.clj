(ns com.blockether.vis.internal.loop.environment-test
  "Extension selection for a new session: `--extensions` in the TUI and the
   `extensions` field of `POST /v1/sessions`."
  (:require [com.blockether.vis.internal.config.scoped :as scoped]
            [com.blockether.vis.internal.extension.core :as extension]
            [com.blockether.vis.internal.loop.environment :as loop-env]
            [com.blockether.vis.internal.python.extensions :as python-extensions]
            [lazytest.core :refer [defdescribe describe expect it]]))

(def ^:private extension-selection (deref #'loop-env/extension-selection))

(def ^:private extensions-to-turn-off (deref #'loop-env/extensions-to-turn-off))

(def ^:private registered
  [{:ext/name "gh" :optional true} {:ext/name "spel" :optional true}
   {:ext/name "clj" :optional true} {:ext/name "foundation-harness"}])

(defn- turned-off
  [names]
  (with-redefs [python-extensions/ensure-python-extensions-loaded!
                (constantly nil)

                extension/registered-extensions
                (constantly registered)

                scoped/engine-setting!
                (fn [ext]
                  (when (:optional ext) {:id (str "extension_" (:ext/name ext))}))]

    (sort (map :ext/name (extensions-to-turn-off (extension-selection names))))))

(defn- error-data [f] (try (f) nil (catch clojure.lang.ExceptionInfo e (ex-data e))))

(defdescribe extension-selection-test
             (describe "parsing"
                       (it "keeps every extension without a selection"
                           (expect (nil? (extension-selection nil))))
                       (it "reads names as a list to keep and -names as a list to turn off"
                           (expect (= {:keep #{"gh" "clj"}} (extension-selection ["gh" " clj"])))
                           (expect (= {:drop #{"spel"}} (extension-selection ["-spel"])))
                           (expect (= {:keep #{}} (extension-selection []))))
                       (it "rejects a list that mixes both forms"
                           (expect (= {:status 400 :type :extension/mixed-selection}
                                      (error-data #(extension-selection ["gh" "-spel"]))))))
             (describe "optional extensions to turn off"
                       (it "turns off every optional extension outside the keep list"
                           (expect (= ["spel"] (turned-off ["gh" "clj"])))
                           (expect (= ["clj" "gh" "spel"] (turned-off []))))
                       (it "turns off only the -names" (expect (= ["spel"] (turned-off ["-spel"]))))
                       (it "rejects an unknown name before the session starts"
                           (let [data (error-data #(turned-off ["-spell"]))]
                             (expect (= :extension/unknown (:type data)))
                             (expect (= ["spell"] (:extensions data)))))
                       (it "rejects an engine part that has no Auto/On/Off setting"
                           (expect (= {:status 400
                                       :type :extension/required
                                       :extensions ["foundation-harness"]}
                                      (error-data #(turned-off ["-foundation-harness"])))))))
