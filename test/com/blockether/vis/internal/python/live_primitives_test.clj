(ns com.blockether.vis.internal.python.live-primitives-test
  (:require [clojure.java.io :as io]
            [com.blockether.vis.internal.extension.core :as extension]
            [com.blockether.vis.internal.python.extensions :as pyx]
            [com.blockether.vis.internal.python.extensions-test :as fixture]
            [com.blockether.vis.internal.view.core :as engine]
            [lazytest.experimental.interfaces.clojure-test :refer [deftest is]]))

(def source
  "import blockether.vis.extension as vis\n\n\ndef show_primitives():\n    \"\"\"Show every new primitive through the real host boundary.\"\"\"\n    with vis.live(\"Primitives\", [\n        vis.heading(\"h\", \"Heading\", level=1),\n        vis.paragraph(\"p\", \"A paragraph\"),\n        vis.code(\"c\", \"  <button>\\n\", language=\"html\"),\n        vis.spinner(\"s\", \"Waiting\", variant=\"line\"),\n        vis.disclosure(\"details\", \"Details\", vis.output(\"a\", label=\"A\"), vis.output(\"b\", label=\"B\")),\n        vis.button(\"go\", \"Continue\"),\n    ], flush_ms=0) as view:\n        while not view.is_interrupted:\n            state = view.state()\n            button = next(n for n in state[\"nodes\"] if n[\"id\"] == \"go\")\n            if button[\"clicks\"]:\n                view[\"s\"].set(\"Done\", is_active=False, variant=\"pulse\")\n                view[\"c\"].set(\"\")\n                view[\"a\"].write(\"ERROR retained\", tone=\"error\")\n                return {\"clicks\": button[\"clicks\"], \"view\": view.state()}\n            if not view.sleep(5):\n                raise TimeoutError(\"test action did not arrive\")\n    raise AssertionError(\"test view was interrupted\")\n\nvis.register(vis.Extension(name=\"primitives\", description=\"Live primitives fixture\", version=\"0.1.0\", kind=\"integration\", alias=\"primitives\", symbols=[vis.Symbol(show_primitives, tag=\"observation\")]))\n")

(deftest sdk-crosses-live-host-boundary-test
  (let [dir
        (io/file "target" (str "live-primitives-runtime-" (random-uuid)))

        mounted
        (promise)

        open!
        engine/open-live!]

    (try
      (with-redefs-fn {(requiring-resolve 'com.blockether.vis.internal.view.sink/views-dir)
                       (constantly dir)
                       #'engine/open-live! (fn [spec]
                                             (let [view (open! spec)]
                                               (deliver mounted view)
                                               view))}
        (fn []
          (#'fixture/with-fresh-loaded
           {"primitives.py" source}
           (fn [loaded _]
             (is (some? (#'fixture/registered "primitives")) (pr-str [loaded (pyx/load-failures)]))
             (let [ext
                   (#'fixture/registered "primitives")

                   entry
                   (first (get-in ext [:ext/engine :ext.engine/symbols]))]

               (is (some? (:ext.symbol/fn entry)) (pr-str (keys entry)))
               (binding [extension/*current-environment* (assoc extension/*current-environment*
                                                           :session-id (str (random-uuid)))]
                 (let [work (future (extension/invoke-symbol-wrapper ext entry [] {}))
                       view (deref mounted 10000 nil)]

                   (try (is (some? view) (pr-str (deref work 0 ::running)))
                        (when view
                          (is (:is-accepted (engine/action! (:id view)
                                                            {:action :activate :node-id "go"}))))
                        (let [result (deref work 10000 ::timeout)]
                          (is (not= ::timeout result))
                          (is (= 1 (get result "clicks")))
                          (is (= "" (get-in result ["view" "nodes" 2 "text"])))
                          (is (false? (get-in result ["view" "nodes" 3 "is_active"])))
                          ;; #209 crosses the bundled SDK, JSON host call and engine materializer.
                          (is (= ["ERROR retained"]
                                 (get-in result ["view" "nodes" 4 "fields" 0 "lines"])))
                          (is (= ["error"]
                                 (get-in result ["view" "nodes" 4 "fields" 0 "line_tones"]))))
                        (finally (when view (engine/close-live! (:id view)))
                                 (future-cancel work))))))))))
      (finally (doseq [file (reverse (file-seq dir))]
                 (io/delete-file file true))))))
