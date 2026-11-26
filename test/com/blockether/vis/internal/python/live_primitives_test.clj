(ns com.blockether.vis.internal.python.live-primitives-test
  (:require [charred.api :as json]
            [clojure.java.io :as io]
            [com.blockether.vis.internal.extension.core :as extension]
            [com.blockether.vis.internal.foundation.core :as foundation]
            [com.blockether.vis.internal.foundation.shell-test :as shell-fixture]
            [com.blockether.vis.internal.python.env :as env-python]
            [com.blockether.vis.internal.python.extensions :as pyx]
            [com.blockether.vis.internal.task :as task]
            [com.blockether.vis.internal.python.extensions-test :as fixture]
            [com.blockether.vis.internal.view.core :as engine]
            [lazytest.core :refer [defdescribe expect it]]))

(def source
  "import blockether.vis.extension as vis\nfrom concurrent.futures import wait\n\n\ndef show_primitives():\n    \"\"\"Show every new primitive through the real host boundary.\"\"\"\n    with vis.live(\"Primitives\", [\n        vis.ui.heading(\"h\", \"Heading\", level=1),\n        vis.ui.paragraph(\"p\", \"A paragraph\"),\n        vis.ui.code(\"c\", \"  <button>\\n\", language=\"html\"),\n        vis.ui.spinner(\"s\", \"Waiting\", variant=\"line\"),\n        vis.ui.collapsible(\"details\", \"Details\", vis.ui.output(\"a\", label=\"A\"), vis.ui.output(\"b\", label=\"B\")),\n        vis.ui.button(\"go\", \"Continue\"),\n        vis.ui.canvas(\"board\", [vis.ui.canvas.frame(\"release\", \"Release\"), vis.ui.canvas.shape(\"ready\", \"Ready?\", shape=\"diamond\", parent=\"release\")]),\n        vis.ui.divider(\"section-break\"),\n    ], flush_ms=0) as view:\n        while not view.is_interrupted:\n            state = view.state()\n            button = next(n for n in state[\"nodes\"] if n[\"id\"] == \"go\")\n            if button[\"clicks\"]:\n                view[\"s\"].set(\"Done\", is_active=False, variant=\"pulse\")\n                view[\"c\"].set(\"\")\n                view[\"a\"].write(\"ERROR retained\", tone=\"error\")\n                view[\"board\"].set(flow=\"down\", pattern=\"grid\")\n                return {\"clicks\": button[\"clicks\"], \"view\": view.state()}\n            watch = view.changed()\n            if not wait([watch], timeout=5).done:\n                watch.cancel()\n                raise TimeoutError(\"test action did not arrive\")\n    raise AssertionError(\"test view was interrupted\")\n\nvis.register_extension(vis.Extension(name=\"primitives\", description=\"Live primitives fixture\", version=\"0.1.0\", kind=\"integration\", alias=\"primitives\", symbols=[vis.Symbol(show_primitives, tag=\"observation\")]))\n")

(defdescribe
  sdk-crosses-live-host-boundary-test
  (it
    "sdk crosses live host boundary"
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
               (expect (some? (#'fixture/registered "primitives"))
                       (pr-str [loaded (pyx/load-failures)]))
               (let [ext
                     (#'fixture/registered "primitives")

                     entry
                     (first (get-in ext [:ext/engine :ext.engine/symbols]))]

                 (expect (some? (:ext.symbol/fn entry)) (pr-str (keys entry)))
                 (binding [extension/*current-environment* (assoc extension/*current-environment*
                                                             :session-id (str (random-uuid)))]
                   (let [work (future (extension/invoke-symbol-wrapper ext entry [] {}))
                         view (deref mounted 10000 nil)]

                     (try
                       (expect (some? view) (pr-str (deref work 0 ::running)))
                       (when view
                         (expect (:is-accepted (engine/action! (:id view)
                                                               {:action :activate :node-id "go"}))))
                       (let [result (deref work 10000 ::timeout)]
                         (expect (not= ::timeout result))
                         (expect (= 1 (get result "clicks")))
                         (expect (= {"id" "section-break" "type" "divider"}
                                    (last (get-in result ["view" "nodes"]))))
                         (let [board (get-in result ["view" "nodes" 6])]
                           (expect (= ["down" "grid" "diamond" "release"]
                                      [(get board "flow") (get board "pattern")
                                       (get-in board ["cards" 1 "shape"])
                                       (get-in board ["cards" 1 "parent"])])))
                         (expect (= "" (get-in result ["view" "nodes" 2 "text"])))
                         (expect (false? (get-in result ["view" "nodes" 3 "is_active"])))
                         ;; #209 crosses the bundled SDK, JSON host call and engine materializer.
                         (expect (= ["ERROR retained"]
                                    (get-in result ["view" "nodes" 4 "children" 0 "lines"])))
                         (expect (= ["error"]
                                    (get-in result ["view" "nodes" 4 "children" 0 "line_tones"]))))
                       (finally (when view (engine/close-live! (:id view)))
                                (future-cancel work))))))))))
        (finally (doseq [file (reverse (file-seq dir))]
                   (io/delete-file file true)))))))


(defdescribe sandbox-live-watch-test
  (it "lets another host task complete during a timed wait and answers private validation"
    (foundation/register!)
    (let [dir (io/file "target" (str "sandbox-live-watch-" (random-uuid)))
          first-watch (promise)
          next-watch (promise)
          close-watch (promise)
          registrations (atom 0)
          watch! engine/app-watch!
          start! task/start!]
      (try
        (with-redefs-fn {(requiring-resolve 'com.blockether.vis.internal.view.sink/views-dir)
                         (constantly dir)
                         #'task/start! (fn [sid opts work]
                                         (assert (not= "view.changed()" (:name opts)))
                                         (start! sid opts work))
                         #'engine/app-watch! (fn [envelope]
                                               (let [watch (watch! envelope)
                                                     id (get (json/read-json envelope) "view_id")]
                                                 (case (swap! registrations inc)
                                                   1 (deliver first-watch id)
                                                   2 (deliver next-watch id)
                                                   3 (deliver close-watch id)
                                                   nil)
                                                 watch))}
          (fn []
            (#'shell-fixture/with-shell-task-context
             (fn [sid ctx]
               (binding [extension/*current-environment* {:session-id sid}]
                 (let [work (future (env-python/run-python-block ctx "import asyncio\nfrom blockether.vis.extension import ui\napp = attach(ui.app('Pull validation', ui.plaintext('port', label='Port', validate=lambda value: None if value.isdigit() else 'need digits'), ui.button('send', 'Send', action='submit'), ui.button('go', 'Go')))\nwatch = app.changed()\nassert isinstance(watch, asyncio.Task)\ncompleted = []\nasync def run_shell():\n    sh = await shell('printf ready')\n    await sh\n    completed.append(sh.out)\nasync with asyncio.TaskGroup() as group:\n    group.create_task(run_shell())\n    done, pending = await asyncio.wait([watch], timeout=.5)\n    assert not done and pending == {watch}, (done, pending)\n    assert completed == ['ready'], completed\nassert app.events() == []\nassert watch.cancel()\ntry:\n    await watch\nexcept asyncio.CancelledError:\n    pass\nelse:\n    raise AssertionError('cancel did not cancel the watch')\nwatch = app.changed()\nassert await asyncio.wait_for(watch, 5) == 'event'\nassert [(e.type, e.node_id) for e in app.events()] == [('activate', 'go')]\nassert app.events() == []\nassert not app.is_interrupted\nimport gc, weakref\napp._validators.clear()\nwatch = app.changed()\noriginal = weakref.ref(watch)\nident = watch._id\ndel watch\ngc.collect()\nassert original() is None\n[watch] = [task for task in asyncio.all_tasks() if getattr(task, '_id', None) == ident]\ndone, pending = await asyncio.wait([watch], timeout=5)\nassert done == {watch} and not pending\nassert app.is_interrupted and app.reason == 'completed'\nassert await watch == 'closed'\nprint('live watch ready')\n"))]
                   (try
                     (let [id (deref first-watch 6000 nil)]
                       (expect (some? id))
                       (when id
                         (expect (= {"port" "need digits"}
                                    (:errors (engine/action! id {:action :submit :node-id "send"
                                                                 :values {"port" "abc"}
                                                                 :idempotency-key "pull-validation"}))))))
                     (let [id (deref next-watch 6000 nil)]
                       (expect (some? id))
                       (when id
                         (expect (:is-accepted (engine/action! id {:action :activate :node-id "go"})))))
                     (let [id (deref close-watch 6000 nil)]
                       (when id (engine/close-live! id)))
                     (let [result (deref work 6000 {:error :timeout})]
                       (expect (nil? (:error result)) (pr-str result))
                       (expect (= "live watch ready\n" (:stdout result))))
                     (finally
                       (when-let [id (deref first-watch 0 nil)] (engine/close-live! id))
                       (future-cancel work)))))))))
        (finally (doseq [file (reverse (file-seq dir))]
                   (io/delete-file file true)))))))
