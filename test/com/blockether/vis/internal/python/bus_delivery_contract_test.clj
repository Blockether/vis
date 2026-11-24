(ns com.blockether.vis.internal.python.bus-delivery-contract-test
  "`vis.bus` in an extension gives the answers of the contract fixture
   `vis-contract/fixtures/bus-delivery.json`. A real extension worker runs the runner
   of the fixture. The SDK test `test_bus_contract.py` runs the same runner outside Vis."
  (:require [clojure.data.json :as json]
            [clojure.java.io :as io]
            [com.blockether.vis.internal.extension.core :as extension]
            [com.blockether.vis.internal.foundation.core :as foundation]
            [com.blockether.vis.internal.persistance.core :as ps]
            [com.blockether.vis.internal.persistance.sqlite.test-helpers :as h]
            [com.blockether.vis.internal.python.env :as ep]
            [com.blockether.vis.internal.python.extensions :as pyx]
            [lazytest.core :refer [defdescribe expect it]])
  (:import [java.nio.file Files]
           [java.nio.file.attribute FileAttribute]))

(defn- fixture-text [] (slurp (io/resource "vis-contract/fixtures/bus-delivery.json")))

(defn- extension-source
  "A Python extension that runs the runner of the fixture in two callbacks."
  [text]
  (str "import json\n"
       "import blockether.vis.extension as vis\n"
       "\n"
       "TABLE = json.loads("
       (pr-str text)
       ")\n"
       "exec(\"\\n\".join(TABLE[\"runner\"]), globals())\n"
       "\n\n"
       "def begin() -> str:\n"
       "    \"Run each case.\"\n"
       "    return json.dumps(start(vis))\n"
       "\n\n"
       "def end() -> str:\n"
       "    \"Return what the handlers kept, or null while a signal waits.\"\n"
       "    return json.dumps(finish(vis))\n"
       "\n\n"
       "vis.register_extension(vis.Extension(name=\"buscontract\", description=\"Bus contract fixture\", alias=\"buscontract\", symbols=[vis.Symbol(begin), vis.Symbol(end)]))\n"))

(defn- answers
  "The answers of the runner by case, from a real extension worker: the answers of
   the calls in `start`, and what the handlers kept."
  [text]
  (let [dir
        (.toFile (Files/createTempDirectory "vis-bus-contract" (make-array FileAttribute 0)))

        store
        (ps/db-create-connection! :memory)]

    (try (spit (io/file dir "buscontract.py") (extension-source text))
         (pyx/reload-python-extensions! {:dirs [(str dir)]})
         (let [ext
               (some #(when (= "buscontract" (:ext/name %)) %) (extension/registered-extensions))

               active
               [ext
                (some #(when (= "foundation-core" (:ext/name %)) %)
                      (extension/registered-extensions))]

               ctx
               (:python-context (ep/create-python-context {}
                                                          (fn []
                                                            [(.getCanonicalPath dir)])
                                                          {:worker? true
                                                           :jail-enabled? false
                                                           :enabled? false
                                                           :allowed-domains []
                                                           :denied-domains []
                                                           :exclude-domains []}
                                                          nil))

               env
               {:db-info store
                :python-context ctx
                :session-id (h/store-session! store {:title "Bus contract"})
                :extensions (atom active)
                :active-extensions (atom active)}

               call
               (fn [symbol]
                 (binding [extension/*current-environment* env]
                   (json/read-str (extension/invoke-symbol-wrapper
                                    ext
                                    (some #(when (= symbol (:ext.symbol/symbol %)) %)
                                          (extension/ext-symbols ext))
                                    []
                                    env))))]

           (try (let [calls
                      (call 'begin)

                      ;; Each `end` is a callback of its own: the handlers run between two.
                      handlers
                      (loop [n 0]
                        (or (call 'end)
                            (when (< n 600)
                              (Thread/sleep 50)
                              (recur (inc n)))))]

                  (into {}
                        (map (fn [[case answer]]
                               [case {"calls" answer "handlers" (get handlers case)}]))
                        calls))
                (finally (ep/dispose-python-context! ctx))))
         (finally (pyx/reload-python-extensions! {:dirs []})
                  (ps/db-dispose-connection! store)
                  (doseq [f (reverse (file-seq dir))]
                    (io/delete-file f true))))))

(defdescribe
  bus-delivery-contract-test
  (it "gives the answers of the fixture through vis.bus in a real extension worker"
      (foundation/register!)
      (let [text
            (fixture-text)

            cases
            (get (json/read-str text) "cases")

            got
            (answers text)]

        (expect (seq cases))
        (expect (= (set (map #(get % "name") cases)) (set (keys got))))
        (let [differences
              (vec (keep (fn [case]
                           (let [answer (get got (get case "name"))]
                             (when (not= (get case "expect") answer)
                               {:case (get case "name") :expect (get case "expect") :answer answer})))
                         cases))]
          (expect (= [] differences) (pr-str differences))))))
