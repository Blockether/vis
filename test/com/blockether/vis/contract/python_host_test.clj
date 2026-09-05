(ns com.blockether.vis.contract.python-host-test
  "The Python host declaration against its engine readers."
  (:require [clojure.string :as str]
            [com.blockether.vis.contract.python-host :as contract]
            [com.blockether.vis.internal.foundation.shell :as fshell]
            [com.blockether.vis.internal.python.extensions :as pyx]
            [com.blockether.vis.internal.python.worker :as pyext]
            [com.blockether.vis.internal.view.core :as view]
            [lazytest.core :refer [defdescribe describe expect it]]))

(defn- host-door-names
  "The `__vis_host_*__` names an extension context is actually given.

   Read from the HOST, not from the bootstrap's source: the bootstrap passes
   through whatever it is handed, and it used to name each door instead, so a
   door added in vis needed a release of the runtime that only forwarded it."
  []
  (set (keys (pyx/host-doors "contract-probe" "contract-probe"))))

(defn- host-op-names
  "The same doors under the names `vis._host` attributes them: the marker
   trimmed off both ends."
  []
  (set (map #(subs % (count "__vis_host_") (- (count %) 2)) (host-door-names))))

(defdescribe
  python-host-contract-test
  (describe
    "the contract document"
    (it "declares every `__vis_host_*` global the host injects"
        (expect (= (host-door-names) (set (contract/host-globals)))))
    (it "names each op the way the module's `_host` object attributes it"
        (expect (= (host-op-names) (set (contract/op-names)))))
    (it "gives every refusing op a reason that names the call the author made"
        (let [refusing (filter #(= :outside/refuse (:op/outside %)) (contract/ops))]
          (expect (seq refusing))
          (expect (every? #(str/includes? (:op/refusal %) (str "vis." (:op/name %))) refusing))))
    (it "speaks the shell vocabulary the engine dispatches on"
        (let [{:shell/keys [default-op spawn-ops handle-ops]}
              (contract/shell-vocabulary)

              op-type
              (fn [op]
                (try (fshell/shell-dispatch {} {"op" op})
                     nil
                     (catch clojure.lang.ExceptionInfo e (:type (ex-data e)))))]

          (expect (every? #(not= ::fshell/unknown-op (op-type %)) (concat spawn-ops handle-ops)))
          (expect (= ::fshell/unknown-op (op-type "detonate")))
          (expect (= (op-type nil) (op-type default-op)))))
    (it "speaks the live vocabulary the engine dispatches on"
        (let [{:live/keys [default-op spawn-ops handle-ops flush-ms]}
              (contract/live-vocabulary)

              op-type
              (fn [op]
                (try (view/live-dispatch (cond-> {}
                                           op
                                           (assoc "op" op)))
                     nil
                     (catch clojure.lang.ExceptionInfo e (:type (ex-data e)))))]

          (expect (every? #(not= :vis/view-unknown-live-op (op-type %))
                          (concat spawn-ops handle-ops)))
          (expect (= :vis/view-unknown-live-op (op-type "detonate")))
          (expect (= (op-type nil) (op-type default-op)))
          (expect (pos-int? flush-ms))))
    (it "declares every global the engine actually binds into a live context"
        (let [sess (pyx/build-context "python-contract-bind-test")]
          (try (pyx/bind-host! sess "python-contract-bind-test")
               (expect (= ""
                          (pyext/eval-str pyext/shared-key
                                          sess
                                          (str "','.join(n for n in ["
                                               (str/join ", " (map pr-str (contract/host-globals)))
                                               "] if n not in globals())"))))
               (finally (pyx/close-context! sess))))))
  (describe
    "a live extension context"
    (it
      "binds exactly the ops the document declares"
      (let [ctx (pyx/build-context "python-contract-test")]
        (try
          (pyx/bind-inert-host! ctx nil)
          (pyext/exec! pyext/shared-key ctx pyx/bootstrap-python)
          (expect
            (=
              (sort (contract/op-names))
              (->
                (pyext/eval-str
                  pyext/shared-key
                  ctx
                  "','.join(sorted(n for n in vars(__import__('vis')._host) if not n.startswith('_')))")
                (str/split #","))))
          (finally (pyx/close-context! ctx)))))
    (it "batches on the window the document declares"
        ;; The flush window is a CONTRACT number, not the module's own taste: the
        ;; engine's durable publish parks the thread that pushed, so a module
        ;; batching on a guess of its own would cost a host call per line.
        (let [ctx (pyx/build-context "python-contract-flush-test")]
          (try (pyx/bind-inert-host! ctx nil)
               (pyext/exec! pyext/shared-key ctx pyx/bootstrap-python)
               (expect (= (str (:live/flush-ms (contract/live-vocabulary)))
                          (pyext/eval-str pyext/shared-key ctx "str(__import__('vis')._FLUSH_MS)")))
               (finally (pyx/close-context! ctx)))))))
