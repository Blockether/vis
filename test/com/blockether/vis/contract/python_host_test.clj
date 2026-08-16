(ns com.blockether.vis.contract.python-host-test
  "The Python host contract (`packages/vis-contract`) against its readers.

   The contract is data precisely so that no two readers can disagree in silence:
   the engine binds `:op/global` into every extension context, the `vis` module
   builds its host out of `:op/name`, and outside a Vis process the
   packaged module answers by `:op/outside`. This file is the seam that fails when
   one of them drifts — including the one proof no regex can give, a live extension
   context whose `vis._host` is read back and compared to the document."
  (:require [clojure.string :as str]
            [com.blockether.vis.internal.foundation.shell :as fshell]
            [com.blockether.vis.contract.python-host :as contract]
            [com.blockether.vis.internal.python-extensions :as pyx]
            [lazytest.core :refer [defdescribe describe expect it]])
  (:import (org.graalvm.polyglot Context)))

(defn- bootstrap-host-keys
  "The `_host` dict keys the bootstrap builds, read out of its source."
  []
  (set (map second (re-seq #"\"([a-z_]+)\": __vis_host_" pyx/bootstrap-python))))

(defdescribe
  python-host-contract-test
  (describe
    "the contract document"
    (it "declares every `__vis_host_*` global the bootstrap injects"
        (expect (= (set (re-seq #"__vis_host_\w+__" pyx/bootstrap-python))
                   (set (contract/host-globals)))))
    (it "names each op the way the module's `_host` dict keys it"
        (expect (= (bootstrap-host-keys) (set (contract/op-names)))))
    (it "gives every refusing op a reason that names the call the author made"
        (let [refusing (filter #(= :outside/refuse (:op/outside %)) (contract/ops))]
          (expect (seq refusing))
          (expect (every? #(str/includes? (:op/refusal %) (str "vis." (:op/name %))) refusing))))
    (it "speaks the shell vocabulary the engine dispatches on"
        ;; The outside host reads these names out of the `vis_contract` document; an op
        ;; the engine does not know would make an extension that runs inside Vis
        ;; refuse outside it (or the other way round).
        (let
          [{:shell/keys [default-op spawn-ops handle-ops]}
           (contract/shell-vocabulary)

           op-type
           (fn [op]
             (try (fshell/shell-dispatch {} {"op" op})
                  nil
                  (catch clojure.lang.ExceptionInfo e (:type (ex-data e)))))]

          (expect (every? #(not= ::fshell/unknown-op (op-type %)) (concat spawn-ops handle-ops)))
          (expect (= ::fshell/unknown-op (op-type "detonate")))
          ;; An options map with no `op` means the default one, so both spell
          ;; the same missing argument.
          (expect (= (op-type nil) (op-type default-op)))))
    (it "declares every global the engine actually binds into a live context"
        ;; `bind-inert-host!` binds the document's own list, so only the REAL
        ;; binder can prove the engine grew a host call the document never heard
        ;; of -- or lost one an extension still calls.
        (let [^Context ctx (pyx/build-context "python-contract-bind-test")]
          (try (pyx/bind-host! ctx "python-contract-bind-test")
               (let [bindings (.getBindings ctx "python")]
                 (expect (every? #(.hasMember bindings ^String %) (contract/host-globals))))
               (finally (.close ctx))))))
  (describe
    "a live extension context"
    (it "binds exactly the ops the document declares"
        (let [^Context ctx (pyx/build-context "python-contract-test")]
          (try (pyx/bind-inert-host! ctx nil)
               (.eval ctx "python" ^String pyx/bootstrap-python)
               (expect (= (sort (contract/op-names))
                          (-> (.eval ctx "python" "import vis\n','.join(sorted(vis._host.keys()))")
                              (.asString)
                              (str/split #","))))
               (finally (.close ctx)))))))
