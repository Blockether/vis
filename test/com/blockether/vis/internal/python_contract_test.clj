(ns com.blockether.vis.internal.python-contract-test
  "The Python host contract (`resources/vis-contract/python-host.edn`) against its
   readers.

   The contract is data precisely so that no two readers can disagree in silence:
   the engine binds `:op/global` into every extension context, the `vis` module
   builds its `_host` dict out of `:op/name`, and outside a Vis process the
   packaged module answers by `:op/outside`. This file is the seam that fails when
   one of them drifts — including the one proof no regex can give, a live extension
   context whose `vis._host` is read back and compared to the document."
  (:require [clojure.string :as str]
            [com.blockether.vis.internal.python-contract :as contract]
            [com.blockether.vis.internal.python-extensions :as pyx]
            [lazytest.core :refer [defdescribe describe expect it]])
  (:import (org.graalvm.polyglot Context)))

(defn- bootstrap-host-keys
  "The `_host` dict keys the bootstrap builds, read out of its source."
  []
  (set (map second (re-seq #"\"([a-z_]+)\": __vis_host_" pyx/bootstrap-python))))

(defdescribe
  python-host-contract-test
  (describe "the contract document"
            (it "declares every `__vis_host_*` global the bootstrap injects"
                (expect (= (set (re-seq #"__vis_host_\w+__" pyx/bootstrap-python))
                           (set (contract/host-globals)))))
            (it "names each op the way the module's `_host` dict keys it"
                (expect (= (bootstrap-host-keys) (set (contract/op-names)))))
            (it "gives every refusing op a reason that names the call the author made"
                (let [refusing (filter #(= :outside/refuse (:op/outside %)) (contract/ops))]
                  (expect (seq refusing))
                  (expect (every? #(str/includes? (:op/refusal %) (str "vis." (:op/name %)))
                                  refusing)))))
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
