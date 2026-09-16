(ns com.blockether.vis.contract.python-host-test
  "The Python host implementation against its real bootstrap boundary."
  (:require [clojure.string :as str]
            [com.blockether.vis.internal.python.extensions :as pyx]
            [com.blockether.vis.internal.python.worker :as pyext]
            [lazytest.core :refer [defdescribe expect it]]))

(defn- host-door-names [] (set (keys (pyx/host-doors "contract-probe" "contract-probe" nil))))

(defn- host-op-names
  []
  (set (map #(subs % (count "__vis_host_") (- (count %) 2)) (host-door-names))))

(defdescribe
  python-host-contract-test
  (it "derives inert members from the actual bindings, without a catalog"
      (expect (= (host-door-names) (set (pyx/host-member-names)))))
  (it "includes a new host binding in the inert checker without a second declaration"
      (with-redefs [pyx/host-doors (fn [_ _ _]
                                     {"__vis_host_probe__" (fn []
                                                             nil)})]
        (expect (= ["__vis_host_probe__"] (pyx/host-member-names)))))
  (it "installs every actual host binding in a live context"
      (let [ctx (pyx/build-context "python-contract-bind-test")]
        (try (pyx/bind-host! ctx "python-contract-bind-test")
             (expect (= ""
                        (pyext/eval-str pyext/shared-key
                                        ctx
                                        (str "','.join(n for n in ["
                                             (str/join ", " (map pr-str (host-door-names)))
                                             "] if n not in globals())"))))
             (finally (pyx/close-context! ctx)))))
  (it
    "implements the canonical SDK Host interface across the bootstrap boundary"
    (let [ctx (pyx/build-context "python-host-interface-parity")]
      (try
        (pyx/bind-inert-host! ctx nil)
        (pyext/exec! pyext/shared-key ctx pyx/bootstrap-python)
        (expect
          (=
            (sort (host-op-names))
            (->
              (pyext/eval-str
                pyext/shared-key
                ctx
                "','.join(sorted(n for n, value in vars(__import__('blockether.vis.extension', fromlist=['Host']).Host).items() if not n.startswith('_') and callable(value)))")
              (str/split #","))))
        (finally (pyx/close-context! ctx)))))
  (it
    "bootstraps exactly the engine's operations"
    (let [ctx (pyx/build-context "python-contract-test")]
      (try
        (pyx/bind-inert-host! ctx nil)
        (pyext/exec! pyext/shared-key ctx pyx/bootstrap-python)
        (expect
          (=
            (sort (host-op-names))
            (->
              (pyext/eval-str
                pyext/shared-key
                ctx
                "','.join(sorted(n for n in vars(__import__('blockether.vis.extension', fromlist=['extension'])._host) if not n.startswith('_')))")
              (str/split #","))))
        (finally (pyx/close-context! ctx))))))
