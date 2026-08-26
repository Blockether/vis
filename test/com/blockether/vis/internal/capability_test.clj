(ns com.blockether.vis.internal.capability-test
  (:require [com.blockether.vis.internal.capability :as capability]
            [lazytest.core :refer [defdescribe expect it]]))

(defn- counting
  "A probe that answers `answers` in order, one per call, throwing anything that is
   a Throwable, and a counter of how many times it was really asked."
  [& answers]
  (let [calls
        (atom 0)

        left
        (atom answers)]

    [calls
     (fn []
       (swap! calls inc)
       (let [a (first @left)]
         (swap! left rest)
         (if (instance? Throwable a) (throw a) a)))]))

(defdescribe
  terminal-error-test
  (it "recognises the JVM refusing to link, at any depth in the cause chain"
      (expect (capability/terminal-error? (UnsatisfiedLinkError.
                                            "no sherpa-onnx-jni in java.library.path")))
      (expect (capability/terminal-error? (ex-info "transcribe failed"
                                                   {}
                                                   (ExceptionInInitializerError.
                                                     (UnsatisfiedLinkError. "no onnxruntime")))))
      (expect (not (capability/terminal-error? (ex-info "the disk is full" {})))))
  (it "stops at the end of the chain instead of dying inside the explanation"
      ;; A shallow failure must not become a NullPointerException out of the very
      ;; code that is supposed to say what went wrong.
      (expect (not (capability/terminal-error? (java.io.IOException. "connection reset"))))))

(defdescribe ensure-test
             (it "asks the machine once and answers what the probe returned"
                 (let [[calls probe]
                       (counting {:source :embedded})

                       id
                       (keyword "vis.capability-test" (str (gensym "ready")))

                       first-ask
                       (capability/ensure! id probe)]

                   (expect (= :ready (:status first-ask)))
                   (expect (= {:source :embedded} (:detail first-ask)))
                   (expect (= id (:capability first-ask)))
                   (expect (= first-ask (capability/ensure! id probe)))
                   (expect (= 1 @calls) "a ready capability is never probed twice")))
             (it "never probes for a verdict that has not been asked for"
                 (let [[calls probe]
                       (counting :ready)

                       id
                       (keyword "vis.capability-test" (str (gensym "unasked")))]

                   (expect (nil? (capability/verdict id)))
                   (expect (zero? @calls))
                   (capability/ensure! id probe)
                   (expect (some? (capability/verdict id)))))
             (it "retries a transient refusal, because the next ask can answer differently"
                 ;; No network, a missing file, a full disk: the machine did not say no, the
                 ;; moment did.
                 (let [[calls probe]
                       (counting (java.io.IOException. "connection reset") {:source :downloaded})

                       id
                       (keyword "vis.capability-test" (str (gensym "transient")))

                       refused
                       (capability/ensure! id probe)]

                   (expect (= :unavailable (:status refused)))
                   (expect (= :transient (:kind refused)))
                   (expect (= "connection reset" (:error refused)))
                   (expect (nil? (capability/verdict id)) "a transient refusal is not remembered")
                   (expect (= :ready (:status (capability/ensure! id probe))))
                   (expect (= 2 @calls))))
             ;; Regression, user report: voice failed with a linker error after the model was
             ;; installed and kept failing until Vis was restarted - and every failing call
             ;; fetched the 13 MB library again first, to meet the same wall.
             (it "freezes a linker refusal, because this JVM can no longer answer differently"
                 (let [[calls probe]
                       (counting (UnsatisfiedLinkError. "no onnxruntime") {:source :downloaded})

                       id
                       (keyword "vis.capability-test" (str (gensym "terminal")))

                       refused
                       (capability/ensure! id probe)]

                   (expect (= :terminal (:kind refused)))
                   (expect (= refused (capability/ensure! id probe)))
                   (expect (= 1 @calls) "a class the JVM already refused is not provisioned again")
                   (expect (instance? UnsatisfiedLinkError (:cause refused))
                           "the caller still gets the throwable it needs to explain itself")))
             (it "takes a linker refusal met somewhere other than the probe"
                 ;; sherpa loads its library from the static initializer of the first class a
                 ;; call touches, so the refusal is normally met INSIDE the call, long after
                 ;; provisioning answered ready.
                 (let [id
                       (keyword "vis.capability-test" (str (gensym "lost")))

                       [calls probe]
                       (counting {:source :embedded})]

                   (capability/ensure! id probe)
                   (expect (= :ready (:status (capability/verdict id))))
                   (capability/fail! id
                                     (ex-info "transcribe failed"
                                              {}
                                              (NoClassDefFoundError.
                                                "com/k2fsa/sherpa/onnx/OfflineTts")))
                   (expect (= :terminal (:kind (capability/verdict id))))
                   (expect (= :unavailable (:status (capability/ensure! id probe))))
                   (expect (= 1 @calls))))
             (it "keeps a failure the machine did not cause out of the record"
                 (let [id (keyword "vis.capability-test" (str (gensym "ordinary")))]
                   (capability/fail! id (ex-info "no such voice" {:type :voice-tts/unknown-voice}))
                   (expect (nil? (capability/verdict id)))))
             (it "holds nothing a native image could bake in"
                 ;; `graal-build-time` initializes this namespace inside the BUILDER, so a
                 ;; verdict computed at load would ship the build machine's answer to every
                 ;; installed binary. Nothing here probes until someone asks, and forgetting
                 ;; leaves the namespace exactly as it loads.
                 (let [id
                       (keyword "vis.capability-test" (str (gensym "empty")))

                       [_ probe]
                       (counting :ready)]

                   (capability/ensure! id probe)
                   (capability/forget-verdicts!)
                   (expect (= {} @@#'capability/verdicts))
                   (expect (nil? (capability/verdict id))))))
