(ns com.blockether.vis.internal.extension-test
  (:require
   [com.blockether.vis.internal.extension :as extension]
   [com.blockether.vis.internal.workspace :as workspace]
   [lazytest.core :refer [defdescribe expect it]]))

(defn- sample-channel-fn
  [& _]
  nil)

(defn ^{:doc "demo"} demo-symbol-fn
  "demo"
  []
  (extension/success {:result {:secret "payload"}}))

(defdescribe prompt-normalization-test
  (it "normalizes string and fn extension prompts"
    (let [prompt-text "\n\n    First line\n\n\n\n      Nested line\n"
          string-ext (extension/extension
                       {:ext/name "test.prompt-string"
                        :ext/description "Test prompt string."
                        :ext/prompt prompt-text})
          fn-ext (extension/extension
                   {:ext/name "test.prompt-fn"
                    :ext/description "Test prompt fn."
                    :ext/prompt (fn [_] prompt-text)})]
      (expect (= "First line\n\n  Nested line" ((:ext/prompt string-ext) {})))
      (expect (= "First line\n\n  Nested line" ((:ext/prompt fn-ext) {}))))))

(defdescribe ctx-contributions-test
  (it "binds active workspace root while building extension ctx"
    (let [root (.getCanonicalPath (java.io.File. "target/test-workspace-ctx"))
          ext  {:ext/name "test.ctx-workspace"
                :ext/ctx  (fn [_]
                            {:project {:ctx-root workspace/*workspace-root*
                                       :cwd      (.getCanonicalPath (workspace/cwd))}})}
          ctx  (extension/ctx-contributions {:workspace/root root} [ext])]
      (expect (= root (get-in ctx [:project :ctx-root])))
      (expect (= root (get-in ctx [:project :cwd]))))))

(defdescribe channel-contributions-test
  (it "extension accepts channel contributions and derives channel kind"
    (let [ext (extension/extension
                {:ext/name "test.channel-contribution"
                 :ext/description "Test channel contribution."
                 :ext/channel-contributions
                 {:tui.slot/commands
                  [{:id :test/command
                    :fn #'sample-channel-fn}]}})]
      (expect (= "channels" (:ext/kind ext)))
      (expect (= {:tui.slot/commands [{:id :test/command
                                       :fn #'sample-channel-fn}]}
                (:ext/channel-contributions ext)))))

  (it "normalizes slot keys into channel-id and slot fields"
    (with-redefs [extension/registered-extensions
                  (fn []
                    [{:ext/channel-contributions
                      {:tui.slot/commands
                       [{:id :voice/input
                         :fn #'sample-channel-fn}]
                       :telegram.slot/preamble
                       [{:id :telegram/preamble
                         :fn #'sample-channel-fn}]}}])]
      (expect (= [{:id :voice/input
                   :fn #'sample-channel-fn
                   :channel-id :tui
                   :slot :tui.slot/commands}]
                (extension/channel-contributions-for :tui :tui.slot/commands)))
      (expect (= [:tui.slot/commands]
                (mapv :slot (extension/channel-contributions-for :tui)))))))

(defdescribe symbol-renderer-test
  (it "requires a render fn for observed tool symbols"
    (let [entry (extension/symbol #'demo-symbol-fn
                  {:symbol 'demo :tag :observation})]
      (expect (= :extension/missing-renderer
                (try
                  (extension/extension
                    {:ext/name "test.missing-renderer"
                     :ext/kind "test"
                     :ext/description "Test missing renderer."
                     :ext/sci {:ext.sci/ns 'test.missing-renderer
                               :ext.sci/alias 'test.missing-renderer
                               :ext.sci/symbols [entry]}})
                  nil
                  (catch clojure.lang.ExceptionInfo e
                    (:type (ex-data e))))))))

  (it "slash path collisions across extensions are rejected at register-extension! time"
    ;; The union of `:ext/slash-commands` across all registered
    ;; extensions must contain unique `[parent name]`
    ;; paths. A second extension that declares the same path as an
    ;; already-registered extension is refused. Hot reload of the
    ;; SAME extension id is still allowed because its prior entry is
    ;; excluded from the conflict scan.
    (try
      (extension/register-extension!
        {:ext/name        "test.slash-collide-a"
         :ext/description "first owner of /probe"
         :ext/slash-commands
         [{:slash/name   "probe"
           :slash/doc    "probe original"
           :slash/run-fn (fn [_] {:slash/status :ok})}]})
      (let [thrown (try
                     (extension/register-extension!
                       {:ext/name        "test.slash-collide-b"
                        :ext/description "duplicate owner of /probe"
                        :ext/slash-commands
                        [{:slash/name   "probe"
                          :slash/doc    "probe dup"
                          :slash/run-fn (fn [_] {:slash/status :ok})}]})
                     nil
                     (catch clojure.lang.ExceptionInfo e
                       (ex-data e)))]
        (expect (= :extension/slash-path-collision (:type thrown)))
        (expect (= ["probe"] (-> thrown :collisions first :path))))
      ;; Hot-reload of the SAME id with the SAME path = allowed.
      (expect (some? (extension/register-extension!
                       {:ext/name        "test.slash-collide-a"
                        :ext/description "reload owner of /probe"
                        :ext/slash-commands
                        [{:slash/name   "probe"
                          :slash/doc    "probe reloaded"
                          :slash/run-fn (fn [_] {:slash/status :ok})}]})))
      (finally
        (extension/deregister-extension! "test.slash-collide-a")
        (extension/deregister-extension! "test.slash-collide-b"))))

  (it "uses the symbol-specific render-fn instead of dumping tool result data"
    (let [entry (extension/symbol #'demo-symbol-fn
                  {:symbol 'demo
                   :tag :observation
                   :render-fn (fn [_] {:summary [:ir {} [:p {} [:span {} "render-specific"]]]
                                       :display [:ir {} [:p {} [:span {} "render-specific"]]]})})
          ext   (extension/register-extension!
                  {:ext/name "test.renderer"
                   :ext/kind "test"
                   :ext/description "Test renderer."
                   :ext/sci {:ext.sci/ns 'test.renderer
                             :ext.sci/alias 'test.renderer
                             :ext.sci/symbols [entry]}})
          channel (atom [])]
      (try
        (binding [extension/*render-sink*    channel
                  extension/*sink-position*  (atom -1)]
          ((get (extension/wrap-extension ext {}) 'demo)))
        (expect (= {:summary [:ir {} [:p {} [:span {} "render-specific"]]]
                    :display [:ir {} [:p {} [:span {} "render-specific"]]]}
                  (-> @channel first :result)))
        (finally
          (extension/deregister-extension! "test.renderer")))))

  ;; Channel entries must carry `:form-idx` so the rebuild path can
  ;; partition the fence's render sink back onto per-form envelopes.
  ;; Without it, a multi-form fence (\"three v/ls calls in three
  ;; (def …) forms\") restores as one bubble whose first form glues
  ;; the IR for every tool call and the rest restore blank.
  (it "stamps `:form-idx` on every render sink entry from `*current-form-idx*`"
    (let [entry (extension/symbol #'demo-symbol-fn
                  {:symbol 'demo
                   :tag :observation
                   :render-fn (fn [_] {:summary [:ir {} [:p {} [:span {} "x"]]]
                                       :display [:ir {} [:p {} [:span {} "x"]]]})})
          ext   (extension/register-extension!
                  {:ext/name "test.form-idx-stamp"
                   :ext/kind "test"
                   :ext/description "Tests form-idx stamping."
                   :ext/sci {:ext.sci/ns 'test.form-idx-stamp
                             :ext.sci/alias 'test.form-idx-stamp
                             :ext.sci/symbols [entry]}})
          channel (atom [])]
      (try
        ;; Simulate run-sci-code: ONE channel atom, per-form binding
        ;; of *current-form-idx* across three sequential forms.
        (binding [extension/*render-sink*   channel
                  extension/*sink-position* (atom -1)]
          (binding [extension/*current-form-idx* 0]
            ((get (extension/wrap-extension ext {}) 'demo)))
          (binding [extension/*current-form-idx* 1]
            ((get (extension/wrap-extension ext {}) 'demo)))
          (binding [extension/*current-form-idx* 2]
            ((get (extension/wrap-extension ext {}) 'demo))))
        (expect (= [0 1 2] (mapv :form-idx @channel)))
        ;; And the entries still carry monotonic :position so multiple
        ;; calls within ONE form remain orderable.
        (expect (= [0 1 2] (mapv :position @channel)))
        (finally
          (extension/deregister-extension! "test.form-idx-stamp")))))

  (it "does not stamp `:form-idx` when `*current-form-idx*` is unbound (back-compat with callers outside run-sci-code)"
    (let [entry (extension/symbol #'demo-symbol-fn
                  {:symbol 'demo
                   :tag :observation
                   :render-fn (fn [_] {:summary [:ir {} [:p {} [:span {} "x"]]]
                                       :display [:ir {} [:p {} [:span {} "x"]]]})})
          ext   (extension/register-extension!
                  {:ext/name "test.form-idx-unbound"
                   :ext/kind "test"
                   :ext/description "Tests form-idx unbound path."
                   :ext/sci {:ext.sci/ns 'test.form-idx-unbound
                             :ext.sci/alias 'test.form-idx-unbound
                             :ext.sci/symbols [entry]}})
          channel (atom [])]
      (try
        (binding [extension/*render-sink*   channel
                  extension/*sink-position* (atom -1)]
          ((get (extension/wrap-extension ext {}) 'demo)))
        (expect (not (contains? (first @channel) :form-idx)))
        (finally
          (extension/deregister-extension! "test.form-idx-unbound"))))))
