(ns com.blockether.vis.internal.extension-test
  (:require
   [com.blockether.vis.internal.extension :as extension]
   [com.blockether.vis.internal.workspace :as workspace]
   [lazytest.core :refer [defdescribe expect it]]))

(defn- sample-channel-fn
  [& _]
  nil)

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
    (extension/register-op! :test.missing-renderer/demo {:tag :observation})
    (let [entry (extension/symbol
                  'demo
                  (fn [] (extension/success {:result {:secret "payload"}}))
                  {:doc "demo" :arglists '([])})]
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

  (it "uses the symbol-specific render-fn instead of dumping tool result data"
    (extension/register-op! :test.renderer/demo {:tag :observation})
    (let [entry (extension/symbol
                  'demo
                  (fn [] (extension/success {:result {:secret "payload"}}))
                  {:doc "demo"
                   :arglists '([])
                   :render-fn (fn [_] [:ir {} [:p {} [:span {} "render-specific"]]])})
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
        (expect (= [:ir {} [:p {} [:span {} "render-specific"]]]
                  (-> @channel first :result)))
        (finally
          (extension/deregister-extension! "test.renderer"))))))
