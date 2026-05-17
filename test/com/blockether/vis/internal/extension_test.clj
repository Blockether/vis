(ns com.blockether.vis.internal.extension-test
  (:require
   [com.blockether.vis.internal.extension :as extension]
   [lazytest.core :refer [defdescribe expect it]]))

(defn- sample-channel-fn
  [& _]
  nil)

(defdescribe prompt-normalization-test
  (it "normalizes string and fn extension prompts"
    (let [prompt-text "\n\n    First line\n\n\n\n      Nested line\n"
          string-ext (extension/extension
                       {:ext/namespace 'test.prompt-string
                        :ext/doc "Test prompt string."
                        :ext/prompt prompt-text})
          fn-ext (extension/extension
                   {:ext/namespace 'test.prompt-fn
                    :ext/doc "Test prompt fn."
                    :ext/prompt (fn [_] prompt-text)})]
      (expect (= "First line\n\n  Nested line" ((:ext/prompt string-ext) {})))
      (expect (= "First line\n\n  Nested line" ((:ext/prompt fn-ext) {}))))))

(defdescribe channel-contributions-test
  (it "extension accepts channel contributions and derives channel kind"
    (let [ext (extension/extension
                {:ext/namespace 'test.channel-contribution
                 :ext/doc "Test channel contribution."
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
  (it "requires renderers for observed tool symbols"
    (extension/register-op! :test.missing-renderer/demo {:tag :op.tag/observation})
    (let [entry (extension/symbol
                  'demo
                  (fn [] (extension/success {:result {:secret "payload"}}))
                  {:doc "demo" :arglists '([])})]
      (expect (= :extension/missing-journal-renderer
                (try
                  (extension/extension
                    {:ext/namespace 'test.missing-renderer
                     :ext/kind "test"
                     :ext/doc "Test missing renderer."
                     :ext/alias {:ns 'test.missing-renderer :alias 'test.missing-renderer}
                     :ext/symbols [entry]})
                  nil
                  (catch clojure.lang.ExceptionInfo e
                    (:type (ex-data e))))))))

  (it "uses symbol-specific renderers instead of dumping tool result data"
    (extension/register-op! :test.renderer/demo {:tag :op.tag/observation})
    (let [entry (extension/symbol
                  'demo
                  (fn [] (extension/success {:result {:secret "payload"}}))
                  {:doc "demo"
                   :arglists '([])
                   :journal-render-fn (fn [_] "journal-specific")
                   :channel-render-fn (fn [_] [:ir {} [:p {} [:span {} "channel-specific"]]])})
          ext   (extension/register-extension!
                  {:ext/namespace 'test.renderer
                   :ext/kind "test"
                   :ext/doc "Test renderer."
                   :ext/alias {:ns 'test.renderer :alias 'test.renderer}
                   :ext/symbols [entry]})
          journal (atom [])
          channel (atom [])]
      (try
        (binding [extension/*journal-render-sink* journal
                  extension/*channel-render-sink* channel
                  extension/*sink-position* (atom -1)]
          ((get (extension/wrap-extension ext {}) 'demo)))
        (expect (= "journal-specific" (-> @journal first :result)))
        (expect (= [:ir {} [:p {} [:span {} "channel-specific"]]]
                  (-> @channel first :result)))
        (finally
          (extension/deregister-extension! 'test.renderer))))))
