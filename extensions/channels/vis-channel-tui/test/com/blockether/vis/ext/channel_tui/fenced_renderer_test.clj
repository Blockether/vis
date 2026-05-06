(ns com.blockether.vis.ext.channel-tui.fenced-renderer-test
  (:require
   [clojure.string :as str]
   [com.blockether.vis.core :as vis]
   [com.blockether.vis.ext.channel-tui.render :as render]
   [lazytest.core :refer [defdescribe expect it]]))

(defdescribe tui-fenced-renderer-test
  (it "routes Markdown fenced code blocks through extension renderers"
    (let [ext-ns 'test.tui-fenced-renderer
          ext (vis/extension
                {:ext/namespace ext-ns
                 :ext/doc "TUI fenced renderer fixture."
                 :ext/fenced-renderers [{:renderer/id :test/tui-fence
                                         :renderer/langs #{"demo"}
                                         :renderer/render-fn (fn [{:keys [source width surface]}]
                                                               {:lines [(str "surface=" (name surface))
                                                                        (str "width=" width)
                                                                        (str "source=" source)]})}]})]
      (try
        (render/invalidate-cache!)
        (vis/register-extension! ext)
        (let [{:keys [lines]} (render/format-answer-markdown-data
                                "```demo\nhello\n```" 80)]
          (expect (some #(str/includes? % "surface=tui") lines))
          (expect (some #(str/includes? % "source=hello") lines))
          (expect (not-any? #(str/includes? % "```demo") lines)))
        (finally
          (render/invalidate-cache!)
          (vis/deregister-extension! ext-ns)))))

  (it "falls back to the normal code block renderer when no extension handles the language"
    (render/invalidate-cache!)
    (let [{:keys [lines]} (render/format-answer-markdown-data
                            "```unknown-lang\nhello\n```" 80)]
      (expect (some #(str/includes? % "hello") lines)))))
