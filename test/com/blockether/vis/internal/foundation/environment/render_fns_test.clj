(ns com.blockether.vis.internal.foundation.environment.render-fns-test
  "Contract tests for the environment extension `:render-fn`s. Every render-fn
   must return the `{:summary :display}` contract (`::render-fn-result`): the
   summary is a labelled zone map and the display is a non-empty `[:ir ...]`."
  (:require
   [com.blockether.vis.internal.foundation.environment.core :as env-core]
   [com.blockether.vis.internal.extension :as extension]
   [lazytest.core :refer [defdescribe describe expect it]]))

(defn- render-fn-for
  "Look up the registered `:render-fn` for the environment symbol named `sym`."
  [sym]
  (some (fn [entry]
          (when (= sym (:ext.symbol/symbol entry))
            (:ext.symbol/render-fn entry)))
    env-core/environment-symbols))

(def ^:private snapshot-result
  {:host         {:cwd "/tmp/proj" :user "alice"}
   :git          {:root "/tmp/proj" :branch "main" :clean? true
                  :modified 0 :untracked 0 :ahead 0 :behind 0}
   :languages    {:primary "Clojure" :total-files 12 :total-bytes 4096
                  :languages [{:language "Clojure" :files 10 :bytes-pct 90.0}
                              {:language "Markdown" :files 2 :bytes-pct 10.0}]}
   :monorepo     {:shape :polylith}
   :repositories {:count 2
                  :repositories [{:path "/tmp/proj/a" :branch "main" :clean? true}
                                 {:path "/tmp/proj/b" :branch "dev" :dirty? true}]}})

(defn- expect-contract
  "Assert `result` conforms to the render contract with a non-empty,
   zone-shaped summary and an `[:ir ...]` display."
  [result]
  (expect (extension/render-fn-result? result))
  (expect (some? (:summary result)))
  (expect (some? (:display result)))
  (expect (extension/render-zones? (:summary result)))
  (expect (= :ir (first (:display result))))
  ;; display carries at least the lead summary paragraph block
  (expect (seq (drop 2 (:display result)))))

(defdescribe environment-render-fns-test
  (describe "refresh render-fn"
    (it "returns the {:summary :display} contract"
      (let [f      (render-fn-for 'refresh!)
            result (f snapshot-result)]
        (expect (fn? f))
        (expect-contract result)
        ;; right zone anchors the fact count
        (expect (= "5 facts" (get-in result [:summary :right]))))))

  (describe "languages render-fn"
    (it "returns the {:summary :display} contract with primary + file count"
      (let [f      (render-fn-for 'languages)
            result (f (:languages snapshot-result))]
        (expect (fn? f))
        (expect-contract result)
        (expect (= "12 files" (get-in result [:summary :right])))
        ;; primary language painted in the center zone as inline code
        (expect (some? (get-in result [:summary :center])))))

    (it "stays contract-valid when the language scan is unavailable (nil)"
      (let [f      (render-fn-for 'languages)
            result (f nil)]
        (expect (extension/render-fn-result? result))
        ;; no primary → no center zone (nil optional zones are dropped)
        (expect (not (contains? (:summary result) :center))))))

  (describe "monorepo render-fn"
    (it "returns the {:summary :display} contract with the shape on the right"
      (let [f      (render-fn-for 'monorepo)
            result (f (:monorepo snapshot-result))]
        (expect (fn? f))
        (expect-contract result)
        (expect (= "polylith" (get-in result [:summary :right])))))

    (it "reports none when no monorepo shape is detected"
      (let [f      (render-fn-for 'monorepo)
            result (f {:shape nil})]
        (expect (extension/render-fn-result? result))
        (expect (= "none" (get-in result [:summary :right]))))))

  (describe "repositories render-fn"
    (it "returns the {:summary :display} contract with the repo count"
      (let [f      (render-fn-for 'repositories)
            result (f (:repositories snapshot-result))]
        (expect (fn? f))
        (expect-contract result)
        (expect (= "2 repos" (get-in result [:summary :right])))
        ;; each repo contributes a line in the display body
        (expect (re-find #"/tmp/proj/a" (pr-str (:display result)))))))

  (describe "guidance render-fn"
    (it "returns the {:summary :display} contract when guidance is found"
      (let [f      (render-fn-for 'main-agent-instructions)
            result (f {:found? true :source :repo :path "AGENTS.md"
                       :content "rule one\nrule two\nrule three"})]
        (expect (fn? f))
        (expect-contract result)
        (expect (= "3 lines" (get-in result [:summary :right])))
        ;; the path is painted in the center zone
        (expect (some? (get-in result [:summary :center])))
        ;; the content is embedded as a code block in the display
        (expect (re-find #"rule one" (pr-str (:display result))))))

    (it "stays contract-valid with a NO GUIDANCE label when not found"
      (let [f      (render-fn-for 'main-agent-instructions)
            result (f {:found? false})]
        (expect (extension/render-fn-result? result))
        (expect (some? (:summary result)))
        (expect (some? (:display result)))
        ;; not-found → no center/right zones, only the label
        (expect (not (contains? (:summary result) :center)))
        (expect (not (contains? (:summary result) :right)))
        (expect (re-find #"NO GUIDANCE" (pr-str (:summary result))))))))
