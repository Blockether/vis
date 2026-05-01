(ns com.blockether.vis.internal.file-picker-test
  (:require [com.blockether.vis.internal.file-picker :as picker]
            [lazytest.core :refer [defdescribe expect it]]))

(def ^:private sample-entries
  [{:path "src/com/blockether/vis/ext/channel_tui/footer.clj"
    :name "footer.clj"
    :parent "src/com/blockether/vis/ext/channel_tui"
    :size  14336
    :mtime-ms 1000
    :git-status :modified
    :ignored? false}
   {:path "docs/src/usage.md"
    :name "usage.md"
    :parent "docs/src"
    :size  8192
    :mtime-ms 3000
    :git-status nil
    :ignored? false}
   {:path "scratch/generated/output.txt"
    :name "output.txt"
    :parent "scratch/generated"
    :size  2048
    :mtime-ms 4000
    :git-status :ignored
    :ignored? true}
   {:path "extensions/channels/vis-channel-tui/src/com/blockether/vis/ext/channel_tui/input.clj"
    :name "input.clj"
    :parent "extensions/channels/vis-channel-tui/src/com/blockether/vis/ext/channel_tui"
    :size  9216
    :mtime-ms 2000
    :git-status :untracked
    :ignored? false}])

(defdescribe file-picker-score-test
  (it "prefers exact and filename-prefix matches over broad path contains"
    (expect (> (picker/file-picker-score "src/input.clj" "input")
              (picker/file-picker-score "src/not-inputish/footer.clj" "input")))
    (expect (= 1000
              (picker/file-picker-score "src/input.clj" "src/input.clj")))))

(defdescribe resolved-sort-mode-test
  (it "auto becomes recent for blank query and relevance for typed query"
    (expect (= :recent (picker/resolved-sort-mode :auto "")))
    (expect (= :relevance (picker/resolved-sort-mode :auto "foo"))))

  (it "explicit relevance with blank query falls back to path ordering"
    (expect (= :path (picker/resolved-sort-mode :relevance "")))))

(defdescribe file-picker-items-test
  (it "blank query defaults to recent-first and hides ignored files"
    (let [items (picker/file-picker-items sample-entries ""
                  {:sort-mode :auto :include-ignored? false :now-ms 5000})]
      (expect (= ["docs/src/usage.md"
                  "extensions/channels/vis-channel-tui/src/com/blockether/vis/ext/channel_tui/input.clj"
                  "src/com/blockether/vis/ext/channel_tui/footer.clj"]
                (mapv :path items)))))

  (it "ignored toggle surfaces ignored files"
    (let [items (picker/file-picker-items sample-entries ""
                  {:sort-mode :auto :include-ignored? true :now-ms 5000})]
      (expect (= "scratch/generated/output.txt"
                (:path (first items))))
      (expect (= "I" (:status-label (first items))))))

  (it "typed query switches to relevance-first, with recency as a tiebreak"
    (let [items (picker/file-picker-items sample-entries "input"
                  {:sort-mode :auto :include-ignored? false :now-ms 5000})]
      (expect (= ["extensions/channels/vis-channel-tui/src/com/blockether/vis/ext/channel_tui/input.clj"
                  "src/com/blockether/vis/ext/channel_tui/footer.clj"]
                (mapv :path items)))))

  (it "decorates entries with compact size + age labels"
    (let [item (first (picker/file-picker-items sample-entries "footer"
                       {:sort-mode :auto :include-ignored? false :now-ms 61000}))]
      (expect (= "14.0K" (:size-label item)))
      (expect (= "1m" (:age-label item))))))

(defdescribe sort-cycle-test
  (it "cycles auto -> recent -> relevance -> auto"
    (expect (= :recent (picker/cycle-sort-mode :auto)))
    (expect (= :relevance (picker/cycle-sort-mode :recent)))
    (expect (= :auto (picker/cycle-sort-mode :relevance)))))

(defdescribe ignored-path-test
  (it "matches exact ignored paths and descendants under ignored directories"
    (let [snapshot {:ignored-exact #{"target" "scratch/output.txt"}
                    :ignored-prefixes ["target/" "scratch/output.txt/"]}]
      (expect (true? (picker/ignored-path? snapshot "target")))
      (expect (true? (picker/ignored-path? snapshot "target/classes/Foo.class")))
      (expect (true? (picker/ignored-path? snapshot "scratch/output.txt")))
      (expect (false? (picker/ignored-path? snapshot "src/core.clj"))))))
