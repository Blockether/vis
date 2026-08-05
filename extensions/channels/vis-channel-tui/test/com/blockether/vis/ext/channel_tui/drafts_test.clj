(ns com.blockether.vis.ext.channel-tui.drafts-test
  "The DRAFT transient's pure half: which rows and keys the band offers, and what
   one finished keystroke means. No terminal, no gateway — `dialogs_test` drives
   the same spec through a real virtual screen."
  (:require [lazytest.core :refer [defdescribe expect it]]
            [com.blockether.vis.ext.channel-tui.dialogs :as dlg]
            [com.blockether.vis.ext.channel-tui.drafts :as drafts]
            [com.blockether.vis.ext.channel-tui.transient :as tr]))

(def ^:private sample
  [{"workspace_id" "ws-parked" "label" "feature-b" "root" "/tmp/b" "is_current" false}
   {"workspace_id" "ws-current" "label" "feature-a" "root" "/tmp/a" "is_current" true}])

(defdescribe
  drafts-transient-test
  (it "rows put the current draft first and hand every row a reachable key"
      (let [rs (drafts/rows sample)]
        (expect (= ["feature-a" "feature-b"] (mapv :label rs)))
        (expect (= [true false] (mapv :is-current rs)))
        (expect (= ["a" "b"] (mapv :key rs)))
        ;; The band's own verbs are never handed to a draft: `t`runk, `-c`lean,
        ;; `n`ew and abandon (`k`) mean the same thing however many drafts exist.
        (expect (empty? (filter #{"t" "c" "n" "k"} (map :key (drafts/rows (repeat 26 {}))))))
        ;; A row with no name is still a row you can point at.
        (expect (= ["Untitled draft"] (mapv :label (drafts/rows [{"workspace_id" "ws-x"}]))))))
  (it "the spec is a magit transient: switch targets are COMMANDS, `-c` is the only FLAG"
      (let
        [spec
         (drafts/spec sample)

         items
         (mapcat :items (:groups spec))

         by-key
         (into {} (map (juxt :key identity)) items)]

        (expect (= ["Switch to" "Actions"] (mapv :title (:groups spec))))
        (expect (= #{:action :switch} (set (map :type items))))
        (expect (= :switch (:type (by-key "c"))))
        (expect (= "--clean" (:arg (by-key "c"))))
        ;; Magit's own glyphs: a flag wears the leading `-`, a command does not.
        (expect (= "-c" (tr/key-glyph (by-key "c"))))
        (expect (= "n" (tr/key-glyph (by-key "n"))))
        (expect (= [:draft "ws-current"] (:id (by-key "a"))))
        ;; ONE status vocabulary with every other choice row in the TUI: an
        ;; EXCLUSIVE ●/○, never the inclusive `[✓]` box.
        (expect (= (str (dlg/choice-mark true true) "feature-a") (:label (by-key "a"))))
        (expect (= (str (dlg/choice-mark true false) "feature-b") (:label (by-key "b"))))
        (expect (= (str (dlg/choice-mark true false) "Trunk") (:label (by-key "t"))))
        ;; `item-by-key` is what the running band resolves a keystroke with.
        (expect (= :abandon (:id (tr/item-by-key spec \k))))))
  (it "with no drafts there is nothing to abandon and trunk is where we are"
      (let [spec (drafts/spec [])]
        (expect (= ["Switch to" "Actions"] (mapv :title (:groups spec))))
        (expect (nil? (tr/item-by-key spec \k)))
        (expect (= (str (dlg/choice-mark true true) "Trunk") (:label (tr/item-by-key spec \t))))
        (expect (= {:action :trunk :label "Trunk" :current? true}
                   (drafts/choice [] {:action :trunk})))))
  (it "a finished run means exactly what the screen's draft executor speaks"
      (expect (= {:action :draft :workspace-id "ws-parked" :label "feature-b" :current? false}
                 (drafts/choice sample {:action [:draft "ws-parked"]})))
      (expect (= {:action :trunk :label "Trunk" :current? false}
                 (drafts/choice sample {:action :trunk})))
      ;; The `-c` flag rides along with the create command.
      (expect (= {:action :new :clean? false} (drafts/choice sample {:action :new})))
      (expect (= {:action :new :clean? true}
                 (drafts/choice sample {:action :new :switches #{:clean}})))
      (expect (= {:action :abandon} (drafts/choice sample {:action :abandon})))
      ;; Esc, and a draft abandoned by someone else while the band was open.
      (expect (nil? (drafts/choice sample nil)))
      (expect (nil? (drafts/choice sample {:action [:draft "ws-gone"]}))))
  (it "abandoning reuses each draft's own band key"
      (expect (= [{:key \a :label "feature-a" :id "ws-current"}
                  {:key \b :label "feature-b" :id "ws-parked"}]
                 (drafts/abandon-choices sample))))
  (it "the start-in transient offers the project itself, a draft, and the clean flag"
      (let
        [by-key (into {} (map (juxt :key identity)) (mapcat :items (:groups drafts/start-in-spec)))]
        (expect (= :switch (:type (by-key "c"))))
        (expect (= "--clean" (:arg (by-key "c"))))
        (expect (= [:trunk :draft] [(:id (by-key "t")) (:id (by-key "d"))]))
        (expect (= {:start-in :trunk} (drafts/start-in-choice {:action :trunk})))
        (expect (= {:start-in :draft :clean? false} (drafts/start-in-choice {:action :draft})))
        (expect (= {:start-in :draft :clean? true}
                   (drafts/start-in-choice {:action :draft :switches #{:clean}})))
        (expect (nil? (drafts/start-in-choice nil)))))
  (it "a draft is only forked when it was actually named"
      ;; Trunk never forks, whatever was typed; an empty name is a cancelled
      ;; prompt, never an unnamed draft.
      (expect (nil? (drafts/draft-spec {:start-in :trunk} "anything")))
      (expect (= {:label "wire-rework" :clean? false}
                 (drafts/draft-spec {:start-in :draft} "  wire-rework  ")))
      (expect (= {:label "wire-rework" :clean? true}
                 (drafts/draft-spec {:start-in :draft :clean? true} "wire-rework")))
      (expect (nil? (drafts/draft-spec {:start-in :draft} "   ")))
      (expect (nil? (drafts/draft-spec {:start-in :draft} nil)))))
