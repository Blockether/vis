(ns com.blockether.vis.ext.channel-tui.drafts-test
  "The DRAFT transient's pure half: which rows and keys the band offers, and what
   one finished keystroke means. No terminal, no gateway — `dialogs_test` drives
   the same spec through a real virtual screen."
  (:require [lazytest.core :refer [defdescribe expect it]]
            [com.blockether.vis.ext.channel-tui.dialogs :as dlg]
            [com.blockether.vis.ext.channel-tui.drafts :as drafts]
            [com.blockether.vis.ext.channel-tui.transient :as tr]
            [com.blockether.vis.internal.foundation.workspace-slashes :as wss]))

(def ^:private sample
  [{"workspace_id" "ws-parked" "label" "feature-b" "root" "/tmp/b" "is_current" false}
   {"workspace_id" "ws-current" "label" "feature-a" "root" "/tmp/a" "is_current" true}])

(defn- by-key [spec] (into {} (map (juxt :key identity)) (mapcat :items (:groups spec))))

(defdescribe
  drafts-transient-test
  (it "rows put the current draft first and hand every row a reachable key"
      (let [rs (drafts/rows sample)]
        (expect (= ["feature-a" "feature-b"] (mapv :label rs)))
        (expect (= [true false] (mapv :is-current rs)))
        (expect (= ["a" "b"] (mapv :key rs)))
        ;; `t` is trunk in the switch band, so no draft may wear it.
        (expect (empty? (filter #{"t"} (map :key (drafts/rows (repeat 26 {}))))))
        ;; A row with no name is still a row you can point at.
        (expect (= ["Untitled draft"] (mapv :label (drafts/rows [{"workspace_id" "ws-x"}]))))))
  (it "creating, switching and abandoning are three COMMANDS, never one list"
      (let
        [spec
         (drafts/spec sample)

         items
         (mapcat :items (:groups spec))

         k
         (by-key spec)]

        (expect (= ["Commands"] (mapv :title (:groups spec))))
        ;; Nothing to arm and remember: `--clean` used to be a flag, so the band
        ;; could not say which working tree the next `n` would fork.
        (expect (= #{:action} (set (map :type items))))
        (expect (empty? (filter :arg items)))
        (expect (= "c" (tr/key-glyph (k "c"))))
        (expect (= [:new-clean :new-dirty :switch :abandon] (mapv :id items)))
        (expect (= ["c" "d" "s" "k"] (mapv :key items)))
        ;; `item-by-key` is what the running band resolves a keystroke with.
        (expect (= :abandon (:id (tr/item-by-key spec \k))))))
  (it "with no drafts there is nowhere to switch to and nothing to abandon"
      (let [spec (drafts/spec [])]
        (expect (= [:new-clean :new-dirty] (mapv :id (mapcat :items (:groups spec)))))
        (expect (nil? (tr/item-by-key spec \s)))
        (expect (nil? (tr/item-by-key spec \k)))))
  (it "a finished command means exactly what the band does next"
      (expect (= {:action :new :clean? true} (drafts/choice {:action :new-clean})))
      (expect (= {:action :new :clean? false} (drafts/choice {:action :new-dirty})))
      (expect (= {:action :switch} (drafts/choice {:action :switch})))
      (expect (= {:action :abandon} (drafts/choice {:action :abandon})))
      ;; Esc.
      (expect (nil? (drafts/choice nil))))
  (it "the switch band is the LIST: trunk first, the workspace we are in marked ●"
      (let
        [spec
         (drafts/switch-spec sample)

         k
         (by-key spec)]

        (expect (= ["Switch to"] (mapv :title (:groups spec))))
        (expect (= [:trunk [:draft "ws-current"] [:draft "ws-parked"]]
                   (mapv :id (mapcat :items (:groups spec)))))
        ;; ONE status vocabulary with every other choice row in the TUI: an
        ;; EXCLUSIVE ●/○, never the inclusive `[✓]` box.
        (expect (= (str (dlg/choice-mark true true) "feature-a") (:label (k "a"))))
        (expect (= (str (dlg/choice-mark true false) "feature-b") (:label (k "b"))))
        (expect (= (str (dlg/choice-mark true false) "Trunk") (:label (k "t"))))
        (expect (= (str (dlg/choice-mark true true) "Trunk")
                   (:label (tr/item-by-key (drafts/switch-spec []) \t))))))
  (it "a finished switch means exactly what the screen's draft executor speaks"
      (expect (= {:action :draft :workspace-id "ws-parked" :label "feature-b" :current? false}
                 (drafts/switch-choice sample {:action [:draft "ws-parked"]})))
      (expect (= {:action :trunk :label "Trunk" :current? false}
                 (drafts/switch-choice sample {:action :trunk})))
      (expect (= {:action :trunk :label "Trunk" :current? true}
                 (drafts/switch-choice [] {:action :trunk})))
      ;; Esc, and a draft abandoned by someone else while the band was open.
      (expect (nil? (drafts/switch-choice sample nil)))
      (expect (nil? (drafts/switch-choice sample {:action [:draft "ws-gone"]}))))
  (it "abandoning reuses each draft's own band key"
      (expect (= [{:key \a :label "feature-a" :id "ws-current"}
                  {:key \b :label "feature-b" :id "ws-parked"}]
                 (drafts/abandon-choices sample))))
  (it "the start-in transient speaks the same vocabulary as the draft band"
      (let [k (by-key drafts/start-in-spec)]
        (expect (= #{:action} (set (map :type (mapcat :items (:groups drafts/start-in-spec))))))
        (expect (= [:trunk :new-clean :new-dirty] [(:id (k "t")) (:id (k "c")) (:id (k "d"))]))
        (expect (= {:start-in :trunk} (drafts/start-in-choice {:action :trunk})))
        (expect (= {:start-in :draft :clean? true} (drafts/start-in-choice {:action :new-clean})))
        (expect (= {:start-in :draft :clean? false} (drafts/start-in-choice {:action :new-dirty})))
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
      (expect (nil? (drafts/draft-spec {:start-in :draft} nil))))
  (it "every `/draft …` question is answered by the band, not by a modal prompt"
      ;; `/draft new` popped a text-input window for the label; it IS the band's
      ;; own `d` key now, pre-pressed, and `/draft` itself is the band.
      (expect (= {:pressed nil} (drafts/slash-band ["draft"])))
      (expect (= {:pressed :new-dirty} (drafts/slash-band ["draft" "new"])))
      (expect (= {:pressed :new-clean} (drafts/slash-band ["draft" "clean"])))
      (expect (= {:pressed :switch} (drafts/slash-band ["draft" "resume"])))
      (expect (= {:pressed :switch} (drafts/slash-band ["draft" "list"])))
      (expect (= {:pressed :abandon} (drafts/slash-band ["draft" "abandon"])))
      ;; A line that already carries its own answer runs as the engine slash it
      ;; is, and so do the two verbs with nothing to ask.
      (expect (nil? (drafts/slash-band ["draft" "new" "wire-rework"])))
      (expect (nil? (drafts/slash-band ["draft" "resume" "wire-rework"])))
      (expect (nil? (drafts/slash-band ["draft" "apply"])))
      (expect (nil? (drafts/slash-band ["draft" "stash"])))
      (expect (nil? (drafts/slash-band ["export"])))
      ;; Every command a slash names is a command the band actually offers.
      (expect (every? #(tr/item-by-id (drafts/spec sample) (:pressed (drafts/slash-band %)))
                      [["draft" "new"] ["draft" "clean"] ["draft" "resume"] ["draft" "abandon"]]))))

(defdescribe draft-slash-coverage-test
             (it "the band answers every `/draft` verb the ENGINE offers, or says why not"
                 ;; The drift guard: `slash-band` is a table, and a table beside another
                 ;; table rots. `/draft`'s own usage line in the engine is the list of verbs
                 ;; a human can type, so a verb added there and not here fails HERE instead
                 ;; of silently falling back to the modal path.
                 (let
                   [usage
                    (->> wss/specs
                         (filter #(= "draft" (:slash/name %)))
                         first
                         :slash/usage)

                    verbs
                    (->> (re-find #"<(.*)>" usage)
                         second
                         (re-seq #"[a-z]+")
                         (remove #{"label"})
                         set)]

                   (expect (= #{"new" "clean" "apply" "stash" "resume" "list" "abandon"} verbs))
                   ;; `apply` and `stash` ask nothing — they carry their own answer and stay
                   ;; engine slashes. Everything else is a question, so it is a band.
                   (expect (= #{"apply" "stash"}
                              (set (remove #(:pressed (drafts/slash-band ["draft" %])) verbs)))))))
