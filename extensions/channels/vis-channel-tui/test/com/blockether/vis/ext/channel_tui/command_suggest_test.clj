(ns com.blockether.vis.ext.channel-tui.command-suggest-test
  (:require [com.blockether.vis.ext.channel-tui.command-suggest :as suggest]
            [lazytest.core :refer [defdescribe it expect]]))

(defdescribe
  slash-command-suggestions-test
  (let [commands [{:id :new-session :label "New Session"} {:id :new-tab :label "New Tab"}
                  {:id :worktree
                   :label "New Worktree"
                   :args [{:name "branch" :kind :positional :required false}]}
                  {:id :settings :label "Settings"}
                  {:id :voice/toggle-recording
                   :label "Voice: Toggle Recording"
                   :args [{:name "mode" :kind :positional :required false}
                          {:name "force" :kind :flag :type :boolean :required false}]}
                  {:id :exa/search
                   :label "Exa Search"
                   :args [{:name "query" :kind :positional :required true}
                          {:name "limit" :kind :flag :type :int :required false}]}]]
    (it "shows menu commands when the prompt starts with slash"
        (expect (= ["new-session" "new-tab" "worktree"]
                   (->> (suggest/suggestions "/" commands {:limit 3})
                        (mapv :slash/name)))))
    (it "fuzzy filters slash commands by typed token"
        (expect (= "new-tab" (:slash/name (first (suggest/suggestions "/nt" commands))))))
    (it "filters by full slash command names"
        (expect (= ["new-tab"] (mapv :slash/name (suggest/suggestions "/new-tab" commands)))))
    (it "renders extension command arguments in usage"
        (expect (= "/exa/search <query> [--limit <limit>]"
                   (:slash/usage (first (suggest/suggestions "/exa" commands))))))
    (it "renders built-in worktree branch argument in usage"
        (expect (= "/worktree [<branch>]"
                   (:slash/usage (first (suggest/suggestions "/work" commands))))))
    (it "tracks the selected suggestion for arrow keys and tab completion"
        (let [suggestions (suggest/suggestions "/" commands {:limit 3 :selected-index 1})]
          (expect (= "new-tab" (:slash/name (suggest/selected-suggestion suggestions))))
          (expect (= "/new-tab "
                     (suggest/completion-text (suggest/selected-suggestion suggestions))))
          (expect (= 2 (suggest/move-index 1 1 3)))
          ;; Clamps at the ends instead of wrapping around.
          (expect (= 0 (suggest/move-index 0 -1 3)))
          (expect (= 2 (suggest/move-index 2 1 3)))))
    (it "hides suggestions after tab completion inserts trailing space"
        (expect (nil? (suggest/suggestions "/new-tab " commands)))
        (expect (nil? (suggest/suggestions "/voice/toggle-recording on" commands))))
    (it "resolves only exact slash command invocations for execution"
        (expect (= {:name "new-tab" :args ""}
                   (let [cmd (suggest/exact-command "/new-tab" commands)]
                     {:name (:slash/name cmd) :args (:slash/args cmd)})))
        (expect (= {:name "voice/toggle-recording" :args "on --force"}
                   (let [cmd (suggest/exact-command "/voice/toggle-recording on --force" commands)]
                     {:name (:slash/name cmd) :args (:slash/args cmd)})))
        (expect (nil? (suggest/exact-command "/nt" commands))))))
