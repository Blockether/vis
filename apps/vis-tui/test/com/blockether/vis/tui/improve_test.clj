(ns com.blockether.vis.tui.improve-test
  (:require [clojure.string :as str]
            [com.blockether.vis.tui.capture :as cap]
            [com.blockether.vis.tui.client :as vis]
            [com.blockether.vis.tui.improve :as improve]
            [com.blockether.vis.tui.keymap :as keymap]
            [com.blockether.vis.tui.dialogs :as dlg]
            [com.blockether.vis.tui.screen :as screen]
            [com.blockether.vis.tui.state :as state]
            [lazytest.core :refer [defdescribe expect it]])
  (:import [com.googlecode.lanterna TerminalPosition]
           [com.googlecode.lanterna.input KeyStroke KeyType]))

(def payload
  "One register with two projects, a grouped chain, a closed child and an issue
   nobody filed under a project."
  {"records" [{"id" 1
               "project_id" "p1"
               "title" "Slow startup"
               "content" "It drags on a cold cache."
               "status" "open"
               "session_id" "session-1"
               "created_at" "2026-01-02"}
              {"id" 2 "project_id" "p1" "title" "Cold cache" "status" "open" "parent_id" 1}
              {"id" 3 "project_id" "p1" "title" "Stale index" "status" "closed" "parent_id" 2}
              {"id" 4 "project_id" "p2" "title" "Typo in help" "status" "closed"}
              {"id" 5 "title" "" "status" "open"}]
   "projects" [{"id" "p1" "name" "Editor"} {"id" "p2" "name" "Docs"}]})

(def records (improve/records payload))

(def projects (improve/project-names payload))

(defn- paint-text
  [component cols rows]
  (cap/frame-text (cap/capture! {:cols cols
                                 :rows rows
                                 :paint! (fn [{:keys [g]}]
                                           (let [state
                                                 (:init component)

                                                 geom
                                                 ((:measure component) state cols rows)

                                                 state
                                                 ((:reconcile component) state geom)]

                                             ((:paint component) g state geom)))})))

(defn- press
  [component state key]
  ((:on-key component) state key ((:measure component) state 96 24)))

(defn- done [result] (:com.blockether.vis.tui.dialogs/done result))

(defdescribe
  improve-mode-is-off-until-the-gateway-says-otherwise-test
  (it "an unavailable or unknown settings document is Off, never a half-open surface"
      (expect (= :off (improve/mode nil)))
      (expect (= :off (improve/mode {"mode" "sometimes"})))
      (expect (= :human (improve/mode {"mode" "human"})))
      (expect (= :automatic (improve/mode {"mode" "AUTOMATIC"})))
      (expect (false? (improve/enabled? nil)))
      (expect (true? (improve/enabled? {"mode" "human"})))
      (expect (false? (improve/automatic? {"mode" "human"})))
      (expect (true? (improve/automatic? {"mode" "automatic"}))))
  (it "normalization is idempotent, so app-db can hold its own result"
      (let [once
            (improve/settings
              {"mode" "automatic" "provider" "anthropic" "model" "claude" "interval_minutes" 30})]
        (expect (= {:mode :automatic :provider "anthropic" :model "claude" :interval-minutes 30}
                   once))
        (expect (= once (improve/settings once)))
        (expect (= "anthropic / claude" (improve/model-label once)))
        (expect (= "30 minutes" (improve/interval-label once)))
        (expect (= "not chosen" (improve/model-label {"mode" "automatic"})))
        (expect (= "not set" (improve/interval-label {"mode" "automatic"}))))))

(defdescribe improve-records-normalize-the-wire-document-test
             (it "improve records normalize the wire document"
                 (expect (= [1 2 3 4 5] (mapv :id records)))
                 (expect (= [:open :open :closed :closed :open] (mapv :status records)))
                 (expect (= "(untitled)" (:title (last records))))
                 (expect (= {"p1" "Editor" "p2" "Docs"} projects))
                 (expect (= "No project" (improve/project-label projects nil)))
                 (expect (= "Project p9" (improve/project-label projects "p9")))
                 ;; a bare list is a register too
                 (expect (= ["Only one"]
                            (mapv :title (improve/records [{"id" 9 "title" "Only one"}]))))))

(defdescribe
  improve-browser-groups-projects-and-parents-test
  (it "improve browser groups projects and parents"
      (let [rows
            (improve/browser-rows records projects)

            kinds
            (mapv :kind rows)

            labels
            (mapv :label rows)]

        ;; one header per project, its issues under it, unfiled work last
        (expect (= [:project :issue :project :issue :issue :issue :project :issue] kinds))
        (expect (= "Docs · 0 open of 1" (first labels)))
        (expect (= "Editor · 2 open of 3" (nth labels 2)))
        (expect (= "No project · 1 open of 1" (nth labels 6)))
        ;; children are indented under the parent they were grouped under
        (expect (= [0 0 1 2 0] (mapv :depth (filterv #(= :issue (:kind %)) rows))))
        (expect (str/includes? (nth labels 3) "• Slow startup"))
        (expect (str/includes? (nth labels 4) "  • Cold cache"))
        (expect (str/includes? (nth labels 5) "    ✓ Stale index"))
        (expect (str/includes? (nth labels 3) "  +1"))
        ;; an empty register says so instead of painting nothing
        (expect (= [{:kind :notice
                     :label "Nothing recorded yet — press n to write the first improvement"}]
                   (improve/browser-display [] {} nil)))
        (expect (= "Improve register unavailable"
                   (:label (first
                             (improve/browser-display [] {} "Improve register unavailable"))))))))

(defdescribe improve-closing-cascades-and-reopening-stays-safe-test
             (it "closing names every descendant still open, and skips the ones already closed"
                 (let [{:keys [ids descendant-count title]} (improve/close-plan records 1)]
                   (expect (= [1 2] ids))
                   (expect (= 1 descendant-count))
                   (expect (= "Slow startup" title))))
             (it "a cycle cannot hang the confirmation"
                 (let [looped (improve/records [{"id" 1 "parent_id" 2 "title" "a"}
                                                {"id" 2 "parent_id" 1 "title" "b"}])]
                   (expect (= #{1 2} (improve/descendant-ids looped 1)))))
             (it "reopening lifts the closed parents that hide the issue, never a closed descendant"
                 (let [closed
                       (improve/records
                         [{"id" 1 "title" "root" "status" "closed"}
                          {"id" 2 "title" "mid" "status" "closed" "parent_id" 1}
                          {"id" 3 "title" "leaf" "status" "closed" "parent_id" 2}
                          {"id" 4 "title" "other leaf" "status" "closed" "parent_id" 3}])

                       {:keys [ids parent-count]}
                       (improve/reopen-plan closed 3)]

                   (expect (= [3 2 1] ids))
                   (expect (= 2 parent-count))
                   (expect (not (contains? (set ids) 4))))))

(defdescribe improve-grouping-stays-inside-one-project-test
             (it "improve grouping stays inside one project"
                 (let [child
                       (second records)

                       candidates
                       (improve/parent-candidates records child)]

                   (expect (= [1] (mapv :id candidates)))
                   ;; never itself, never its own descendant, never another project
                   (expect (empty? (improve/parent-candidates records (first records))))
                   ;; the chooser always offers a way back to the top of the project
                   (let [items (improve/parent-items records child)]
                     (expect (= [nil 1] (mapv :parent-id items)))
                     (expect (str/includes? (:label (first items)) "no parent"))
                     (expect (= "#1" (:hint (second items))))))))

(defdescribe improve-detail-reads-as-markdown-test
             (it "improve detail reads as markdown"
                 (let [md (improve/detail-markdown (first records) "Editor")]
                   (expect (str/starts-with? md "# Slow startup"))
                   (expect (str/includes? md "It drags on a cold cache."))
                   (expect (str/includes? md "- Project: Editor"))
                   (expect (str/includes? md "- Session: session-1"))
                   (expect (str/includes? md "- Recorded: 2026-01-02"))
                   (expect (= "Improve #1 · open" (improve/detail-title (first records)))))
                 ;; a record without analysis says so rather than painting an empty page
                 (expect (str/includes?
                           (improve/detail-markdown (improve/record {"id" 7 "title" "Empty"}) nil)
                           "_No analysis written yet._"))))

(defdescribe
  improve-browser-component-paints-and-answers-keys-test
  (it
    "improve browser component paints and answers keys"
    (let [component
          (improve/browser-modal-component records projects nil {:mode :human})

          text
          (paint-text component 96 24)]

      ;; the register paints its mode, its projects and its issues
      (expect (str/includes? text "Improve · Governed by human"))
      (expect (str/includes? text "Editor · 2 open of 3"))
      (expect (str/includes? text "Slow startup"))
      (expect (str/includes? text "Stale index"))
      ;; every state row survives a narrow terminal
      (expect (str/includes? (paint-text component 60 20) "Improve · Governed by human"))
      (let [closed-row
            (:init component)

            open-row
            (assoc (:init component) :selected 1)]

        ;; closing is offered on an open issue only, reopening on a closed one only
        (expect (nil? (done (press component closed-row (KeyStroke. \c false false)))))
        (expect (= {:action :reopen :row (nth records 3)}
                   (done (press component closed-row (KeyStroke. \r false false)))))
        (expect (= {:action :close :row (first records)}
                   (done (press component open-row (KeyStroke. \c false false)))))
        (expect (nil? (done (press component open-row (KeyStroke. \r false false)))))
        ;; reading, editing, grouping, recording and the mode chooser are all one key
        (expect (= :read (:action (done (press component open-row (KeyStroke. KeyType/Enter))))))
        (expect (= :edit (:action (done (press component open-row (KeyStroke. \e false false))))))
        (expect (= :group (:action (done (press component open-row (KeyStroke. \g false false))))))
        (expect (= :new (:action (done (press component open-row (KeyStroke. \n false false))))))
        (expect (= {:action :settings}
                   (done (press component open-row (KeyStroke. \s false false)))))
        ;; Esc closes and moving the cursor writes nothing
        (expect (contains? (press component open-row (KeyStroke. KeyType/Escape))
                           :com.blockether.vis.tui.dialogs/done))
        (expect (= 2 (:selected (press component open-row (KeyStroke. KeyType/ArrowDown)))))))))

(defdescribe
  improve-browser-pages-over-project-headings-test
  (it "improve browser pages over project headings"
      (let [many-records
            (improve/records (mapv (fn [idx]
                                     {"id" idx "project_id" "p1" "title" (str "Issue " idx)})
                                   (range 50)))

            component
            (improve/browser-modal-component many-records {"p1" "Editor"} nil {:mode :human})

            geom
            ((:measure component) (:init component) 96 24)

            step
            (fn [state key]
              ((:reconcile component) ((:on-key component) state (KeyStroke. key) geom) geom))

            start
            ((:reconcile component) (:init component) geom)

            down
            (step start KeyType/PageDown)

            back
            (step down KeyType/PageUp)]

        (expect (<= (dec (:list-h geom)) (:selected down)))
        (expect (= 0 (:selected back))))))

(defdescribe
  improve-settings-component-gates-model-calls-test
  (it "improve settings component gates model calls"
      ;; provider, model, schedule and review exist in Automatic only
      (expect (= [:mode :mode :mode] (mapv :kind (improve/settings-rows {:mode :human}))))
      (expect (= [:mode :mode :mode :model :interval :review]
                 (mapv :kind (improve/settings-rows {:mode :automatic}))))
      (let [component
            (improve/settings-modal-component
              {:mode :automatic :provider "anthropic" :model "claude" :interval-minutes 30})

            text
            (paint-text component 96 24)]

        (expect (str/includes? text "Improve mode"))
        (expect (str/includes? text "● Automatic"))
        (expect (str/includes? text "○ Governed by human"))
        (expect (str/includes? text "Model · anthropic / claude"))
        (expect (str/includes? text "Reviews every 30 minutes"))
        (expect (str/includes? text "Review now"))
        ;; each row answers an intent; the caller owns every write
        (expect (= {:action :set-mode :mode :off}
                   (done (press component (:init component) (KeyStroke. KeyType/Enter)))))
        (expect (= {:action :pick-model}
                   (done (press component
                                (assoc (:init component) :selected 3)
                                (KeyStroke. KeyType/Enter)))))
        (expect (= {:action :set-interval}
                   (done (press component
                                (assoc (:init component) :selected 4)
                                (KeyStroke. KeyType/Enter)))))
        (expect (= {:action :review}
                   (done (press component
                                (assoc (:init component) :selected 5)
                                (KeyStroke. KeyType/Enter))))))))

(defdescribe improve-reads-the-register-through-the-facade-test
             (it "an empty register is not a failure"
                 (with-redefs [vis/improve-records (constantly {"records" []})]
                   (expect (= {:records [] :projects {}} (improve/fetch-register!)))))
             (it "a register the daemon cannot answer is UNAVAILABLE, painted differently"
                 (with-redefs [vis/improve-records (constantly nil)]
                   (expect (= "Improve register unavailable" (:error (improve/fetch-register!))))))
             (it "settings the daemon cannot answer stay Off"
                 (with-redefs [vis/improve-settings (constantly nil)]
                   (expect (= :off (:mode (improve/fetch-settings!)))))
                 (with-redefs [vis/improve-settings (constantly {"mode" "human"})]
                   (expect (= :human (:mode (improve/fetch-settings!)))))))

(defdescribe improve-verb-is-advertised-only-when-the-mode-is-on-test
             (it "improve verb is advertised only when the mode is on"
                 (let [verb (first (filter #(= :improve (:action %)) keymap/prefix-commands))]
                   (expect (= \e (:key verb)))
                   (expect (= "C-x e" (keymap/label-for :improve)))
                   (expect (= :improve (keymap/prefix-action-for \e)))
                   ;; the hydra row appears only while Improve is on
                   (expect (false? (keymap/verb-available? {} verb)))
                   (expect (false? (keymap/verb-available? {:improve {:mode :off}} verb)))
                   (expect (true? (keymap/verb-available? {:improve {:mode :human}} verb)))
                   ;; both Improve commands stay hidden until the mode is on
                   (let [off (set (map :id (dlg/palette-commands-for nil)))
                         on (set (map :id (dlg/palette-commands-for {:improve? true})))]

                     (expect (not (contains? off :improve)))
                     (expect (not (contains? off :improve-settings)))
                     (expect (contains? on :improve))
                     (expect (contains? on :improve-settings))))))

(defdescribe improve-typed-commands-follow-the-visible-mode-test
             (it "improve typed commands follow the visible mode"
                 (doseq [mode [:off :human :automatic :off]]
                   (with-redefs-fn {#'state/app-db (atom {:improve {:mode mode}})
                                    #'vis/registered-slashes (constantly [])
                                    #'screen/template-slash-commands (constantly [])}
                     (fn []
                       (let [ids (set (map :id (#'screen/menu-commands nil)))
                             enabled? (not= :off mode)]

                         (expect (= enabled? (contains? ids :improve)))
                         (expect (= enabled? (contains? ids :improve-settings)))))))))

(defdescribe
  improve-palette-renders-no-entry-while-disabled-test
  (it "improve palette renders no entry while disabled"
      (doseq [cols
              [40 96]

              enabled?
              [false true false]]

        (let [capture
              (cap/capture! {:cols cols
                             :rows 32
                             :keys (concat "Improve" [:esc])
                             :paint! (fn [{:keys [screen]}]
                                       (dlg/command-palette! screen [] {:improve? enabled?}))})

              frames
              (map cap/frame-text (:frames capture))]

          (expect (nil? (:error capture)))
          (expect (= enabled? (boolean (some #(str/includes? % "Improve — Projects") frames))))
          (expect (= enabled? (boolean (some #(str/includes? % "Improve Mode") frames))))))))

(defdescribe
  improve-walks-every-window-of-the-register-test
  (it "improve walks every window of the register"
      ;; The contract windows `/v1/improve` with `after`/`has_more`, so a register
      ;; longer than one page must still arrive WHOLE — and a window the daemon
      ;; drops mid-walk keeps the rows already read instead of losing them.
      (let [asked
            (atom [])

            pages
            (atom [{"records" [{"id" 1 "title" "One" "project_id" "p1" "version" 4}]
                    "projects" [{"id" "p1" "name" "Editor"}]
                    "after" 1
                    "has_more" true}
                   {"records" [{"id" 2 "title" "Two" "project_id" "p2"}]
                    "projects" [{"id" "p2" "name" "Docs"}]
                    "after" 2
                    "has_more" false}])]

        (with-redefs [vis/improve-records (fn [opts]
                                            (swap! asked conj opts)
                                            (let [[page & more] @pages]
                                              (reset! pages (vec more))
                                              page))]
          (let [{:keys [records projects error]} (improve/fetch-register!)]
            (expect (nil? error))
            (expect (= [1 2] (mapv :id records)))
            (expect (= 4 (:version (first records)))
                    "a record carries the version an edit has to gate on")
            (expect (= #{"Editor" "Docs"} (set (vals projects))))
            (expect (= [{:limit 200} {:limit 200 :after 1}] @asked)))))
      ;; a window the daemon cannot answer mid-walk keeps what was read
      (let [pages (atom [{"records" [{"id" 1 "title" "One"}] "after" 1 "has_more" true}])]
        (with-redefs [vis/improve-records (fn [_]
                                            (let [[page & more] @pages]
                                              (reset! pages (vec more))
                                              page))]
          (let [{:keys [records error]} (improve/fetch-register!)]
            (expect (= [1] (mapv :id records)))
            (expect (nil? error)))))))

(defdescribe
  improve-review-says-what-it-did-and-what-it-did-not-test
  (it "improve review says what it did and what it did not"
      ;; A review reads and writes analysis only. The summary must never imply that
      ;; anything was reproduced or fixed.
      (expect (= "Review finished — 2 reviewed, 1 failed. Nothing was reproduced."
                 (improve/review-summary
                   {"projects"
                    [{"project_id" "p1" "status" "reviewed"} {"project_id" "p2" "status" "reviewed"}
                     {"project_id" "p3" "status" "failed" "error" "provider_unavailable"}]
                    "reproduction" "not_attempted"})))
      (expect (= "Review finished — 1 skipped. Nothing was reproduced."
                 (improve/review-summary {"projects" [{"project_id" "p1" "status" "skipped"}]})))
      (doseq [empty-answer [{"projects" []} {} nil]]
        (expect (= "Review finished — no project was reviewed."
                   (improve/review-summary empty-answer))))))

(defdescribe improve-review-failure-is-not-painted-as-a-clean-save-test
             (it "improve review failure is not painted as a clean save"
                 ;; The gateway answers a review per project, so a run that reports a failed
                 ;; project has to read as a failure, not as a save.
                 (expect (true? (improve/review-failed? {"projects"
                                                         [{"project_id" "p1" "status" "reviewed"}
                                                          {"project_id" "p2" "status" "failed"}]})))
                 (expect (false? (improve/review-failed?
                                   {"projects" [{"project_id" "p1" "status" "reviewed"}
                                                {"project_id" "p2" "status" "skipped"}]})))
                 (doseq [empty-answer [{"projects" []} {} nil]]
                   (expect (false? (improve/review-failed? empty-answer))))))

(defdescribe improve-project-chooser-reaches-a-project-with-no-issues-test
             (it "the gateway's project list keeps an empty project choosable"
                 (let [choices (improve/project-choices projects
                                                        [{"id" "p1" "name" "Editor"}
                                                         {"id" "p9" "name" "Fresh"}])]
                   (expect (= "No project" (:label (first choices)))
                           "work that belongs to no project is always offered first")
                   (expect (nil? (:project-id (first choices))))
                   (expect (= #{"p1" "p2" "p9"} (set (keep :project-id choices))))
                   (expect (= ["No project" "Docs" "Editor" "Fresh"] (mapv :label choices)))))
             (it "a project the gateway cannot list still keeps its register header"
                 (expect (= ["No project" "Docs" "Editor"]
                            (mapv :label (improve/project-choices projects nil))))))

(def ^:private versioned-records
  (improve/records
    {"records"
     [{"id" 1 "project_id" "p1" "title" "Parent" "content" "One line" "status" "open" "version" 7}
      {"id" 2 "project_id" "p1" "title" "Child" "status" "open" "parent_id" 1 "version" 2}]
     "projects" [{"id" "p1" "name" "Editor"}]}))

(def ^:private row-of
  #(first (filter (fn [record]
                    (= % (:id record)))
                  versioned-records)))

(def ^:private close-improve-record! (deref #'screen/close-improve-record!))

(def ^:private reopen-improve-record! (deref #'screen/reopen-improve-record!))

(def ^:private edit-improve-record! (deref #'screen/edit-improve-record!))

(def ^:private new-improve-record! (deref #'screen/new-improve-record!))

(def ^:private open-improve! (deref #'screen/open-improve!))

(defdescribe disabled-improve-shortcut-opens-no-view-test
             (it "disabled improve shortcut opens no view"
                 (doseq [settings [nil {:mode :off}]]
                   (let [opened (atom [])]
                     (with-redefs-fn {#'screen/refresh-improve-settings! (constantly settings)
                                      #'screen/open-improve-settings! (fn [_]
                                                                        (swap! opened conj
                                                                          :settings))
                                      #'improve/fetch-register! (fn []
                                                                  (swap! opened conj :register))}
                       #(open-improve! nil))
                     (expect (empty? @opened))))))

(defdescribe improve-mode-view-requires-the-experimental-flag-test
             (it "improve mode view requires the experimental flag"
                 (doseq [toggle [nil {"enabled" false} {"enabled" true} {"enabled" false}]]
                   (let [paints (atom 0)]
                     (with-redefs-fn {#'vis/setting (fn [id]
                                                      (expect (= "improve" id))
                                                      toggle)
                                      #'screen/refresh-improve-settings! (constantly {:mode :off})
                                      #'screen/with-dialog-lock (fn [f]
                                                                  (f))
                                      #'improve/show-settings! (fn [& _]
                                                                 (swap! paints inc)
                                                                 nil)}
                       #(#'screen/open-improve-settings! nil))
                     (expect (= (if (true? (get toggle "enabled")) 1 0) @paints))))))

(defdescribe closing-settings-refreshes-improve-visibility-test
             (it "closing settings refreshes improve visibility"
                 (let [db (atom {:settings {} :improve {:mode :human}})]
                   (doseq [mode [:off :human :off]]
                     (with-redefs-fn {#'state/app-db db
                                      #'dlg/settings-dialog! (fn [& _]
                                                               nil)
                                      #'improve/fetch-settings! (constantly {:mode mode})
                                      #'state/dispatch (fn [[event settings]]
                                                         (when (= :set-improve-settings event)
                                                           (swap! db assoc :improve settings)))}
                       #(#'screen/open-settings-modal! nil))
                     (expect (= mode (get-in @db [:improve :mode])))))))

(defdescribe improve-close-is-one-versioned-write-that-names-the-whole-cascade-test
             (it "improve close is one versioned write that names the whole cascade"
                 ;; The gateway closes descendants atomically, so the TUI sends ONE patch and the
                 ;; confirmation admits that the cascade reaches issues this register never read.
                 (let [writes
                       (atom [])

                       shown
                       (atom nil)]

                   (with-redefs-fn {#'screen/with-dialog-lock (fn [f]
                                                                (f))
                                    #'dlg/confirm-dialog! (fn [_ _ message]
                                                            (reset! shown message)
                                                            true)
                                    #'vis/improve-update! (fn [id patch]
                                                            (swap! writes conj [id patch])
                                                            {})
                                    #'vis/notify! (fn [& _]
                                                    nil)}
                     #(close-improve-record! nil versioned-records (row-of 1)))
                   (expect (= [[1 {:status "closed" :expected_version 7}]] @writes)
                           "one versioned write, never a client-side loop over the descendants")
                   (expect (str/includes? (str/join " " @shown) "not shown here")
                           "the confirmation says the count it shows is a floor"))))

(defdescribe improve-reopen-is-one-versioned-write-test
             (it "improve reopen is one versioned write"
                 (let [writes
                       (atom [])

                       note
                       (atom nil)]

                   (with-redefs-fn {#'vis/improve-update! (fn [id patch]
                                                            (swap! writes conj [id patch])
                                                            {})
                                    #'vis/notify! (fn [text & _]
                                                    (reset! note text))}
                     #(reopen-improve-record! versioned-records (row-of 2)))
                   (expect (= [[2 {:status "open" :expected_version 2}]] @writes))
                   (expect (= "Reopened" @note)))
                 ;; a refused reopen never reads as a save
                 (let [failed (atom nil)]
                   (with-redefs-fn {#'vis/improve-update! (constantly nil)
                                    #'vis/notify! (fn [text & _]
                                                    (reset! failed text))}
                     #(reopen-improve-record! versioned-records (row-of 2)))
                   (expect (str/includes? (str @failed) "still closed")))))

(defdescribe
  improve-edit-keeps-the-typed-words-when-a-write-is-refused-test
  (it "improve edit keeps the typed words when a write is refused"
      (let [titles
            (atom [])

            drafts
            (atom [])

            writes
            (atom [])]

        (with-redefs-fn {#'screen/with-dialog-lock (fn [f]
                                                     (f))
                         #'dlg/text-input-dialog! (fn [_ title _ & {:keys [initial]}]
                                                    (swap! titles conj [title initial])
                                                    "Typed title")
                         #'improve/edit-analysis! (fn [_ _ text]
                                                    (swap! drafts conj text)
                                                    (when (= 1 (count @drafts)) "Typed body"))
                         #'vis/improve-update! (fn [id patch]
                                                 (swap! writes conj [id patch])
                                                 nil)
                         #'vis/notify! (fn [& _]
                                         nil)}
          #(edit-improve-record! nil (row-of 1)))
        (expect (= [[1 {:title "Typed title" :content "Typed body" :expected_version 7}]] @writes))
        (expect (= ["Improve — title" "Typed title"] (last @titles))
                "the refused edit comes back holding the title that was typed")
        (expect (= ["One line" "Typed body"] @drafts)
                "and the editor reopens on the draft, so a conflict never costs the analysis"))))

(defdescribe
  improve-edit-writes-a-multi-line-analysis-whole-test
  (it "improve edit writes a multi line analysis whole"
      ;; Markdown is written in paragraphs: the editor opens on the analysis as it
      ;; stands and saves every line the human kept, flattening nothing.
      (let [row
            (assoc (row-of 1) :content "First line\n\n- second line")

            opened
            (atom nil)

            writes
            (atom [])]

        (with-redefs-fn {#'screen/with-dialog-lock (fn [f]
                                                     (f))
                         #'dlg/text-input-dialog! (fn [_ _ _ & _]
                                                    "Retitled")
                         #'improve/edit-analysis! (fn [_ _ text]
                                                    (reset! opened text)
                                                    (str text "\n\n- third line"))
                         #'vis/improve-update! (fn [id patch]
                                                 (swap! writes conj [id patch])
                                                 {})
                         #'vis/notify! (fn [& _]
                                         nil)}
          #(edit-improve-record! nil row))
        (expect (= "First line\n\n- second line" @opened)
                "the editor opens on the Markdown exactly as it was stored")
        (expect (= [[1
                     {:title "Retitled"
                      :content "First line\n\n- second line\n\n- third line"
                      :expected_version 7}]]
                   @writes)
                "the whole analysis is written back"))))

(defdescribe
  improve-new-record-writes-nothing-until-every-prompt-is-answered-test
  (it "the project chooser offers a project the register cannot show"
      (let [created
            (atom nil)

            offered
            (atom nil)]

        (with-redefs-fn {#'screen/with-dialog-lock (fn [f]
                                                     (f))
                         #'vis/list-projects (fn []
                                               [{"id" "p1" "name" "Editor"}
                                                {"id" "p9" "name" "Fresh"}])
                         #'dlg/searchable-select! (fn [_ _ items _]
                                                    (reset! offered items)
                                                    (first (filter #(= "p9" (:project-id %))
                                                                   items)))
                         #'dlg/text-input-dialog! (fn [_ _ _ & _]
                                                    "New issue")
                         #'improve/edit-analysis! (fn [_ _ _]
                                                    "Body")
                         #'vis/improve-create! (fn [record]
                                                 (reset! created record)
                                                 {})
                         #'vis/notify! (fn [& _]
                                         nil)}
          #(new-improve-record! nil projects))
        (expect (= {:title "New issue" :content "Body" :project_id "p9"} @created))
        (expect (some #(= "Fresh" (:label %)) @offered))))
  (it "Esc at the Markdown prompt writes NOTHING, not an empty analysis"
      (let [created (atom nil)]
        (with-redefs-fn {#'screen/with-dialog-lock (fn [f]
                                                     (f))
                         #'vis/list-projects (fn []
                                               [])
                         #'dlg/searchable-select! (fn [_ _ items _]
                                                    (first items))
                         #'dlg/text-input-dialog! (fn [_ _ _ & _]
                                                    "Only a title")
                         #'improve/edit-analysis! (fn [_ _ _]
                                                    nil)
                         #'vis/improve-create! (fn [record]
                                                 (reset! created record)
                                                 {})
                         #'vis/notify! (fn [& _]
                                         nil)}
          #(new-improve-record! nil projects))
        (expect (nil? @created)))))

(defdescribe improve-register-closes-when-the-mode-is-switched-off-test
             (it "improve register closes when the mode is switched off"
                 ;; Switching Improve Off inside the register must take the rows with it.
                 (let [paints (atom 0)]
                   (with-redefs-fn {#'screen/with-dialog-lock (fn [f]
                                                                (f))
                                    #'screen/refresh-improve-settings! (constantly {:mode :human})
                                    #'screen/open-improve-settings! (constantly {:mode :off})
                                    #'improve/fetch-register!
                                    (constantly {:records versioned-records :projects projects})
                                    #'improve/show-browser! (fn [& _]
                                                              (swap! paints inc)
                                                              {:action :settings})}
                     #(open-improve! nil))
                   (expect (= 1 @paints)
                           "the register is painted once and never again after Off"))))

(defn- paint-cursor
  "Where the caret lands in the painted frame: `capture!` hands back the paint's
   own return, which is the cursor position the renderer honours."
  ^TerminalPosition [component cols rows]
  (:ret (cap/capture! {:cols cols
                       :rows rows
                       :paint! (fn [{:keys [g]}]
                                 (let [state
                                       (:init component)

                                       geom
                                       ((:measure component) state cols rows)

                                       state
                                       ((:reconcile component) state geom)]

                                   ((:paint component) g state geom)))})))

(def ^:private paste-start (KeyStroke. KeyType/PasteStart))

(def ^:private paste-end (KeyStroke. KeyType/PasteEnd))

(defn- typed [c] (KeyStroke. (Character/valueOf (char c)) false false))

(defn- ctrl [c] (KeyStroke. (Character/valueOf (char c)) true false))

(defdescribe improve-mode-hints-say-what-each-mode-authorizes-test
             (it "improve mode hints say what each mode authorizes"
                 ;; Human mode makes no model calls at all, so its hint must promise none.
                 (let [hint (fn [id]
                              (:hint (first (filter #(= id (:id %)) improve/modes))))]
                   ;; the chooser offers Off, human and automatic in that order
                   (expect (= [:off :human :automatic] (mapv :id improve/modes)))
                   ;; the human mode is human AUTHORSHIP, never a model proposal
                   (expect (str/includes? (hint :human) "no model calls"))
                   (expect (not (str/includes? (str/lower-case (hint :human)) "propose")))
                   ;; Off reviews nothing and automatic says a schedule runs
                   (expect (str/includes? (hint :off) "nothing is reviewed"))
                   (expect (str/includes? (hint :automatic) "schedule")))))

(defdescribe improve-browser-footer-advertises-the-mode-and-the-way-out-test
             (it "improve browser footer advertises the mode and the way out"
                 ;; A narrow register drops trailing chords, so the ones nobody can guess have
                 ;; to survive the cut at the widths people actually run.
                 (doseq [cols [60 96]]
                   (let [painted
                         (paint-text
                           (improve/browser-modal-component records projects nil {:mode :human})
                           cols
                           24)]
                     (expect (str/includes? painted "s mode")
                             (str "the mode chooser stays visible at " cols " columns"))
                     (expect (str/includes? painted "Esc back")
                             (str "the way back out stays visible at " cols " columns"))))))

(defdescribe
  improve-editor-treats-a-paste-as-text-not-as-commands-test
  (it "^S and Esc INSIDE a bracketed paste are typed, never obeyed"
      (let [after (reduce improve/editor-key
                          (improve/editor-state "")
                          [paste-start (typed \a) (ctrl \s) (KeyStroke. KeyType/Escape)
                           (KeyStroke. KeyType/Enter) (typed \b) paste-end])]
        (expect (not (contains? after ::dlg/done)) "a pasted control key must not end the editor")
        (expect (= "as\nb" (improve/editor-text after)))))
  (it "lanterna's private-use paste markers never reach the Markdown"
      (let [after (reduce improve/editor-key
                          (improve/editor-state "")
                          [paste-start (typed \uE200) (typed \x) (typed \uE201) paste-end])]
        (expect (= "x" (improve/editor-text after)))))
  (it "^S the human actually presses still saves the whole analysis"
      (expect (= "note"
                 (:text (done (improve/editor-key (improve/editor-state "note") (ctrl \s))))))))

(defdescribe
  improve-editor-pages-by-viewport-test
  (it "improve editor pages by viewport"
      (let [component
            (improve/analysis-editor-component "Analysis" (str/join "\n" (map str (range 50))))

            geom
            ((:measure component) (:init component) 96 24)

            start
            ((:reconcile component) (:init component) geom)

            page!
            (fn [state key]
              ((:reconcile component) ((:on-key component) state (KeyStroke. key) geom) geom))

            up
            (page! start KeyType/PageUp)

            back
            (page! up KeyType/PageDown)]

        (expect (= (- 49 (:list-h geom)) (:crow up)))
        (expect (= 49 (:crow back)))
        (expect (= 0 (:crow (nth (iterate #(page! % KeyType/PageUp) start) 50)))))))

(defdescribe
  improve-editor-places-the-caret-by-display-width-test
  (it "a wide glyph before the caret takes TWO cells, not one char"
      (let [col (fn [text]
                  (.getColumn
                    (paint-cursor (improve/analysis-editor-component "Analysis" text) 96 24)))]
        (expect (= (col "abcd") (col "日本")) "two wide glyphs end where four ASCII cells do")
        (expect (not= (col "ab") (col "日本")))))
  (it "the sideways window is measured in cells, so wide glyphs cannot overflow the box"
      (let [component
            (improve/analysis-editor-component "Analysis" (apply str (repeat 80 "日")))

            geom
            ((:measure component) (:init component) 60 24)

            text-w
            (- (long (:inner-w (:bounds geom))) 3)

            row
            (first (filter #(str/includes? % "日") (str/split-lines (paint-text component 60 24))))]

        (expect (some? row))
        ;; The virtual terminal keeps one cell per COLUMN, so a wide glyph shows up
        ;; twice in the captured row: the count IS the cells it occupies.
        (expect (<= (count (re-seq #"日" (str row))) text-w))
        (expect (str/ends-with? (str/trimr (str row)) "│")
                "a cell-measured window leaves the box border standing"))))
