(ns com.blockether.vis.tui.improve-test
  (:require [clojure.string :as str]
            [com.blockether.vis.tui.capture :as cap]
            [com.blockether.vis.tui.client :as vis]
            [com.blockether.vis.tui.improve :as improve]
            [com.blockether.vis.tui.keymap :as keymap]
            [com.blockether.vis.tui.dialogs :as dlg]
            [com.blockether.vis.tui.screen :as screen]
            [com.blockether.vis.tui.state :as state]
            [lazytest.experimental.interfaces.clojure-test :refer [deftest is testing]])
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

(deftest improve-mode-is-off-until-the-gateway-says-otherwise-test
  (testing "an unavailable or unknown settings document is Off, never a half-open surface"
    (is (= :off (improve/mode nil)))
    (is (= :off (improve/mode {"mode" "sometimes"})))
    (is (= :human (improve/mode {"mode" "human"})))
    (is (= :automatic (improve/mode {"mode" "AUTOMATIC"})))
    (is (false? (improve/enabled? nil)))
    (is (true? (improve/enabled? {"mode" "human"})))
    (is (false? (improve/automatic? {"mode" "human"})))
    (is (true? (improve/automatic? {"mode" "automatic"}))))
  (testing "normalization is idempotent, so app-db can hold its own result"
    (let [once
          (improve/settings
            {"mode" "automatic" "provider" "anthropic" "model" "claude" "interval_minutes" 30})]
      (is (= {:mode :automatic :provider "anthropic" :model "claude" :interval-minutes 30} once))
      (is (= once (improve/settings once)))
      (is (= "anthropic / claude" (improve/model-label once)))
      (is (= "30 minutes" (improve/interval-label once)))
      (is (= "not chosen" (improve/model-label {"mode" "automatic"})))
      (is (= "not set" (improve/interval-label {"mode" "automatic"}))))))

(deftest improve-records-normalize-the-wire-document-test
  (is (= [1 2 3 4 5] (mapv :id records)))
  (is (= [:open :open :closed :closed :open] (mapv :status records)))
  (is (= "(untitled)" (:title (last records))))
  (is (= {"p1" "Editor" "p2" "Docs"} projects))
  (is (= "No project" (improve/project-label projects nil)))
  (is (= "Project p9" (improve/project-label projects "p9")))
  (testing "a bare list is a register too"
    (is (= ["Only one"] (mapv :title (improve/records [{"id" 9 "title" "Only one"}]))))))

(deftest improve-browser-groups-projects-and-parents-test
  (let [rows
        (improve/browser-rows records projects)

        kinds
        (mapv :kind rows)

        labels
        (mapv :label rows)]

    (testing "one header per project, its issues under it, unfiled work last"
      (is (= [:project :issue :project :issue :issue :issue :project :issue] kinds))
      (is (= "Docs · 0 open of 1" (first labels)))
      (is (= "Editor · 2 open of 3" (nth labels 2)))
      (is (= "No project · 1 open of 1" (nth labels 6))))
    (testing "children are indented under the parent they were grouped under"
      (is (= [0 0 1 2 0] (mapv :depth (filterv #(= :issue (:kind %)) rows))))
      (is (str/includes? (nth labels 3) "• Slow startup"))
      (is (str/includes? (nth labels 4) "  • Cold cache"))
      (is (str/includes? (nth labels 5) "    ✓ Stale index"))
      (is (str/includes? (nth labels 3) "  +1")))
    (testing "an empty register says so instead of painting nothing"
      (is (= [{:kind :notice
               :label "Nothing recorded yet — press n to write the first improvement"}]
             (improve/browser-display [] {} nil)))
      (is (= "Improve register unavailable"
             (:label (first (improve/browser-display [] {} "Improve register unavailable"))))))))

(deftest improve-closing-cascades-and-reopening-stays-safe-test
  (testing "closing names every descendant still open, and skips the ones already closed"
    (let [{:keys [ids descendant-count title]} (improve/close-plan records 1)]
      (is (= [1 2] ids))
      (is (= 1 descendant-count))
      (is (= "Slow startup" title))))
  (testing "a cycle cannot hang the confirmation"
    (let [looped (improve/records [{"id" 1 "parent_id" 2 "title" "a"}
                                   {"id" 2 "parent_id" 1 "title" "b"}])]
      (is (= #{1 2} (improve/descendant-ids looped 1)))))
  (testing "reopening lifts the closed parents that hide the issue, never a closed descendant"
    (let [closed
          (improve/records [{"id" 1 "title" "root" "status" "closed"}
                            {"id" 2 "title" "mid" "status" "closed" "parent_id" 1}
                            {"id" 3 "title" "leaf" "status" "closed" "parent_id" 2}
                            {"id" 4 "title" "other leaf" "status" "closed" "parent_id" 3}])

          {:keys [ids parent-count]}
          (improve/reopen-plan closed 3)]

      (is (= [3 2 1] ids))
      (is (= 2 parent-count))
      (is (not (contains? (set ids) 4))))))

(deftest improve-grouping-stays-inside-one-project-test
  (let [child
        (second records)

        candidates
        (improve/parent-candidates records child)]

    (is (= [1] (mapv :id candidates)))
    (testing "never itself, never its own descendant, never another project"
      (is (empty? (improve/parent-candidates records (first records)))))
    (testing "the chooser always offers a way back to the top of the project"
      (let [items (improve/parent-items records child)]
        (is (= [nil 1] (mapv :parent-id items)))
        (is (str/includes? (:label (first items)) "no parent"))
        (is (= "#1" (:hint (second items))))))))

(deftest improve-detail-reads-as-markdown-test
  (let [md (improve/detail-markdown (first records) "Editor")]
    (is (str/starts-with? md "# Slow startup"))
    (is (str/includes? md "It drags on a cold cache."))
    (is (str/includes? md "- Project: Editor"))
    (is (str/includes? md "- Session: session-1"))
    (is (str/includes? md "- Recorded: 2026-01-02"))
    (is (= "Improve #1 · open" (improve/detail-title (first records)))))
  (testing "a record without analysis says so rather than painting an empty page"
    (is (str/includes? (improve/detail-markdown (improve/record {"id" 7 "title" "Empty"}) nil)
                       "_No analysis written yet._"))))

(deftest improve-browser-component-paints-and-answers-keys-test
  (let [component
        (improve/browser-modal-component records projects nil {:mode :human})

        text
        (paint-text component 96 24)]

    (testing "the register paints its mode, its projects and its issues"
      (is (str/includes? text "Improve · Governed by human"))
      (is (str/includes? text "Editor · 2 open of 3"))
      (is (str/includes? text "Slow startup"))
      (is (str/includes? text "Stale index")))
    (testing "every state row survives a narrow terminal"
      (is (str/includes? (paint-text component 60 20) "Improve · Governed by human")))
    (let [closed-row
          (:init component)

          open-row
          (assoc (:init component) :selected 1)]

      (testing "closing is offered on an open issue only, reopening on a closed one only"
        (is (nil? (done (press component closed-row (KeyStroke. \c false false)))))
        (is (= {:action :reopen :row (nth records 3)}
               (done (press component closed-row (KeyStroke. \r false false)))))
        (is (= {:action :close :row (first records)}
               (done (press component open-row (KeyStroke. \c false false)))))
        (is (nil? (done (press component open-row (KeyStroke. \r false false))))))
      (testing "reading, editing, grouping, recording and the mode chooser are all one key"
        (is (= :read (:action (done (press component open-row (KeyStroke. KeyType/Enter))))))
        (is (= :edit (:action (done (press component open-row (KeyStroke. \e false false))))))
        (is (= :group (:action (done (press component open-row (KeyStroke. \g false false))))))
        (is (= :new (:action (done (press component open-row (KeyStroke. \n false false))))))
        (is (= {:action :settings} (done (press component open-row (KeyStroke. \s false false))))))
      (testing "Esc closes and moving the cursor writes nothing"
        (is (contains? (press component open-row (KeyStroke. KeyType/Escape))
                       :com.blockether.vis.tui.dialogs/done))
        (is (= 2 (:selected (press component open-row (KeyStroke. KeyType/ArrowDown)))))))))

(deftest improve-browser-pages-over-project-headings-test
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

    (is (<= (dec (:list-h geom)) (:selected down)))
    (is (= 0 (:selected back)))))

(deftest improve-settings-component-gates-model-calls-test
  (testing "provider, model, schedule and review exist in Automatic only"
    (is (= [:mode :mode :mode] (mapv :kind (improve/settings-rows {:mode :human}))))
    (is (= [:mode :mode :mode :model :interval :review]
           (mapv :kind (improve/settings-rows {:mode :automatic})))))
  (let [component
        (improve/settings-modal-component
          {:mode :automatic :provider "anthropic" :model "claude" :interval-minutes 30})

        text
        (paint-text component 96 24)]

    (is (str/includes? text "Improve mode"))
    (is (str/includes? text "● Automatic"))
    (is (str/includes? text "○ Governed by human"))
    (is (str/includes? text "Model · anthropic / claude"))
    (is (str/includes? text "Reviews every 30 minutes"))
    (is (str/includes? text "Review now"))
    (testing "each row answers an intent; the caller owns every write"
      (is (= {:action :set-mode :mode :off}
             (done (press component (:init component) (KeyStroke. KeyType/Enter)))))
      (is (= {:action :pick-model}
             (done
               (press component (assoc (:init component) :selected 3) (KeyStroke. KeyType/Enter)))))
      (is (= {:action :set-interval}
             (done
               (press component (assoc (:init component) :selected 4) (KeyStroke. KeyType/Enter)))))
      (is (= {:action :review}
             (done (press component
                          (assoc (:init component) :selected 5)
                          (KeyStroke. KeyType/Enter))))))))

(deftest improve-reads-the-register-through-the-facade-test
  (testing "an empty register is not a failure"
    (with-redefs [vis/improve-records (constantly {"records" []})]
      (is (= {:records [] :projects {}} (improve/fetch-register!)))))
  (testing "a register the daemon cannot answer is UNAVAILABLE, painted differently"
    (with-redefs [vis/improve-records (constantly nil)]
      (is (= "Improve register unavailable" (:error (improve/fetch-register!))))))
  (testing "settings the daemon cannot answer stay Off"
    (with-redefs [vis/improve-settings (constantly nil)]
      (is (= :off (:mode (improve/fetch-settings!)))))
    (with-redefs [vis/improve-settings (constantly {"mode" "human"})]
      (is (= :human (:mode (improve/fetch-settings!)))))))

(deftest improve-verb-is-advertised-only-when-the-mode-is-on-test
  (let [verb (first (filter #(= :improve (:action %)) keymap/prefix-commands))]
    (is (= \e (:key verb)))
    (is (= "C-x e" (keymap/label-for :improve)))
    (is (= :improve (keymap/prefix-action-for \e)))
    (testing "the hydra row appears only while Improve is on"
      (is (false? (keymap/verb-available? {} verb)))
      (is (false? (keymap/verb-available? {:improve {:mode :off}} verb)))
      (is (true? (keymap/verb-available? {:improve {:mode :human}} verb))))
    (testing "both Improve commands stay hidden until the mode is on"
      (let [off (set (map :id (dlg/palette-commands-for nil)))
            on (set (map :id (dlg/palette-commands-for {:improve? true})))]

        (is (not (contains? off :improve)))
        (is (not (contains? off :improve-settings)))
        (is (contains? on :improve))
        (is (contains? on :improve-settings))))))

(deftest improve-typed-commands-follow-the-visible-mode-test
  (doseq [mode [:off :human :automatic :off]]
    (with-redefs-fn {#'state/app-db (atom {:improve {:mode mode}})
                     #'vis/registered-slashes (constantly [])
                     #'screen/template-slash-commands (constantly [])}
      (fn []
        (let [ids (set (map :id (#'screen/menu-commands nil)))
              enabled? (not= :off mode)]

          (is (= enabled? (contains? ids :improve)))
          (is (= enabled? (contains? ids :improve-settings))))))))

(deftest improve-palette-renders-no-entry-while-disabled-test
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

      (is (nil? (:error capture)))
      (is (= enabled? (boolean (some #(str/includes? % "Improve — Projects") frames))))
      (is (= enabled? (boolean (some #(str/includes? % "Improve Mode") frames)))))))

(deftest improve-walks-every-window-of-the-register-test
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
        (is (nil? error))
        (is (= [1 2] (mapv :id records)))
        (is (= 4 (:version (first records))) "a record carries the version an edit has to gate on")
        (is (= #{"Editor" "Docs"} (set (vals projects))))
        (is (= [{:limit 200} {:limit 200 :after 1}] @asked)))))
  (testing "a window the daemon cannot answer mid-walk keeps what was read"
    (let [pages (atom [{"records" [{"id" 1 "title" "One"}] "after" 1 "has_more" true}])]
      (with-redefs [vis/improve-records (fn [_]
                                          (let [[page & more] @pages]
                                            (reset! pages (vec more))
                                            page))]
        (let [{:keys [records error]} (improve/fetch-register!)]
          (is (= [1] (mapv :id records)))
          (is (nil? error)))))))

(deftest improve-review-says-what-it-did-and-what-it-did-not-test
  ;; A review reads and writes analysis only. The summary must never imply that
  ;; anything was reproduced or fixed.
  (is (= "Review finished — 2 reviewed, 1 failed. Nothing was reproduced."
         (improve/review-summary
           {"projects" [{"project_id" "p1" "status" "reviewed"}
                        {"project_id" "p2" "status" "reviewed"}
                        {"project_id" "p3" "status" "failed" "error" "provider_unavailable"}]
            "reproduction" "not_attempted"})))
  (is (= "Review finished — 1 skipped. Nothing was reproduced."
         (improve/review-summary {"projects" [{"project_id" "p1" "status" "skipped"}]})))
  (doseq [empty-answer [{"projects" []} {} nil]]
    (is (= "Review finished — no project was reviewed." (improve/review-summary empty-answer)))))

(deftest improve-review-failure-is-not-painted-as-a-clean-save-test
  ;; The gateway answers a review per project, so a run that reports a failed
  ;; project has to read as a failure, not as a save.
  (is (true? (improve/review-failed? {"projects" [{"project_id" "p1" "status" "reviewed"}
                                                  {"project_id" "p2" "status" "failed"}]})))
  (is (false? (improve/review-failed? {"projects" [{"project_id" "p1" "status" "reviewed"}
                                                   {"project_id" "p2" "status" "skipped"}]})))
  (doseq [empty-answer [{"projects" []} {} nil]]
    (is (false? (improve/review-failed? empty-answer)))))

(deftest improve-project-chooser-reaches-a-project-with-no-issues-test
  (testing "the gateway's project list keeps an empty project choosable"
    (let [choices (improve/project-choices projects
                                           [{"id" "p1" "name" "Editor"}
                                            {"id" "p9" "name" "Fresh"}])]
      (is (= "No project" (:label (first choices)))
          "work that belongs to no project is always offered first")
      (is (nil? (:project-id (first choices))))
      (is (= #{"p1" "p2" "p9"} (set (keep :project-id choices))))
      (is (= ["No project" "Docs" "Editor" "Fresh"] (mapv :label choices)))))
  (testing "a project the gateway cannot list still keeps its register header"
    (is (= ["No project" "Docs" "Editor"] (mapv :label (improve/project-choices projects nil))))))

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

(deftest disabled-improve-shortcut-opens-no-view-test
  (doseq [settings [nil {:mode :off}]]
    (let [opened (atom [])]
      (with-redefs-fn {#'screen/refresh-improve-settings! (constantly settings)
                       #'screen/open-improve-settings! (fn [_]
                                                         (swap! opened conj :settings))
                       #'improve/fetch-register! (fn []
                                                   (swap! opened conj :register))}
        #(open-improve! nil))
      (is (empty? @opened)))))

(deftest improve-mode-view-requires-the-experimental-flag-test
  (doseq [toggle [nil {"enabled" false} {"enabled" true} {"enabled" false}]]
    (let [paints (atom 0)]
      (with-redefs-fn {#'vis/setting (fn [id]
                                       (is (= "improve" id))
                                       toggle)
                       #'screen/refresh-improve-settings! (constantly {:mode :off})
                       #'screen/with-dialog-lock (fn [f]
                                                   (f))
                       #'improve/show-settings! (fn [& _]
                                                  (swap! paints inc)
                                                  nil)}
        #(#'screen/open-improve-settings! nil))
      (is (= (if (true? (get toggle "enabled")) 1 0) @paints)))))

(deftest closing-settings-refreshes-improve-visibility-test
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
      (is (= mode (get-in @db [:improve :mode]))))))

(deftest improve-close-is-one-versioned-write-that-names-the-whole-cascade-test
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
    (is (= [[1 {:status "closed" :expected_version 7}]] @writes)
        "one versioned write, never a client-side loop over the descendants")
    (is (str/includes? (str/join " " @shown) "not shown here")
        "the confirmation says the count it shows is a floor")))

(deftest improve-reopen-is-one-versioned-write-test
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
    (is (= [[2 {:status "open" :expected_version 2}]] @writes))
    (is (= "Reopened" @note)))
  (testing "a refused reopen never reads as a save"
    (let [failed (atom nil)]
      (with-redefs-fn {#'vis/improve-update! (constantly nil)
                       #'vis/notify! (fn [text & _]
                                       (reset! failed text))}
        #(reopen-improve-record! versioned-records (row-of 2)))
      (is (str/includes? (str @failed) "still closed")))))

(deftest improve-edit-keeps-the-typed-words-when-a-write-is-refused-test
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
    (is (= [[1 {:title "Typed title" :content "Typed body" :expected_version 7}]] @writes))
    (is (= ["Improve — title" "Typed title"] (last @titles))
        "the refused edit comes back holding the title that was typed")
    (is (= ["One line" "Typed body"] @drafts)
        "and the editor reopens on the draft, so a conflict never costs the analysis")))

(deftest improve-edit-writes-a-multi-line-analysis-whole-test
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
    (is (= "First line\n\n- second line" @opened)
        "the editor opens on the Markdown exactly as it was stored")
    (is (= [[1
             {:title "Retitled"
              :content "First line\n\n- second line\n\n- third line"
              :expected_version 7}]]
           @writes)
        "the whole analysis is written back")))

(deftest improve-new-record-writes-nothing-until-every-prompt-is-answered-test
  (testing "the project chooser offers a project the register cannot show"
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
                                                  (first (filter #(= "p9" (:project-id %)) items)))
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
      (is (= {:title "New issue" :content "Body" :project_id "p9"} @created))
      (is (some #(= "Fresh" (:label %)) @offered))))
  (testing "Esc at the Markdown prompt writes NOTHING, not an empty analysis"
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
      (is (nil? @created)))))

(deftest improve-register-closes-when-the-mode-is-switched-off-test
  ;; Switching Improve Off inside the register must take the rows with it.
  (let [paints (atom 0)]
    (with-redefs-fn {#'screen/with-dialog-lock (fn [f]
                                                 (f))
                     #'screen/refresh-improve-settings! (constantly {:mode :human})
                     #'screen/open-improve-settings! (constantly {:mode :off})
                     #'improve/fetch-register! (constantly {:records versioned-records
                                                            :projects projects})
                     #'improve/show-browser! (fn [& _]
                                               (swap! paints inc)
                                               {:action :settings})}
      #(open-improve! nil))
    (is (= 1 @paints) "the register is painted once and never again after Off")))

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

(deftest improve-mode-hints-say-what-each-mode-authorizes-test
  ;; Human mode makes no model calls at all, so its hint must promise none.
  (let [hint (fn [id]
               (:hint (first (filter #(= id (:id %)) improve/modes))))]
    (testing "the chooser offers Off, human and automatic in that order"
      (is (= [:off :human :automatic] (mapv :id improve/modes))))
    (testing "the human mode is human AUTHORSHIP, never a model proposal"
      (is (str/includes? (hint :human) "no model calls"))
      (is (not (str/includes? (str/lower-case (hint :human)) "propose"))))
    (testing "Off reviews nothing and automatic says a schedule runs"
      (is (str/includes? (hint :off) "nothing is reviewed"))
      (is (str/includes? (hint :automatic) "schedule")))))

(deftest improve-browser-footer-advertises-the-mode-and-the-way-out-test
  ;; A narrow register drops trailing chords, so the ones nobody can guess have
  ;; to survive the cut at the widths people actually run.
  (doseq [cols [60 96]]
    (let [painted (paint-text (improve/browser-modal-component records projects nil {:mode :human})
                              cols
                              24)]
      (testing (str "the mode chooser stays visible at " cols " columns")
        (is (str/includes? painted "s mode")))
      (testing (str "the way back out stays visible at " cols " columns")
        (is (str/includes? painted "Esc back"))))))

(deftest improve-editor-treats-a-paste-as-text-not-as-commands-test
  (testing "^S and Esc INSIDE a bracketed paste are typed, never obeyed"
    (let [after (reduce improve/editor-key
                        (improve/editor-state "")
                        [paste-start (typed \a) (ctrl \s) (KeyStroke. KeyType/Escape)
                         (KeyStroke. KeyType/Enter) (typed \b) paste-end])]
      (is (not (contains? after ::dlg/done)) "a pasted control key must not end the editor")
      (is (= "as\nb" (improve/editor-text after)))))
  (testing "lanterna's private-use paste markers never reach the Markdown"
    (let [after (reduce improve/editor-key
                        (improve/editor-state "")
                        [paste-start (typed \uE200) (typed \x) (typed \uE201) paste-end])]
      (is (= "x" (improve/editor-text after)))))
  (testing "^S the human actually presses still saves the whole analysis"
    (is (= "note" (:text (done (improve/editor-key (improve/editor-state "note") (ctrl \s))))))))

(deftest improve-editor-pages-by-viewport-test
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

    (is (= (- 49 (:list-h geom)) (:crow up)))
    (is (= 49 (:crow back)))
    (is (= 0 (:crow (nth (iterate #(page! % KeyType/PageUp) start) 50))))))

(deftest improve-editor-places-the-caret-by-display-width-test
  (testing "a wide glyph before the caret takes TWO cells, not one char"
    (let [col (fn [text]
                (.getColumn
                  (paint-cursor (improve/analysis-editor-component "Analysis" text) 96 24)))]
      (is (= (col "abcd") (col "日本")) "two wide glyphs end where four ASCII cells do")
      (is (not= (col "ab") (col "日本")))))
  (testing "the sideways window is measured in cells, so wide glyphs cannot overflow the box"
    (let [component
          (improve/analysis-editor-component "Analysis" (apply str (repeat 80 "日")))

          geom
          ((:measure component) (:init component) 60 24)

          text-w
          (- (long (:inner-w (:bounds geom))) 3)

          row
          (first (filter #(str/includes? % "日") (str/split-lines (paint-text component 60 24))))]

      (is (some? row))
      ;; The virtual terminal keeps one cell per COLUMN, so a wide glyph shows up
      ;; twice in the captured row: the count IS the cells it occupies.
      (is (<= (count (re-seq #"日" (str row))) text-w))
      (is (str/ends-with? (str/trimr (str row)) "│")
          "a cell-measured window leaves the box border standing"))))
