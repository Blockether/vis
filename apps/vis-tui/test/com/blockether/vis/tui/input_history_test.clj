(ns com.blockether.vis.tui.input-history-test
  "Regression coverage for session-scoped composer recall."
  (:require [com.blockether.vis.tui.chat :as chat]
            [com.blockether.vis.tui.client :as vis]
            [com.blockether.vis.tui.dialogs :as dlg]
            [com.blockether.vis.tui.input :as input]
            [com.blockether.vis.tui.screen :as screen]
            [com.blockether.vis.tui.state :as state]
            [lazytest.core :refer [defdescribe describe expect it]]))

(defn- step
  [db id & args]
  (let [handler
        (:fn (get @@#'state/event-registry id))

        result
        (handler db (into [id] args))]

    (if (contains? result :db) (:db result) result)))

(defn- prompts [n] (mapv #(str "prompt " %) (range n)))

(defn- transcript [texts] (mapv #(hash-map :role :user :text %) texts))

(defn- open-session
  [sid texts]
  (step {:render-version 0} :init-session {:id sid} (transcript texts) nil))

(defdescribe
  session-input-history-test
  (describe
    "session ownership"
    (it "switches cold and cached sessions without sharing prompts or recall drafts"
        (let [a
              (-> (open-session "a" ["saved A"])
                  (#'state/remember-input "new A")
                  (step :update-input (input/paste-text (input/empty-input) "draft A"))
                  (step :history-up))

              b
              (step a :open-session-tab {:id "b"} (transcript ["saved B"]) nil)

              recalled-b
              (step b :history-up)

              returned-a
              (step recalled-b :select-tab-by-session "a")

              draft-a
              (step returned-a :history-down)]

          (expect (= "saved B" (input/input->text (:input recalled-b))))
          (expect (= ["saved B"] (:input-history recalled-b)))
          (expect (= "new A" (input/input->text (:input returned-a))))
          (expect (= ["saved A" "new A"] (:input-history returned-a)))
          (expect (= "draft A" (input/input->text (:input draft-a))))))
    ;; User report: a previous session's recall ring remained reachable after switching.
    (it "rejects stale cached history and its saved draft from another session"
        (let [stale {:session {:id "b"}
                     :input (input/empty-input)
                     :input-history-session-id "a"
                     :input-history ["only A"]
                     :input-history-index 0
                     :input-history-draft "draft A"}]
          (doseq [direction [:history-up :history-down]]
            (let [result (step stale direction)]
              (expect (input/input-empty? (:input result)))
              (expect (empty? (:input-history result)))
              (expect (nil? (:input-history-index result)))
              (expect (nil? (:input-history-draft result)))))
          (expect (= ["only B"] (:input-history (#'state/remember-input stale "only B"))))))
    (it "does not borrow history when a preallocated session tab has no cached ring"
        (let [a
              (open-session "a" ["only A"])

              pending
              (step a :preallocate-project-tabs [{:session-id "b" :label "B" :project-id "p"}])

              b
              (step pending :select-tab-by-session "b")

              recalled
              (step b :history-up)]

          (expect (empty? (:input-history recalled)))
          (expect (input/input-empty? (:input recalled))))))
  (describe "the 20-entry limit"
            (it "keeps only the newest 20 submitted prompts and ignores commands and repeats"
                (let [texts
                      (prompts 25)

                      result
                      (reduce #'state/remember-input (open-session "a" []) texts)]

                  (expect (= (subvec texts 5) (:input-history result)))
                  (expect (= "a" (:input-history-session-id result)))
                  (expect (= (:input-history result)
                             (:input-history (#'state/remember-input result (peek texts)))))
                  (expect (= (:input-history result)
                             (:input-history (#'state/remember-input result "/reload"))))))
            (it "caps hydrated transcripts after excluding commands and assistant messages"
                (let [texts
                      (prompts 25)

                      history
                      (conj (transcript texts)
                            {:role :assistant :text "answer"}
                            {:role :user :text "/reload"})

                      result
                      (step {} :init-session {:id "a"} history nil)]

                  (expect (= (subvec texts 5) (:input-history result)))
                  (expect (= "a" (:input-history-session-id result)))))
            (it "bounds both cold-open and pending-tab hydration"
                (let [texts
                      (prompts 25)

                      a
                      (open-session "a" ["only A"])

                      cold
                      (step a :open-session-tab {:id "b"} (transcript texts) nil)

                      pending
                      (step a :preallocate-project-tabs [{:session-id "b" :label "B"}])

                      hydrated
                      (step pending :open-session-tab {:id "b"} (transcript texts) nil)]

                  (doseq [result [cold hydrated]]
                    (expect (= (subvec texts 5) (:input-history result)))
                    (expect (= "b" (:input-history-session-id result))))))
            (it "caps older transcript pages without moving the selected recall entry"
                (let [newer
                      (mapv #(str "new " %) (range 15))

                      older
                      (prompts 15)

                      browsing
                      (step (open-session "a" newer) :history-up)

                      page
                      {:messages (transcript older) :offset 0 :total 30 :has-more false}

                      result
                      (step browsing :prepend-history "a" page 0)

                      next-up
                      (step result :history-up)]

                  (expect (= (into (subvec older 10) newer) (:input-history result)))
                  (expect (= 19 (:input-history-index result)))
                  (expect (= "new 13" (input/input->text (:input next-up))))))
            (it "does not grow a full recall ring when older pages arrive"
                (let [texts
                      (prompts 20)

                      db
                      (open-session "a" texts)

                      result
                      (step db
                            :prepend-history
                            "a"
                            {:messages (transcript ["older"]) :offset 0 :total 21 :has-more false}
                            0)]

                  (expect (= texts (:input-history result))))))
  (describe
    "archive and deletion cleanup"
    (it "hydrates no recall entries for an archived session"
        (let [db
              (step {} :init-session {:id "a" :archived-at 1} (transcript ["archived prompt"]) nil)

              recalled
              (step db :history-up)

              submitted
              (#'state/remember-input db "must not be remembered")]

          (expect (empty? (:input-history db)))
          (expect (input/input-empty? (:input recalled)))
          (expect (empty? (:input-history submitted)))))
    (it "clears active and background archived sessions without clearing another session"
        (let [a
              (step (open-session "a" ["only A"]) :history-up)

              b
              (step a :open-session-tab {:id "b"} (transcript ["only B"]) nil)

              archived-a
              (step b :session-archive-changed "a" true)

              archived-b
              (step archived-a :session-archive-changed "b" true)]

          (expect (= ["only B"] (:input-history archived-a)))
          (let [locals (get-in archived-a [:tab-locals :main])]
            (expect (empty? (:input-history locals)))
            (expect (nil? (:input-history-index locals)))
            (expect (nil? (:input-history-draft locals))))
          (expect (empty? (:input-history archived-b)))
          (expect (empty? (:input-history (step archived-b :select-tab-by-session "a"))))))
    (it "removes deleted session histories and cannot recall them from a fresh tab"
        (let [a
              (open-session "a" ["only A"])

              b
              (step a :open-session-tab {:id "b"} (transcript ["only B"]) nil)

              deleted-a
              (step b :session-deleted "a")

              deleted-b
              (step deleted-a :session-deleted "b")]

          (expect (= ["only B"] (:input-history deleted-a)))
          (expect (nil? (get-in deleted-a [:tab-locals :main])))
          (expect (empty? (:input-history deleted-b)))
          (expect (input/input-empty? (:input (step deleted-b :history-up))))))))

(defdescribe
  input-history-archive-integration-test
  (it "carries persisted archive and group identity from resume into composer state"
      (let [sid (str (random-uuid))]
        (with-redefs [vis/gateway-soul (constantly {"id" sid "archived_at" 7 "group_id" "g"})
                      vis/gateway-list-turns (constantly [])
                      chat/history-page (fn [& _]
                                          {:messages (transcript ["archived prompt"])})]

          (let [session (chat/resume-session sid)
                db (step {} :init-session session (:history session) nil)]

            (expect (= 7 (:archived-at session)))
            (expect (= "g" (:group-id session)))
            (expect (empty? (:input-history db)))))))
  (it "routes confirmed session and group archives to their owning histories"
      (doseq [[action entry effect stamp]
              [[:archive-session {:session {"id" "s"}} :session-archive-changed 7]
               [:unarchive-session {:session {"id" "s"}} :session-archive-changed nil]
               [:archive-group {:group {"id" "g"}} :session-group-archive-changed 7]
               [:unarchive-group {:group {"id" "g"}} :session-group-archive-changed nil]]]
        (let [events (atom [])]
          (with-redefs-fn {#'state/app-db (atom {})
                           #'state/dispatch #(swap! events conj %)
                           #'dlg/select-dialog! (fn [& _]
                                                  {:id action})
                           #'vis/worker-future (fn [_ f]
                                                 (f))
                           #'vis/gateway-set-session-archived! (fn [& _]
                                                                 {"archived_at" stamp})
                           #'vis/gateway-update-session-group! (fn [& _]
                                                                 {"archived_at" stamp})
                           #'screen/refresh-projects! (fn [& _])}
            (fn []
              (#'screen/sidebar-row-menu! nil entry nil)))
          (expect (= [[effect (if (:session entry) "s" "g") stamp]]
                     (filterv #(= effect (first %)) @events))))))
  (it "does not discard history when the gateway refuses an archive"
      (let [events (atom [])]
        (with-redefs-fn {#'state/app-db (atom {})
                         #'state/dispatch #(swap! events conj %)
                         #'dlg/select-dialog! (fn [& _]
                                                {:id :archive-session})
                         #'vis/worker-future (fn [_ f]
                                               (f))
                         #'vis/gateway-set-session-archived! (fn [& _]
                                                               (throw (ex-info "Session is busy"
                                                                               {:http-status 409})))
                         #'vis/notify! (fn [& _])
                         #'screen/refresh-projects! (fn [& _])}
          (fn []
            (#'screen/sidebar-row-menu! nil {:session {"id" "s"}} nil)))
        (expect (empty? (filterv #(#{:session-archive-changed :session-group-archive-changed}
                                    (first %))
                          @events))))))

(defdescribe
  input-history-lifecycle-test
  (it "bounds queued prompts on a background session without changing the active ring"
      (let [a
            (open-session "a" ["initial A"])

            b
            (step a :open-session-tab {:id "b"} (transcript ["only B"]) nil)

            queued
            (reduce #(step %1 :enqueue-message %2 :main) b (prompts 25))

            a-locals
            (get-in queued [:tab-locals :main])]

        (expect (= ["only B"] (:input-history queued)))
        (expect (= (subvec (prompts 25) 5) (:input-history a-locals)))
        (expect (= "a" (:input-history-session-id a-locals)))))
  (it "bounds the recall ring when a building tab is bound to its session"
      (let [texts
            (prompts 25)

            building
            (step (open-session "a" ["only A"]) :open-building-tab "build-b")

            bound
            (step building :bind-built-session "build-b" {:id "b"} (transcript texts) nil)]

        (expect (= (subvec texts 5) (:input-history bound)))
        (expect (= "b" (:input-history-session-id bound)))
        (expect (nil? (:input-history-index bound)))
        (expect (nil? (:input-history-draft bound)))))
  (it "normalizes UUID session identity for recall ownership"
      (let [sid
            (random-uuid)

            db
            (open-session sid ["only this session"])

            recalled
            (step (assoc-in db [:session :id] (str sid)) :history-up)]

        (expect (= (str sid) (:input-history-session-id db)))
        (expect (= "only this session" (input/input->text (:input recalled))))))
  (it "walks no more than 20 entries and restores the current session's draft"
      (let [db
            (step (open-session "a" (prompts 25))
                  :update-input
                  (input/paste-text (input/empty-input) "my draft"))

            oldest
            (nth (iterate #(step % :history-up) db) 25)

            draft
            (nth (iterate #(step % :history-down) oldest) 20)]

        (expect (= "prompt 5" (input/input->text (:input oldest))))
        (expect (= "my draft" (input/input->text (:input draft))))
        (expect (nil? (:input-history-index draft)))))
  (it "clears only the archived group's sessions, including background tabs"
      (let [a
            (step {} :init-session {:id "a" :group-id "g1"} (transcript ["only A"]) nil)

            b
            (step a :open-session-tab {:id "b" :group-id "g2"} (transcript ["only B"]) nil)

            archived-a
            (step b :session-group-archive-changed "g1" 7)

            archived-b
            (step archived-a :session-group-archive-changed "g2" 7)

            unarchived-b
            (step archived-b :session-group-archive-changed "g2" nil)]

        (expect (= ["only B"] (:input-history archived-a)))
        (expect (empty? (get-in archived-a [:tab-locals :main :input-history])))
        (expect (empty? (:input-history archived-b)))
        (expect (empty? (:input-history (#'state/remember-input archived-b "not retained"))))
        (expect (empty? (:input-history unarchived-b)))
        (expect (= ["new B"] (:input-history (#'state/remember-input unarchived-b "new B"))))))
  (it "does not repopulate an archived session from older transcript pages"
      (let [db
            (step (open-session "a" ["only A"]) :session-archive-changed "a" 7)

            result
            (step db
                  :prepend-history
                  "a"
                  {:messages (transcript ["old A"]) :offset 0 :total 2 :has-more false}
                  0)]

        (expect (empty? (:input-history result)))))
  (it "suppresses history when a loaded group record says the session is archived"
      (let [db (step {:project-sidebar {:groups {"p" [{"id" "g" "archived_at" 7}]}}}
                     :init-session
                     {:id "a" :group-id "g"}
                     (transcript ["only A"])
                     nil)]
        (expect (empty? (:input-history db)))
        (expect (empty? (:input-history (#'state/remember-input db "not retained"))))))
  (it "clears a cached tab when reopening observes a persisted archive"
      (let [a
            (step (open-session "a" ["only A"]) :history-up)

            b
            (step a :open-session-tab {:id "b"} (transcript ["only B"]) nil)

            archived
            (step b :open-session-tab {:id "a" :archived-at 7} (transcript ["only A"]) nil true)

            focused
            (step archived :select-tab-by-session "a")]

        (expect (= ["only B"] (:input-history archived)))
        (expect (empty? (:input-history focused)))
        (expect (nil? (:input-history-index focused)))
        (expect (nil? (:input-history-draft focused)))))
  (it "preserves recall when a refresh confirms the session is not archived"
      (let [db
            (open-session "a" ["only A"])

            result
            (step db :session-archive-changed "a" nil)]

        (expect (= ["only A"] (:input-history result))))))

(defdescribe
  input-history-refresh-test
  (it "clears the archived owner's history when the background metadata refresh observes it"
      (let [db (atom (step (open-session "a" ["only A"])
                           :open-session-tab
                           {:id "b"}
                           (transcript ["only B"])
                           nil))]
        (with-redefs [state/app-db db
                      vis/gateway-soul (fn [_]
                                         {"archived_at" 7 "title" "Archived A"})]

          (#'screen/refresh-session-metadata! "a"))
        (expect (= ["only B"] (:input-history @db)))
        (expect (empty? (get-in @db [:tab-locals :main :input-history])))
        (expect (= "Archived A" (get-in @db [:tab-locals :main :title])))))
  (it "does not clear recall when metadata is unavailable"
      (let [initial
            (open-session "a" ["only A"])

            db
            (atom initial)]

        (with-redefs [state/app-db
                      db

                      vis/gateway-soul
                      (fn [_]
                        (throw (ex-info "Gateway unavailable" {})))]

          (#'screen/refresh-session-metadata! "a"))
        (expect (= initial @db))))
  (it "updates a cached group stamp when unarchiving so new prompts can be remembered"
      (let [db
            (step {:project-sidebar {:groups {"p" [{"id" "g" "archived_at" 7}]}}}
                  :init-session
                  {:id "a" :group-id "g"}
                  (transcript ["old A"])
                  nil)

            result
            (step db :session-group-archive-changed "g" nil)]

        (expect (= ["new A"] (:input-history (#'state/remember-input result "new A")))))))
