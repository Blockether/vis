(ns com.blockether.vis.internal.workspace.cli-test
  (:require [com.blockether.vis.internal.loop :as lp]
            [com.blockether.vis.internal.workspace.cli :as workspace-cli]
            [com.blockether.vis.internal.workspace.core :as workspace]
            [lazytest.core :refer [defdescribe expect it]]))

;; `vis-agent sessions delete` has always existed; there was no way at all to
;; remove a PROJECT and the conversations in it, so the blast radius is opt-in
;; (`--with-sessions`) and the default stays the scatter delete the schema has
;; always had.

(def ^:private match-projects @#'workspace-cli/match-projects)

(def ^:private delete-project-tree! @#'workspace-cli/delete-project-tree!)

(defdescribe
  project-id-resolution-test
  (let [projects [{:id "9f2c1a44-0000-0000-0000-000000000001" :name "vis"}
                  {:id "9f2c1a44-0000-0000-0000-000000000002" :name "demo"}
                  {:id "b1000000-0000-0000-0000-000000000003" :name "svar"}]]
    (it "resolves a full id, and an unambiguous prefix"
        (expect (= ["svar"] (mapv :name (match-projects projects "b1000000"))))
        (expect (= ["vis"]
                   (mapv :name (match-projects projects "9F2C1A44-0000-0000-0000-000000000001")))))
    (it "reports EVERY candidate for an ambiguous prefix instead of picking one"
        ;; Deleting a project is irreversible: a prefix that fits two projects
        ;; must never silently resolve to the first one.
        (expect (= ["vis" "demo"] (mapv :name (match-projects projects "9f2c")))))
    (it "matches nothing for an unknown id or blank input"
        (expect (= [] (match-projects projects "zzzz")))
        (expect (= [] (match-projects projects "  ")))
        (expect (= [] (match-projects projects nil))))))

(defdescribe
  cli-project-delete-blast-radius-test
  (let [pid
        "9f2c1a44-0000-0000-0000-000000000001"

        exec
        (fn [opts]
          (let [log (atom [])]
            (with-redefs [lp/project-session-ids (fn [p]
                                                   (if (= pid p) ["s-a" "s-b"] []))
                          workspace/discard-session-clones! (fn [_d sid]
                                                              (swap! log conj [:drafts sid])
                                                              (future nil))
                          lp/delete! (fn [sid]
                                       (swap! log conj [:session sid]))
                          lp/delete-project! (fn [p]
                                               (swap! log conj [:project p]))]

              {:result (delete-project-tree! ::db pid opts) :log @log})))]

    (it "keeps the scatter default: the project row goes, not one conversation"
        (let [{:keys [result log]} (exec {})]
          (expect (= [[:project pid]] log))
          (expect (= [] (:deleted-session-ids result)))
          (expect (= ["s-a" "s-b"] (:kept-session-ids result)))))
    (it "--with-sessions deletes each member's drafts and tree BEFORE the project row"
        ;; Sessions first: an interrupted teardown must leave a project holding
        ;; survivors, never orphaned sessions with a dead parent.
        (let [{:keys [result log]} (exec {:with-sessions true})]
          (expect (= [[:drafts "s-a"] [:session "s-a"] [:drafts "s-b"] [:session "s-b"]
                      [:project pid]]
                     log))
          (expect (= ["s-a" "s-b"] (:deleted-session-ids result)))
          (expect (= [] (:kept-session-ids result)))
          (expect (= pid (:project-id result)))))))
