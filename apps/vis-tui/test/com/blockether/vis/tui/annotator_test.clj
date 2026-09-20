(ns com.blockether.vis.tui.annotator-test
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [com.blockether.vis.contract.diff :as diff]
            [com.blockether.vis.contract.plan :as plan]
            [com.blockether.vis.tui.annotator :as annotator]
            [com.blockether.vis.tui.artifact-inspector :as inspector]
            [com.blockether.vis.tui.capture :as cap]
            [com.blockether.vis.tui.client :as vis]
            [com.blockether.vis.tui.dialogs :as dlg]
            [com.blockether.vis.tui.external-opener :as opener]
            [com.blockether.vis.tui.frame :as frame]
            [com.blockether.vis.tui.theme :as theme]
            [lazytest.experimental.interfaces.clojure-test :refer [deftest is]])
  (:import [com.googlecode.lanterna TerminalSize]
           [com.googlecode.lanterna.terminal.html HtmlTerminalView]
           [java.nio.file Files]))

(def ^String review-text
  "# Session search\n\n**Feature:** search\n**Status:** ready\n\n## Spec\nFind sessions by title.\n\n## Tasks\n1. Search and open a matching session.\n\n## Open questions\nNone.\n\n## Plan state\nNext: approve the spec.\n\n## Resolved comments\n")

(def ^String implementation-text
  "# Session search\n\n**Feature:** search\n**Status:** done\n\n## Completed tasks\nSearch sessions by title and restore the selected session.\n\n## Verification\nSearch and keyboard navigation tests passed.\n\n## Changes\nReview the attached DIFF-search.json.\n")

(def review-row
  {:filename "PLAN-search.md"
   :media-type "text/markdown"
   :commentable true
   :version 3
   :index 0
   :iteration-id "iteration-1"})

(defn review-component
  "Deterministic production fixture for virtual-terminal and HTML review."
  ([] (review-component true))
  ([plans?]
   (annotator/modal-component "PLAN-search.md"
                              false
                              plans?
                              (annotator/artifact-state review-row review-text nil))))

(defn component-view
  "Production modal painter wrapped in the shared GUI2 grid for browser review."
  ^com.googlecode.lanterna.gui2.Panel [component cols rows]
  (frame/view cols
              rows
              (fn [^com.googlecode.lanterna.graphics.TextGraphics graphics
                   ^com.googlecode.lanterna.gui2.TextGraphicsComponent view]
                (let [size
                      (.getSize view)

                      geom
                      ((:measure component) (:init component) (.getColumns size) (.getRows size))

                      state
                      ((:reconcile component) (:init component) geom)]

                  (doto graphics
                    (.setForegroundColor theme/text-fg)
                    (.setBackgroundColor theme/terminal-bg)
                    (.fill \space))
                  ((:paint component) graphics state geom)))))

(defn review-view
  "The specification review fixture rendered by the production modal."
  [cols rows]
  (component-view (review-component) cols rows))

(deftest production-review-html
  (doseq [[cols rows] [[40 24] [96 32]]]
    (let [html (HtmlTerminalView/render (review-view cols rows)
                                        (TerminalSize. cols rows)
                                        "Specification review")]
      (is (str/includes? html "PLAN-search.md"))
      (is (str/includes? html "Approve and start"))
      (is (str/includes? html "ready")))))

(deftest production-review-layout
  (doseq [[cols rows] [[40 24] [96 32]]]
    (let [capture (cap/capture! {:cols cols
                                 :rows rows
                                 :keys [:esc]
                                 :paint! #(dlg/run-modal! (:screen %) (review-component))})
          text (cap/frame-text capture)]

      (is (nil? (:error capture)))
      (is (str/includes? text "PLAN-search.md"))
      (is (str/includes? text "PLAN-search.md · v3"))
      (is (str/includes? text "ready · 0 comments"))
      (is (str/includes? text "Session search"))
      (is (str/includes? text "a Approve and start"))
      (is (str/includes? text "Specification"))
      (is (not (str/includes? text "s Save")))
      (is (not (str/includes? text "r Send for revision")))
      (is (= :close (get-in capture [:ret :action]))))))

(deftest one-workflow-action-for-each-review-state
  (let [component
        (review-component)

        state
        (:init component)

        key!
        #((:on-key component) %1 (cap/key-stroke %2) {:doc-h 10})]

    (is (= :approve (get-in (key! state \a) [::dlg/done :action])))
    (is (= state (key! state \i)))
    (is (= state (key! state \r)))
    (is (= (assoc state :dirty? true) (key! (assoc state :dirty? true) \s)))
    (is (= (assoc state :dirty? true) (key! (assoc state :dirty? true) \a)))
    (is (= :revise (get-in (key! (assoc state :dirty? true) \r) [::dlg/done :action])))
    (is (= (assoc state :sent? true) (key! (assoc state :sent? true) \a))))
  (let [component (review-component false)]
    (is (= (:init component)
           ((:on-key component) (:init component) (cap/key-stroke \r) {:doc-h 10})))
    (is (= :save
           (get-in ((:on-key component)
                     (assoc (:init component) :dirty? true)
                     (cap/key-stroke \s)
                     {:doc-h 10})
                   [::dlg/done :action])))))

(deftest pending-comments-offer-only-revision
  (doseq [[cols rows] [[40 24] [96 32]]]
    (let [component (annotator/modal-component
                      "PLAN-search.md" false
                      true (assoc (annotator/artifact-state review-row review-text nil)
                             :comments [{:quote "" :body "Include archived sessions"}]))
          capture
          (cap/capture!
            {:cols cols :rows rows :keys [:esc] :paint! #(dlg/run-modal! (:screen %) component)})
          text (cap/frame-text capture)]

      (is (nil? (:error capture)))
      (is (str/includes? text "r Send for revision"))
      (is (not (str/includes? text "Approve and start")))
      (is (not (str/includes? text "s Save"))))))

(deftest approval-sends-one-exact-version-without-an-extra-save
  (doseq [status ["ready" "accepted"]]
    (let [sent (atom [])
          text (str/replace review-text "**Status:** ready" (str "**Status:** " status))
          state (annotator/initial-state text 3 nil)]

      (with-redefs [vis/toggle-enabled? (constantly true)
                    vis/save-artifact-text! (fn [& _]
                                              (throw (ex-info "Unexpected save" {})))
                    vis/submit-turn! (fn [_ opts]
                                       (swap! sent conj opts)
                                       {:turn {"turn_id" "new"}})]

        (let [submitted (annotator/send-state! "s" review-row state :approve)]
          (is (:sent? submitted))
          (is (= [{:request (plan/action-request "PLAN-search.md" 3 :approve)}] @sent))
          (is (= :blocked
                 (try (annotator/send-state! "s" review-row submitted :approve)
                      :sent
                      (catch clojure.lang.ExceptionInfo _ :blocked)))))))))

(deftest commenting-keeps-a-draft-without-saving-or-sending
  (let [drafts (atom [])]
    (with-redefs [vis/gateway-iteration-attachment-bytes (fn [& _]
                                                           (.getBytes review-text "UTF-8"))
                  vis/toggle-enabled? (constantly true)
                  annotator/read-draft (constantly nil)
                  annotator/keep-draft! (fn [_ _ state]
                                          (swap! drafts conj state))
                  vis/save-artifact-text! (fn [& _]
                                            (throw (AssertionError. "Unexpected save")))
                  vis/submit-turn! (fn [& _]
                                     (throw (AssertionError. "Unexpected send")))]

      (let [capture (cap/capture! {:cols 96
                                   :rows 32
                                   :keys
                                   (concat [\w] (seq "Include archived sessions") [:enter :esc])
                                   :paint! #(annotator/show! (:screen %) "s" review-row)})]
        (is (nil? (:error capture)))
        (is (:dirty? (last @drafts)))
        (is (= [{:quote "" :body "Include archived sessions"}] (:comments (last @drafts))))))))

(deftest block-and-cell-quotes
  (let
    [blocks
     (annotator/document-blocks
       "# Heading\n\nA **useful** paragraph.\n\n- First\n- Second\n\n| Name | State |\n| --- | --- |\n| Search | Ready |\n"
       false)]
    (is (= "Heading" (:quote (first blocks))))
    (is (str/includes? (:quote (second blocks)) "useful"))
    (is (= ["First" "Second"] (:passages (nth blocks 2))))
    (is (= ["Name" "State" "Search" "Ready"] (:passages (nth blocks 3))))))

(deftest comment-save-and-send-round-trip
  (let [saved
        (atom [])

        sent
        (atom [])

        drafts
        (atom [])

        keys
        (concat [\w] (seq "Keep scope narrow") [:enter \r :esc])]

    (with-redefs [vis/gateway-iteration-attachment-bytes
                  (fn [& _]
                    (.getBytes review-text "UTF-8"))

                  vis/toggle-enabled?
                  (constantly true)

                  annotator/read-draft
                  (fn [& _]
                    nil)

                  annotator/keep-draft!
                  (fn [_ _ state]
                    (swap! drafts conj state))

                  vis/save-artifact-text!
                  (fn [& args]
                    (swap! saved conj args)
                    {"version" 4})

                  vis/submit-turn!
                  (fn [_ opts]
                    (swap! sent conj opts)
                    {:turn {"turn_id" "new-turn"}})]

      (let [capture (cap/capture! {:cols 96
                                   :rows 32
                                   :keys keys
                                   :paint! #(annotator/show! (:screen %) "session-1" review-row)})]
        (is (nil? (:error capture)))
        (is (= 1 (count @saved)))
        (is (str/includes? (last (first @saved)) "- **Whole document** — Keep scope narrow"))
        (is (str/includes? (:request (first @sent)) "version=4"))
        (is (str/includes? (:request (first @sent)) "Do not implement"))
        (is (false? (:dirty? (last @drafts))))))))

(deftest failed-send-keeps-the-saved-version-for-retry
  (let [saves
        (atom 0)

        sends
        (atom 0)

        state
        (assoc (annotator/initial-state review-text 3 nil)
          :dirty? true
          :comments [{:quote "" :body "Keep this"}])]

    (with-redefs [vis/toggle-enabled?
                  (constantly true)

                  vis/save-artifact-text!
                  (fn [& _]
                    (swap! saves inc)
                    {"version" 4})

                  vis/submit-turn!
                  (fn [& _]
                    (when (= 2 (swap! sends inc)) {:turn {"turn_id" "new"}}))]

      (let [failed
            (annotator/send-state! "s" review-row state :revise)

            retried
            (annotator/send-state! "s" review-row failed :revise)]

        (is (= 4 (:version failed)))
        (is (false? (:dirty? failed)))
        (is (:sent? retried))
        (is (= 1 @saves))))))

(deftest removed-final-comment-retries-revision-instead-of-approving
  (let [saves
        (atom 0)

        requests
        (atom [])

        state
        (assoc (annotator/initial-state review-text 3 nil) :dirty? true)]

    (with-redefs [vis/toggle-enabled?
                  (constantly true)

                  vis/save-artifact-text!
                  (fn [& _]
                    (swap! saves inc)
                    {"version" 4})

                  vis/submit-turn!
                  (fn [_ opts]
                    (swap! requests conj (:request opts))
                    (when (= 2 (count @requests)) {:turn {"turn_id" "new"}}))]

      (let [failed
            (annotator/send-state! "s" review-row state :revise)

            component
            (annotator/modal-component "PLAN-search.md" false true failed)

            key!
            #((:on-key component) failed (cap/key-stroke %) {:doc-h 10})

            retried
            (annotator/send-state! "s" review-row failed :revise)]

        (is (= failed (key! \a)))
        (is (= :revise (get-in (key! \r) [::dlg/done :action])))
        (is (= 4 (:version retried)))
        (is (:sent? retried))
        (is (= 1 @saves))
        (is (= (repeat 2 (plan/action-request "PLAN-search.md" 4 :revise)) @requests))))))

(deftest failed-save-does-not-send-or-clear-comments
  (let [sent
        (atom false)

        state
        (assoc (annotator/initial-state review-text 3 nil) :dirty? true)]

    (with-redefs [vis/toggle-enabled?
                  (constantly true)

                  vis/save-artifact-text!
                  (fn [& _]
                    (throw (ex-info "offline" {})))

                  vis/submit-turn!
                  (fn [& _]
                    (reset! sent true))]

      (is (= :failed
             (try (annotator/send-state! "s" review-row state :revise)
                  :sent
                  (catch clojure.lang.ExceptionInfo _ :failed))))
      (is (false? @sent)))))

(deftest drafts-survive-reopening-but-never-silently-overwrite-a-newer-body
  (let [directory
        (.toFile (Files/createTempDirectory "annotation-test"
                                            (make-array java.nio.file.attribute.FileAttribute 0)))

        file
        (io/file directory "draft.edn")

        state
        (assoc (annotator/initial-state review-text 3 nil)
          :dirty? true
          :comments [{:quote "" :body "Persisted"}])]

    (try (with-redefs [annotator/draft-file (fn [& _]
                                              file)]
           (annotator/keep-draft! "s" "PLAN-search.md" state)
           (let [draft (annotator/read-draft "s" "PLAN-search.md")]
             (is (= (:comments state) (:comments (annotator/initial-state review-text 3 draft))))
             (is (empty? (:comments (annotator/initial-state "# Different body" 4 draft)))))
           (annotator/keep-draft! "s" "PLAN-search.md" (assoc state :dirty? false))
           (is (nil? (annotator/read-draft "s" "PLAN-search.md"))))
         (finally (.delete file) (.delete directory)))))

(deftest inspector-opens-text-in-terminal-and-retains-external-action
  (let [component
        (inspector/inspector-modal-component []
                                             [{"filename" "PLAN-search.md"
                                               "media_type" "text/markdown"
                                               "version" 3
                                               "iteration_id" "i"
                                               "index" 0}]
                                             nil
                                             :plans?
                                             true)

        key!
        #((:on-key component) (:init component) (cap/key-stroke %) {})]

    (is (= :annotate (get-in (key! :enter) [::dlg/done :action])))
    (is (= :open (get-in (key! \o) [::dlg/done :action])))))

(deftest read-only-capability-gates-all-review-paths
  (doseq [filename
          ["notes.md" "PLAN-search.md" "IMPLEMENTATION-search.md"]

          capability
          [nil false "true"]]

    (let [row
          (assoc review-row
            :filename filename
            :commentable capability)

          state
          (annotator/artifact-state row
                                    review-text
                                    {:base-text review-text
                                     :comments [{:quote "" :body "Old draft"}]})

          component
          (annotator/modal-component filename false true state)]

      (is (false? (:commentable state)))
      (is (empty? (:comments state)))
      (doseq [key [:enter :delete :tab \w \s \r \a \?]]
        (is (= state ((:on-key component) state (cap/key-stroke key) {:doc-h 10})))))))

(deftest read-only-report-renders-without-review-controls-or-draft-io
  (doseq [[cols rows] [[40 24] [96 32]]]
    (with-redefs [vis/gateway-iteration-attachment-bytes (fn [& _]
                                                           (.getBytes implementation-text "UTF-8"))
                  vis/toggle-enabled? (constantly true)
                  annotator/read-draft (fn [& _]
                                         (throw (AssertionError. "Read draft")))
                  annotator/keep-draft! (fn [& _]
                                          (throw (AssertionError. "Wrote draft")))
                  vis/save-artifact-text! (fn [& _]
                                            (throw (AssertionError. "Saved")))
                  vis/submit-turn! (fn [& _]
                                     (throw (AssertionError. "Sent")))]

      (let [capture (cap/capture! {:cols cols
                                   :rows rows
                                   :keys [\w \s \r \a :enter :esc]
                                   :paint! #(annotator/show! (:screen %)
                                                             "s"
                                                             (assoc review-row
                                                               :commentable false
                                                               :filename
                                                               "IMPLEMENTATION-search.md"))})
            text (cap/frame-text capture)]

        (is (nil? (:error capture)))
        (is (str/includes? text "Read only"))
        (is (str/includes? text "Session search"))
        (doseq [label ["Approve and start" "Send for revision" "s Save" "comment" "whole"]]
          (is (not (str/includes? text label))))))))

(deftest read-only-cannot-call-save-or-send-directly
  (doseq [capability [nil false]]
    (let [row (assoc review-row :commentable capability)
          state (assoc (annotator/initial-state review-text 3 nil) :dirty? true)]

      (with-redefs [vis/save-artifact-text! (fn [& _]
                                              (throw (AssertionError. "Saved")))
                    vis/submit-turn! (fn [& _]
                                       (throw (AssertionError. "Sent")))]

        (is (= :attachment/read-only
               (try (annotator/save-state! "s" row state)
                    nil
                    (catch clojure.lang.ExceptionInfo e (:type (ex-data e))))))
        (is (= :blocked
               (try (annotator/send-state! "s" row state :revise)
                    nil
                    (catch clojure.lang.ExceptionInfo _ :blocked))))))))

(def diff-envelope
  {"schema_version" 1
   "patch"
   "diff --git a/search.clj b/search.clj\n--- a/search.clj\n+++ b/search.clj\n@@ -1 +1 @@\n-(search title)\n+(search title {:archived true})  \n\n"
   "source" {"type" "draft" "backend" "rift" "label" "search" "base_revision" "abc123"}
   "comments" []})

(def diff-row
  (assoc review-row
    :filename "DIFF-search.json"
    :media-type diff/media-type))

(deftest diff-review-preserves-patch-and-retries-one-saved-round
  (let [saves
        (atom [])

        sends
        (atom [])

        state
        (assoc (annotator/artifact-state diff-row (diff/render diff-envelope) nil)
          :dirty? true
          :comments [{:quote "+(search title" :body "Make this optional."}])]

    (with-redefs [vis/toggle-enabled?
                  (constantly false)

                  vis/save-artifact-text!
                  (fn [& args]
                    (swap! saves conj args)
                    {"version" 4})

                  vis/submit-turn!
                  (fn [_ opts]
                    (when (= 2 (count (swap! sends conj (:request opts))))
                      {:turn {"turn_id" "review"}}))]

      (let [failed
            (annotator/send-state! "s" diff-row state :revise)

            retried
            (annotator/send-state! "s" diff-row failed :revise)

            envelope
            (diff/parse! (last (first @saves)))]

        (is (= 1 (count @saves)))
        (is (= (dissoc diff-envelope "comments") (dissoc envelope "comments")))
        (is (= [{"quote" "+(search title" "body" "Make this optional."}] (get envelope "comments")))
        (is (= (repeat 2 (diff/review-request "DIFF-search.json" 4)) @sends))
        (is (:sent? retried))
        (is (= :blocked
               (try (annotator/send-state! "s" diff-row state :approve)
                    nil
                    (catch clojure.lang.ExceptionInfo _ :blocked))))))))

(deftest diff-view-renders-source-lines-empty-and-malformed-states
  (doseq [[cols rows]
          [[40 24] [96 32]]

          [payload expected]
          [[(diff/render diff-envelope) "-(search title)"]
           [(diff/render (assoc diff-envelope "patch" "")) "No changes"] ["{bad json" "malformed"]]]

    (let [state
          (annotator/artifact-state diff-row payload nil)

          component
          (annotator/modal-component "DIFF-search.json" true false state)

          capture
          (cap/capture! {:cols cols
                         :rows rows
                         :keys
                         (if (= expected "-(search title)") [:down :down :down :down :esc] [:esc])
                         :paint! #(dlg/run-modal! (:screen %) component)})

          text
          (cap/frame-text capture)]

      (is (nil? (:error capture)))
      (is (str/includes? text expected))
      (is (not (str/includes? text "Approve and start")))
      (if (:invalid-diff? state)
        (is (not (str/includes? text "comment")))
        (do (is (str/includes? text "draft · rift · search"))
            (is (str/includes? text "Code changes")))))))

(deftest diff-comments-round-is-explicit
  (let [state
        (annotator/artifact-state diff-row (diff/render diff-envelope) nil)

        component
        (annotator/modal-component "DIFF-search.json" true false state)

        with-comments
        (assoc state :comments [{:quote "" :body "Review this."}])

        key!
        #((:on-key component) %1 (cap/key-stroke %2) {:doc-h 10})]

    (is (= state (key! state \r)))
    (is (= :revise (get-in (key! with-comments \r) [::dlg/done :action])))
    (is (= with-comments (key! with-comments \a)))
    (is (= with-comments (key! with-comments \s)))))

;; BLO-172 follow-up: a patch header is the only place a snapshot says which file
;; its hunks belong to, so it is the address the reader presses to reach the file.
(deftest patch-headers-open-the-file-they-name
  (let [state
        (annotator/artifact-state diff-row (diff/render diff-envelope) nil)

        component
        (annotator/modal-component "DIFF-search.json" true false state)

        key!
        #(get-in ((:on-key component) (assoc state :selected %) (cap/key-stroke \o) {:doc-h 10})
                 [::dlg/done :action])]

    ;; `diff --git`, `---` and `+++` name the file; the hunk, the removal and the
    ;; addition under them do not.
    (is (= [:open :open :open nil nil nil] (mapv key! (range 6))))
    (is (= (assoc state :selected 4)
           ((:on-key component) (assoc state :selected 4) (cap/key-stroke \o) {:doc-h 10})))))

(deftest opening-a-patch-header-resolves-it-in-the-session-workspace
  (let [directory
        (.toFile (Files/createTempDirectory "annotator-open"
                                            (make-array java.nio.file.attribute.FileAttribute 0)))

        file
        (io/file directory "search.clj")

        opened
        (atom [])

        open!
        (fn [state]
          (with-redefs [vis/session-workspace-info
                        (fn [_]
                          {"root" (str directory)})

                        opener/open-file-in-editor!
                        (fn [path]
                          (swap! opened conj path)
                          {:status :ok})]

            (#'annotator/open-named-file! "session-1" state true)))

        state
        (annotator/artifact-state diff-row (diff/render diff-envelope) nil)

        escaping
        (annotator/artifact-state diff-row
                                  (diff/render (assoc diff-envelope
                                                 "patch" "+++ b/../escape.clj\n"))
                                  nil)]

    (try (spit file "(search title)\n")
         ;; The header names a path in the tree the patch was taken in; the session's
         ;; own workspace root is what it is relative to.
         (is (= "Opening search.clj" (:note (open! (assoc state :selected 2)))))
         (is (= [(.getPath file)] @opened))
         ;; A file this workspace does not have says so instead of opening something else.
         (.delete file)
         (is (= "No such file: search.clj" (:note (open! (assoc state :selected 2)))))
         ;; A line naming no file, and a path climbing out of the workspace, open nothing.
         (is (= (assoc state :selected 4) (open! (assoc state :selected 4))))
         (is (= (assoc escaping :selected 0) (open! (assoc escaping :selected 0))))
         (is (= 1 (count @opened)))
         (finally (.delete file) (.delete directory)))))
