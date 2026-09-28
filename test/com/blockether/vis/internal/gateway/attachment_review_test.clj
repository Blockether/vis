(ns com.blockether.vis.internal.gateway.attachment-review-test
  "Attachment capabilities survive storage and constrain human review writes."
  (:require [com.blockether.vis.contract.diff :as diff]
            [com.blockether.vis.contract.wire :as wire]
            [com.blockether.vis.internal.gateway.state :as state]
            [com.blockether.vis.internal.loop :as lp]
            [com.blockether.vis.internal.loop.python-exec :as python-exec]
            [com.blockether.vis.internal.persistance.core :as persistence]
            [com.blockether.vis.internal.persistance.sqlite.test-helpers :as h]
            [lazytest.core :refer [defdescribe expect it]]))

(h/use-mem-store!)

(defn- encode
  [^String text]
  (.encodeToString (java.util.Base64/getEncoder) (.getBytes text "UTF-8")))

(defn- fixture
  [attachments]
  (let [db
        (h/store)

        sid
        (h/store-session! db {:channel :cli})

        tid
        (persistence/db-store-session-turn! db {:parent-session-id sid :user-request "Review"})

        iid
        (h/store-iteration!
          db
          {:session-turn-id tid :status :done :code "attach(...)" :attachments attachments})]

    {:db db :sid sid :tid tid :iid iid}))

(defn- note
  [name commentable]
  {:filename name
   :kind "doc"
   :media-type "text/markdown"
   :base64 (encode "# Report\n")
   :audience "user"
   :commentable commentable})

(defn- refusal [f] (try (f) nil (catch clojure.lang.ExceptionInfo e (:type (ex-data e)))))

(defdescribe capabilities-survive-all-descriptors
             (it "capabilities survive all descriptors"
                 (let [{:keys [db sid iid]} (fixture [(dissoc (note "ordinary.md" false)
                                                        :commentable) (note "PLAN-feature.md" true)
                                                      (note "IMPLEMENTATION-feature.md" false)])]
                   (doseq [rows [(persistence/db-list-iteration-attachments db iid)
                                 (persistence/db-list-iteration-attachments-meta db iid)
                                 (persistence/db-list-session-attachments db sid)
                                 (persistence/db-list-session-attachments-meta db sid)]]
                     (expect (= [false true false] (mapv :commentable rows)))
                     (expect (= [false true false]
                                (mapv (comp :commentable python-exec/attachment-descriptor) rows)))
                     (expect (= [false true false]
                                (mapv #(-> (persistence/db-read-attachment db (:id %))
                                           :commentable)
                                      rows))))
                   (with-redefs [lp/db-info (constantly db)]
                     (expect (= [false true false]
                                (mapv :commentable (state/session-artifacts (str sid)))))))))

(defdescribe
  human-revisions-require-explicit-session-owned-capability
  (it "human revisions require explicit session owned capability"
      (let [{:keys [db sid iid]}
            (fixture [(note "PLAN-feature.md" true) (note "IMPLEMENTATION-feature.md" false)])

            another
            (h/store-session! db {:channel :cli})

            update-note
            (note "PLAN-feature.md" false)]

        (with-redefs [lp/db-info (constantly db)]
          (let [saved (state/revise-iteration-attachment! (str sid) (str iid) update-note)]
            (expect (= 2 (:version saved)))
            (expect (true? (:commentable saved)))
            (expect (= "doc" (:kind saved))))
          (doseq [[expected request-sid att]
                  [[:attachment/read-only sid (note "IMPLEMENTATION-feature.md" true)]
                   [:attachment/not-found another update-note]
                   [:attachment/not-found sid (note "invented.md" true)]
                   [:attachment/invalid-revision sid (assoc update-note :media-type "text/plain")]]]
            (expect (= expected
                       (refusal
                         #(state/revise-iteration-attachment! (str request-sid) (str iid) att)))))
          (expect (= 3 (count (persistence/db-list-iteration-attachments db iid))))))))

(defdescribe latest-agent-version-can-disable-review-without-disabling-publication
             (it "latest agent version can disable review without disabling publication"
                 (let [{:keys [db sid tid iid]} (fixture [(note "PLAN-feature.md" true)])]
                   (h/store-iteration! db
                                       {:session-turn-id tid
                                        :status :done
                                        :code "attach(...)"
                                        :attachments [(note "PLAN-feature.md" false)]})
                   (with-redefs [lp/db-info (constantly db)]
                     (expect (= :attachment/read-only
                                (refusal #(state/revise-iteration-attachment!
                                            (str sid)
                                            (str iid)
                                            (note "PLAN-feature.md" true))))))
                   (expect (= [true false]
                              (mapv :commentable
                                    (persistence/db-list-session-attachments-meta db sid)))))))

(defdescribe
  diffs-preserve-patch-and-source-through-comment-rounds
  (it
    "diffs preserve patch and source through comment rounds"
    (let [envelope
          {"schema_version" 1
           "patch" "--- a/a\n+++ b/a\n@@ -1 +1 @@\n-old\n+new  \n"
           "source" {"type" "draft" "backend" "rift" "base_revision" "abc"}
           "comments" []}

          original
          {:filename "DIFF-feature.json"
           :kind "diff"
           :media-type diff/media-type
           :audience "user"
           :commentable true
           :base64 (encode (diff/render envelope))}

          {:keys [db sid iid]}
          (fixture [original])

          revised
          (assoc envelope "comments" [{"quote" "+new" "body" "Explain the new behavior."}])]

      (with-redefs [lp/db-info (constantly db)]
        (let [saved (state/revise-iteration-attachment! (str sid)
                                                        (str iid)
                                                        (assoc original
                                                          :kind "doc"
                                                          :base64 (encode (diff/render revised))))
              stored (persistence/db-read-attachment db (:attachment_id saved))]

          (expect (= "diff" (:kind saved)))
          (expect (true? (:commentable saved)))
          (expect (= revised
                     (diff/parse! (String. (.decode (java.util.Base64/getDecoder)
                                                    ^String (:base64 stored))
                                           "UTF-8")))))
        (doseq [changed [(assoc revised "patch" "different")
                         (assoc-in revised ["source" "base_revision"] "other")
                         (assoc revised "schema_version" 2)
                         (assoc revised "comments" [{"body" "missing quote"}])]]
          (expect (= :attachment/invalid-revision
                     (refusal #(state/revise-iteration-attachment!
                                 (str sid)
                                 (str iid)
                                 (assoc original :base64 (encode (wire/json-str changed))))))))
        (expect (= 2 (count (persistence/db-list-iteration-attachments db iid))))))))

(defdescribe internal-late-artifacts-are-not-human-revisions
             (it "internal late artifacts are not human revisions"
                 (let [{:keys [db iid]} (fixture [])]
                   (with-redefs [lp/db-info (constantly db)]
                     (let [saved (state/append-iteration-attachment! (str iid)
                                                                     (note "late-record.md" false))]
                       (expect (= 1 (:version saved)))
                       (expect (false? (:commentable saved))))))))
