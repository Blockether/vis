(ns com.blockether.vis.internal.gateway.attachment-review-test
  "Attachment capabilities survive storage and constrain human review writes."
  (:require [com.blockether.vis.contract.diff :as diff]
            [com.blockether.vis.contract.wire :as wire]
            [com.blockether.vis.internal.gateway.state :as state]
            [com.blockether.vis.internal.loop :as lp]
            [com.blockether.vis.internal.loop.python-exec :as python-exec]
            [com.blockether.vis.internal.persistance.core :as persistence]
            [com.blockether.vis.internal.persistance.sqlite.test-helpers :as h]
            [lazytest.experimental.interfaces.clojure-test :refer [deftest is]]))

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

(deftest capabilities-survive-all-descriptors
  (let [{:keys [db sid iid]} (fixture [(dissoc (note "ordinary.md" false) :commentable)
                                       (note "PLAN-feature.md" true)
                                       (note "IMPLEMENTATION-feature.md" false)])]
    (doseq [rows [(persistence/db-list-iteration-attachments db iid)
                  (persistence/db-list-iteration-attachments-meta db iid)
                  (persistence/db-list-session-attachments db sid)
                  (persistence/db-list-session-attachments-meta db sid)]]
      (is (= [false true false] (mapv :commentable rows)))
      (is (= [false true false] (mapv (comp :commentable python-exec/attachment-descriptor) rows)))
      (is (= [false true false]
             (mapv #(-> (persistence/db-read-attachment db (:id %))
                        :commentable)
                   rows))))
    (with-redefs [lp/db-info (constantly db)]
      (is (= [false true false] (mapv :commentable (state/session-artifacts (str sid))))))))

(deftest human-revisions-require-explicit-session-owned-capability
  (let [{:keys [db sid iid]}
        (fixture [(note "PLAN-feature.md" true) (note "IMPLEMENTATION-feature.md" false)])

        another
        (h/store-session! db {:channel :cli})

        update-note
        (note "PLAN-feature.md" false)]

    (with-redefs [lp/db-info (constantly db)]
      (let [saved (state/revise-iteration-attachment! (str sid) (str iid) update-note)]
        (is (= 2 (:version saved)))
        (is (true? (:commentable saved)))
        (is (= "doc" (:kind saved))))
      (doseq [[expected request-sid att]
              [[:attachment/read-only sid (note "IMPLEMENTATION-feature.md" true)]
               [:attachment/not-found another update-note]
               [:attachment/not-found sid (note "invented.md" true)]
               [:attachment/invalid-revision sid (assoc update-note :media-type "text/plain")]]]
        (is (= expected
               (refusal #(state/revise-iteration-attachment! (str request-sid) (str iid) att)))))
      (is (= 3 (count (persistence/db-list-iteration-attachments db iid)))))))

(deftest latest-agent-version-can-disable-review-without-disabling-publication
  (let [{:keys [db sid tid iid]} (fixture [(note "PLAN-feature.md" true)])]
    (h/store-iteration! db
                        {:session-turn-id tid
                         :status :done
                         :code "attach(...)"
                         :attachments [(note "PLAN-feature.md" false)]})
    (with-redefs [lp/db-info (constantly db)]
      (is (= :attachment/read-only
             (refusal #(state/revise-iteration-attachment! (str sid)
                                                           (str iid)
                                                           (note "PLAN-feature.md" true))))))
    (is (= [true false]
           (mapv :commentable (persistence/db-list-session-attachments-meta db sid))))))

(deftest diffs-preserve-patch-and-source-through-comment-rounds
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

        (is (= "diff" (:kind saved)))
        (is (true? (:commentable saved)))
        (is (= revised
               (diff/parse! (String. (.decode (java.util.Base64/getDecoder)
                                              ^String (:base64 stored))
                                     "UTF-8")))))
      (doseq [changed [(assoc revised "patch" "different")
                       (assoc-in revised ["source" "base_revision"] "other")
                       (assoc revised "schema_version" 2)
                       (assoc revised "comments" [{"body" "missing quote"}])]]
        (is (= :attachment/invalid-revision
               (refusal #(state/revise-iteration-attachment!
                           (str sid)
                           (str iid)
                           (assoc original :base64 (encode (wire/json-str changed))))))))
      (is (= 2 (count (persistence/db-list-iteration-attachments db iid)))))))

(deftest internal-late-artifacts-are-not-human-revisions
  (let [{:keys [db iid]} (fixture [])]
    (with-redefs [lp/db-info (constantly db)]
      (let [saved (state/append-iteration-attachment! (str iid) (note "late-record.md" false))]
        (is (= 1 (:version saved)))
        (is (false? (:commentable saved)))))))
