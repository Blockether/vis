(ns com.blockether.vis.contract.diff-test
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [com.blockether.vis.contract.diff :as diff]
            [com.blockether.vis.contract.document :as document]
            [lazytest.core :refer [defdescribe expect it]]))

(defn fixture [] (diff/parse! (slurp (io/resource "vis-contract/fixtures/diff.json"))))

(defdescribe
  portable-diff-envelope
  (it "portable diff envelope"
      (expect (nil? (io/resource "vis-contract/diff.json")))
      (expect (= diff/media-type
                 (get-in (document/schema-document "diff")
                         ["$defs" "attachment" "contentMediaType"])))
      (expect (document/valid? "diff" (fixture)))
      (expect (not (document/valid? "diff" {"version" 1 "media_type" diff/media-type})))
      (let [sample
            (fixture)

            patch
            "diff --git a/file b/file\r\n+trailing spaces  \n\n"

            sample
            (assoc sample "patch" patch)

            reviewed
            (diff/with-comments sample [{:quote "+trailing spaces" :body "Keep these."}])]

        (expect (= patch (get (diff/parse! (diff/render reviewed)) "patch")))
        (expect (= (dissoc sample "comments") (dissoc reviewed "comments")))
        (expect (= [{"quote" "+trailing spaces" "body" "Keep these."}] (get reviewed "comments")))
        (expect (diff/valid? (assoc sample "patch" ""))))))

(defdescribe malformed-diffs-are-rejected
             (it "malformed diffs are rejected"
                 (let [sample (fixture)]
                   (doseq [invalid [(dissoc sample "patch") (assoc sample "schema_version" 2)
                                    (assoc sample "extra" true) (assoc sample "patch" nil)
                                    (assoc sample "source" {"type" "unknown"})
                                    (assoc sample "source" {"type" "draft" "unknown" "value"})
                                    (assoc sample "comments" [{"quote" "line"}])
                                    (assoc sample "comments" [{"quote" "line" "body" ""}])]]
                     (expect (not (diff/valid? invalid)))))
                 (doseq [text [nil "not JSON" "null" "[]"]]
                   (expect (= :diff/invalid
                              (try (diff/parse! text)
                                   nil
                                   (catch clojure.lang.ExceptionInfo e (:type (ex-data e)))))))))

(defdescribe review-addresses-exact-version-without-expanding-permission
             (it "review addresses exact version without expanding permission"
                 (let [request (diff/review-request "DIFF-search.json" 3)]
                   (expect (str/includes? request
                                          "read_attachment(\"DIFF-search.json\", version=3)"))
                   (expect (str/includes? request "already authorized implementation scope"))
                   (expect (str/includes? request "permission to commit, push or deploy")))
                 (expect (= :diff/invalid
                            (try (diff/review-request "DIFF-search.json" 0)
                                 nil
                                 (catch clojure.lang.ExceptionInfo e (:type (ex-data e))))))))
