(ns com.blockether.vis.contract.diff-test
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [com.blockether.vis.contract.diff :as diff]
            [com.blockether.vis.contract.document :as document]
            [lazytest.experimental.interfaces.clojure-test :refer [deftest is]]))

(defn fixture [] (diff/parse! (slurp (io/resource "vis-contract/fixtures/diff.json"))))

(deftest portable-diff-envelope
  (is (= diff/media-type (get (document/load! "diff") "media_type")))
  (let [sample
        (fixture)

        patch
        "diff --git a/file b/file\r\n+trailing spaces  \n\n"

        sample
        (assoc sample "patch" patch)

        reviewed
        (diff/with-comments sample [{:quote "+trailing spaces" :body "Keep these."}])]

    (is (= patch (get (diff/parse! (diff/render reviewed)) "patch")))
    (is (= (dissoc sample "comments") (dissoc reviewed "comments")))
    (is (= [{"quote" "+trailing spaces" "body" "Keep these."}] (get reviewed "comments")))
    (is (diff/valid? (assoc sample "patch" "")))))

(deftest malformed-diffs-are-rejected
  (let [sample (fixture)]
    (doseq [invalid [(dissoc sample "patch") (assoc sample "schema_version" 2)
                     (assoc sample "extra" true) (assoc sample "patch" nil)
                     (assoc sample "source" {"type" "unknown"})
                     (assoc sample "source" {"type" "draft" "unknown" "value"})
                     (assoc sample "comments" [{"quote" "line"}])
                     (assoc sample "comments" [{"quote" "line" "body" ""}])]]
      (is (not (diff/valid? invalid)))))
  (doseq [text [nil "not JSON" "null" "[]"]]
    (is (= :diff/invalid
           (try (diff/parse! text) nil (catch clojure.lang.ExceptionInfo e (:type (ex-data e))))))))

(deftest review-addresses-exact-version-without-expanding-permission
  (let [request (diff/review-request "DIFF-search.json" 3)]
    (is (str/includes? request "read_attachment(\"DIFF-search.json\", version=3)"))
    (is (str/includes? request "already authorized implementation scope"))
    (is (str/includes? request "permission to commit, push or deploy")))
  (is (= :diff/invalid
         (try (diff/review-request "DIFF-search.json" 0)
              nil
              (catch clojure.lang.ExceptionInfo e (:type (ex-data e)))))))
