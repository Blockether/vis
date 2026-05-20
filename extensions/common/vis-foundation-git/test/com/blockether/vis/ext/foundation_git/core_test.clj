(ns com.blockether.vis.ext.foundation-git.core-test
  (:require
   [clojure.string]
   [com.blockether.vis.ext.foundation-git.core :as git]
   [com.blockether.vis.internal.extension :as extension]
   [lazytest.core :refer [defdescribe expect it]]))

(defdescribe git-diff-test
  (it "accepts optional opts map for stat-only compatibility"
    (let [calls  (atom [])
          result (with-redefs [git/git-out (fn [dir args]
                                             (swap! calls conj {:dir dir :args args})
                                             (case args
                                               ["diff" "--numstat" "HEAD"] "2\t1\tsrc/a.clj\n"
                                               ["status" "--porcelain"] " M src/a.clj\n"
                                               ["rev-parse" "HEAD"] "abc123\n"))]
                   (git/git-diff-fn {:workspace/root "/repo"} {:stat? true}))]
      (expect (extension/tool-result? result))
      (expect (= {:files 1 :+ 2 :- 1} (get-in result [:result :stat])))
      (expect (= [{:file "src/a.clj" :+ 2 :- 1}] (get-in result [:result :files])))
      (expect (= [{:status "M" :file "src/a.clj"}] (get-in result [:result :porcelain])))
      (expect (= ["/repo" "/repo" "/repo"] (mapv :dir @calls)))))

  (it "rejects non-map opts"
    (try
      (git/git-diff-fn {:workspace/root "/repo"} :bad)
      (expect false)
      (catch clojure.lang.ExceptionInfo e
        (expect (= :foundation-git/invalid-opts (:type (ex-data e))))
        (expect (clojure.string/includes? (ex-message e)
                  "git/diff expected optional opts map, got :bad"))
        (expect (clojure.string/includes? (ex-message e)
                  "Call (git/diff) or (git/diff {:stat? true})."))))))
