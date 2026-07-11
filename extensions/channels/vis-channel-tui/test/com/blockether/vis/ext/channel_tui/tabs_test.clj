(ns com.blockether.vis.ext.channel-tui.tabs-test
  "Per-place tab sidecar: snapshot shape tolerance (current `{:id … :root …}`
   entries AND legacy plain id strings) plus MULTI-TUI SYNC — several TUIs in
   the same place must merge their tab sets on save instead of clobbering
   each other (see `tabs/merge-clients`)."
  (:require [clojure.java.io :as io]
            [com.blockether.vis.ext.channel-tui.tabs :as tabs]
            [lazytest.core :refer [defdescribe expect it]]))

(defdescribe snapshot-session-ids-test
             (it "reads the current {:id :root} entry shape in order"
                 (expect (= ["a" "b"]
                            (tabs/snapshot-session-ids
                              {:active "a" :sessions [{:id "a" :root "/p"} {:id "b"}]}))))
             (it "tolerates legacy plain-string entries and mixed/malformed shapes"
                 (expect (= ["a" "b" "c"]
                            (tabs/snapshot-session-ids {:sessions ["a" {:id "b" :root "/p"} nil ""
                                                                   {:id nil} "c"]}))))
             (it "nil / empty snapshot yields an empty vector"
                 (expect (= [] (tabs/snapshot-session-ids nil)))
                 (expect (= [] (tabs/snapshot-session-ids {})))))

(defdescribe
  merge-clients-test
  (it "a LIVE sibling TUI's tabs survive this process's save (no clobbering)"
      (let [disk
            {:active "a"
             :sessions [{:id "a" :root "/p"}]
             :clients {"111" {:active "a" :sessions [{:id "a" :root "/p"}] :at 5}}}

            merged
            (tabs/merge-clients disk "222" {:active "b" :sessions [{:id "b"}]} 10 #{"111"})]

        ;; our tabs first (order + :active win), the sibling's appended
        (expect (= "b" (:active merged)))
        (expect (= ["b" "a"] (tabs/snapshot-session-ids merged)))
        ;; both processes ride under :clients, each with its own view
        (expect (= #{"111" "222"} (set (keys (:clients merged)))))
        (expect (= ["a"] (tabs/snapshot-session-ids (get-in merged [:clients "111"]))))
        (expect (= 10 (get-in merged [:clients "222" :at])))))
  (it
    "a DEAD process's tabs are pruned (crashed TUIs stop pinning tabs)"
    (let [disk
          {:active "x"
           :sessions [{:id "x"}]
           :clients {"111" {:active "x" :sessions [{:id "x"}] :at 5}}}

          merged
          (tabs/merge-clients disk "222" {:active "b" :sessions [{:id "b"}]} 10 (constantly false))]

      (expect (= ["b"] (tabs/snapshot-session-ids merged)))
      (expect (= ["222"] (keys (:clients merged))))))
  (it
    "closing a tab HERE removes it even when the sidecar still lists it for us"
    (let [disk
          {:active "a"
           :sessions [{:id "a"} {:id "b"}]
           :clients {"222" {:active "a" :sessions [{:id "a"} {:id "b"}] :at 5}}}

          ;; our previous entry is replaced wholesale — "b" was closed here
          merged
          (tabs/merge-clients disk "222" {:active "a" :sessions [{:id "a"}]} 10 (constantly true))]

      (expect (= ["a"] (tabs/snapshot-session-ids merged)))))
  (it "a session open in BOTH TUIs appears once; sibling-only dupes collapse oldest-first"
      (let [disk
            {:active "a"
             :sessions []
             :clients {"111" {:active "shared" :sessions [{:id "shared"} {:id "c"}] :at 5}
                       "333" {:active "c" :sessions [{:id "c"} {:id "d"}] :at 7}}}

            merged
            (tabs/merge-clients disk
                                "222" {:active "shared" :sessions [{:id "shared"} {:id "b"}]}
                                10 (constantly true))]

        (expect (= ["shared" "b" "c" "d"] (tabs/snapshot-session-ids merged)))))
  (it "a legacy sidecar (no :clients) is NOT resurrected — old self-heal semantics"
      (let [merged (tabs/merge-clients {:active "old" :sessions [{:id "old"} "older"]}
                                       "222" {:active "b" :sessions [{:id "b"}]}
                                       10 (constantly true))]
        (expect (= ["b"] (tabs/snapshot-session-ids merged)))))
  (it "nil disk (first save ever) yields just this process's snapshot"
      (let [merged
            (tabs/merge-clients nil "222" {:active "b" :sessions [{:id "b"}]} 10 (constantly true))]
        (expect (= "b" (:active merged)))
        (expect (= ["b"] (tabs/snapshot-session-ids merged))))))

(defdescribe save-read-roundtrip-test
             (it "save! merges + persists and read-snapshot returns the well-formed merged view"
                 (let [dir
                       (java.nio.file.Files/createTempDirectory
                         "vis-tabs-test"
                         (make-array java.nio.file.attribute.FileAttribute 0))

                       f
                       (io/file (.toFile dir) "place.edn")]

                   (try (tabs/save! {:active "a" :sessions [{:id "a" :root "/p"}]} f)
                        (let [snap (tabs/read-snapshot f)]
                          (expect (= "a" (:active snap)))
                          (expect (= ["a"] (tabs/snapshot-session-ids snap)))
                          ;; our own (live) pid rides under :clients
                          (expect (= 1 (count (:clients snap)))))
                        ;; a second save REPLACES our own entry instead of merging with it
                        (tabs/save! {:active "b" :sessions [{:id "b"}]} f)
                        (let [snap (tabs/read-snapshot f)]
                          (expect (= ["b"] (tabs/snapshot-session-ids snap)))
                          (expect (= 1 (count (:clients snap)))))
                        (finally (io/delete-file f true) (io/delete-file (.toFile dir) true))))))
