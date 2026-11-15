(ns com.blockether.vis.tui.session-export-test
  (:require [com.blockether.vis.tui.session-export :as session-export]
            [lazytest.core :refer [defdescribe expect it]])
  (:import [java.io File]
           [java.nio.file Files]
           [java.nio.file.attribute FileAttribute]))

(defdescribe
  file-name-test
  ;; The title of a session is free text. A slash, a colon or a long title in the file
  ;; name makes a file that the file manager cannot show or that lands in another folder.
  (it "names the file with a safe slug of the title and the start of the session id"
      (expect (= "fix-a-b-c-d-0f3f2a1e.html"
                 (session-export/file-name "  Fix: a/b\\c.. D!  "
                                           "0f3f2a1e-1111-4222-8333-444455556666"
                                           "html")))
      (expect (= "vis-session-0f3f2a1e.md" (session-export/file-name nil "0f3f2a1e-1111" "md")))
      (expect (= "vis-session-0f3f2a1e.vissnap"
                 (session-export/file-name "…/…" "0f3f2a1e-1111" "vissnap")))
      (let [long-name (session-export/file-name (apply str (repeat 30 "deploy ")) "0f3f2a1e" "md")]
        (expect (= (str (subs (apply str (repeat 30 "deploy-")) 0 48) "-0f3f2a1e.md") long-name))
        (expect (not (re-find #"--" long-name))))))

(defdescribe
  write-test
  (it "replaces the file of an earlier export and leaves no temporary file"
      (let [dir
            (.toFile (Files/createTempDirectory "vis-export-test" (make-array FileAttribute 0)))

            folder
            (File. dir "Vis Artifacts")]

        (try (session-export/write! folder "a.md" (.getBytes "first" "UTF-8"))
             (let [file (session-export/write! folder "a.md" (.getBytes "second" "UTF-8"))]
               (expect (= "second" (slurp file)))
               (expect (= ["a.md"] (vec (.list folder)))))
             (finally (run! #(.delete ^File %) (reverse (file-seq dir))))))))
