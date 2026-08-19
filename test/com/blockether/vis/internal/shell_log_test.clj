(ns com.blockether.vis.internal.shell-log-test
  "The storage contract of a background shell's output: a FILE plus a byte cursor.

   Every test here reads with the cursor the model is told to use — start where
   the last read ended — because that loop is the whole promise: no overlap, no
   gap, and no head lost to a buffer that ran out of room."
  (:require [clojure.java.io :as io]
            [clojure.spec.alpha :as s]
            [clojure.string :as str]
            [com.blockether.vis.core :as vis]
            [com.blockether.vis.ext.persistance-sqlite.test-helpers :as h]
            [com.blockether.vis.internal.shell-log :as shell-log]
            [lazytest.core :refer [defdescribe expect it]])
  (:import (java.io File)))

(defn- session-id
  "A session id no other run shares, so the logs directory is this test's alone."
  [label]
  (str "shell-log-test-" label "-" (System/nanoTime)))

(defn- write-log!
  "Put `text` in `session`/`id`'s log the way the pump does — through the sink."
  [session id ^String text]
  (let [sink (shell-log/open! session id)]
    (.write ^java.io.OutputStream (:out sink) (.getBytes text "UTF-8"))
    (shell-log/close! sink)
    sink))

(defn- read-all
  "Walk the whole log from byte 0 the way a polling loop does: read, append,
   continue at `next-offset`. Returns the joined text."
  [session id limit]
  (let [file (shell-log/log-file session id)]
    (loop [off 0
           acc []]

      (let [chunk (shell-log/read-chunk id file {:offset off :limit limit})
            next-off (long (:next-offset chunk))]

        (if (or (:is-eof chunk) (<= next-off off))
          (str/join (conj acc (:text chunk)))
          (recur next-off (conj acc (:text chunk))))))))

(defdescribe
  read-chunk-test
  ;; Regression, issue #shell-log-ring: a background shell's output lived only in
  ;; a bounded in-memory ring, so a command that printed more lines than the ring
  ;; held lost its head before the first poll and NO sequence of reads could get
  ;; it back — the tool answered "dropped: 8000" and the bytes were gone.
  (it "walks a log far bigger than one read with no overlap and no gap"
      (let [sid
            (session-id "whole")

            text
            (str/join (map #(str "line " % " of the build\n") (range 10000)))]

        (try (write-log! sid "build" text)
             (expect (< 100000 (count text)))
             ;; Read in small windows: the loop, not the window, is what has to
             ;; return the whole stream.
             (expect (= text (read-all sid "build" 4096)))
             ;; And the default window says the same thing about the same file.
             (expect (= text (read-all sid "build" nil)))
             (finally (shell-log/delete-session-logs! sid)))))
  (it "answers a spec-valid chunk"
      (let [sid (session-id "spec")]
        (try (write-log! sid "s" "hello\n")
             (let [chunk (shell-log/read-chunk "s" (shell-log/log-file sid "s"))]
               (expect (s/valid? ::shell-log/log-chunk chunk))
               (expect (= "hello\n" (:text chunk)))
               (expect (= 0 (:offset chunk)))
               (expect (= 6 (:next-offset chunk)))
               (expect (true? (:is-eof chunk)))
               (expect (false? (:is-truncated chunk))))
             (finally (shell-log/delete-session-logs! sid)))))
  (it "reads the TAIL when no offset is named, and the head is one read away"
      (let [sid
            (session-id "tail")

            text
            (str/join (map #(str "chunk-" % "\n") (range 20000)))]

        (try (write-log! sid "t" text)
             (let [tail
                   (shell-log/read-chunk "t" (shell-log/log-file sid "t"))

                   head
                   (shell-log/read-chunk "t" (shell-log/log-file sid "t") {:offset 0})]

               ;; A watcher wants the END of a live command…
               (expect (str/ends-with? (:text tail) "chunk-19999\n"))
               (expect (true? (:is-eof tail)))
               (expect (<= (count (:text tail)) shell-log/default-chunk-bytes))
               ;; …and the beginning is not gone, it is one `{:offset 0}` away.
               (expect (str/starts-with? (:text head) "chunk-0\n"))
               (expect (false? (:is-eof head)))
               (expect (true? (:is-truncated head))))
             (finally (shell-log/delete-session-logs! sid)))))
  (it "counts LINES back from the end when the offset is NEGATIVE"
      ;; Regression, handle audit: `sh.logs(-50)` was refused with "offset must not be
      ;; negative", so "the last 50 lines" cost a read of the whole log plus byte
      ;; arithmetic over output the caller had just paid to receive.
      (let [sid
            (session-id "neg")

            text
            (str/join (map #(str "line-" % "\n") (range 500)))

            text-at
            (fn [off]
              (:text (shell-log/read-chunk "n" (shell-log/log-file sid "n") {:offset off})))]

        (try (write-log! sid "n" text)
             ;; -1 is the LAST line: the trailing newline ENDS it rather than
             ;; beginning an empty one after it.
             (expect (= "line-499\n" (text-at -1)))
             (expect (= "line-497\nline-498\nline-499\n" (text-at -3)))
             ;; More tail than the log holds is the whole log, never an error.
             (expect (= text (text-at -100000)))
             (let [c (shell-log/read-chunk "n" (shell-log/log-file sid "n") {:offset -3})]
               (expect (s/valid? ::shell-log/log-chunk c))
               (expect (true? (:is-eof c)))
               ;; The offset it answers with is a real line boundary, so feeding it
               ;; back as a POSITIVE cursor reads exactly the same bytes.
               (expect (= (:text c) (text-at (:offset c)))))
             ;; A byte cap still caps: a tail bigger than the window clamps into it.
             (let [c
                   (shell-log/read-chunk "n" (shell-log/log-file sid "n") {:offset -500 :limit 64})]
               (expect (>= 64 (count (:text c))))
               (expect (true? (:is-eof c))))
             (finally (shell-log/delete-session-logs! sid)))))
  (it "reads a negative tail off a log whose last line is unfinished"
      ;; A live command mid-line is the normal case for a watcher, and it has no
      ;; trailing newline to count from.
      (let [sid
            (session-id "neg-partial")

            text-at
            (fn [off]
              (:text (shell-log/read-chunk "p" (shell-log/log-file sid "p") {:offset off})))]

        (try (write-log! sid "p" "alpha\nbeta\ngamma")
             (expect (= "gamma" (text-at -1)))
             (expect (= "beta\ngamma" (text-at -2)))
             (expect (= "alpha\nbeta\ngamma" (text-at -9)))
             (finally (shell-log/delete-session-logs! sid)))))
  (it "never splits a multi-byte character across two reads"
      (let [sid
            (session-id "utf8")

            ;; Four bytes per emoji plus three per arrow: a byte cap lands mid
            ;; character constantly, and a chunk that returned half of one would
            ;; hand the model a replacement glyph it can never repair.
            text
            (str/join (map #(str "→ 🚀 " % "\n") (range 4000)))]

        (try (write-log! sid "u" text)
             (expect (= text (read-all sid "u" 101)))
             (let [tail (shell-log/read-chunk "u" (shell-log/log-file sid "u") {:limit 103})]
               (expect (not (str/includes? (:text tail) "\ufffd"))))
             (finally (shell-log/delete-session-logs! sid)))))
  (it "reads a log that is still being written, and says there is more"
      (let [sid (session-id "live")]
        (try (let [sink (shell-log/open! sid "live")
                   file (shell-log/log-file sid "live")
                   out ^java.io.OutputStream (:out sink)]

               (.write out (.getBytes "first\n" "UTF-8"))
               (.flush out)
               (let [a (shell-log/read-chunk "live" file {:offset 0})]
                 (expect (= "first\n" (:text a)))
                 (expect (true? (:is-eof a)))
                 (.write out (.getBytes "second\n" "UTF-8"))
                 (.flush out)
                 ;; Continuing at the cursor returns ONLY what is new.
                 (let [b (shell-log/read-chunk "live" file {:offset (:next-offset a)})]
                   (expect (= "second\n" (:text b)))
                   (expect (= (:next-offset a) (:offset b)))))
               (shell-log/close! sink))
             (finally (shell-log/delete-session-logs! sid)))))
  (it "treats a missing log as an empty chunk rather than an error"
      (let [chunk (shell-log/read-chunk "never-ran" (shell-log/log-file (session-id "gone") "x"))]
        (expect (= "" (:text chunk)))
        (expect (= 0 (:offset chunk)))
        (expect (= 0 (:next-offset chunk)))
        (expect (true? (:is-eof chunk)))))
  (it "keeps a hostile id inside the log directory"
      (let [sid
            (session-id "escape")

            file
            (shell-log/log-file sid "../../etc/passwd")]

        (expect (= (.getCanonicalPath (shell-log/session-dir sid))
                   (.getCanonicalPath ^File (.getParentFile file)))))))

;; The switches a reader of a long log actually reaches for: "the last ten lines",
;; then "the next ten". Before `:lines` the only line-aware read was the whole
;; tail, so walking a log FORWARD meant guessing byte counts.
(defdescribe
  line-window-test
  "`:lines` is a LINE window: the last N with no offset, the NEXT N from one."
  (it "with no offset it reads the LAST n lines"
      (let [sid
            (session-id "linetail")

            text
            (str/join (map #(str "line-" % "\n") (range 500)))]

        (try (write-log! sid "w" text)
             (let [c (shell-log/read-chunk "w" (shell-log/log-file sid "w") {:lines 3})]
               (expect (s/valid? ::shell-log/log-chunk c))
               (expect (= "line-497\nline-498\nline-499\n" (:text c)))
               (expect (true? (:is-eof c))))
             ;; More lines than the log holds is the whole log, never an error.
             (let [c (shell-log/read-chunk "w" (shell-log/log-file sid "w") {:lines 100000})]
               (expect (= text (:text c))))
             (finally (shell-log/delete-session-logs! sid)))))
  (it "from an offset it reads the NEXT n lines, and next-offset continues there"
      (let [sid
            (session-id "linewalk")

            text
            (str/join (map #(str "line-" % "\n") (range 500)))

            file
            (shell-log/log-file sid "w")]

        (try (write-log! sid "w" text)
             (let [a
                   (shell-log/read-chunk "w" file {:offset 0 :lines 10})

                   b
                   (shell-log/read-chunk "w" file {:offset (:next-offset a) :lines 10})]

               (expect (= (mapv #(str "line-" %) (range 10)) (str/split-lines (:text a))))
               (expect (= (mapv #(str "line-" %) (range 10 20)) (str/split-lines (:text b))))
               ;; The window closed the chunk early, so there IS more: a reader who
               ;; sleeps here would sleep on a log that is already written.
               (expect (false? (:is-eof a)))
               (expect (true? (:is-truncated a))))
             (finally (shell-log/delete-session-logs! sid))))))
(defdescribe
  tee-test
  (it "writes through every byte the pump reads"
      (let [sid (session-id "tee")]
        (try (let [sink (shell-log/open! sid "p")
                   source (java.io.ByteArrayInputStream. (.getBytes "alpha\nbeta\n" "UTF-8"))
                   copy (with-open [r (io/reader (shell-log/tee source sink))]
                          (slurp r))]

               (shell-log/close! sink)
               ;; What the pump saw and what the file holds are the same bytes —
               ;; the two views can never disagree about what the shell printed.
               (expect (= "alpha\nbeta\n" copy))
               (expect (= "alpha\nbeta\n"
                          (:text
                            (shell-log/read-chunk "p" (shell-log/log-file sid "p") {:offset 0})))))
             (finally (shell-log/delete-session-logs! sid))))))

(defdescribe retention-test
             (it "deletes every log of a session and nothing of another"
                 (let [a
                       (session-id "keep")

                       b
                       (session-id "drop")]

                   (try (write-log! a "one" "a\n")
                        (write-log! b "one" "b\n")
                        (write-log! b "two" "b\n")
                        (shell-log/delete-session-logs! b)
                        (expect (not (.exists (shell-log/session-dir b))))
                        (expect (.isFile (shell-log/log-file a "one")))
                        (finally (shell-log/delete-session-logs! a)))))
             (it "is silent about a session that never ran a shell"
                 (expect (nil? (shell-log/delete-session-logs! (session-id "absent"))))))

(defdescribe
  index-test
  (it "makes a log findable by session, newest first, with its exit"
      (let [s (vis/db-create-connection! :memory)]
        (try (let [cid (h/store-session! s {:channel :tui :title "Shell log fixture"})
                   base
                   {:command "npm run build" :script "npm run build" :dir "." :log-path "/x.log"}]

               (shell-log/index! s cid "build" (assoc base :started-at 100))
               (shell-log/index! s cid "serve" (assoc base :started-at 200))
               ;; The same id is one log, updated in place — a shell that exits
               ;; does not become a second row.
               (shell-log/index! s
                                 cid
                                 "build"
                                 (assoc base
                                   :started-at 100
                                   :ended-at 900
                                   :exit 0))
               ;; The row outlives the process, so it is JSON on disk and wears
               ;; the wire's snake_case string keys.
               (let [rows (shell-log/session-logs s cid)]
                 (expect (= 2 (count rows)))
                 (expect (= ["serve" "build"] (mapv #(get % "id") rows)))
                 (let [build (first (filter #(= "build" (get % "id")) rows))]
                   (expect (= 0 (get build "exit")))
                   (expect (= 900 (get build "ended_at")))
                   (expect (= "npm run build" (get build "script")))
                   (expect (= (str cid) (get build "session_id"))))))
             (finally (vis/db-dispose-connection! s)))))
  (it "is best effort: no database is no index and no throw"
      (expect (nil? (shell-log/index! nil "sid" "id" {:started-at 1})))
      (expect (= [] (shell-log/session-logs nil "sid")))))
