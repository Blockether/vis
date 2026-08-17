(ns com.blockether.vis.internal.foundation.rewind-test
  "Adversarial tests for durable file-state rewind.

   Every test runs against a THROWAWAY store root and a THROWAWAY working
   directory, with the git baseline disabled unless the test is specifically
   about it — otherwise the suite would snapshot this repository."
  {:clj-kondo/config
   '{:linters {:unresolved-symbol
               {:exclude [(com.blockether.vis.internal.foundation.rewind-test/with-store)]}}}}
  (:refer-clojure :exclude [abs])
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [com.blockether.vis.core :as vis]
            [com.blockether.vis.internal.foundation.rewind :as rw]
            [lazytest.core :refer [defdescribe expect it]])
  (:import (java.io File)
           (java.nio.file Files LinkOption)
           (java.nio.file.attribute FileAttribute)))

;; Fixtures

(defn- tmp-dir
  ^File [prefix]
  (.toFile (Files/createTempDirectory (str prefix) (make-array FileAttribute 0))))

(defn- rm-rf
  [^File f]
  (when (and f (Files/exists (.toPath f) (into-array LinkOption [LinkOption/NOFOLLOW_LINKS])))
    (doseq [c (reverse (file-seq f))]
      (io/delete-file (io/file c) true))))

(defmacro with-store
  "Runs `body` with a private store root, a private work dir, no git baseline,
   and a clean memo. Both temp trees are removed afterwards."
  [[store work] & body]
  `(let
     [root#
      (tmp-dir "vis-rewind-store")

      wd#
      (tmp-dir "vis-rewind-work")]

     (try (binding
            [rw/*store-root*
             root#

             rw/*git-baseline?*
             false]

            (rw/reset-memo!)
            (let
              [~store
               root#

               ~work
               wd#]

              ~@body))
          (finally (rm-rf root#) (rm-rf wd#)))))

(defn- ctx
  [session turn op]
  {:session session
   :turn turn
   :turn-id (str "turn-" turn)
   :op op
   :user-request (str "prompt for turn " turn)})

(defn- wfile ^File [^File work & segs] (apply io/file work segs))

(defn- put! [^File f content] (io/make-parents f) (spit f content) f)

(defn- abs ^String [^File f] (str (.normalize (.toAbsolutePath (.toPath f)))))

(defn- objects
  "Every blob file currently pooled for `session`."
  [session]
  (let [d (io/file (rw/store-dir session) "objects")]
    (if (.isDirectory d) (filterv #(.isFile ^File %) (file-seq d)) [])))

(defn- journal-lines
  [session]
  (let [f (io/file (rw/store-dir session) "journal.ndjson")]
    (if (.isFile f) (str/split-lines (slurp f)) [])))

(defn- append-raw!
  "Hand-append a raw journal line, the way a corrupted store or a hostile
   editor would."
  [session line]
  (let [f (io/file (rw/store-dir session) "journal.ndjson")]
    (io/make-parents f)
    (spit f (str line "\n") :append true)))

;; Round trips

(defdescribe round-trip-test
             (it "restores the exact pre-mutation bytes"
                 (with-store [_ work]
                             (let
                               [f
                                (put! (wfile work "a.txt") "ORIGINAL")

                                s
                                "sess-round"]

                               (rw/record-pre! (ctx s 1 :patch) [(abs f)])
                               (spit f "WRECKED")
                               (let [r (rw/restore! s 1)]
                                 (expect (= "ORIGINAL" (slurp f)))
                                 (expect (= 1 (get r "restored")))
                                 (expect (= [] (get r "failed")))
                                 (expect (= "complete" (get r "coverage")))))))
             (it "round-trips unicode, CRLF and NUL bytes without mangling them"
                 (with-store [_ work]
                             (let
                               [content
                                "héllo — 世界\r\nline2\u0000tail"

                                f
                                (put! (wfile work "utf8 with spaces.txt") content)

                                s
                                "sess-utf8"]

                               (rw/record-pre! (ctx s 1 :patch) [(abs f)])
                               (spit f "")
                               (rw/restore! s 1)
                               (expect (= content (slurp f))))))
             (it "round-trips a zero-byte file"
                 (with-store [_ work]
                             (let
                               [f
                                (put! (wfile work "empty.txt") "")

                                s
                                "sess-empty"]

                               (rw/record-pre! (ctx s 1 :patch) [(abs f)])
                               (spit f "no longer empty")
                               (rw/restore! s 1)
                               (expect (= "" (slurp f)))
                               (expect (zero? (.length f))))))
             (it "deletes a file that did not exist before the turn"
                 (with-store [_ work]
                             (let
                               [f
                                (wfile work "created.txt")

                                s
                                "sess-created"]

                               (expect (not (.exists f)))
                               (rw/record-pre! (ctx s 1 :patch) [(abs f)])
                               (put! f "brand new")
                               (let [r (rw/restore! s 1)]
                                 (expect (not (.exists f)))
                                 (expect (= 1 (get r "deleted")))))))
             (it "recreates a file the turn deleted, including its exec bit"
                 (with-store [_ work]
                             (let
                               [f
                                (put! (wfile work "script.sh") "#!/bin/sh\necho hi\n")

                                s
                                "sess-deleted"]

                               (.setExecutable f true)
                               (rw/record-pre! (ctx s 1 :fs) [(abs f)])
                               (io/delete-file f)
                               (expect (not (.exists f)))
                               (rw/restore! s 1)
                               (expect (.isFile f))
                               (expect (= "#!/bin/sh\necho hi\n" (slurp f)))
                               (expect (.canExecute f)))))
             (it "is idempotent — restoring twice leaves the same tree"
                 (with-store [_ work]
                             (let
                               [f
                                (put! (wfile work "a.txt") "ORIGINAL")

                                s
                                "sess-idem"]

                               (rw/record-pre! (ctx s 1 :patch) [(abs f)])
                               (spit f "WRECKED")
                               (rw/restore! s 1)
                               (rw/restore! s 1)
                               (expect (= "ORIGINAL" (slurp f)))))))

;; Turn semantics

(defdescribe
  turn-semantics-test
  (it "keeps the FIRST pre-image of a turn, not the last"
      (with-store [_ work]
                  (let
                    [f
                     (put! (wfile work "a.txt") "V1")

                     s
                     "sess-first-wins"

                     c
                     (ctx s 1 :patch)]

                    (rw/record-pre! c [(abs f)])
                    (spit f "V2")
                    (rw/record-pre! c [(abs f)])
                    (spit f "V3")
                    (rw/record-pre! c [(abs f)])
                    (rw/restore! s 1)
                    (expect (= "V1" (slurp f)))
                    (expect (= 1 (count (filter #(= "pre" (get % "kind")) (rw/journal s))))))))
  (it "rewinds to a chosen turn without undoing earlier turns"
      (with-store [_ work]
                  (let
                    [a
                     (put! (wfile work "a.txt") "A1")

                     b
                     (put! (wfile work "b.txt") "B1")

                     s
                     "sess-multi"]

                    (rw/record-pre! (ctx s 1 :patch) [(abs a)])
                    (spit a "A2")
                    (rw/record-pre! (ctx s 2 :patch) [(abs b)])
                    (spit b "B2")
                    (rw/restore! s 2)
                    (expect (= "A2" (slurp a)) "turn 1's change survives a rewind to turn 2")
                    (expect (= "B1" (slurp b)))
                    (rw/restore! s 1)
                    (expect (= "A1" (slurp a))))))
  (it "restores a path to its state at the START of the target turn"
      (with-store [_ work]
                  (let
                    [f
                     (put! (wfile work "a.txt") "T1")

                     s
                     "sess-earliest"]

                    (rw/record-pre! (ctx s 1 :patch) [(abs f)])
                    (spit f "T2")
                    (rw/record-pre! (ctx s 2 :patch) [(abs f)])
                    (spit f "T3")
                    (rw/record-pre! (ctx s 3 :patch) [(abs f)])
                    (spit f "T4")
                    ;; Exactly ONE restore entry per path, and it is the EARLIEST at/after
                    ;; the target turn.
                    (let [pl (rw/plan s 2)]
                      (expect (= 1 (count (get pl "restore")))))
                    (rw/restore! s 2)
                    (expect (= "T2" (slurp f))))))
  (it "reports one rewind point per turn, in order, with the prompt"
      (with-store [_ work]
                  (let
                    [a
                     (put! (wfile work "a.txt") "A")

                     b
                     (put! (wfile work "b.txt") "B")

                     s
                     "sess-points"]

                    (rw/record-pre! (ctx s 1 :patch) [(abs a)])
                    (rw/record-pre! (ctx s 2 :patch) [(abs a) (abs b)])
                    (let [ps (rw/points s)]
                      (expect (= [1 2] (mapv #(get % "turn") ps)))
                      (expect (= [1 2] (mapv #(get % "files") ps)))
                      (expect (= ["patch"] (get (first ps) "ops")))
                      (expect (= ["patch"] (get (second ps) "ops")))
                      (expect (= "prompt for turn 2" (get (second ps) "user_request")))))))
  (it "keeps no rewind point for a turn that touched nothing"
      (with-store [_ _work]
                  (let [s "sess-noop"]
                    (rw/record-pre! (ctx s 1 :patch) [])
                    (expect (= [] (rw/points s))))))
  (it "does not leak the memo across turns of the same session"
      (with-store [_ work]
                  (let
                    [f
                     (put! (wfile work "a.txt") "V1")

                     s
                     "sess-memo"]

                    (rw/record-pre! (ctx s 1 :patch) [(abs f)])
                    (spit f "V2")
                    ;; A new turn must pre-image the SAME path again.
                    (rw/record-pre! (ctx s 2 :patch) [(abs f)])
                    (expect (= 2 (count (filter #(= "pre" (get % "kind")) (rw/journal s)))))))))

;; Store integrity

(defdescribe
  store-integrity-test
  (it "deduplicates identical content into ONE pooled blob"
      (with-store [_ work]
                  (let [s "sess-dedup"]
                    (doseq [n ["a.txt" "b.txt" "c.txt"]]
                      (put! (wfile work n) "same bytes everywhere"))
                    (rw/record-pre! (ctx s 1 :patch)
                                    (mapv #(abs (wfile work %)) ["a.txt" "b.txt" "c.txt"]))
                    (expect (= 1 (count (objects s)))))))
  (it "survives a corrupt journal tail — a crashed append cannot brick history"
      (with-store [_ work]
                  (let
                    [f
                     (put! (wfile work "a.txt") "GOOD")

                     s
                     "sess-corrupt"]

                    (rw/record-pre! (ctx s 1 :patch) [(abs f)])
                    (append-raw! s "{\"kind\": \"pre\", trunc")
                    (append-raw! s "")
                    (append-raw! s "null")
                    (append-raw! s "[1,2,3]")
                    (spit f "BAD")
                    (expect (= 1 (count (rw/journal s))))
                    (rw/restore! s 1)
                    (expect (= "GOOD" (slurp f))))))
  (it "refuses a hand-edited journal entry that points outside an absolute path"
      (with-store
        [_ work]
        (let
          [s
           "sess-traversal"

           victim
           (put! (wfile work "victim.txt") "UNTOUCHED")]

          (append-raw! s
                       (str "{\"kind\":\"pre\",\"state\":\"absent\",\"turn\":1,\"session\":\"" s
                            "\"," "\"path\":\"../../../../../../etc/hosts\"}"))
          (append-raw! s
                       (str "{\"kind\":\"pre\",\"state\":\"absent\",\"turn\":1,\"session\":\""
                            s
                            "\","
                            "\"path\":\"" (.getParent victim)
                            "/./victim.txt" "\"}"))
          (let [r (rw/restore! s 1)]
            (expect (every? #(= "skipped" (get % "action")) (get r "applied")))
            (expect (= "UNTOUCHED" (slurp victim)) "a non-normalized path is never applied")))))
  (it "skips instead of crashing when a blob has vanished from the pool"
      (with-store [_ work]
                  (let
                    [f
                     (put! (wfile work "a.txt") "ORIGINAL")

                     s
                     "sess-lost-blob"]

                    (rw/record-pre! (ctx s 1 :patch) [(abs f)])
                    (spit f "WRECKED")
                    (doseq [o (objects s)]
                      (io/delete-file o))
                    (let [r (rw/restore! s 1)]
                      (expect (= "WRECKED" (slurp f)))
                      (expect (= 1 (count (get r "failed"))))
                      (expect (= "blob missing from pool"
                                 (get (first (get r "failed")) "error")))))))
  (it "confines a hostile session id to one segment inside the store root"
      (with-store [root _work]
                  (doseq [evil ["../../etc" "a/b/c" "" "  " (apply str (repeat 400 "x"))]]
                    (let [d (rw/store-dir evil)]
                      (expect (= (abs root) (str (.getParentFile d)))
                              (str "session id escaped the store root: " (pr-str evil)))
                      (expect (<= (count (.getName d)) 128))))))
  (it "never rewrites a pooled blob once written"
      (with-store [_ work]
                  (let
                    [f
                     (put! (wfile work "a.txt") "STABLE")

                     s
                     "sess-immutable"]

                    (rw/record-pre! (ctx s 1 :patch) [(abs f)])
                    (let
                      [o
                       (first (objects s))

                       before
                       (.lastModified ^File o)]

                      (spit f "STABLE")
                      (rw/record-pre! (ctx s 2 :patch) [(abs f)])
                      (expect (= 1 (count (objects s))))
                      (expect (= before (.lastModified ^File o))))))))

;; Nasty filesystem shapes

(defdescribe
  filesystem-shapes-test
  (it "restores a symlink as a SYMLINK, not as a copy of its target"
      (with-store
        [_ work]
        (let
          [target
           (put! (wfile work "target.txt") "TARGET")

           link
           (wfile work "link.txt")

           s
           "sess-symlink"]

          (Files/createSymbolicLink (.toPath link) (.toPath target) (make-array FileAttribute 0))
          (rw/record-pre! (ctx s 1 :fs) [(abs link)])
          (io/delete-file link)
          (put! link "now a real file")
          (rw/restore! s 1)
          (expect (Files/isSymbolicLink (.toPath link)))
          (expect (= (.toPath target) (Files/readSymbolicLink (.toPath link)))))))
  (it "pre-images the SYMLINK itself and leaves its target alone"
      (with-store
        [_ work]
        (let
          [target
           (put! (wfile work "target.txt") "TARGET")

           link
           (wfile work "link.txt")

           s
           "sess-symlink-target"]

          (Files/createSymbolicLink (.toPath link) (.toPath target) (make-array FileAttribute 0))
          (rw/record-pre! (ctx s 1 :fs) [(abs link)])
          (let [e (first (filter #(= (abs link) (get % "path")) (rw/journal s)))]
            (expect (= "symlink" (get e "state")))
            (expect (nil? (get e "sha")) "a symlink must not be pooled as file content"))
          (expect (= "TARGET" (slurp target))))))
  (it "restores a whole subtree the turn deleted, and prunes what it added"
      (with-store [_ work]
                  (let
                    [d
                     (wfile work "tree")

                     s
                     "sess-tree"]

                    (put! (io/file d "one.txt") "ONE")
                    (put! (io/file d "nested" "two.txt") "TWO")
                    (rw/record-pre! (ctx s 1 :fs) [(abs d)] {:recurse? true})
                    (rm-rf d)
                    (expect (not (.exists d)))
                    (rw/restore! s 1)
                    (expect (= "ONE" (slurp (io/file d "one.txt"))))
                    (expect (= "TWO" (slurp (io/file d "nested" "two.txt")))))))
  (it "prunes a file the turn ADDED to a captured directory"
      (with-store [_ work]
                  (let
                    [d
                     (wfile work "tree")

                     s
                     "sess-tree-prune"]

                    (put! (io/file d "one.txt") "ONE")
                    (rw/record-pre! (ctx s 1 :fs) [(abs d)] {:recurse? true})
                    (put! (io/file d "intruder.txt") "SHOULD NOT SURVIVE")
                    (rw/restore! s 1)
                    (expect (= "ONE" (slurp (io/file d "one.txt"))))
                    (expect (not (.exists (io/file d "intruder.txt")))))))
  (it "restores a file that the turn replaced with a directory"
      (with-store [_ work]
                  (let
                    [f
                     (put! (wfile work "shape.txt") "I AM A FILE")

                     s
                     "sess-file-to-dir"]

                    (rw/record-pre! (ctx s 1 :fs) [(abs f)])
                    (io/delete-file f)
                    (.mkdirs f)
                    (put! (io/file f "surprise.txt") "x")
                    (rw/restore! s 1)
                    (expect (.isFile f))
                    (expect (= "I AM A FILE" (slurp f))))))
  (it "restores a directory that the turn replaced with a file"
      (with-store [_ work]
                  (let
                    [d
                     (wfile work "shape")

                     s
                     "sess-dir-to-file"]

                    (put! (io/file d "inner.txt") "INNER")
                    (rw/record-pre! (ctx s 1 :fs) [(abs d)] {:recurse? true})
                    (rm-rf d)
                    (put! d "I AM A FILE NOW")
                    (rw/restore! s 1)
                    (expect (.isDirectory d))
                    (expect (= "INNER" (slurp (io/file d "inner.txt")))))))
  (it "marks an oversized file uncovered instead of pretending to cover it"
      (with-store [_ work]
                  (binding [rw/*max-blob-bytes* 8]
                    (let
                      [f (put! (wfile work "big.bin") (apply str (repeat 1000 "x")))
                       s "sess-big"]

                      (rw/record-pre! (ctx s 1 :patch) [(abs f)])
                      (let
                        [e (first (rw/journal s))
                         pl (rw/plan s 1)]

                        (expect (= "uncovered" (get e "kind")))
                        (expect (= "too-large" (get e "reason")))
                        (expect (= "partial" (get pl "coverage")))
                        (expect (= 1 (count (get pl "uncovered"))))
                        (expect (= [] (get pl "restore"))))))))
  (it "counts an oversized file as an uncovered rewind point, not a silent skip"
      (with-store [_ work]
                  (binding [rw/*max-blob-bytes* 8]
                    (let
                      [f (put! (wfile work "big.bin") (apply str (repeat 1000 "x")))
                       s "sess-big-points"]

                      (rw/record-pre! (ctx s 1 :patch) [(abs f)])
                      (expect (= 1 (get (first (rw/points s)) "uncovered"))))))))

;; Dry runs

(defdescribe dry-run-test
             (it "reports the plan without touching a single byte"
                 (with-store [_ work]
                             (let
                               [f
                                (put! (wfile work "a.txt") "ORIGINAL")

                                gone
                                (wfile work "created.txt")

                                s
                                "sess-dry"]

                               (rw/record-pre! (ctx s 1 :patch) [(abs f) (abs gone)])
                               (spit f "WRECKED")
                               (put! gone "new")
                               (let [r (rw/restore! s 1 {:is-dry-run true})]
                                 (expect (true? (get r "is_dry_run")))
                                 (expect (= [] (get r "applied")))
                                 (expect (= 2 (count (get r "restore"))))
                                 (expect (= "WRECKED" (slurp f)))
                                 (expect (.exists gone)))))))

;; The op-hook must be transparent

(defn- env-for
  [session turn]
  {:session-id session
   :turn-state-atom (atom {:turn-position turn
                           :session-turn-id (str "turn-" turn)
                           :user-request (str "prompt for turn " turn)})})

(defdescribe
  around-hook-test
  (it "returns the wrapped op's value unchanged"
      (with-store [_ work]
                  (let
                    [f
                     (put! (wfile work "a.txt") "ORIGINAL")

                     s
                     "sess-hook-value"

                     args
                     {:path (abs f)}

                     out
                     (rw/around-hook (env-for s 1)
                                     :patch
                                     args
                                     (fn [a]
                                       {:echo a :ok true}))]

                    (expect (= {:echo args :ok true} out)))))
  (it "captures a pre-image before the op runs"
      (with-store [_ work]
                  (let
                    [f
                     (put! (wfile work "a.txt") "ORIGINAL")

                     s
                     "sess-hook-capture"]

                    (rw/around-hook (env-for s 3)
                                    :patch
                                    {:path (abs f)}
                                    (fn [_]
                                      (spit f "WRECKED")
                                      :done))
                    (expect (= "WRECKED" (slurp f)))
                    (rw/restore! s 3)
                    (expect (= "ORIGINAL" (slurp f))))))
  (it "still captures when the wrapped op THROWS — a failed write can be partial"
      (with-store [_ work]
                  (let
                    [f
                     (put! (wfile work "a.txt") "ORIGINAL")

                     s
                     "sess-hook-throw"

                     thrown
                     (try (rw/around-hook (env-for s 1)
                                          :patch
                                          {:path (abs f)}
                                          (fn [_]
                                            (spit f "HALF")
                                            (throw (ex-info "boom" {}))))
                          (catch Exception e e))]

                    (expect (instance? clojure.lang.ExceptionInfo thrown))
                    (expect (= "boom" (ex-message thrown)))
                    (rw/restore! s 1)
                    (expect (= "ORIGINAL" (slurp f))))))
  (it "is a pass-through when disabled"
      (with-store [_ work]
                  (let
                    [f
                     (put! (wfile work "a.txt") "ORIGINAL")

                     s
                     "sess-hook-off"]

                    (binding [rw/*enabled?* false]
                      (expect (= :ok
                                 (rw/around-hook (env-for s 1)
                                                 :patch
                                                 {:path (abs f)}
                                                 (fn [_]
                                                   :ok)))))
                    (expect (= [] (rw/journal s))))))
  (it "never fails the tool when the store itself is unusable"
      (with-store [_ work]
                  (let
                    [f
                     (put! (wfile work "a.txt") "ORIGINAL")

                     blocker
                     (put! (wfile work "blocked") "not a directory")]

                    ;; Point the store root at a REGULAR FILE: every store write must fail.
                    (binding [rw/*store-root* blocker]
                      (expect (= :ok
                                 (rw/around-hook (env-for "sess-broken-store" 1)
                                                 :patch
                                                 {:path (abs f)}
                                                 (fn [_]
                                                   :ok))))))))
  (it "finds paths at any nesting depth and under every path key"
      (with-store
        [_ work]
        (let
          [a
           (put! (wfile work "a.txt") "A")

           b
           (put! (wfile work "b.txt") "B")

           c
           (put! (wfile work "c.txt") "C")

           s
           "sess-hook-walk"

           args
           {:edits [{:path (abs a)} {"path" (abs b)}] :nested {:deep {:files [(abs c)]}}}]

          (rw/around-hook (env-for s 1)
                          :patch
                          args
                          (fn [_]
                            :ok))
          (let [paths (set (map #(get % "path") (rw/journal s)))]
            (expect (contains? paths (abs a)))
            (expect (contains? paths (abs b)))
            (expect (contains? paths (abs c)))))))
  (it "hooks every mutating op and the shell sweep"
      (expect (= #{:patch :fs :format_code} rw/mutation-ops))
      (expect (contains? rw/sweep-ops :shell))
      (let [hooked (set (map :op rw/op-hooks))]
        (expect (every? hooked rw/mutation-ops))
        (expect (every? hooked rw/sweep-ops))
        (expect (every? #(= :around (:phase %)) rw/op-hooks)))))

;; Concurrency

(defdescribe
  concurrency-test
  (it "keeps the journal line-atomic under parallel captures"
      (with-store [_ work]
                  (let
                    [s
                     "sess-parallel"

                     n
                     40

                     files
                     (mapv (fn [i]
                             (put! (wfile work (str "f" i ".txt")) (str "content-" i)))
                           (range n))]

                    (->> files
                         (mapv (fn [f]
                                 (future (rw/record-pre! (ctx s 1 :patch) [(abs f)]))))
                         (run! deref))
                    (let
                      [lines
                       (journal-lines s)

                       es
                       (rw/journal s)]

                      (expect (= n (count lines)) "one line per capture, never a torn write")
                      (expect (= n (count es)) "every line parses")
                      (expect (= n (count (set (map #(get % "path") es)))))))))
  (it "captures a path exactly once even when many threads race on it"
      (with-store [_ work]
                  (let
                    [f
                     (put! (wfile work "hot.txt") "ORIGINAL")

                     s
                     "sess-race-one-path"]

                    (->> (range 40)
                         (mapv (fn [_]
                                 (future (rw/record-pre! (ctx s 1 :patch) [(abs f)]))))
                         (run! deref))
                    (spit f "WRECKED")
                    (rw/restore! s 1)
                    (expect (= "ORIGINAL" (slurp f)))
                    ;; The memo may admit a few racers, but every pre-image is identical, so
                    ;; the pool holds exactly one blob and the plan holds exactly one entry.
                    (expect (= 1 (count (objects s))))
                    (expect (= 1 (count (get (rw/plan s 1) "restore"))))))))

;; Extension contract

(defdescribe extension-shape-test
             (it "exposes a /rewind slash command"
                 (let [ids (set (map #(or (:slash/name %) (:id %) (:name %)) rw/slash-specs))]
                   (expect (some #(str/includes? (str %) "rewind") ids))))
             (it "contributes gateway routes"
                 (let [rs (rw/routes-contribution)]
                   (expect (seq rs))
                   (expect (some #(str/includes? (pr-str %) "rewind") rs))))
             (it "registers as a vis extension with op-hooks"
                 (expect (map? rw/vis-extension))
                 (expect (seq (:ext/op-hooks rw/vis-extension)))))

;; Slash rendering — the SAME Markdown is painted by the TUI bubble and by the
;; companion's react-markdown, so the shape of the body is a contract, not
;; cosmetics. These pin the three bugs that made `/rewind` unusable:
;; a prompt-arg that hid the list, fixed-width columns Markdown collapsed, and
;; plan bullets whose verb came from a key only `apply-entry!` ever writes.

(defn- rewind-spec [] (first (filter #(= "rewind" (:slash/name %)) rw/slash-specs)))

(defn- run-rewind
  "Invoke the registered run-fn exactly as `slash/dispatch` does."
  [session ^File work argv]
  ((:slash/run-fn (rewind-spec))
    {:session/id session :workspace/root (abs work) :command/argv argv}))

(defdescribe
  slash-rendering-test
  (it "declares no :slash/prompt-arg, so bare /rewind reaches the list"
      ;; With `:slash/prompt-arg` the TUI popped a text-input for bare `/rewind`
      ;; and Esc simply cancelled — the list was unreachable from the keyboard.
      (expect (nil? (:slash/prompt-arg (rewind-spec))))
      (expect (str/includes? (:slash/usage (rewind-spec)) "[<turn>]")))
  (it "says so plainly when there is nothing to rewind"
      (with-store [_ work]
                  (let [r (run-rewind "sess-empty" work [])]
                    (expect (= :ok (:slash/status r)))
                    (expect (str/includes? (:slash/title r) "Nothing to rewind"))
                    (expect (= [] (:points (:slash/data r)))))))
  (it "renders the points as a GFM table, one row per turn"
      (with-store [_ work]
                  (let
                    [a
                     (put! (wfile work "src" "app.clj") "A")

                     s
                     "sess-render"]

                    (rw/record-pre! (ctx s 1 :patch) [(abs a)])
                    (spit a "A2")
                    (let
                      [r
                       (run-rewind s work [])

                       ls
                       (str/split-lines (:slash/body r))]

                      (expect (str/includes? (:slash/title r) "1 rewind point"))
                      (expect (= "| Turn | Files | Ops | What you asked |" (first ls)))
                      (expect (str/starts-with? (second ls) "| ---"))
                      (expect (str/includes? (nth ls 2) "| 1 "))
                      (expect (str/includes? (nth ls 2) "patch"))
                      (expect (str/includes? (nth ls 2) "prompt for turn 1"))
                      (expect (str/includes? (:slash/body r) "`/rewind 1`")
                              "the body names the exact next command")
                      (expect (= 1 (count (:points (:slash/data r)))))))))
  (it "keeps a user prompt from tearing the table apart"
      (with-store
        [_ work]
        (let
          [a
           (put! (wfile work "src" "app.clj") "A")

           s
           "sess-pipe"]

          (rw/record-pre! (assoc (ctx s 1 :patch) :user-request "run a | b\nand pipe it") [(abs a)])
          (spit a "A2")
          (let
            [ls
             (str/split-lines (:slash/body (run-rewind s work [])))

             row
             (nth ls 2)]

            ;; 4 columns + 2 rim pipes, counting only UNESCAPED bars:
            ;; a raw `|` or newline from the prompt would add cells or
            ;; split the row in half.
            (expect (= 5 (count (re-seq #"(?<!\\)\|" row))) (pr-str row))
            (expect (str/includes? row "a \\| b"))
            (expect (str/includes? row "b and pipe") "newlines are folded, not emitted")))))
  (it "gives every dry-run bullet a verb and a workspace-relative path"
      (with-store [_ work]
                  (let
                    [a
                     (put! (wfile work "src" "app.clj") "A")

                     s
                     "sess-plan"]

                    (rw/record-pre! (ctx s 1 :patch) [(abs a) (abs (wfile work "src" "new.clj"))])
                    (spit a "A2")
                    (put! (wfile work "src" "new.clj") "NEW")
                    (let
                      [r
                       (run-rewind s work ["1" "--dry-run"])

                       bullets
                       (filter #(str/starts-with? % "- ") (str/split-lines (:slash/body r)))]

                      (expect (str/includes? (:slash/title r) "Rewind plan for turn 1"))
                      (expect (= 2 (count bullets)))
                      ;; The plan's verb comes from the entry STATE; reading the
                      ;; `action` key here (only `apply-entry!` writes it) printed
                      ;; an empty verb: "-  `src/app.clj`".
                      (expect (every? #(re-matches #"- [a-z][a-z ]* `[^`]+`" %) bullets)
                              (pr-str bullets))
                      (expect (some #(= "- restore `src/app.clj`" %) bullets))
                      (expect (some #(= "- delete `src/new.clj`" %) bullets))
                      (expect (not (str/includes? (:slash/body r) (abs work)))
                              "absolute temp paths are noise in a narrow bubble")
                      (expect (str/includes? (:slash/body r) "Nothing has changed yet"))
                      (expect (= "A2" (slurp a)) "a dry run touches nothing")))))
  (it "reports what it actually did after applying"
      (with-store [_ work]
                  (let
                    [a
                     (put! (wfile work "src" "app.clj") "A")

                     s
                     "sess-apply"]

                    (rw/record-pre! (ctx s 1 :patch) [(abs a)])
                    (spit a "A2")
                    (let [r (run-rewind s work ["1"])]
                      (expect (= :ok (:slash/status r)))
                      (expect (str/includes? (:slash/title r) "Rewound to turn 1"))
                      (expect (str/includes? (:slash/body r) "- restored `src/app.clj`"))
                      (expect (str/includes? (:slash/body r) "`/rewind` lists the other points"))
                      (expect (= "A" (slurp a)))
                      (expect (= 1 (:files (:slash/data r))))))))
  (it "answers a non-numeric turn with the usage, not a stack trace"
      (with-store [_ work]
                  (let [r (run-rewind "sess-bad" work ["nope"])]
                    (expect (= :error (:slash/status r)))
                    (expect (str/includes? (pr-str r) "/rewind [<turn>] [--dry-run]"))))))

;; Context reporting — the journal knows FILES; what a turn cost the
;; CONVERSATION lives in the session store and is joined on `position`. A
;; rewind list reads as "undo everything", so the slash has to show that cost
;; AND say the conversation is never rewound. The store is best-effort: when it
;; is absent or broken the command still works, minus one column.

(defn- run-rewind-with-store
  "`run-rewind` with a session store behind it. `rows-fn` stands in for
   `db-list-session-turns`, so a test can serve rows OR throw like a broken
   store."
  [session ^File work argv rows-fn]
  (with-redefs [vis/db-list-session-turns rows-fn]
    ((:slash/run-fn (rewind-spec))
      {:session/id session :workspace/root (abs work) :command/argv argv :db-info :test-db})))

(defdescribe
  context-reporting-test
  (it
    "adds a Ctx column carrying each turn's context size"
    (with-store
      [_ work]
      (let
        [a
         (put! (wfile work "src" "app.clj") "A")

         s
         "sess-ctx-list"]

        (rw/record-pre! (ctx s 1 :patch) [(abs a)])
        (spit a "A2")
        (let
          [r
           (run-rewind-with-store
             s
             work
             []
             (fn [_ _]
               [{:position 1 :input-tokens 12480 :output-tokens 500 :iteration-count 3}]))

           ls
           (str/split-lines (:slash/body r))

           point
           (first (:points (:slash/data r)))]

          (expect (= "| Turn | Files | Ops | Ctx | What you asked |" (first ls)))
          ;; `format "%.1f"` is locale-dependent and rendered "12,5k" on a
          ;; Polish JVM; the cell must read the same everywhere.
          (expect (str/includes? (nth ls 2) "| 12.5k |") (pr-str ls))
          (expect (str/includes? (:slash/body r) "Rewinding restores files only")
                  "the list must say what rewind does NOT touch")
          (expect (= 12480 (get point "ctx_tokens")))
          (expect (= 3 (get point "ctx_iterations")))
          (expect (= {:tokens 12480 :output 500 :iterations 3}
                     (get (:context (:slash/data r)) 1)))))))
  (it "still lists points when the session store is unreachable"
      (with-store [_ work]
                  (let
                    [a
                     (put! (wfile work "src" "app.clj") "A")

                     s
                     "sess-ctx-down"]

                    (rw/record-pre! (ctx s 1 :patch) [(abs a)])
                    (spit a "A2")
                    (let
                      [r
                       (run-rewind-with-store s
                                              work
                                              []
                                              (fn [_ _]
                                                (throw (ex-info "db down" {}))))

                       ls
                       (str/split-lines (:slash/body r))]

                      (expect (= :ok (:slash/status r)))
                      (expect (= "| Turn | Files | Ops | What you asked |" (first ls))
                              "no readings means no empty column")
                      (expect (str/includes? (:slash/body r) "Rewinding restores files only"))
                      (expect (nil? (get (first (:points (:slash/data r))) "ctx_tokens")))))))
  (it "tells a dry run which turns stay in the conversation"
      (with-store
        [_ work]
        (let
          [a
           (put! (wfile work "src" "app.clj") "A")

           s
           "sess-ctx-plan"]

          (rw/record-pre! (ctx s 1 :patch) [(abs a)])
          (spit a "A2")
          (let
            [r
             (run-rewind-with-store s
                                    work
                                    ["1" "--dry-run"]
                                    (fn [_ _]
                                      [{:position 1 :input-tokens 12000 :iteration-count 2}
                                       {:position 2 :input-tokens 34000 :iteration-count 4}]))

             body
             (:slash/body r)]

            (expect (str/includes? body "**Context is untouched**"))
            (expect (str/includes? body "2 turns from turn 1 on are still in the conversation")
                    body)
            (expect (str/includes? body "rewinding moves files only") body)
            ;; A turn's INPUT already contains every earlier turn, so summing
            ;; the readings would report 46k of context that does not exist.
            (expect (= {:turns 2 :tokens 34000} (:context (:slash/data r))))
            (expect (= "A2" (slurp a)) "a dry run still touches nothing")))))
  (it "keeps the conversation caveat in past tense after applying"
      (with-store
        [_ work]
        (let
          [a
           (put! (wfile work "src" "app.clj") "A")

           s
           "sess-ctx-applied"]

          (rw/record-pre! (ctx s 1 :patch) [(abs a)])
          (spit a "A2")
          (let
            [r
             (run-rewind-with-store s
                                    work
                                    ["1"]
                                    (fn [_ _]
                                      [{:position 1 :input-tokens 12000 :iteration-count 2}]))

             body
             (:slash/body r)]

            (expect (= "A" (slurp a)))
            (expect (str/includes? body "**Context is untouched**"))
            (expect (str/includes? body "1 turn from turn 1 on is still in the conversation") body)
            (expect (str/includes? body "rewinding moved files only") body)
            (expect (= 1 (:turns (:context (:slash/data r))))))))))

;; Round 2 regressions: invented directories, store isolation, hostile types

(defdescribe invented-directory-test
             (it "removes the directories the turn invented, not just the file"
                 (with-store [_ work]
                             (let
                               [f
                                (wfile work "a" "b" "c" "new.txt")

                                s
                                "sess-invented"]

                               (rw/record-pre! (ctx s 1 :patch) [(abs f)])
                               (put! f "created by the turn")
                               (expect (.isFile f))
                               (rw/restore! s 1)
                               (expect (not (.exists f)))
                               (expect (not (.exists (wfile work "a" "b" "c"))))
                               (expect (not (.exists (wfile work "a" "b"))))
                               (expect (not (.exists (wfile work "a")))))))
             (it "keeps a directory that already held something"
                 (with-store [_ work]
                             (let
                               [kept
                                (put! (wfile work "d" "keep.txt") "PRE-EXISTING")

                                f
                                (wfile work "d" "e" "new.txt")

                                s
                                "sess-invented-keep"]

                               (rw/record-pre! (ctx s 1 :patch) [(abs f)])
                               (put! f "created by the turn")
                               (rw/restore! s 1)
                               (expect (not (.exists f)))
                               (expect (not (.exists (wfile work "d" "e"))))
                               (expect (.isFile kept))
                               (expect (= "PRE-EXISTING" (slurp kept))))))
             (it "keeps an invented directory the turn also filled with an unrecorded sibling"
                 (with-store [_ work]
                             (let
                               [f
                                (wfile work "g" "h" "new.txt")

                                sibling
                                (wfile work "g" "h" "unrecorded.txt")

                                s
                                "sess-invented-sibling"]

                               (rw/record-pre! (ctx s 1 :patch) [(abs f)])
                               (put! f "created by the turn")
                               (put! sibling "the turn also wrote this, unnamed")
                               (rw/restore! s 1)
                               (expect (not (.exists f)))
                               (expect (.isFile sibling))
                               (expect (.isDirectory (wfile work "g" "h")))))))

(defdescribe
  store-isolation-test
  (it "never lets two different session ids share one store directory"
      (expect (not= (str (rw/store-dir "proj/main")) (str (rw/store-dir "proj:main"))))
      (let [prefix (str/join (repeat 130 "x"))]
        (expect (not= (str (rw/store-dir (str prefix "-alpha")))
                      (str (rw/store-dir (str prefix "-beta")))))))
  (it "leaves an ordinary session id readable on disk"
      (expect (str/ends-with? (str (rw/store-dir "01H8-session-abc")) "01H8-session-abc"))
      (expect (= (str (rw/store-dir "01H8-session-abc")) (str (rw/store-dir "01H8-session-abc")))))
  (it "does not let one session rewind another session's files"
      (with-store [_ work]
                  (let [f (put! (wfile work "a.txt") "ORIGINAL")]
                    (rw/record-pre! (ctx "proj/main" 1 :patch) [(abs f)])
                    (spit f "WRECKED")
                    (expect (empty? (rw/points "proj:main")))
                    (expect (empty? (get (rw/restore! "proj:main" 1) "restore")))
                    (expect (= "WRECKED" (slurp f)))
                    (rw/restore! "proj/main" 1)
                    (expect (= "ORIGINAL" (slurp f)))))))

(defdescribe
  hostile-journal-type-test
  (it "survives a durable journal line whose turn is a STRING, not a number"
      (with-store [_ work]
                  (let
                    [f
                     (put! (wfile work "a.txt") "ORIGINAL")

                     ghost
                     (wfile work "ghost.txt")

                     s
                     "sess-hostile-types"]

                    (rw/record-pre! (ctx s 1 :patch) [(abs f)])
                    (spit f "WRECKED")
                    (append-raw!
                      s
                      (str "{\"kind\":\"pre\",\"turn\":\"2\",\"state\":\"absent\",\"path\":\""
                           (abs ghost)
                           "\"}"))
                    (let [ts (mapv #(get % "turn") (rw/points s))]
                      (expect (= [1 2] ts))
                      (expect (every? integer? ts)))
                    (put! ghost "invented")
                    (expect (= 1 (count (get (rw/plan s "2") "restore"))))
                    (expect (seq (get (rw/restore! s 1) "applied")))
                    (expect (= "ORIGINAL" (slurp f)))
                    (expect (not (.exists ghost))))))
  (it
    "treats a missing turn the same way instead of throwing"
    (with-store
      [_ work]
      (let
        [f
         (put! (wfile work "a.txt") "ORIGINAL")

         s
         "sess-hostile-missing-turn"]

        (rw/record-pre! (ctx s 1 :patch) [(abs f)])
        (append-raw! s "{\"kind\":\"pre\",\"state\":\"absent\",\"path\":\"/nope/never.txt\"}")
        (append-raw!
          s
          "{\"kind\":\"pre\",\"turn\":{\"nope\":1},\"state\":\"absent\",\"path\":\"/nope/x.txt\"}")
        (spit f "WRECKED")
        (expect (seq (rw/points s)))
        (expect (every? #(integer? (get % "turn")) (rw/points s)))
        (rw/restore! s 1)
        (expect (= "ORIGINAL" (slurp f)))))))

;; Round 3 regression: a write through a SYMLINK lands on the resolved file

(defdescribe
  symlink-test
  (it "pre-images the file a symlinked path writes THROUGH, not only the link"
      (with-store [_ work]
                  (let
                    [t
                     (put! (wfile work "target.txt") "TARGET")

                     l
                     (io/file work "link.txt")

                     s
                     "sess-symlink"]

                    (Files/createSymbolicLink (.toPath l) (.toPath t) (make-array FileAttribute 0))
                    (rw/record-pre! (ctx s 1 :patch) [(abs l)])
                    ;; Every write tool follows the link, so the bytes destroyed
                    ;; are the TARGET's — restoring only the link loses them.
                    (spit l "WRECKED")
                    (let [r (rw/restore! s 1)]
                      (expect (= [] (get r "failed")))
                      (expect (= "TARGET" (slurp t)))
                      (expect (= "TARGET" (slurp l)))
                      (expect (Files/isSymbolicLink (.toPath l)))))))
  (it "degrades on a dangling link, a symlink LOOP and a linked directory"
      (with-store [_ work]
                  (let
                    [dangling
                     (io/file work "dangling")

                     a
                     (io/file work "a")

                     b
                     (io/file work "b")

                     s
                     "sess-symlink-edges"]

                    (Files/createSymbolicLink (.toPath dangling)
                                              (.toPath (io/file work "nope"))
                                              (make-array FileAttribute 0))
                    (Files/createSymbolicLink (.toPath a) (.toPath b) (make-array FileAttribute 0))
                    (Files/createSymbolicLink (.toPath b) (.toPath a) (make-array FileAttribute 0))
                    (rw/record-pre! (ctx s 1 :patch) [(abs dangling) (abs a)])
                    (let [r (rw/restore! s 1)]
                      (expect (= [] (get r "failed")))
                      (expect (= #{"symlink"} (set (map #(get % "action") (get r "applied")))))
                      (expect (Files/isSymbolicLink (.toPath dangling)))
                      (expect (Files/isSymbolicLink (.toPath a))))))))

;; Plan shape at scale

(defdescribe
  plan-uncovered-dedup-test
  "`plan` hides an uncovered entry whose path an earlier restore entry already
   covers. That membership test is a SET, not a linear `some` over `restore` for
   every uncovered entry: both lists hold one entry per FILE, so the old scan was
   O(uncovered x restore) and a `shell` turn that sweeps a big tree measured
   6.3 -> 10.2 -> 31.1 -> 123.8 ms across 200/400/800/1600 entries — quadrupling
   per doubling, while the set is flat."
  (it "drops a path that is also restorable and keeps a genuinely uncovered one"
      (with-store [_ work]
                  (let
                    [f
                     (put! (wfile work "small.txt") "ORIGINAL")

                     g
                     (put! (wfile work "big.bin") (apply str (repeat 1000 "x")))

                     s
                     "sess-plan-dedup"]

                    (rw/record-pre! (ctx s 1 :patch) [(abs f)])
                    ;; Turn 2 finds the SAME file grown past the blob limit, so it
                    ;; journals an `uncovered` entry for a path turn 1's image still
                    ;; rewinds perfectly.
                    (spit f (apply str (repeat 1000 "y")))
                    (binding [rw/*max-blob-bytes* 8]
                      (rw/record-pre! (ctx s 2 :patch) [(abs f) (abs g)]))
                    (expect (some #(and (= (abs f) (get % "path")) (= "uncovered" (get % "kind")))
                                  (rw/journal s)))
                    (let [pl (rw/plan s 1)]
                      (expect (= [(abs f)] (mapv #(get % "path") (get pl "restore"))))
                      (expect (= [(abs g)] (mapv #(get % "path") (get pl "uncovered"))))))))
  (it "reports complete coverage when every uncovered path is also restorable"
      (with-store [_ work]
                  (let
                    [f
                     (put! (wfile work "small.txt") "ORIGINAL")

                     s
                     "sess-plan-dedup-complete"]

                    (rw/record-pre! (ctx s 1 :patch) [(abs f)])
                    (spit f (apply str (repeat 1000 "y")))
                    (binding [rw/*max-blob-bytes* 8]
                      (rw/record-pre! (ctx s 2 :patch) [(abs f)]))
                    (let [pl (rw/plan s 1)]
                      (expect (= [] (get pl "uncovered")))
                      (expect (= "complete" (get pl "coverage"))))))))
