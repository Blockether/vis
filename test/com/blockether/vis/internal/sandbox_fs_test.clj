(ns com.blockether.vis.internal.sandbox-fs-test
  "Security regression guard for the confined Python sandbox filesystem.
   These assertions are the safety net: a confinement bug = sandbox escape.
   Plus the OUTBOX tap: a WRITE under the engine outbox dir fires `on-close`;
   reads and writes anywhere else are untouched."
  (:require [clojure.string :as str]
            [com.blockether.vis.internal.foundation.mpl-capture :as mc]
            [com.blockether.vis.internal.sandbox-fs :as sfs]
            [lazytest.core :refer [defdescribe expect it]])
  (:import [org.graalvm.polyglot Context]
           [org.graalvm.polyglot.io IOAccess]
           [java.nio ByteBuffer]
           [java.nio.file Files Paths StandardOpenOption]
           [java.nio.file.attribute FileAttribute]))

(defn- tmp-root
  ^String []
  (let [d (Files/createTempDirectory "vis-fs-test" (make-array FileAttribute 0))]
    (spit (str d "/inside.txt") "ROOT-DATA")
    (str (.toRealPath d (make-array java.nio.file.LinkOption 0)))))

(defn- denied? [thunk] (try (thunk) false (catch SecurityException _ true)))

(defn- p ^java.nio.file.Path [s] (Paths/get s (make-array String 0)))

(defdescribe confine-test
             (it "allows paths under a root (existing AND not-yet-created); denies outside"
                 (let [root
                       (tmp-root)

                       roots-fn
                       (fn []
                         [root])

                       confine
                       #(@#'sfs/confine! roots-fn (atom {}) [] (p %))]

                   ;; allowed
                   (expect (= (str root "/inside.txt") (str (confine (str root "/inside.txt")))))
                   (expect (= (str root "/sub/new.txt") (str (confine (str root "/sub/new.txt")))))
                   ;; denied
                   (expect (denied? #(confine "/etc/passwd")))
                   (expect (denied? #(confine (str root "/../etc/passwd")))) ; .. escape
                   (expect (denied? #(confine "/tmp")))))
             (it
               "denies a symlink (inside a root) that points OUTSIDE"
               (let [root
                     (tmp-root)

                     link
                     (str root "/evil")]

                 (Files/createSymbolicLink (p link) (p "/etc/passwd") (make-array FileAttribute 0))
                 (expect (denied? #(@#'sfs/confine!
                                     (fn []
                                       [root])
                                     (atom {})
                                     []
                                     (p link))))))
             (it "fails CLOSED with zero roots (denies everything)"
                 (let [root (tmp-root)]
                   (expect (denied? #(@#'sfs/confine!
                                       (fn []
                                         [])
                                       (atom {})
                                       []
                                       (p (str root "/inside.txt")))))
                   (expect (denied? #(@#'sfs/confine!
                                       (fn []
                                         nil)
                                       (atom {})
                                       []
                                       (p (str root "/inside.txt")))))))
             (it "allows a path under the OUTBOX dir even though it is not a /fs root"
                 (let [root
                       (tmp-root)

                       outbox
                       (str (.toRealPath (Files/createTempDirectory "vis-outbox-t"
                                                                    (make-array FileAttribute 0))
                                         (make-array java.nio.file.LinkOption 0)))

                       confine
                       #(@#'sfs/confine!
                          (fn []
                            [root])
                          (atom {})
                          [(p outbox)]
                          (p %))]

                   (expect (= (str outbox "/a.csv") (str (confine (str outbox "/a.csv")))))
                   ;; still denies outside both root and outbox
                   (expect (denied? #(confine "/etc/passwd"))))))

(defdescribe
  confined-graalpy-fs-test
  (it
    "GraalPy open()/listdir is confined to the root; stdlib still loads"
    (let [root
          (tmp-root)

          fs
          (sfs/confined-filesystem (fn []
                                     [root]))

          io
          (-> (IOAccess/newBuilder)
              (.fileSystem fs)
              (.build))

          ctx
          (-> (Context/newBuilder (into-array String ["python"]))
              (.allowIO io)
              (.allowAllAccess false)
              (.build))

          ev
          (fn [code]
            (.eval ctx "python" code))]

      (try
        ;; read inside the root works
        (expect (= "ROOT-DATA"
                   (.asString (ev (str "open(" (pr-str (str root "/inside.txt")) ").read()")))))
        ;; write inside the root works
        (ev (str "open(" (pr-str (str root "/w.txt")) ",\"w\").write(\"x\")"))
        ;; read OUTSIDE the root is denied (PermissionError on the guest side)
        (expect (try (ev "open(\"/etc/passwd\").read()") false (catch Throwable _ true)))
        ;; listing outside is denied (no leak)
        (expect (try (ev "__import__(\"os\").listdir(\"/etc\")") false (catch Throwable _ true)))
        ;; GraalPy's own stdlib (outside the roots) still imports
        (expect (str/includes? (.asString (ev "__import__(\"json\").dumps({\"ok\":1})")) "ok"))
        (finally (.close ctx true))))))

(defn- write-channel!
  "Open `path` for write through `fs`, write `s`, close — driving the outbox tap."
  [fs ^java.nio.file.Path path ^String s]
  (let [ch (.newByteChannel fs
                            path
                            #{StandardOpenOption/WRITE StandardOpenOption/CREATE
                              StandardOpenOption/TRUNCATE_EXISTING}
                            (make-array FileAttribute 0))]
    (.write ch (ByteBuffer/wrap (.getBytes s)))
    (.close ch)))

(defdescribe
  outbox-tap-test
  (it
    "captures a WRITE under the outbox, but NOT a root write or an outbox read"
    (let [outdir
          (Files/createTempDirectory "vis-outbox-tap" (make-array FileAttribute 0))

          rootdir
          (Files/createTempDirectory "vis-outbox-root" (make-array FileAttribute 0))

          sink
          (atom [])

          seen
          (atom #{})

          fs
          (sfs/confined-filesystem (fn []
                                     [(str rootdir)])
                                   {:dir (str outdir)
                                    :on-close (fn [p]
                                                (mc/record-file! p))})]

      (binding [mc/*attachment-sink*
                sink

                mc/*outbox-seen*
                seen]

        (write-channel! fs (.resolve outdir "report.csv") "x,y\n1,2\n")
        ;; write under a normal root — untouched
        (write-channel! fs (.resolve rootdir "work.txt") "not captured")
        ;; read under the outbox — untouched
        (.close (.newByteChannel fs
                                 (.resolve outdir "report.csv")
                                 #{StandardOpenOption/READ}
                                 (make-array FileAttribute 0))))
      (let [[att] @sink]
        (expect (= 1 (count @sink)))
        (expect (= "report.csv" (:filename att)))
        (expect (= "text/csv" (:media-type att)))
        (expect (= "file" (:kind att)))
        (expect (= 8 (:size att))))))
  (it "de-dups the same outbox path re-closed within one block"
      (let [outdir
            (Files/createTempDirectory "vis-outbox-dedup" (make-array FileAttribute 0))

            sink
            (atom [])

            seen
            (atom #{})

            fs
            (sfs/confined-filesystem (fn []
                                       [(str outdir)])
                                     {:dir (str outdir)
                                      :on-close (fn [p]
                                                  (mc/record-file! p))})]

        (binding [mc/*attachment-sink*
                  sink

                  mc/*outbox-seen*
                  seen]

          (write-channel! fs (.resolve outdir "a.csv") "1")
          (write-channel! fs (.resolve outdir "a.csv") "22"))
        (expect (= 1 (count @sink))))))
