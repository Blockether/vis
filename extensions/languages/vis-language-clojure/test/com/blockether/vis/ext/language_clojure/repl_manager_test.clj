(ns com.blockether.vis.ext.language-clojure.repl-manager-test
  "Hermetic tests for the owned, session-scoped REPL manager. The actual
   subprocess self-start is exercised in REPL-driven verification, not here, so
   these stay fast and side-effect-free."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [com.blockether.vis.ext.language-clojure.core :as core]
            [com.blockether.vis.ext.language-clojure.repl-manager :as rm]
            [lazytest.core :refer [defdescribe expect it]])
  (:import (java.nio.file Files)
           (java.nio.file.attribute FileAttribute)))

(defn- tmp-dir
  ^String []
  (.getAbsolutePath (.toFile (Files/createTempDirectory "vis-rm-" (into-array FileAttribute [])))))

(defn- with-file [^String dir name content] (spit (io/file dir name) content) dir)

(defdescribe
  launcher-for-test
  ;; launcher-for is now 3-arity: [dir aliases port]. We always know our port.
  (it "selects clojure for deps.edn"
      (expect (= :clj (:tool (rm/launcher-for (with-file (tmp-dir) "deps.edn" "{}") nil 12345)))))
  (it "selects lein for project.clj"
      (expect (= :lein
                 (:tool (rm/launcher-for (with-file (tmp-dir) "project.clj" "(defproject x)")
                                         nil
                                         12345)))))
  (it "selects bb for bb.edn"
      (expect (= :bb (:tool (rm/launcher-for (with-file (tmp-dir) "bb.edn" "{}") nil 12345)))))
  (it "returns nil when no known build file is present"
      (expect (nil? (rm/launcher-for (tmp-dir) nil 12345))))
  (it "the clojure launcher injects the nrepl dep and runs nrepl.cmdline on our explicit port"
      (let [cmd (:cmd (rm/launcher-for (with-file (tmp-dir) "deps.edn" "{}") nil 61234))]
        (expect (= "clojure" (first cmd)))
        ;; nrepl.cmdline rides the synthetic `:vis/nrepl-launch` alias's :main-opts
        ;; inside the -Sdeps EDN, with --port <ours> so we never read a file back.
        (expect (some #(str/includes? (str %) "nrepl.cmdline") cmd))
        (expect (some #(str/includes? (str %) "--port") cmd))
        (expect (some #(str/includes? (str %) "61234") cmd))
        ;; -M carries only the synthetic launch alias when no user aliases
        (expect (some #(= "-M:vis/nrepl-launch" %) cmd))))
  (it "threads deps.edn aliases into the clojure -M flag"
      (let [cmd (:cmd (rm/launcher-for (with-file (tmp-dir) "deps.edn" "{}") [:dev :test] 12345))]
        ;; user aliases come first, then the synthetic launch alias (last-wins)
        (expect (some #(= "-M:dev:test:vis/nrepl-launch" %) cmd))))
  (it "threads lein profiles via with-profile and passes our port"
      (let [cmd (:cmd (rm/launcher-for (with-file (tmp-dir) "project.clj" "(defproject x)")
                                       [:dev :test]
                                       55123))]
        (expect (some #(= "with-profile" %) cmd))
        (expect (some #(= "+dev,+test" %) cmd))
        (expect (some #(= "55123" %) cmd)))))

(defdescribe status+stop-test
             ;; status/stop are SESSION-scoped and return STRING-keyed lifecycle maps (they
             ;; cross the strings-only boundary as tool `:result`s).
             (it "status reports a down, unmanaged REPL with a stable id for a fresh dir"
                 (let [dir
                       (tmp-dir)

                       s
                       (rm/status "sess-a" dir)]

                   (expect (= "status" (get s "result")))
                   (expect (= "down" (get s "status")))
                   (expect (= (rm/id-of dir) (get s "id")))
                   (expect (nil? (get s "running")))))
             (it "stop is a safe no-op when nothing is managed"
                 (let [dir
                       (tmp-dir)

                       r
                       (rm/stop! "sess-a" dir)]

                   (expect (= "not-managed" (get r "result")))
                   (expect (= (rm/id-of dir) (get r "id"))))))

(defdescribe id-of-test
             (it "derives a stable nrepl:<dir> id" (expect (= "nrepl:/x/y" (rm/id-of "/x/y")))))

(defdescribe resolve-target-ownership-test
             ;; The ownership contract: an explicit id names a REPL; one owned REPL is the
             ;; implicit default; several demand an id. session-repls is stubbed so no
             ;; subprocess is spawned.
             (it "uses the single owned REPL as the implicit default (no id needed)"
                 (with-redefs [rm/session-repls (fn [_]
                                                  [{:id "nrepl:/p" :dir "/p" :port 7001}])]
                   (expect (= {:id "nrepl:/p" :dir "/p" :port 7001}
                              (rm/resolve-target! "sess" nil "/p")))))
             (it "resolves an explicit id to that owned REPL"
                 (with-redefs [rm/session-repls (fn [_]
                                                  [{:id "nrepl:/a" :dir "/a" :port 1}
                                                   {:id "nrepl:/b" :dir "/b" :port 2}])]
                   (expect (= {:id "nrepl:/b" :dir "/b" :port 2}
                              (rm/resolve-target! "sess" "nrepl:/b" "/a")))))
             (it "throws :clj/unknown-repl-id for an id with no live REPL"
                 (with-redefs [rm/session-repls (fn [_]
                                                  [])]
                   (let [t (try (rm/resolve-target! "sess" "nrepl:/nope" "/p")
                                :no-throw
                                (catch clojure.lang.ExceptionInfo e (:type (ex-data e))))]
                     (expect (= :clj/unknown-repl-id t)))))
             (it "throws :clj/ambiguous-repl when >1 REPLs live and no id is given"
                 (with-redefs [rm/session-repls (fn [_]
                                                  [{:id "nrepl:/a" :dir "/a" :port 1}
                                                   {:id "nrepl:/b" :dir "/b" :port 2}])]
                   (let [t (try (rm/resolve-target! "sess" nil "/a")
                                :no-throw
                                (catch clojure.lang.ExceptionInfo e (ex-data e)))]
                     (expect (= :clj/ambiguous-repl (:type t)))
                     (expect (= ["nrepl:/a" "nrepl:/b"] (:ids t)))))))

(defdescribe
  clj-repl-tool-gating-test
  (it "\"status\" always succeeds (start/stop are never flag-gated)"
      (expect (:success? (core/clj-repl-fn {:workspace/root (tmp-dir) :session-id "s"} "status"))))
  (it "rejects an unknown op"
      (let [t (try (core/clj-repl-fn {:workspace/root (tmp-dir) :session-id "s"} "frobnicate")
                   :no-throw
                   (catch clojure.lang.ExceptionInfo e (:type (ex-data e))))]
        (expect (= :clj/bad-args t)))))

(defdescribe resolve-repl-dir-test
             ;; resolve-repl-dir returns canonical paths (stable process-map keys), so
             ;; expectations canonicalize too.
             (let [resolve
                   #'core/resolve-repl-dir

                   canon
                   (fn [p]
                     (.getCanonicalPath (io/file p)))]

               (it "blank/nil dir resolves to the workspace root"
                   (let [root (tmp-dir)]
                     (expect (= (canon root) (resolve root nil)))
                     (expect (= (canon root) (resolve root "")))))
               (it "a relative dir resolves under the workspace root"
                   (let [root
                         (tmp-dir)

                         _
                         (.mkdirs (io/file root "a" "b"))]

                     (expect (= (canon (io/file root "a" "b")) (resolve root "a/b")))))
               (it "an absolute dir is used as-is"
                   (let [root
                         (tmp-dir)

                         abs
                         (tmp-dir)]

                     (expect (= (canon abs) (resolve root abs)))))
               (it "a leading ~ expands to the user's home dir (not a subdir of root)"
                   (let [root
                         (tmp-dir)

                         home
                         (System/getProperty "user.home")]

                     (expect (= (canon home) (resolve root "~")))
                     (expect (= (canon (io/file home "foo" "bar")) (resolve root "~/foo/bar")))
                     ;; the same home target resolves to ONE id regardless of spelling
                     (expect (= (resolve root "~") (resolve root home)))))))
