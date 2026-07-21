(ns com.blockether.vis.internal.process-jail-test
  "The OS process jail: the SBPL compiler is asserted as pure data, and — on a
   macOS host that can actually enforce Seatbelt — a real wrapped `bash` proves
   containment end to end (workspace RW allowed; outside-read, write-outside and
   network all denied)."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [com.blockether.vis.internal.process-jail :as pj]))

(deftest macos-profile-compiler
  (testing "net OFF denies sockets; net ON allows them"
    (is (str/includes? (pj/macos-profile {:rw [] :net-enabled? false}) "(deny network*)"))
    (is (str/includes? (pj/macos-profile {:rw [] :net-enabled? true}) "(allow network*)")))
  (testing "default-deny base with the dyld/system import (else binaries abort)"
    (let [p (pj/macos-profile {:rw [] :net-enabled? false})]
      (is (str/includes? p "(deny default)"))
      (is (str/includes? p "(import \"system.sb\")"))))
  (testing "resolvable RW roots become subpath rules on their REAL path"
    (let
      [dir
       (doto (io/file (System/getProperty "java.io.tmpdir") (str "vis-jail-" (System/nanoTime)))
         (.mkdirs))

       real
       (.getCanonicalPath dir)

       p
       (pj/macos-profile {:rw [(.getPath dir)] :net-enabled? false})]

      (is (str/includes? p (str "(subpath \"" real "\")"))
          "rule must template the canonical path, not the raw /tmp path")
      (.delete dir))))

(deftest wrap-argv-is-off-by-default
  (testing "no active policy => argv passes through untouched"
    (pj/set-active-policy! nil)
    (is (= ["bash" "-lc" "echo hi"] (pj/wrap-argv ["bash" "-lc" "echo hi"])))))

(defn- run-jailed
  [argv]
  (let
    [pb
     (doto (ProcessBuilder. ^java.util.List argv) (.redirectErrorStream true))

     p
     (.start pb)

     out
     (slurp (.getInputStream p))]

    {:exit (.waitFor p) :out out}))

(deftest real-containment
  (when (pj/supported?)
    (let
      [ws
       (doto (io/file (System/getProperty "java.io.tmpdir") (str "vis-jail-ws-" (System/nanoTime)))
         (.mkdirs))

       secret
       (io/file (System/getProperty "java.io.tmpdir")
                (str "vis-jail-secret-" (System/nanoTime) ".txt"))]

      (spit (io/file ws "inside.txt") "workspace-ok")
      (spit secret "TOP-SECRET")
      (pj/set-active-policy! {:roots-fn (constantly [(.getPath ws)]) :net-enabled? false})
      (try (testing "reads + writes inside the workspace succeed"
             (let
               [r (run-jailed (pj/wrap-argv ["bash" "--noprofile" "--norc" "-lc"
                                             (str "cat "
                                                  (.getCanonicalPath ws)
                                                  "/inside.txt"
                                                  " && echo x > "
                                                  (.getCanonicalPath ws)
                                                  "/w.txt && echo WROTE")]))]
               (is (zero? (:exit r)))
               (is (str/includes? (:out r) "workspace-ok"))
               (is (str/includes? (:out r) "WROTE"))))
           (testing "reading a secret OUTSIDE the workspace is denied"
             (let
               [r (run-jailed (pj/wrap-argv ["bash" "--noprofile" "--norc" "-lc"
                                             (str "cat " (.getCanonicalPath secret))]))]
               (is (not (str/includes? (:out r) "TOP-SECRET")))))
           (testing "writing outside the workspace is denied"
             (let
               [target
                (io/file (System/getProperty "java.io.tmpdir")
                         (str "vis-jail-escape-" (System/nanoTime) ".txt"))

                _
                (run-jailed (pj/wrap-argv ["bash" "--noprofile" "--norc" "-lc"
                                           (str "echo escaped > " (.getCanonicalPath target))]))]

               (is (not (.exists target)))))
           (finally (pj/set-active-policy! nil)
                    (io/delete-file (io/file ws "inside.txt") true)
                    (io/delete-file (io/file ws "w.txt") true)
                    (io/delete-file ws true)
                    (io/delete-file secret true))))))
