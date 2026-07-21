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
  ;; The threat model is a secret OUTSIDE every allowed root (e.g. ~/.ssh). Temp
  ;; dirs are INTENTIONALLY writable roots (mirrors the Python sandbox), so the
  ;; secret + escape target live under $HOME, which the jail never allows.
  (when (pj/supported?)
    (let
      [home
       (System/getProperty "user.home")

       ws
       (doto (io/file (System/getProperty "java.io.tmpdir") (str "vis-jail-ws-" (System/nanoTime)))
         (.mkdirs))

       secret
       (io/file home (str ".vis-jail-secret-" (System/nanoTime) ".txt"))

       escape
       (io/file home (str ".vis-jail-escape-" (System/nanoTime) ".txt"))

       wsc
       (.getCanonicalPath ws)]

      (spit (io/file ws "inside.txt") "workspace-ok")
      (spit secret "TOP-SECRET")
      (pj/set-active-policy! {:roots-fn (constantly [(.getPath ws)]) :net-enabled? false})
      (try (testing "reads + writes inside the workspace succeed"
             (let
               [r (run-jailed
                    (pj/wrap-argv
                      ["bash" "--noprofile" "--norc" "-lc"
                       (str "cat " wsc "/inside.txt" " && echo x > " wsc "/w.txt && echo WROTE")]))]
               (is (zero? (:exit r)))
               (is (str/includes? (:out r) "workspace-ok"))
               (is (str/includes? (:out r) "WROTE"))))
           (testing "reading a secret OUTSIDE every root ($HOME) is denied"
             (let
               [r (run-jailed (pj/wrap-argv ["bash" "--noprofile" "--norc" "-lc"
                                             (str "cat " (.getCanonicalPath secret) " 2>&1")]))]
               (is (not (str/includes? (:out r) "TOP-SECRET")))))
           (testing "writing outside every root ($HOME) is denied"
             (run-jailed (pj/wrap-argv
                           ["bash" "--noprofile" "--norc" "-lc"
                            (str "echo escaped > " (.getCanonicalPath escape) " 2>&1")]))
             (is (not (.exists escape))))
           (testing "network is denied when the policy is net-off"
             (let
               [r (run-jailed
                    (pj/wrap-argv
                      ["bash" "--noprofile" "--norc" "-lc"
                       "curl -sS --max-time 4 https://example.com -o /dev/null && echo GOTNET"]))]
               (is (not (str/includes? (:out r) "GOTNET")))))
           (finally (pj/set-active-policy! nil)
                    (io/delete-file (io/file ws "inside.txt") true)
                    (io/delete-file (io/file ws "w.txt") true)
                    (io/delete-file ws true)
                    (io/delete-file secret true)
                    (io/delete-file escape true))))))
