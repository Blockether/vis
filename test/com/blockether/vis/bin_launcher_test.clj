(ns com.blockether.vis.bin-launcher-test
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [lazytest.core :refer [defdescribe expect it]])
  (:import [java.nio.file Files StandardCopyOption]
           [java.nio.file.attribute FileAttribute]))

(defn- write-executable!
  [file body]
  (spit file body)
  (.setExecutable ^java.io.File file true)
  file)

(defn- delete-tree!
  [root]
  (doseq [file (reverse (file-seq root))]
    (io/delete-file file true)))

(defn- launcher-fixture
  [vendor]
  (let [root (.toFile (Files/createTempDirectory "vis-launcher-test-"
                                                 (make-array FileAttribute 0)))
        repo (doto (io/file root "repo") .mkdirs)
        bin (doto (io/file repo "bin") .mkdirs)
        old-home (doto (io/file root "old-jdk") .mkdirs)
        old-bin (doto (io/file old-home "bin") .mkdirs)
        pinned-home (doto (io/file root "pinned-jdk") .mkdirs)
        pinned-bin (doto (io/file pinned-home "bin") .mkdirs)
        tools (doto (io/file root "tools") .mkdirs)
        home (doto (io/file root "home") .mkdirs)]
    (Files/copy (.toPath (io/file "bin/vis"))
                (.toPath (io/file bin "vis"))
                (into-array StandardCopyOption [StandardCopyOption/REPLACE_EXISTING]))
    (.setExecutable (io/file bin "vis") true)
    (spit (io/file repo ".graalvm-version")
          (str "GRAAL_EDITION=\"GraalVM CE\"\n"
               "GRAAL_VERSION=\"25.2.4\"\n"
               "GRAAL_VENDOR_VERSION=\"GraalVM CE 25.2.4+7.1\"\n"))
    (write-executable!
      (io/file bin "require-graalvm")
      "#!/usr/bin/env bash\nprintf '%s\\n' \"$VIS_TEST_PINNED_HOME\"\n")
    (write-executable!
      (io/file old-bin "java")
      (str "#!/usr/bin/env bash\n"
           "echo '    java.vendor.version = " vendor "' >&2\n"))
    (write-executable! (io/file pinned-bin "java") "#!/usr/bin/env bash\nexit 0\n")
    (write-executable!
      (io/file tools "clojure")
      (str "#!/usr/bin/env bash\n"
           "printf 'JAVA_HOME=%s\\n' \"$JAVA_HOME\"\n"
           "printf 'GRAALVM_HOME=%s\\n' \"${GRAALVM_HOME:-}\"\n"
           "printf 'JAVA_CMD=%s\\n' \"${JAVA_CMD:-}\"\n"))
    {:root root
     :repo repo
     :home home
     :old-home old-home
     :pinned-home pinned-home
     :tools tools}))

(defn- run-launcher
  [{:keys [repo home old-home pinned-home tools]}]
  (let [old-java (.getAbsolutePath (io/file old-home "bin/java"))
        pb (ProcessBuilder. ^java.util.List ["bash" (.getAbsolutePath (io/file repo "bin/vis"))
                                             "--jvm" "--version"])
        env (.environment pb)]
    (.directory pb repo)
    (.redirectErrorStream pb true)
    (.put env "HOME" (.getAbsolutePath home))
    (.put env "PATH" (str (.getAbsolutePath (io/file old-home "bin"))
                          ":" (.getAbsolutePath tools) ":/usr/bin:/bin"))
    (.put env "JAVA_HOME" (.getAbsolutePath old-home))
    (.put env "JAVA_CMD" old-java)
    (.put env "VIS_TEST_PINNED_HOME" (.getAbsolutePath pinned-home))
    (.put env "VIS_NO_DEV_CHECKOUT" "1")
    (.put env "VIS_NO_NATIVE_HINT" "1")
    (let [process (.start pb)
          output (slurp (.getInputStream process))
          exit (.waitFor process)]
      {:exit exit :output output})))

(defdescribe
  jvm-launcher-runtime-test
  (it "replaces a stale mismatched GraalVM inherited from the parent runner"
      (let [{:keys [root pinned-home] :as fixture}
            (launcher-fixture "GraalVM CE 25.1.3+9.1")]
        (try
          (let [{:keys [exit output]} (run-launcher fixture)
                pinned-java (.getAbsolutePath (io/file pinned-home "bin/java"))]
            (expect (= 0 exit))
            (expect (str/includes? output
                                   "replaced incompatible GraalVM CE 25.1.3+9.1"))
            (expect (str/includes? output (str "JAVA_HOME=" (.getAbsolutePath pinned-home))))
            (expect (str/includes? output (str "GRAALVM_HOME="
                                               (.getAbsolutePath pinned-home))))
            (expect (str/includes? output (str "JAVA_CMD=" pinned-java))))
          (finally
            (delete-tree! root)))))
  (it "preserves a stock JDK because it has no built-in Truffle to collide"
      (let [{:keys [root old-home] :as fixture}
            (launcher-fixture "Eclipse Adoptium-25.0.3+8")]
        (try
          (let [{:keys [exit output]} (run-launcher fixture)]
            (expect (= 0 exit))
            (expect (not (str/includes? output "replaced incompatible")))
            (expect (str/includes? output (str "JAVA_HOME=" (.getAbsolutePath old-home))))
            (expect (str/includes? output
                                   (str "JAVA_CMD="
                                        (.getAbsolutePath (io/file old-home "bin/java"))))))
          (finally
            (delete-tree! root))))))
