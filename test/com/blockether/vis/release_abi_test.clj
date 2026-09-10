(ns com.blockether.vis.release-abi-test
  (:require [clojure.java.io :as io]
            [clojure.java.shell :as shell]
            [clojure.string :as str]
            [lazytest.experimental.interfaces.clojure-test :refer [deftest is testing]])
  (:import [java.nio.file Files]
           [java.nio.file.attribute FileAttribute]))

(defn- check-bundle
  [files]
  (let [dir
        (.toFile (Files/createTempDirectory "vis-abi-" (make-array FileAttribute 0)))

        bundle
        (doto (io/file dir "bundle with spaces") .mkdirs)

        tools
        (doto (io/file dir "tools") .mkdirs)

        readelf
        (io/file tools "readelf")]

    (try
      ;; Stub readelf, not the checker: exercise traversal, version parsing and exit status
      ;; on any host. Linux CI separately inspects and executes the real release artifacts.
      (spit readelf "#!/usr/bin/env bash\nset -eu\ncat \"$3.versions\"\n")
      (.setExecutable readelf true)
      (spit (io/file bundle "README") "not an ELF")
      (doseq [[path versions]
              files

              :let [file
                    (io/file bundle path)]]

        (io/make-parents file)
        (spit file "\u007fELF")
        (when versions
          (spit (str file ".versions")
                (str/join "\n"
                          (map #(str "  0x0010: Name: " % " Flags: none Version: 2") versions)))))
      (shell/sh "bash"
                "bin/verify-linux-abi" (.getPath bundle)
                :env (assoc (into {} (System/getenv))
                       "PATH" (str tools java.io.File/pathSeparator (System/getenv "PATH"))))
      (finally (doseq [file (reverse (file-seq dir))]
                 (io/delete-file file true))))))

(deftest complete-bundle-glibc-check-test
  ;; Regression: checking only the launcher missed libvispython's glibc 2.38 requirement.
  (doseq [[files expected token]
          [[{"vis" ["GLIBC_2.2.5" "GLIBC_2.9" "GLIBC_2.35"]
             "python/lib/libpython.so" ["GLIBC_2.17"]} 0 "2 ELF files"]
           [{"vis-python-worker" []} 0 "1 ELF files"]
           [{"libvispython.so" ["GLIBC_2.38"]} 1 "libvispython.so requires GLIBC_2.38"]
           [{"vis" ["GLIBC_2.35"] "vis-python-worker" ["GLIBC_2.36"]} 1
            "vis-python-worker requires"] [{"vis-tui" ["GLIBC_2.38"]} 1 "vis-tui requires"]
           [{"python/lib/lib-dynload/_ssl.so" ["GLIBC_2.36"]} 1 "_ssl.so requires"]
           [{"libvisjail.so" ["GLIBC_ABI_DT_RELR"]} 1 "GLIBC_ABI_DT_RELR"]
           [{"libvispython.so" ["GLIBC_3.0"]} 1 "GLIBC_3.0"] [{"broken.so" nil} 1 ".versions"]
           [{} 1 "no ELF files found"]]]
    (testing (pr-str files)
      (let [{:keys [exit out err]} (check-bundle files)]
        (is (= expected exit) (str out err))
        (is (str/includes? (str out err) token) (str out err))))))
