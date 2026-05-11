(ns com.blockether.vis.internal.external-opener-test
  "Tests for the shared OS-opener layer.

   Two surfaces exercised:

     1. Pure classification + command-builder logic (no I/O).
     2. End-to-end `open!` against safe synthetic commands so the
        spawn path is actually walked, with stdio redirection
        verified."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [com.blockether.vis.internal.external-opener :as opener]
            [com.blockether.vis.internal.workspace :as workspace]
            [lazytest.core :refer [defdescribe expect it]])
  (:import (java.nio.file Paths)))

(defn- cwd-target [path]
  (.getAbsolutePath (.toFile (.resolve (Paths/get (System/getProperty "user.dir") (make-array String 0)) path))))

(defdescribe classify-scheme-test
  (it "classifies known whitelist schemes case-insensitively"
    (expect (= :http  (opener/classify-scheme "http://example.com")))
    (expect (= :https (opener/classify-scheme "HTTPS://example.com")))
    (expect (= :file  (opener/classify-scheme "file:///etc/hosts")))
    (expect (= :file  (opener/classify-scheme "FiLe:relative"))))

  (it "treats bare paths as :rel"
    (expect (= :rel (opener/classify-scheme "src/foo.clj")))
    (expect (= :rel (opener/classify-scheme "./diagram.png")))
    (expect (= :rel (opener/classify-scheme "../up/one.txt")))
    (expect (= :rel (opener/classify-scheme "no-extension"))))

  (it "rejects unknown and dangerous schemes"
    (expect (= :rejected (opener/classify-scheme "javascript:alert(1)")))
    (expect (= :rejected (opener/classify-scheme "data:text/html,<script>")))
    (expect (= :rejected (opener/classify-scheme "ssh://host")))
    (expect (= :rejected (opener/classify-scheme "mailto:nope")))
    (expect (= :rejected (opener/classify-scheme "vbscript:msgbox"))))

  (it "rejects nil and blank input"
    (expect (= :rejected (opener/classify-scheme nil)))
    (expect (= :rejected (opener/classify-scheme "")))
    (expect (= :rejected (opener/classify-scheme "   "))))

  (it "windows drive letters do not register as supported schemes"
    (expect (= :rejected (opener/classify-scheme "C:\\Windows\\foo")))))

(defdescribe safe-target-test
  (it "passes http(s) URLs through unchanged"
    (expect (= {:scheme :http :target "http://example.com" :line nil}
              (opener/safe-target "http://example.com")))
    (expect (= {:scheme :https :target "https://x.example.com/y" :line nil}
              (opener/safe-target "https://x.example.com/y"))))

  (it "rejects bad schemes"
    (expect (nil? (opener/safe-target "javascript:alert(1)")))
    (expect (nil? (opener/safe-target "data:text/html,x")))
    (expect (nil? (opener/safe-target nil)))
    (expect (nil? (opener/safe-target ""))))

  (it "resolves relative paths under the current working directory"
    (let [out (opener/safe-target "deps.edn")]
      (expect (= :rel (:scheme out)))
      (expect (= (cwd-target "deps.edn") (:target out)))
      (expect (nil? (:line out)))))

  (it "resolves relative paths under the active workspace root binding"
    (let [root (.toFile (java.nio.file.Files/createTempDirectory "vis-opener-ws" (make-array java.nio.file.attribute.FileAttribute 0)))]
      (try
        (spit (java.io.File. root "from-workspace.txt") "ok")
        (binding [workspace/*workspace-root* (.getCanonicalPath root)]
          (let [out (opener/safe-target "from-workspace.txt")]
            (expect (= :rel (:scheme out)))
            (expect (= (.getCanonicalPath (java.io.File. root "from-workspace.txt")) (:target out)))))
        (finally
          (io/delete-file (java.io.File. root "from-workspace.txt") true)
          (io/delete-file root true)))))

  (it "extracts a line anchor from path#Lline"
    (let [out (opener/safe-target "deps.edn#L42")]
      (expect (= :rel (:scheme out)))
      (expect (= (cwd-target "deps.edn") (:target out)))
      (expect (= 42 (:line out)))))

  (it "rejects .. traversal paths"
    (expect (nil? (opener/safe-target "../../../../etc/passwd")))
    (expect (nil? (opener/safe-target "src/../../../../etc/passwd"))))

  (it "decodes file:// URLs and re-runs the cwd-escape guard"
    (let [cwd (System/getProperty "user.dir")
          out (opener/safe-target (str "file://" cwd "/deps.edn"))]
      (expect (= :file (:scheme out)))
      (expect (= (cwd-target "deps.edn") (:target out))))
    (expect (nil? (opener/safe-target "file:///etc/passwd")))))

(defdescribe file-editor-command-test
  (it "local file editor commands preserve line anchors"
    (let [commands (@#'opener/file-editor-commands "/repo/deps.edn" 42)]
      (expect (= ["code" "-g" "/repo/deps.edn:42"] (first commands)))
      (expect (some #(= ["cursor" "--goto" "/repo/deps.edn:42"] %) commands))))

  (it "open-file-in-editor! tries editor commands before generic OS open"
    (let [spawned (atom [])]
      (with-redefs [opener/open-command (fn [target] ["open" target])
                    opener/spawn! (fn [argv]
                                    (swap! spawned conj argv)
                                    nil)]
        (let [r (opener/open-file-in-editor! "deps.edn#L42")]
          (expect (= :ok (:status r)))
          (expect (= :rel (:scheme r)))
          (expect (= 42 (:line r)))
          (expect (= ["code" "-g" (str (cwd-target "deps.edn") ":42")]
                    (:command r)))
          (expect (= [(:command r)] @spawned)))))))

(defdescribe open-command-test
  (it "macOS dispatches to open <target>"
    (with-redefs [opener/os-name (constantly "mac os x")]
      (expect (= ["open" "https://example.com"]
                (opener/open-command "https://example.com")))))

  (it "Linux dispatches to xdg-open <target>"
    (with-redefs [opener/os-name (constantly "linux")]
      (expect (= ["xdg-open" "/abs/file"]
                (opener/open-command "/abs/file"))))
    (with-redefs [opener/os-name (constantly "freebsd")]
      (expect (= ["xdg-open" "/abs/file"]
                (opener/open-command "/abs/file")))))

  (it "Windows dispatches to cmd /c start \"\" <target>"
    (with-redefs [opener/os-name (constantly "windows 11")]
      (expect (= ["cmd" "/c" "start" "" "https://x"]
                (opener/open-command "https://x")))))

  (it "returns nil on unsupported OS"
    (with-redefs [opener/os-name (constantly "haiku")]
      (expect (nil? (opener/open-command "x"))))))

(defdescribe open!-test
  (it "rejected scheme returns :rejected-scheme without spawning"
    (let [r (opener/open! "javascript:alert(1)")]
      (expect (= :rejected-scheme (:status r)))
      (expect (nil? (:command r)))
      (expect (some? (:error r)))))

  (it ".. escape relative path returns :path-escape"
    (let [r (opener/open! "../../../../etc/passwd")]
      (expect (= :path-escape (:status r)))
      (expect (nil? (:command r)))
      (expect (some? (:error r)))))

  (it "spawn-failed surfaces the IOException message"
    (with-redefs [opener/open-command (constantly ["__nonexistent_binary_42__" "x"])]
      (let [r (opener/open! "https://example.com")]
        (expect (= :spawn-failed (:status r)))
        (expect (some? (:error r))))))

  (it "ok status when spawn succeeds using true on Unix"
    (when-not (str/includes? (str/lower-case (or (System/getProperty "os.name") ""))
                "windows")
      (with-redefs [opener/open-command (fn [t] ["true" t])]
        (let [r (opener/open! "deps.edn")]
          (expect (= :ok (:status r)))
          (expect (= :rel (:scheme r)))
          (expect (str/ends-with? (:target r) "deps.edn")))))))
