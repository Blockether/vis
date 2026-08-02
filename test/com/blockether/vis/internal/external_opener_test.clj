(ns com.blockether.vis.internal.external-opener-test
  "`external-opener` is the one gate between a link inside a transcript and a
   process on the user's machine, so the parts worth pinning are the two pure
   halves: what scheme a string is allowed to claim, and which argv the host OS
   would be handed. Everything past `safe-target` spawns a real process, so it
   stays out of this namespace on purpose — `os-name` is indirected precisely so
   `open-command` can be exercised for every platform from one machine."
  (:require [clojure.string :as str]
            [com.blockether.vis.internal.external-opener :as eo]
            [lazytest.core :refer [defdescribe expect it]]))

(defdescribe
  classify-scheme-test
  (it "labels each input by the scheme it claims, case-insensitively"
      ;; The whole allow-list in one table: only http/https/file get a scheme,
      ;; anything schemeless is a candidate relative path, everything else is
      ;; refused before it can reach a process.
      (expect (= [:http :https :file :rel :rel :rejected :rejected :rejected :rejected :rejected]
                 (mapv eo/classify-scheme
                       ["http://example.com" "HTTPS://example.com" "file:/tmp/a.txt" "src/a.clj"
                        "./a.png" "javascript:alert(1)" "mailto:a@b.c" "C:/foo" "" nil]))))
  (it "rejects a Windows drive letter instead of reading it as a scheme"
      ;; `C:` matches the shape of a scheme, so without the guard a drive-letter
      ;; path would be handed to the opener as an unknown protocol.
      (expect (= :rejected (eo/classify-scheme "C:/Users/x/file.txt")))))

(defdescribe safe-target-test
             (it "passes an absolute web URL through untouched, query string included"
                 (let [url "https://example.com/a?b=1"]
                   (expect (= {:scheme :https :target url :line nil} (eo/safe-target url)))))
             (it "absolutizes a workspace-relative path and lifts its #L anchor"
                 (let
                   [{:keys [scheme target line]} (eo/safe-target
                                                   "src/com/blockether/vis/internal/paths.clj#L12")]
                   (expect (= :rel scheme))
                   (expect (= 12 line) "the #L suffix becomes data, it never stays in the target")
                   (expect (str/starts-with? target "/") "the opener needs an absolute path")
                   (expect (str/ends-with? target "src/com/blockether/vis/internal/paths.clj")
                           "and the anchor is stripped off the file name")))
             (it "refuses everything that would open a file outside the workspace"
                 ;; Traversal, an absolute system path, and a file: URL are three spellings
                 ;; of the same escape; all three must resolve to nil rather than a target.
                 (expect (= [nil nil nil]
                            (mapv eo/safe-target
                                  ["../etc/passwd" "/etc/passwd" "file:///etc/passwd"]))))
             (it "refuses a rejected scheme and blank input"
                 (expect (= [nil nil nil] (mapv eo/safe-target ["javascript:alert(1)" "" nil])))))

(defdescribe open-command-test
             (it "picks the platform opener from os-name alone"
                 ;; `os-name` exists as a fn so this table can be checked from any host.
                 (expect (= ["open" "https://example.com"]
                            (with-redefs [eo/os-name (constantly "mac os x")]
                              (eo/open-command "https://example.com"))))
                 (expect (= ["xdg-open" "https://example.com"]
                            (with-redefs [eo/os-name (constantly "linux")]
                              (eo/open-command "https://example.com")))))
             (it "returns nil on a platform it has no opener for"
                 ;; nil means \"do not spawn\" — callers must not fall back to a shell.
                 (expect (nil? (with-redefs [eo/os-name (constantly "windows 11")]
                                 (eo/open-command "https://example.com"))))
                 (expect (nil? (with-redefs [eo/os-name (constantly "plan 9")]
                                 (eo/open-command "https://example.com"))))))
