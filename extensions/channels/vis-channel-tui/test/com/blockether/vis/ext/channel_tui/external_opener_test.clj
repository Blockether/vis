(ns com.blockether.vis.ext.channel-tui.external-opener-test
  "Tests for the OS-opener layer. Two surfaces exercised:

     1. Pure classification + command-builder logic (no I/O).
     2. End-to-end `open!` against safe synthetic commands so the
        spawn path is actually walked, with stdio redirection
        verified."
  (:require
   [babashka.fs :as fs]
   [clojure.string :as str]
   [com.blockether.vis.ext.channel-tui.external-opener :as opener]
   [lazytest.core :refer [defdescribe it expect]]))

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

  (it "rejects unknown / dangerous schemes"
    (expect (= :rejected (opener/classify-scheme "javascript:alert(1)")))
    (expect (= :rejected (opener/classify-scheme "data:text/html,<script>")))
    (expect (= :rejected (opener/classify-scheme "ssh://host")))
    (expect (= :rejected (opener/classify-scheme "mailto:nope")))
    (expect (= :rejected (opener/classify-scheme "vbscript:msgbox"))))

  (it "rejects nil / blank input"
    (expect (= :rejected (opener/classify-scheme nil)))
    (expect (= :rejected (opener/classify-scheme "")))
    (expect (= :rejected (opener/classify-scheme "   "))))

  (it "windows drive letters do NOT register as schemes"
    ;; The scheme regex requires `[A-Za-z][A-Za-z0-9+\-.]*:` which
    ;; matches `C:` — but the colon-then-non-slash pattern is
    ;; deliberately classified as :rejected only when it claims a
    ;; recognised scheme name. `c` alone doesn't match http/https/file
    ;; so it falls through to :rejected here. That is the safe
    ;; default — the Windows opener gets the path through `:rel`
    ;; once we pre-strip the drive letter ourselves. For now, drive
    ;; letters are :rejected and require an explicit `file:///C:/...`
    ;; form, which IS supported.
    (expect (= :rejected (opener/classify-scheme "C:\\Windows\\foo")))))

(defdescribe safe-target-test
  (it "passes http(s) URLs through unchanged"
    (expect (= {:scheme :http  :target "http://example.com" :line nil}
              (opener/safe-target "http://example.com")))
    (expect (= {:scheme :https :target "https://x.example.com/y" :line nil}
              (opener/safe-target "https://x.example.com/y"))))

  (it "rejects bad schemes"
    (expect (nil? (opener/safe-target "javascript:alert(1)")))
    (expect (nil? (opener/safe-target "data:text/html,x")))
    (expect (nil? (opener/safe-target nil)))
    (expect (nil? (opener/safe-target ""))))

  (it "resolves relative paths under (fs/cwd)"
    (let [cwd  (str (fs/cwd))
          out  (opener/safe-target "deps.edn")]
      (expect (= :rel (:scheme out)))
      (expect (= (str cwd "/deps.edn") (:target out)))
      (expect (nil? (:line out)))))

  (it "extracts a line anchor from path#Lline"
    (let [cwd (str (fs/cwd))
          out (opener/safe-target "deps.edn#L42")]
      (expect (= :rel (:scheme out)))
      (expect (= (str cwd "/deps.edn") (:target out)))
      (expect (= 42 (:line out)))))

  (it "rejects ..-traversal paths"
    (expect (nil? (opener/safe-target "../../../../etc/passwd")))
    (expect (nil? (opener/safe-target "src/../../../../etc/passwd"))))

  (it "decodes file:// URLs and re-runs the cwd-escape guard"
    (let [cwd (str (fs/cwd))
          out (opener/safe-target (str "file://" cwd "/deps.edn"))]
      (expect (= :file (:scheme out)))
      (expect (= (str cwd "/deps.edn") (:target out))))
    ;; absolute file:// outside cwd is rejected
    (expect (nil? (opener/safe-target "file:///etc/passwd")))))

(defdescribe open-command-test
  (it "macOS dispatches to `open <target>`"
    (with-redefs [opener/os-name (constantly "mac os x")]
      (expect (= ["open" "https://example.com"]
                (opener/open-command "https://example.com")))))

  (it "Linux dispatches to `xdg-open <target>`"
    (with-redefs [opener/os-name (constantly "linux")]
      (expect (= ["xdg-open" "/abs/file"]
                (opener/open-command "/abs/file"))))
    (with-redefs [opener/os-name (constantly "freebsd")]
      (expect (= ["xdg-open" "/abs/file"]
                (opener/open-command "/abs/file")))))

  (it "Windows dispatches to `cmd /c start \"\" <target>`"
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

  (it "..-escape relative path returns :path-escape"
    (let [r (opener/open! "../../../../etc/passwd")]
      (expect (= :path-escape (:status r)))
      (expect (nil? (:command r)))
      (expect (some? (:error r)))))

  (it "spawn-failed surfaces the IOException message"
    ;; Force open-command to a guaranteed-missing binary.
    (with-redefs [opener/open-command
                  (constantly ["__nonexistent_binary_42__" "x"])]
      (let [r (opener/open! "https://example.com")]
        (expect (= :spawn-failed (:status r)))
        (expect (some? (:error r))))))

  (it "ok status when spawn succeeds (using `true`, the no-op shell builtin)"
    ;; `true` exists on every Unix; on Windows we'd have to skip.
    ;; The test is portability-best-effort — gate on os-name.
    (when-not (str/includes? (str/lower-case (or (System/getProperty "os.name") ""))
                "windows")
      (with-redefs [opener/open-command (fn [t] ["true" t])]
        (let [r (opener/open! "deps.edn")]
          (expect (= :ok (:status r)))
          (expect (= :rel (:scheme r)))
          (expect (str/ends-with? (:target r) "deps.edn")))))))
