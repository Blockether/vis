(ns com.blockether.vis.internal.posix-compat-shim-test
  "The POSIX-compat shim installed into every sandbox context: subprocess /
   os.system / os.popen are replaced with wrappers that DELEGATE to the ONE
   `shell` tool. Tool callables are looked up in globals at CALL time, so the
   shim self-adapts — it routes when `shell` is bound and raises a clear
   'enable the shell tool' message when it isn't."
  (:require [clojure.string :as str]
            [com.blockether.vis.internal.env-python :as ep]
            [lazytest.core :refer [defdescribe expect it]])
  (:import [org.graalvm.polyglot Context]))

(defn- fake-shell
  "Records one-options-map shell calls and returns lifecycle-shaped results."
  [calls]
  (fn [opts]
    (let
      [cmd
       (first (or (get opts "commands") (get opts :commands)))

       op
       (or (get opts "op") (get opts :op))]

      (swap! calls conj {:cmd cmd :opts opts})
      (case op
        "logs"
        {"status" "running" "exit" nil "lines" []}

        "stop"
        {"status" "stopped" "stopped" true}

        (let [failed? (str/includes? (str cmd) "boom")]
          {"commands" [(cond->
                         {"cmd" cmd
                          "exit" (if failed? 7 0)
                          "stdout" (if failed? "partial\n" "hello\n")
                          "stderr" (if failed? "boom!\n" "")
                          "duration_ms" (if failed? 3 2)
                          "started" true
                          "timed_out" false
                          "timeout_secs" 120
                          "stdout_truncated" false
                          "stderr_truncated" false
                          "stdout_omitted_chars" 0
                          "stderr_omitted_chars" 0}
                         (nil? cmd)
                         (assoc "exit" nil))]})))))

(defn- ev [^Context c code] (ep/->clj (.eval c "python" code)))

(defdescribe
  subprocess-bridge-test
  (it
    "routes subprocess.run through the shell tool and returns a CompletedProcess"
    (let
      [calls
       (atom [])

       {:keys [^Context python-context]}
       (ep/create-python-context {'shell (fake-shell calls)})]

      (.eval python-context "python" "import subprocess")
      (expect
        (=
          [0 "hello\n"]
          (ev
            python-context
            "r = subprocess.run(['echo','hi'], capture_output=True, text=True)\n[r.returncode, r.stdout]")))
      ;; argv list was shell-quoted + joined into the direct bash line.
      (expect (= "echo hi" (:cmd (last @calls))))))
  (it
    "passes run options and keeps Popen lifecycle ids inside the options map"
    (let
      [calls
       (atom [])

       {:keys [^Context python-context]}
       (ep/create-python-context {'shell (fake-shell calls)})]

      (.eval python-context "python" "import subprocess")
      (.eval python-context "python" "subprocess.run('sleep 1', shell=True, timeout=30, cwd='src')")
      (expect (= 30 (get (:opts (last @calls)) "timeout_secs")))
      (expect (= "src" (get (:opts (last @calls)) "cwd")))
      (.eval python-context "python" "p = subprocess.Popen('sleep 1', shell=True, cwd='src')")
      (let [popen-id (get (:opts (last @calls)) "id")]
        (expect (= "background" (get (:opts (last @calls)) "op")))
        (expect (= "src" (get (:opts (last @calls)) "cwd")))
        (.eval python-context "python" "p.poll()")
        (expect (nil? (:cmd (last @calls))))
        (expect (= {"op" "logs" "id" popen-id} (:opts (last @calls))))
        (.eval python-context "python" "p.terminate()")
        (expect (nil? (:cmd (last @calls))))
        (expect (= {"op" "stop" "id" popen-id} (:opts (last @calls)))))))
  (it "check_output returns stdout and raises on a non-zero exit"
      (let
        [calls
         (atom [])

         {:keys [^Context python-context]}
         (ep/create-python-context {'shell (fake-shell calls)})]

        (.eval python-context "python" "import subprocess")
        (expect (= "hello\n" (ev python-context "subprocess.check_output('echo hi', shell=True)")))
        (let
          [msg (try (.eval python-context "python" "subprocess.check_output('boom', shell=True)")
                    nil
                    (catch Throwable t (.getMessage t)))]
          (expect (some? msg))
          (expect (str/includes? (str msg) "non-zero")))))
  (it "os.system returns the exit code"
      (let
        [calls
         (atom [])

         {:keys [^Context python-context]}
         (ep/create-python-context {'shell (fake-shell calls)})]

        (expect (= 7 (ev python-context "import os\nos.system('boom')"))))))

(defdescribe subprocess-gate-test
             (it "raises a helpful 'enable the shell tool' message when `shell` is absent"
                 (let [{:keys [^Context python-context]} (ep/create-python-context {})]
                   (.eval python-context "python" "import subprocess")
                   (let
                     [msg (try (.eval python-context "python" "subprocess.run(['echo','hi'])")
                               nil
                               (catch Throwable t (.getMessage t)))]
                     (expect (some? msg))
                     (expect (or (str/includes? (str msg) "not enabled")
                                 (str/includes? (str msg) "Shell commands"))))))
             (it "does not leak shim internals into the live-vars baseline"
                 (let [{:keys [initial-ns-keys]} (ep/create-python-context {})]
                   ;; subprocess lives in sys.modules, not globals; the installer is del'd
                   (expect (not (contains? initial-ns-keys "subprocess")))
                   (expect (not (contains? initial-ns-keys "__vis_install_posix_compat__"))))))
