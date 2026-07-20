(ns com.blockether.vis.internal.posix-compat-shim-test
  "The POSIX-compat shim installed into every sandbox context: subprocess /
   os.system / os.popen are replaced with wrappers that DELEGATE to the
   `shell_run` / `shell_bg` tools. Tool callables are looked up in globals at
   CALL time, so the shim self-adapts — it routes when shell_run is bound and
   raises a clear 'enable the shell tool' message when it isn't."
  (:require [clojure.string :as str]
            [com.blockether.vis.internal.env-python :as ep]
            [lazytest.core :refer [defdescribe expect it]])
  (:import [org.graalvm.polyglot Context]))

(defn- fake-shell-run
  "Records calls into `calls`; emulates exit 0 normally, exit 7 + stderr for a
   command containing 'boom'. `wrap-ifn` marshals the Python args to Clojure
   before we see them, so `opts` is already a Clojure map (string keys)."
  [calls]
  (fn shell-run ([cmd] (shell-run cmd nil))
    ([cmd opts] (swap! calls conj {:cmd cmd :opts opts})
     (if (str/includes? (str cmd) "boom")
       {"cmd" cmd "exit" 7 "stdout" "partial\n" "stderr" "boom!\n" "duration_ms" 3}
       {"cmd" cmd "exit" 0 "stdout" "hello\n" "duration_ms" 2}))))

(defn- ev [^Context c code] (ep/->clj (.eval c "python" code)))

(defdescribe
  subprocess-bridge-test
  (it
    "routes subprocess.run through shell_run and returns a CompletedProcess"
    (let
      [calls
       (atom [])

       {:keys [^Context python-context]}
       (ep/create-python-context {'shell-run (fake-shell-run calls)})]

      (.eval python-context "python" "import subprocess")
      (expect
        (=
          [0 "hello\n"]
          (ev
            python-context
            "r = subprocess.run(['echo','hi'], capture_output=True, text=True)\n[r.returncode, r.stdout]")))
      ;; argv list was shell-quoted + joined into the command shell_run received
      (expect (= "echo hi" (:cmd (last @calls))))))
  (it "passes timeout/cwd through as shell_run opts"
      (let
        [calls
         (atom [])

         {:keys [^Context python-context]}
         (ep/create-python-context {'shell-run (fake-shell-run calls)})]

        (.eval python-context "python" "import subprocess")
        (.eval python-context "python" "subprocess.run('sleep 1', shell=True, timeout=30)")
        ;; strings-only boundary: the crossed opts dict keeps VERBATIM string
        ;; keys, so the recorded key is "timeout_secs"
        (expect (= 30 (get (:opts (last @calls)) "timeout_secs")))))
  (it "check_output returns stdout and raises on a non-zero exit"
      (let
        [calls
         (atom [])

         {:keys [^Context python-context]}
         (ep/create-python-context {'shell-run (fake-shell-run calls)})]

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
         (ep/create-python-context {'shell-run (fake-shell-run calls)})]

        (expect (= 7 (ev python-context "import os\nos.system('boom')"))))))

(defdescribe subprocess-gate-test
             (it "raises a helpful 'enable the shell tool' message when shell_run is absent"
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
