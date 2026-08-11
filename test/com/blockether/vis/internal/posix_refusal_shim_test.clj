(ns com.blockether.vis.internal.posix-refusal-shim-test
  "The POSIX refusal shim installed into every sandbox context: `subprocess`,
   `os.system` and `os.popen` never spawn — they raise ONE message naming the
   `shell` tool, the single door every process goes through. The tool is looked
   up at CALL time, so the same file also answers the toggle-off state, where it
   must say that BOTH doors are closed rather than offering subprocess as a way
   around a disabled shell — and the same host sentences answer the DISCOVERY
   surface, where `doc` on `shell` reads the toggle state instead of a binding."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [com.blockether.vis.internal.env-python :as ep]
            [com.blockether.vis.internal.prompt :as prompt]
            [lazytest.core :refer [defdescribe expect it]])
  (:import [org.graalvm.polyglot Context]))

(defn- raised
  "The message `code` raises in `ctx`, or nil when it did not raise. GraalPy
   prefixes the guest exception type, which is not part of the wording."
  [^Context ctx ^String code]
  (try (.eval ctx "python" code)
       nil
       (catch Throwable t
         (some-> (.getMessage t)
                 (str/replace #"^RuntimeError: " "")))))

(defn- shell-bound-context
  "A sandbox context with the `shell` tool present — the shim must STILL refuse."
  ^Context []
  (:python-context (ep/create-python-context {'shell (fn [& _]
                                                       {"exit" 0})})))

(defdescribe subprocess-refusal-test
             ;; Regression, handle audit: `subprocess` used to be a second spawn door that
             ;; delegated to the shell tool, so containment, the log file and the handle had
             ;; two entry points and the model picked whichever it remembered first.
             (it "refuses subprocess.run even when the shell tool is bound, and names `shell`"
                 (let [ctx (shell-bound-context)]
                   (.eval ctx "python" "import subprocess")
                   (let [msg (raised ctx "subprocess.run(['echo','hi'], capture_output=True)")]
                     (expect (some? msg))
                     (expect (str/includes? msg "`shell`"))
                     (expect (str/includes? msg "await shell("))
                     (expect (not (str/includes? msg "DISABLED"))))))
             (it "refuses every spawning entry point of the module"
                 (let [ctx (shell-bound-context)]
                   (.eval ctx "python" "import subprocess")
                   (doseq
                     [code ["subprocess.call('ls')" "subprocess.check_call('ls')"
                            "subprocess.check_output('ls')" "subprocess.getoutput('ls')"
                            "subprocess.getstatusoutput('ls')" "subprocess.Popen('ls')"]]
                     (expect (some? (raised ctx code)) code))))
             (it
               "refuses os.system and os.popen with the same message"
               (let [ctx (shell-bound-context)]
                 (expect (str/includes? (str (raised ctx "import os\nos.system('ls')")) "`shell`"))
                 (expect (str/includes? (str (raised ctx "import os\nos.popen('ls')")) "`shell`"))))
             (it "keeps the exception types importable so a handler line does not NameError"
                 (let [ctx (shell-bound-context)]
                   (.eval ctx "python" "import subprocess")
                   (expect (nil? (raised ctx
                                         (str "try:\n" "    subprocess.run('ls')\n"
                                              "except subprocess.CalledProcessError:\n" "    pass\n"
                                              "except RuntimeError:\n" "    pass\n")))))))

(defdescribe
  shell-toggle-off-test
  ;; Regression, handle audit: with the shell toggle off the shim advertised only
  ;; that subprocess could not run, which read as "the other door might work".
  (it "says BOTH the shell tool and subprocess are disabled when `shell` is absent"
      (let [ctx (:python-context (ep/create-python-context {}))]
        (.eval ctx "python" "import subprocess")
        (let [msg (raised ctx "subprocess.run(['echo','hi'])")]
          (expect (some? msg))
          (expect (str/includes? msg "DISABLED"))
          (expect (str/includes? msg "`shell` is not bound"))
          (expect (str/includes? msg "Shell commands")))))
  ;; Regression, discovery audit: with the toggle off `shell` left the discovery
  ;; corpus entirely, so a shell-shaped query answered nothing — silence the model
  ;; read as "nothing here can start a process at all".
  (it "keeps `shell` in the corpus with the toggle state AND the door it leaves open"
      (let
        [^Context ctx
         (:python-context (ep/create-python-context {}))

         hits
         (fn [q]
           (str/split (.asString
                        (.eval ctx "python" ^String (str "','.join(apropos(" (pr-str q) "))")))
                      #","))

         doc-text
         (fn [q]
           (.asString (.eval ctx "python" ^String (str "doc(" (pr-str q) ")"))))]

        (doseq [q ["shell" "she"]]
          (expect (some #{"shell"} (hits q)) q))
        (expect (str/includes? (doc-text "shell") (get ep/PROCESS_SURFACE "off")))
        (expect (str/includes? (doc-text "shell") (get ep/PROCESS_SURFACE "extension")))))
  (it "says nothing about the extension door while the shell tool is bound"
      (let [^Context ctx (shell-bound-context)]
        (expect (not (str/includes? (.asString (.eval ctx "python" "doc('shell')"))
                                    (get ep/PROCESS_SURFACE "extension"))))))
  (it "does not leak shim internals into the live-vars baseline"
      (let [{:keys [initial-ns-keys]} (ep/create-python-context {})]
        ;; subprocess lives in sys.modules, not globals; the installer is del'd
        (expect (not (contains? initial-ns-keys "subprocess")))
        (expect (not (contains? initial-ns-keys "__vis_install_posix_compat__"))))))

(defdescribe process-surface-is-said-once-test
             ;; Regression, prompt audit: the same fact about this sandbox's process surface
             ;; was worded four times - the prompt block, the `subprocess` refusal, the
             ;; toggle-off refusal and the handle refusal each had their own sentence, so
             ;; fixing one left the model reading a different rule from the next.
             (it "gives the prompt, `subprocess` and a live handle the SAME host sentences"
                 (let
                   [ban
                    (get ep/PROCESS_SURFACE "ban")

                    use'
                    (get ep/PROCESS_SURFACE "use")

                    off
                    (get ep/PROCESS_SURFACE "off")]

                   ;; The prompt says the rule, never the invocation grammar.
                   (expect (str/includes? (#'prompt/sandbox-shims-prompt-block
                                           [{:ext/name "foundation-shell"}])
                                          ban))
                   (expect (str/includes? (#'prompt/sandbox-shims-prompt-block []) off))
                   ;; The call site says the rule AND how to do the work instead.
                   (let [ctx (shell-bound-context)]
                     (.eval ctx "python" "import subprocess")
                     (expect (= (str ban " " use') (raised ctx "subprocess.run('ls')"))))
                   ;; No shell tool: one sentence for the tool, `subprocess` and the handle.
                   (let [ctx (:python-context (ep/create-python-context {}))]
                     (.eval ctx "python" "import subprocess")
                     (expect (= off (raised ctx "subprocess.run('ls')")))
                     (expect (= off (raised ctx "__VisShell__({'id': 'p1'}).logs()"))))))
             (it "keeps the wording out of the Python files that say it"
                 ;; All of them read `__vis_process_surface__`; a literal copy in any file is a
                 ;; second source of truth that drifts on the next edit.
                 (doseq [f ["vis-shims/posix.py" "vis-python/async_runtime.py"]]
                   (let [src (slurp (io/resource f))]
                     (expect (str/includes? src "__vis_process_surface__") f)
                     (doseq [copy ["never spawn in the vis sandbox" "Shell commands are DISABLED"]]
                       (expect (not (str/includes? src copy)) (str f " " copy)))))))
