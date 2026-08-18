(ns com.blockether.vis.internal.foundation.shim-ruff-test
  "The `ruff` sandbox shim: `import ruff` inside the Python sandbox reaches the
   in-process ruff cdylib (com.blockether/ruff), so `vis-agent python -m ruff` formats
   and lints with no ruff on PATH and no PyPI install.

   Files are written on the Clojure side into a system temp dir and the Context
   is built with a `roots-fn`, so Python `open()`/`os.walk` see them."
  (:require [com.blockether.vis.internal.env-python :as ep]
            [clojure.string :as str]
            [lazytest.core :refer [defdescribe expect it]])
  (:import [org.graalvm.polyglot Context]
           [java.nio.file Files]
           [java.nio.file.attribute FileAttribute]))

(defn- ev [^Context c code] (ep/->clj (.eval c "python" code)))

(defn- tmp-dir
  ^String []
  (str (Files/createTempDirectory "vis-ruff-shim-" (make-array FileAttribute 0))))

(defmacro with-fs-context
  "A sandbox Context whose Python filesystem is confined to `dir`."
  [dir & body]
  `(let [~(with-meta 'python-context {:tag `Context})
         (:python-context (ep/create-python-context {} (constantly [~dir])))]
     (try ~@body (finally (.close ~'python-context)))))

(defdescribe
  ruff-shim-availability-test
  (it "is absent at context creation and present after import"
      (let [d (tmp-dir)]
        (with-fs-context
          d
          (expect (= false (ev python-context "'ruff' in __import__('sys').modules")))
          (expect (= true
                     (ev python-context "import ruff\n'ruff' in __import__('sys').modules"))))))
  (it "reports ruff's own version first"
      (let [d (tmp-dir)]
        (with-fs-context d
                         (expect (str/starts-with? (ev python-context "import ruff\nruff.version()")
                                                   "ruff "))))))

(defdescribe
  ruff-shim-api-test
  (it "formats a source string"
      (let [d (tmp-dir)]
        (with-fs-context d
                         (expect (= "x = 1\n"
                                    (ev python-context "import ruff\nruff.format_str('x=1')"))))))
  (it "lints a source string into diagnostic dicts"
      (let [d (tmp-dir)]
        (with-fs-context d
                         (expect (= "F401"
                                    (ev python-context
                                        (str "import ruff\n"
                                             "ds = ruff.check_str('import os\\n')\n"
                                             "ds[0]['code']")))))))
  (it "formats a file in place"
      (let [d (tmp-dir)]
        (spit (str d "/a.py") "x=1\n")
        (with-fs-context d
                         (ev python-context
                             (str "import ruff\nruff.format_file(" (pr-str (str d "/a.py")) ")"))
                         (expect (= "x = 1\n" (slurp (str d "/a.py")))))))
  (it "finds the project's ruff configuration file"
      (let [d (tmp-dir)]
        (spit (str d "/ruff.toml") "line-length = 60\n")
        (spit (str d "/a.py") "x = 1\n")
        (with-fs-context d
                         (expect (str/ends-with? (ev python-context
                                                     (str "import ruff\nruff.config_for("
                                                          (pr-str (str d "/a.py"))
                                                          ")"))
                                                 "ruff.toml")))))
  (it "returns None when the tree has no ruff configuration"
      (let [d (tmp-dir)]
        (spit (str d "/a.py") "x = 1\n")
        (with-fs-context
          d
          (expect (nil? (ev python-context
                            (str "import ruff\nruff.config_for(" (pr-str (str d "/a.py")) ")")))))))
  ;; Regression: a file reached through a SYMLINKED path lost every per-file
  ;; ignore — ruff discovered the config from the canonicalized path but matched
  ;; the `per-file-ignores` globs against the path it was handed, so `check_file`
  ;; reported F401 on a file the project had exempted.
  (it "honours per-file-ignores through a symlinked path"
      (let [d (tmp-dir)]
        (.mkdirs (java.io.File. (str d "/real/pkg")))
        (spit (str d "/real/ruff.toml") "[lint.per-file-ignores]\n\"pkg/*.py\" = [\"F401\"]\n")
        (spit (str d "/real/pkg/exempt.py") "import os\n")
        (spit (str d "/real/checked.py") "import os\n")
        (Files/createSymbolicLink (.toPath (java.io.File. (str d "/link")))
                                  (.toPath (java.io.File. (str d "/real")))
                                  (make-array FileAttribute 0))
        (with-fs-context d
                         (expect (= 0
                                    (ev python-context
                                        (str "import ruff\nlen(ruff.check_file("
                                             (pr-str (str d "/link/pkg/exempt.py"))
                                             "))"))))
                         (expect (= "F401"
                                    (ev python-context
                                        (str "import ruff\nruff.check_file("
                                             (pr-str (str d "/link/checked.py"))
                                             ")[0]['code']"))))))))

(defdescribe
  ruff-shim-cli-test
  "`vis-agent python -m ruff <argv>` — console_main returns the process exit code."
  (it "check exits 1 and reports the rule on a file with a finding"
      (let [d (tmp-dir)]
        (spit (str d "/bad.py") "import os\n")
        (with-fs-context d
                         (expect (= 1
                                    (ev python-context
                                        (str "import ruff\nruff.console_main(['check', "
                                             (pr-str (str d))
                                             "])")))))))
  (it "check exits 0 on clean source"
      (let [d (tmp-dir)]
        (spit (str d "/ok.py") "x = 1\n")
        (with-fs-context d
                         (expect (= 0
                                    (ev python-context
                                        (str "import ruff\nruff.console_main(['check', "
                                             (pr-str (str d))
                                             "])")))))))
  (it "format rewrites the file and --check then passes"
      (let [d (tmp-dir)]
        (spit (str d "/f.py") "x=1\n")
        (with-fs-context
          d
          (expect (= 0
                     (ev python-context
                         (str "import ruff\nruff.console_main(['format', " (pr-str (str d)) "])"))))
          (expect (= "x = 1\n" (slurp (str d "/f.py"))))
          (expect (= 0
                     (ev python-context
                         (str "import ruff\nruff.console_main(['format', "
                              "'--check', "
                              (pr-str (str d))
                              "])")))))))
  (it "format --check exits 1 when a file would be reformatted"
      (let [d (tmp-dir)]
        (spit (str d "/g.py") "x=1\n")
        (with-fs-context d
                         (expect (= 1
                                    (ev python-context
                                        (str "import ruff\nruff.console_main(['format', "
                                             "'--check', "
                                             (pr-str (str d))
                                             "])"))))
                         (expect (= "x=1\n" (slurp (str d "/g.py")))))))
  (it "honours the project's ruff.toml rule selection"
      (let [d (tmp-dir)]
        (spit (str d "/ruff.toml") "lint.select = [\"E501\"]\nline-length = 20\n")
        (spit (str d "/h.py") (str "x = \"" (apply str (repeat 40 "a")) "\"\n"))
        (with-fs-context d
                         ;; F401 is NOT selected by this config, E501 is
                         (expect (= 1
                                    (ev python-context
                                        (str "import ruff\nruff.console_main(['check', "
                                             (pr-str (str d))
                                             "])")))))))
  (it "--version exits 0"
      (let [d (tmp-dir)]
        (with-fs-context
          d
          (expect (= 0 (ev python-context "import ruff\nruff.console_main(['--version'])")))))))
(defdescribe ruff-shim-fix-test
             ;; Regression, issue #131: `check --fix` used to be rejected by the shim.
             (it "check --fix applies safe fixes"
                 (let [d (tmp-dir)]
                   (spit (str d "/fix.py") "import os\nx = 1\n")
                   (with-fs-context
                     d
                     (expect (= 0
                                (ev python-context
                                    (str "import ruff\nruff.console_main(['check', '--fix', "
                                         (pr-str (str d "/fix.py"))
                                         "])"))))
                     (expect (= "x = 1\n" (slurp (str d "/fix.py")))))))
             ;; Regression, issue #131: directory checks used to lint files excluded by Ruff config.
             (it "honours exclude for directory targets"
                 (let [d (tmp-dir)]
                   (spit (str d "/ruff.toml") "exclude = [\"ignored.py\"]\n")
                   (spit (str d "/ignored.py") "import os\n")
                   (spit (str d "/included.py") "import os\n")
                   (with-fs-context d
                                    (expect (= 1
                                               (ev python-context
                                                   (str "import ruff\nruff.console_main(['check', "
                                                        (pr-str (str d))
                                                        "])"))))))))
