(ns com.blockether.vis.native-extension-sync-test
  "Declarative package synchronization through the built CLI, without a gateway."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [com.blockether.vis.native-binary-test :as native]
            [lazytest.core :refer [defdescribe expect it]])
  (:import [java.nio.file Files]))

(defdescribe
  native-extension-sync-test
  (it
    "syncs global and project configuration, caches uv and prunes only owned links"
    (let [home
          (#'native/temp-dir "vis-native-sync")

          cwd
          (doto (io/file home "workspace") .mkdirs)

          global
          (doto (io/file home ".vis") .mkdirs)

          source
          (doto (io/file cwd "source") .mkdirs)

          sdk
          (doto (io/file source "sdk") .mkdirs)

          bin
          (#'native/require-binary)

          args
          [(.getAbsolutePath bin) (str "-Duser.home=" home) "extension" "sync"]

          project-link
          (io/file cwd ".vis/extensions/native-sync-example/current")

          global-link
          (io/file global "extensions/native-sync-example/current")

          shared
          (doto (io/file home "shared") .mkdirs)

          environment
          (assoc (#'native/native-environment) "VIS_PYTHON_PACKAGES" (.getCanonicalPath shared))

          invoke
          (fn [argv timeout]
            (with-redefs-fn {#'native/native-environment (constantly environment)}
              (fn []
                (#'native/run-binary cwd argv timeout))))

          run
          #(let [result (invoke (into args %) 90)] (expect (= 0 (:exit result)) (:output result))
             (:output result))]

      (try
        (spit (io/file sdk "pyproject.toml") "[project]\nname='vis-agent'\nversion='0.1.0'\n")
        (spit (io/file source "pyproject.toml")
              (str "[project]\nname='native-sync-example'\nversion='1.0.0'\n"
                   "description='Native sync fixture'\nrequires-python='>=3.11'\n"
                   "dependencies=['vis-agent>=0.1.0']\n[tool.vis]\ncategory='tools'\n"
                   "[tool.uv.sources]\nvis-agent={path='sdk'}\n"))
        (spit (io/file source "extension.py")
              (str "from pathlib import Path\n"
                   "Path(__file__).with_name('imported').write_text('unexpected')\n"
                   "raise RuntimeError('sync must not import extensions')\n"))
        (spit (io/file cwd "vis.yml") "extensions:\n  native-sync-example:\n    source: ./source\n")
        (spit (io/file global "vis.yml")
              "extensions:\n  native-sync-example:\n    source: ../workspace/source\n")
        (expect (str/includes? (run ["--dry-run"]) "would-sync"))
        (expect (not (.exists project-link)))
        (expect (not (.exists global-link)))
        (expect (not= 0 (:exit (invoke args 60))))
        (expect (str/includes? (run ["--trust"]) "installed"))
        ;; Versioned installs select a local-source snapshot through a relative current link.
        (doseq [link [project-link global-link]]
          (expect (Files/isSymbolicLink (.toPath link)))
          (expect (= "1.0.0" (str (Files/readSymbolicLink (.toPath link)))))
          (expect (Files/isSymbolicLink (.toPath (io/file (.getParentFile link) "1.0.0"))))
          (expect (= (.getCanonicalFile source) (.getCanonicalFile link))))
        (expect (not (.exists (io/file source ".venv"))))
        (expect (.isFile (io/file source "uv.lock")))
        (let [lock
              (slurp (io/file source "uv.lock"))

              receipt
              (slurp (io/file cwd ".vis/extensions/.sync.json"))

              started
              (System/nanoTime)

              output
              (run ["--trust"])]

          (expect
            (= 2 (count (re-seq #"(?m)^(?:global|project)  native-sync-example  cached" output)))
            output)
          (expect (not (.exists (io/file source "imported")))
                  "Sync must not import installed extensions")
          (expect (= lock (slurp (io/file source "uv.lock"))))
          (expect (= receipt (slurp (io/file cwd ".vis/extensions/.sync.json"))))
          (println "NATIVE_EXTENSION_SYNC_WARM_MS" (/ (- (System/nanoTime) started) 1e6))
          (expect (not (.exists (io/file source ".venv"))))
          ;; Registration proves source-only packages use shared, not a private environment.
          (spit (io/file shared "native_sync_shared.py") "VALUE = 42\n")
          (spit (io/file source "extension.py")
                (str "from pathlib import Path\n" "import blockether.vis.extension as vis\n"
                     "import native_sync_shared\nassert native_sync_shared.VALUE == 42\n"
                     "vis.register_extension(vis.Extension(name='native-sync-example', "
                     "description='Verified shared selection'))\n"
                     "Path(__file__).with_name('imported').write_text('loaded')\n"))
          (let [listed (invoke [(.getAbsolutePath bin) (str "-Duser.home=" home) "extension" "list"]
                               90)]
            (expect (= 0 (:exit listed)) (:output listed))
            (expect (str/includes? (:output listed) "native-sync-example") (:output listed))
            (expect (.isFile (io/file source "imported")))
            (io/delete-file (io/file source "imported"))
            (expect (= lock (slurp (io/file source "uv.lock"))))
            (expect (= receipt (slurp (io/file cwd ".vis/extensions/.sync.json"))))
            (expect (not (.exists (io/file source ".venv"))))))
        (spit (io/file cwd "vis.yml") "extensions: {}\n")
        (expect (str/includes? (run ["--project" "--trust"]) "orphaned"))
        (expect (str/includes? (run ["--project" "--prune" "--dry-run"]) "would-remove"))
        (expect (.exists project-link))
        (expect (not (.exists (io/file source "imported")))
                "Dry-run must not import installed extensions")
        (expect (str/includes? (run ["--project" "--prune" "--trust"]) "removed"))
        (expect (not (.exists project-link)))
        (expect (.isFile (io/file source "extension.py")))
        (expect (.exists global-link))
        (finally (#'native/delete-tree! home))))))
