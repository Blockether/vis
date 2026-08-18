(ns com.blockether.vis.internal.python-project
  "What a Python project DECLARES about its own layout: import roots (the `src`
   layout every packaging backend spells differently) and pytest's `testpaths`.

   The metadata is parsed by PYTHON'S OWN parsers inside a GraalPy context --
   `tomllib` for `pyproject.toml`, `configparser` for `setup.cfg` / `pytest.ini`
   / `tox.ini` -- never a regex over the file text. The Python side
   (`resources/vis-python/project_config.py`) returns RAW declared strings;
   everything host-shaped (`~` expansion, resolution against the project dir,
   existence, canonicalisation, dedup, `python.source_paths` config) lives here.

   Inference is purely declarative: a project without such metadata gets
   nothing inferred, and `python.source_paths` is how a user says it outright.
   Every failure degrades to nothing rather than breaking the caller."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [com.blockether.vis.internal.config :as config]
            [com.blockether.vis.internal.paths :as paths]
            [com.blockether.vis.internal.python-extensions :as pyx])
  (:import [java.io File]
           [org.graalvm.polyglot Context Value]))

(set! *warn-on-reflection* true)

(def ^:private project-config-src
  "Python source of the packaging-metadata reader, embedded in the native image
   by build.clj's `-H:IncludeResources=vis-python/.*`."
  (delay (some-> (io/resource "vis-python/project_config.py")
                 slurp)))

(defn- resolve-project-path
  "`path` as a `java.io.File`: absolute entries stand alone, relative ones
   resolve against `dir`, and a leading `~` expands to the home directory."
  ^File [^String dir ^String path]
  (let [expanded
        (paths/expand-home path)

        f
        (io/file expanded)]

    (if (.isAbsolute f) f (io/file dir expanded))))

(defn existing-paths
  "`paths` (raw, relative to `dir`) reduced to the canonical paths of the
   entries that actually exist, in declaration order, without duplicates.
   `pred` (default: exists) decides what counts."
  ([^String dir paths] (existing-paths dir paths #(.exists ^File %)))
  ([^String dir paths pred]
   (->> paths
        (remove str/blank?)
        (map str/trim)
        distinct
        (map #(resolve-project-path dir %))
        (filter pred)
        (mapv #(.getCanonicalPath ^File %)))))

(defn- existing-dirs
  "`existing-paths` restricted to directories, ignoring `.` (already on
   `sys.path` / already the run's target)."
  [^String dir paths]
  (existing-paths dir
                  (remove #{"." "./"} (map str/trim (remove str/blank? paths)))
                  #(.isDirectory ^File %)))

(defn- throwable-msg
  "A non-empty description of `t` for a `:error` / `:warning` string."
  ^String [^Throwable t]
  (or (not-empty (str (ex-message t))) (.getName (class t))))

(defn declared-config
  "Raw layout `dir`'s packaging metadata declares, read inside `ctx`:

     {:import-roots [\"src\" …] :testpaths [\"tests\" …]}

   in declaration order, exactly as written. Sources:

     [tool.setuptools.packages.find]   where       = [\"src\"]
     [tool.setuptools]                 package-dir = {\"\" = \"src\"}
     [tool.poetry]                     packages    = [{include = \"pkg\", from = \"src\"}]
     [tool.hatch.build.targets.wheel]  packages    = [\"src/pkg\"]
     [tool.pdm.build]                  package-dir = \"src\"
     [tool.pytest.ini_options]         pythonpath  = [\"src\"]   testpaths = [\"tests\"]
     setup.cfg [options]               package_dir = =src
     setup.cfg [tool:pytest] / pytest.ini / tox.ini [pytest]   pythonpath / testpaths

   Unreadable or absent metadata yields empty vectors, never a throw; a read that
   FAILED (as opposed to one that found nothing) also carries `:error` with why,
   so a caller can say so instead of reporting a project with no layout. The
   caller's globals are left exactly as they were found."
  [^Context ctx ^String dir]
  (let [^Value bindings
        (.getBindings ctx "python")

        entry
        "__vis_project_config__"

        arg
        "__vis_project_dir__"

        strings
        (fn [^Value v]
          (mapv #(.asString (.getArrayElement v (long %))) (range (.getArraySize v))))]

    (try (.eval ctx "python" ^String @project-config-src)
         (.putMember bindings arg dir)
         (let [^Value res (.eval ctx "python" (str entry "(globals()[" (pr-str arg) "])"))]
           {:import-roots (strings (.getArrayElement res 0))
            :testpaths (strings (.getArrayElement res 1))})
         (catch Throwable t {:import-roots [] :testpaths [] :error (throwable-msg t)})
         ;; The CLI interpreter is the human's own scope -- leave nothing behind.
         (finally (.removeMember bindings arg) (.removeMember bindings entry)))))

(defn- configured-import-roots
  "Import roots the user declared in merged config as `python.source_paths` --
   the explicit escape hatch for a project whose layout Vis cannot infer (or
   does not infer the way the user wants):

     python:
       source_paths: [src, lib/vendor]

   Relative entries resolve against `dir`, `~` expands, and any config failure
   degrades to nothing rather than breaking the caller."
  [^String dir]
  (try (let [configured (get-in (config/load-config-raw) ["python" "source_paths"])]
         (existing-dirs dir
                        (cond (string? configured) [configured]
                              (sequential? configured) configured
                              :else nil)))
       (catch Throwable _ [])))

(defn import-roots
  "Import roots for `dir`, read through the already-built `ctx`: the ones
   configured in `python.source_paths` first, then whatever the packaging
   metadata declares. Canonical paths of directories that actually exist, in
   declaration order, so `vis-agent python -m pytest tests/` imports the project
   the same way an explicit `PYTHONPATH=src` invocation would."
  [^Context ctx ^String dir]
  (vec (distinct (concat (configured-import-roots dir)
                         (existing-dirs dir (:import-roots (declared-config ctx dir)))))))

(defn- read-layout
  "ONE attempt at `project-layout`, in a throwaway trusted GraalPy context.
   `:warning` (present only on failure) says why the read degraded to nothing."
  [^String dir]
  (let [built
        (try {:ctx (pyx/build-context "python-project-layout")}
             (catch Throwable t
               {:warning (str "GraalPy context unavailable, project layout not read: "
                              (throwable-msg t))}))

        ^Context ctx
        (:ctx built)]

    (if (nil? ctx)
      {:import-roots [] :testpaths [] :warning (:warning built)}
      (try (let [declared (declared-config ctx dir)]
             (cond-> {:import-roots (vec (distinct (concat
                                                     (configured-import-roots dir)
                                                     (existing-dirs dir (:import-roots declared)))))
                      :testpaths (existing-paths dir (:testpaths declared))}
               (:error declared)
               (assoc :warning
                 (str "project metadata unreadable, import roots not applied: "
                      (:error declared)))))
           (catch Throwable t
             {:import-roots []
              :testpaths []
              :warning (str "project layout not read: " (throwable-msg t))})
           (finally (try (.close ctx true) (catch Throwable _)))))))

(defn project-layout
  "`{:import-roots [abs…] :testpaths [abs…]}` for `dir`, read in a THROWAWAY
   trusted GraalPy context (~130ms) -- for callers that have no context of
   their own, such as the `run_tests` handler. Both are canonical paths of
   entries that exist; either may be empty.

   A FAILED read is retried once (a cold context can lose its first attempt) and,
   if it fails again, the map carries `:warning`. Degrading silently to \"no
   import roots\" is what makes a `src`-layout project report bogus
   `No module named <pkg>` errors from the user's own tests."
  [^String dir]
  (let [first-try (read-layout dir)]
    (if (:warning first-try)
      (let [retry (read-layout dir)]
        (if (:warning retry) (update retry :warning #(str % " (retried once)")) retry))
      first-try)))
