(ns com.blockether.vis.internal.foundation.sandbox-shim-contract-test
  "Repo-wide contract for the built-in Python sandbox shims.

   Every shim is three things that must stay in step: a lazy `shim_*.clj`
   initializer named by the one distribution manifest, a registered extension,
   and a `resources/vis-shims/<name>.py` classpath resource. Drift in any one is
   invisible until an agent imports the module, so this test pins the boundary."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [com.blockether.vis.internal.doc-corpus :as doc-corpus]
            [com.blockether.vis.internal.env-python :as ep]
            [com.blockether.vis.internal.extension :as extension]
            [com.blockether.vis.internal.manifest :as manifest]
            [lazytest.core :refer [defdescribe expect it]])
  (:import [java.io File]
           [org.graalvm.polyglot Context]))

(def ^:private shim-ns-dir "src/com/blockether/vis/internal/foundation")

(def ^:private shim-resource-dir "resources/vis-shims")

(def ^:private env-python-installed
  "Python shim sources NOT contributed as `:ext/sandbox-shims`. `posix.py` is
   installed directly by `env-python/install-posix-refusal-shim!` into every
   sandbox context, so it deliberately has no `shim_*.clj`."
  #{"vis-shims/posix.py"})

(defn- shim-initializers
  []
  (->> (:initialization (manifest/read-manifest))
       (filter #(str/starts-with? (namespace %) "com.blockether.vis.internal.foundation.shim-"))
       vec))

(defn- shim-ns-files
  []
  (->> (.listFiles (io/file shim-ns-dir))
       (filter (fn [^File f]
                 (re-matches #"shim_.*\.clj" (.getName f))))
       vec))

(defn- shim-ns-sym
  [^File f]
  (symbol (str "com.blockether.vis.internal.foundation."
               (-> (.getName f)
                   (str/replace #"\.clj$" "")
                   (str/replace "_" "-")))))

(defn- registered-shims
  "Invoke the shim initializers named by the distribution manifest."
  []
  (doseq [initializer (shim-initializers)]
    ((requiring-resolve initializer)))
  (extension/sandbox-shims))

(defdescribe shim-registration-test
             (it "finds the shim namespaces on disk at all"
                 ;; Guards the guard: a wrong directory would make every check below vacuous.
                 (expect (< 10 (count (shim-ns-files)))))
             (it "lists every shim_*.clj in manifest initialization"
                 (let [listed
                       (set (map (comp symbol namespace) (shim-initializers)))

                       unlisted
                       (remove listed (map shim-ns-sym (shim-ns-files)))]

                   (expect (empty? unlisted)
                           (str "shim namespaces missing from manifest initialization: "
                                (pr-str unlisted)))))
             (it "registers exactly one shim per shim namespace"
                 (expect (= (count (shim-ns-files)) (count (registered-shims))))))

(defdescribe
  shim-source-test
  (it "names its Python source under vis-shims/, never inline Clojure strings"
      (let [bad (remove #(str/starts-with? (str (:shim/source %)) "vis-shims/") (registered-shims))]
        (expect (empty? bad)
                (str "shims with a non-vis-shims source: " (pr-str (map :shim/name bad))))))
  (it "resolves every :shim/source as a classpath resource"
      ;; `extension/shim-python-source` slurps this resource at sandbox boot; a
      ;; typo here is a hard failure the moment an agent imports the module.
      (let [missing (remove #(io/resource (:shim/source %)) (registered-shims))]
        (expect (empty? missing)
                (str "shims whose source resource is missing: "
                     (pr-str (map :shim/source missing))))))
  (it "keeps shim names and their imports unique"
      (let [shims
            (registered-shims)

            dupe-names
            (->> shims
                 (map :shim/name)
                 frequencies
                 (filter #(< 1 (val %)))
                 (map key))

            dupe-imports
            (->> shims
                 (mapcat :shim/imports)
                 frequencies
                 (filter #(< 1 (val %)))
                 (map key))]

        (expect (empty? dupe-names) (str "duplicate shim names: " (pr-str dupe-names)))
        (expect (empty? dupe-imports)
                (str "two shims claim the same import: " (pr-str dupe-imports)))))
  (it "declares a name and a way for the sandbox to reach it"
      ;; A shim reaches Python either as an importable module (`:shim/imports`) or as
      ;; prebound globals (`:shim/bindings`, how the `attach` shim publishes `attach`
      ;; and friends) — declaring NEITHER makes it unreachable. WHAT each name does is
      ;; documented in the shim's own Python `__doc__`, harvested into the
      ;; manifest's static `META-INF/vis/apropos/shims.edn` resource.
      (doseq [{:shim/keys [name imports bindings]} (registered-shims)]
        (expect (not (str/blank? name)))
        (expect (or (seq imports) (some? bindings))
                (str name " declares neither :shim/imports nor :shim/bindings")))))

(defdescribe shim-resource-test
             (it "has a shim declaring every resources/vis-shims/*.py"
                 ;; The reverse direction: an orphan .py is either dead weight in the native
                 ;; image or a shim someone forgot to register.
                 (let [declared
                       (set (map :shim/source (registered-shims)))

                       on-disk
                       (->> (.listFiles (io/file shim-resource-dir))
                            (map (fn [^File f]
                                   (.getName f)))
                            ;; Only Python source files are shim implementations.
                            (filter (fn [^String n]
                                      (.endsWith n ".py")))
                            (map (fn [^String n]
                                   (str "vis-shims/" n)))
                            (remove env-python-installed))

                       orphans
                       (remove declared on-disk)]

                   (expect (empty? orphans)
                           (str "Python shim sources nobody declares: " (pr-str orphans)))))
             (it "still installs the env-python-owned posix bridge"
                 ;; The one documented exception must keep existing, or `subprocess` /
                 ;; `os.system` stop routing to the shell tools.
                 (doseq [resource env-python-installed]
                   (expect (some? (io/resource resource)) (str resource " is missing"))))
             (it "embeds vis-shims/ in the native image"
                 ;; Without this native-image argument every shim resolves in dev and none
                 ;; of them resolves in the shipped binary.
                 (expect (str/includes? (slurp "build.clj") "-H:IncludeResources=vis-shims/.*"))))

(defdescribe
  shim-docs-are-pulled-test
  "The detail a description no longer carries has to be reachable, or cutting it
   was a deletion. `:shim/docs` is what `doc(name)` answers inside the sandbox."
  (it "answers `doc(name)` with :shim/docs, not the pushed prompt line"
      (let [shim
            (->> (registered-shims)
                 (filter :shim/docs)
                 first)

            ^Context ctx
            (:python-context (ep/create-python-context {}))

            name
            (first (concat (:shim/imports shim) (:shim/globals shim)))

            doc
            (:stdout
              (ep/run-python-block ctx (str "print(__vis_docs__[" (pr-str name) "])") "t1/i1"))]

        (expect (some? shim) "no shim declares :shim/docs — the pull path is untested")
        (expect (str/includes? doc (subs (:shim/docs shim) 0 60))
                (str name " doc() does not serve :shim/docs"))
        (let [entry (some #(when (= name (:name %)) %) (doc-corpus/entries))]
          (expect (some? entry) (str name " is absent from manifest apropos data"))
          (expect (str/includes? doc (first (str/split-lines (:text entry))))
                  (str name " doc() does not serve its Python __doc__"))))))

(defdescribe
  shim-globals-name-their-call-test
  "A prebound global is typed straight into a block — `ls(...)`, `nippy_decode(...)` — with
   no import and no signature to inspect first, so something has to state its arguments.
   The canonical home is STRUCTURE: `__vis_calls__[name]`, which `doc(name)` prints above
   the document, exactly where a tool's `:call` line goes — a page that opens with its own
   signature previews as a signature and stops matching the words its prose is written in.
   A shim that has not moved yet may still carry the call form inside its text; naming it
   NOWHERE is the bug (the two nippy globals were pages that named neither an argument nor
   a result)."
  (it "documents every shim global with its own call form"
      (let [names
            (->> (registered-shims)
                 (mapcat :shim/globals)
                 (filter string?)
                 distinct
                 vec)

            ^Context ctx
            (:python-context (ep/create-python-context {}))

            code
            (str "_names = [" (str/join ", " (map pr-str names))
                 "]\n" "_docs = globals().get('__vis_docs__', {})\n"
                 "_calls = globals().get('__vis_calls__', {})\n" "print([n for n in _names\n"
                 "       if (n + '(') not in str(_calls.get(n, ''))\n"
                 "       and (n + '(') not in str(_docs.get(n, ''))])")

            undocumented
            (try (:stdout (ep/run-python-block ctx code "t1/i1")) (finally (.close ctx)))]

        ;; Guards the guard: an empty name list would make the check vacuous.
        (expect (< 5 (count names)))
        (expect (= "[]" (str/trim undocumented))
                (str "shim globals whose page never shows a call: " undocumented)))))
