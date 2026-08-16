(ns com.blockether.vis.internal.foundation.sandbox-shim-contract-test
  "Repo-wide contract for the built-in Python sandbox shims.

   Every shim is THREE things that must stay in step: a lazy `shim_*.clj`
   registering one extension, an entry in `builtin-extension-nses` (or the
   extension is never loaded and `import <mod>` dies in the sandbox), and a
   `resources/vis-shims/<name>.py` reachable as a CLASSPATH RESOURCE — including
   inside the native image, which only embeds what build.clj asks for.

   Drift in any one of them is invisible until an agent's `import` fails at
   runtime, so it is checked here rather than discovered in the field."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [com.blockether.vis.internal.env-python :as ep]
            [com.blockether.vis.internal.extension :as extension]
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

(def ^:private builtin-nses
  (delay @(requiring-resolve 'com.blockether.vis.internal.extension/builtin-extension-nses)))

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
  "Load every builtin extension ns, then read back the registered shims."
  []
  (run! require @builtin-nses)
  (extension/sandbox-shims))

(defdescribe shim-registration-test
             (it "finds the shim namespaces on disk at all"
                 ;; Guards the guard: a wrong directory would make every check below vacuous.
                 (expect (< 10 (count (shim-ns-files)))))
             (it "lists every shim_*.clj in builtin-extension-nses"
                 ;; A shim ns that is not listed is dead code: nothing ever requires it, so
                 ;; its extension never registers and `import <mod>` fails in the sandbox.
                 (let
                   [listed
                    (set @builtin-nses)

                    unlisted
                    (remove listed (map shim-ns-sym (shim-ns-files)))]

                   (expect (empty? unlisted)
                           (str "shim namespaces missing from builtin-extension-nses: "
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
      (let
        [shims
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
  (it "declares a name, a description, and a way for the sandbox to reach it"
      ;; The sandbox capability docs are generated from these; a blank one ships
      ;; an unusable capability the model cannot discover. A shim reaches Python
      ;; either as an importable module (`:shim/imports`) or as prebound globals
      ;; (`:shim/bindings`, how the `attach` shim publishes `attach` & friends) —
      ;; declaring NEITHER makes it unreachable.
      (doseq [{:shim/keys [name description imports bindings]} (registered-shims)]
        (expect (not (str/blank? name)))
        (expect (not (str/blank? description)) (str name " has no :shim/description"))
        (expect (or (seq imports) (some? bindings))
                (str name " declares neither :shim/imports nor :shim/bindings"))))
  ;; A description is WRITTEN over several lines — `(str "…" "…")` — so a human
  ;; edits ONE sentence instead of reflowing a 700-character literal. Its VALUE
  ;; stays a single line: it is printed as ONE bullet of the system prompt's
  ;; sandbox-shims block and as one `doc()` / `apropos` gist, so a raw multi-line
  ;; literal would break the bullet apart and smuggle the source file's own
  ;; indentation into the model's context.
  (it "keeps every shim description a single line"
      (doseq [{:shim/keys [name description]} (registered-shims)]
        (expect (not (str/includes? (str description) "\n"))
                (str name " :shim/description spans lines — write it as (str \"…\" \"…\")"))))
  ;; The description is PUSHED into the system prompt of every request, whether or
  ;; not the session ever imports the shim; `:shim/docs` is PULLED by `doc(name)`
  ;; and costs nothing. So the budget is a ratchet: a line that wants to teach a
  ;; query language, every fixture name or a server API has outgrown the prompt
  ;; and belongs in `:shim/docs`, with the description pointing at it.
  (it "keeps every pushed shim description under the prompt budget"
      (doseq [{:shim/keys [name description]} (registered-shims)]
        (expect (<= (count (str description)) 340)
                (str name
                     " :shim/description is "
                     (count (str description))
                     " chars — move the detail to :shim/docs")))))

(defdescribe shim-resource-test
             (it "has a shim declaring every resources/vis-shims/*.py"
                 ;; The reverse direction: an orphan .py is either dead weight in the native
                 ;; image or a shim someone forgot to register.
                 (let
                   [declared
                    (set (map :shim/source (registered-shims)))

                    on-disk
                    (->> (.listFiles (io/file shim-resource-dir))
                         (map (fn [^File f]
                                (str "vis-shims/" (.getName f))))
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
  (it
    "answers `doc(name)` with :shim/docs, not the pushed prompt line"
    (let
      [shim
       (->> (registered-shims)
            (filter :shim/docs)
            first)

       ^Context ctx
       (:python-context (ep/create-python-context {}))

       name
       (first (concat (:shim/imports shim) (:shim/globals shim)))

       doc
       (:stdout (ep/run-python-block ctx (str "print(__vis_docs__[" (pr-str name) "])") "t1/i1"))]

      (expect (some? shim) "no shim declares :shim/docs — the pull path is untested")
      (expect (str/includes? doc (subs (:shim/docs shim) 0 60))
              (str name " doc() does not serve :shim/docs"))
      (expect (not (str/includes? doc (:shim/description shim)))
              (str name " doc() still serves the pushed description")))))

(defdescribe
  shim-globals-name-their-call-test
  "A prebound global is typed straight into a block — `ls(...)`, `nippy_decode(...)` — with
   no import and no signature to inspect first, so its page is the only place its arguments
   are ever stated. Every one of them shows its own call form; the two nippy globals were
   pages of prose that named neither an argument nor a result."
  (it "documents every shim global with its own call form"
      (let
        [names
         (->> (registered-shims)
              (mapcat :shim/globals)
              (filter string?)
              distinct
              vec)

         ^Context ctx
         (:python-context (ep/create-python-context {}))

         code
         (str "_names = ["
              (str/join ", " (map pr-str names))
              "]\n"
              "_docs = globals().get('__vis_docs__', {})\n"
              "print([n for n in _names if (n + '(') not in str(_docs.get(n, ''))])")

         undocumented
         (try (:stdout (ep/run-python-block ctx code "t1/i1")) (finally (.close ctx)))]

        ;; Guards the guard: an empty name list would make the check vacuous.
        (expect (< 5 (count names)))
        (expect (= "[]" (str/trim undocumented))
                (str "shim globals whose page never shows a call: " undocumented)))))
