(ns com.blockether.vis.internal.apropos-resource-test
  "Harvest the Python shim surface into the static apropos resource.

   A shim documents itself in Python. Importing every shim is expensive, so this
   test performs that introspection once and pins the flat records consumed by
   `apropos` and `doc`. Regenerate after changing a shim surface:

     (require (quote com.blockether.vis.internal.apropos-resource-test) :reload)
     (com.blockether.vis.internal.apropos-resource-test/regenerate!)"
  (:require [charred.api :as json]
            [clojure.edn :as edn]
            [clojure.string :as str]
            [com.blockether.vis.internal.env-python :as ep]
            [com.blockether.vis.internal.extension :as extension]
            [com.blockether.vis.internal.manifest :as manifest]
            [lazytest.core :refer [defdescribe expect it]])
  (:import [org.graalvm.polyglot Context]))

(def ^:private resource-file "resources/META-INF/vis/apropos/shims.edn")

(defn- shim-initializers
  []
  (->> (:initialization (manifest/read-manifest))
       (filter #(str/starts-with? (namespace %) "com.blockether.vis.internal.foundation.shim-"))
       vec))

(defn- registered-shims
  "Invoke exactly the shim initializers named by the distribution manifest."
  []
  (doseq [initializer (shim-initializers)]
    ((requiring-resolve initializer)))
  (extension/sandbox-shims))

(def ^:private harvest-python
  "The harvester runs inside one throwaway sandbox context and answers JSON."
  "import builtins, inspect, json, types


def _own_doc(obj):
    if isinstance(obj, type):
        d = obj.__dict__.get('__doc__')
    else:
        d = getattr(obj, '__doc__', None)
    return d if isinstance(d, str) else ''


def _first_line(name, obj):
    for line in _own_doc(obj).splitlines():
        line = line.strip()
        if not line:
            continue
        if line.startswith(name + '('):
            return ''
        return line
    return ''


def _owned(mod, obj):
    if '__vis_install_' in str(getattr(obj, '__qualname__', '')):
        return True
    root = mod.__name__.split('.')[0]
    if isinstance(obj, types.ModuleType):
        return str(getattr(obj, '__name__', '')).split('.')[0] == root
    owner = getattr(obj, '__module__', None)
    if owner is None:
        return True
    return str(owner).split('.')[0] == root


def _kind(obj):
    if isinstance(obj, types.ModuleType):
        return 'module'
    if isinstance(obj, type):
        return 'class'
    if callable(obj):
        return 'function'
    return 'data'


def _sig(name, obj):
    if not callable(obj):
        return name
    try:
        return name + str(inspect.signature(obj))
    except Exception:
        return name + '(...)'


def _members(mod):
    out = []
    for name in sorted(dir(mod)):
        if name.startswith('_'):
            continue
        try:
            obj = getattr(mod, name)
        except Exception:
            continue
        if not _owned(mod, obj):
            continue
        kind = _kind(obj)
        row = {'name': name, 'kind': kind, 'sig': _sig(name, obj)}
        doc = '' if kind == 'data' else _first_line(name, obj)
        if doc:
            row['doc'] = doc
        out.append(row)
    return out


def __vis_harvest__(modules, names):
    out = {}
    for m in modules:
        try:
            mod = __import__(m)
            for part in m.split('.')[1:]:
                mod = getattr(mod, part)
        except Exception as exc:
            out[m] = {'error': '%s: %s' % (type(exc).__name__, exc)}
            continue
        out[m] = {'kind': 'module',
                  'sig': m,
                  'doc': inspect.cleandoc(_own_doc(mod)),
                  'names': _members(mod)}
    for n in names:
        obj = globals().get(n, None)
        if obj is None:
            obj = getattr(builtins, n, None)
        if obj is None:
            out[n] = {'error': 'not bound'}
            continue
        out[n] = {'kind': _kind(obj),
                  'sig': _sig(n, obj),
                  'doc': inspect.cleandoc(_own_doc(obj)),
                  'names': []}
    return json.dumps(out, sort_keys=True)")

(defn- py-list [xs] (str "[" (str/join ", " (map pr-str xs)) "]"))

(defn harvest!
  "Import every declared shim in one throwaway context."
  []
  (let [shims
        (registered-shims)

        modules
        (vec (distinct (filter string? (mapcat :shim/imports shims))))

        globals
        (vec (distinct (filter string? (mapcat :shim/globals shims))))

        ^Context ctx
        (:python-context (ep/create-python-context {}))

        code
        (str harvest-python
             "\nprint(__vis_harvest__("
             (py-list modules)
             ", "
             (py-list globals)
             "))")

        out
        (try (:stdout (ep/run-python-block ctx code)) (finally (.close ctx)))]

    (into (sorted-map) (json/read-json (str/trim (str out)) :key-fn identity))))

(def ^:private kind-labels
  [["class" "Classes"] ["function" "Functions"] ["module" "Modules"] ["data" "Data"]])

(defn- member-line
  [{:strs [sig doc]}]
  (let [d (str/trim (str doc))]
    (if (str/blank? d) sig (str sig " — " (if (> (count d) 90) (str (subs d 0 88) "…") d)))))

(defn- page
  [nm {doc "doc" names "names"}]
  (let [by-kind
        (group-by #(get % "kind") names)

        section
        (fn [[kind label]]
          (when-let [members (seq (get by-kind kind))]
            (str label ":\n" (str/join "\n" (map #(str "  " (member-line %)) members)))))

        body
        (keep section kind-labels)]

    (str/trim (str/join
                "\n\n"
                (cond-> [(str/trim (str doc))]
                  (seq body)
                  (into body)

                  (seq names)
                  (conj
                    (str "Read one member: doc(\"" nm "." (get (first names) "name") "\")")))))))

(defn- member-entry
  [root member]
  (let [member-name
        (str (get member "name"))

        kind
        (str (get member "kind"))

        text
        (str/trim (str (get member "doc")))]

    (when (and (seq member-name) (seq text))
      (cond-> (array-map :name (if (str/starts-with? member-name (str root "."))
                                 member-name
                                 (str root "." member-name))
                         :kind (or (#{"function" "class" "module"} kind) "function")
                         :text text)
        (#{"function" "class"} kind)
        (assoc :call (str (get member "sig")))))))

(defn- apropos-entries
  [harvest]
  (into []
        (mapcat (fn [[nm entry]]
                  (let [kind
                        (str (get entry "kind"))

                        root
                        (cond-> (array-map :name nm :kind kind :text (page nm entry))
                          (#{"function" "class"} kind)
                          (assoc :call (str (get entry "sig"))))]

                    (cons root (keep #(member-entry nm %) (get entry "names"))))))
        harvest))

(defn- stored-entries [] (edn/read-string (slurp resource-file)))

(defn- edn-text
  [entries]
  (str ";; GENERATED from sandbox Python docstrings; regenerate with apropos-resource-test.\n"
       "[\n"
       (str/join "\n" (map #(str " " (pr-str %)) entries))
       "]\n"))

(defn regenerate!
  "Re-harvest and write the flat shim apropos resource. Returns its record count."
  []
  (let [entries (apropos-entries (harvest!))]
    (spit resource-file (edn-text entries))
    (count entries)))

(defdescribe
  apropos-resource-test
  (it "lists every shim initializer explicitly in the manifest"
      (let [listed
            (set (map (comp symbol namespace) (shim-initializers)))

            expected
            (->> (.listFiles (java.io.File. "src/com/blockether/vis/internal/foundation"))
                 (keep (fn [^java.io.File file]
                         (when-let [[_ stem] (re-matches #"(shim_.*)\.clj" (.getName file))]
                           (symbol (str "com.blockether.vis.internal.foundation."
                                        (str/replace stem "_" "-"))))))
                 set)]

        (expect (< 10 (count expected)))
        (expect (= expected listed))))
  (it "declares the generated resource in the manifest"
      (expect (some #{"META-INF/vis/apropos/shims.edn"} (:apropos (manifest/read-manifest)))))
  (it "stores one unique flat record for every documented name"
      (let [entries
            (stored-entries)

            names
            (map :name entries)]

        (expect (< 20 (count (registered-shims))))
        (expect (< 500 (count entries)))
        (expect (= (count names) (count (distinct names))))
        (expect (every? #(and (string? (:name %))
                              (not (str/blank? (:name %)))
                              (string? (:kind %))
                              (not (str/blank? (:text %))))
                        entries))))
  (it "contains roots and dotted members as flat document records"
      (let [by-name (into {} (map (juxt :name identity)) (stored-entries))]
        (expect (contains? by-name "pandas"))
        (expect (contains? by-name "pandas.read_csv"))
        (expect (= "class" (:kind (get by-name "pandas.DataFrame"))))
        (expect (every? #(contains? #{#{:name :kind :text} #{:name :kind :text :call}}
                                    (set (keys %)))
                        (vals by-name)))))
  (it
    "matches one live harvest"
    (let [live-harvest
          (harvest!)

          failures
          (into {} (filter (comp #(contains? % "error") val)) live-harvest)

          missing-docs
          (for [[root entry]
                live-harvest

                :when (str/blank? (str (get entry "doc")))]

            root)

          bare-members
          (for [[root entry]
                live-harvest

                member
                (get entry "names")

                :when (and (not= "data" (get member "kind")) (str/blank? (str (get member "doc"))))]

            (str root "." (get member "name")))]

      (expect (empty? failures) (str "shim imports failed: " (pr-str failures)))
      (expect (empty? missing-docs) (str "shim roots without docstrings: " (pr-str missing-docs)))
      (expect (empty? bare-members) (str "shim members without docstrings: " (pr-str bare-members)))
      (expect (= (apropos-entries live-harvest) (stored-entries))
              "shims.edn is stale; run apropos-resource-test/regenerate!"))))
