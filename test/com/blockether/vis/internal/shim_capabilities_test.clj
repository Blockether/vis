(ns com.blockether.vis.internal.shim-capabilities-test
  "Harvest what every sandbox shim lends, and pin the generated resource.

   A shim documents itself in Python: the module (or prebound global) its own
   source builds carries the prose in `__doc__`, and every public member it lends
   carries its signature and its own first line. Nothing about a shim is written
   twice in Clojure.

   Reading that costs an IMPORT — materializing pandas/numpy/PIL inside a GraalPy
   context is exactly the heap lazy shims exist to defer — so the harvest runs
   HERE, once, into `resources/vis-shims/capabilities.edn`, and production only
   reads the resource (`shim-capabilities`). Change what a shim lends and this
   test fails until `regenerate!` runs:

     (require (quote com.blockether.vis.internal.shim-capabilities-test) :reload)
     (com.blockether.vis.internal.shim-capabilities-test/regenerate!)"
  (:require [charred.api :as json]
            [clojure.string :as str]
            [com.blockether.vis.internal.env-python :as ep]
            [com.blockether.vis.internal.extension :as extension]
            [com.blockether.vis.internal.shim-capabilities :as caps]
            [lazytest.core :refer [defdescribe expect it]])
  (:import [org.graalvm.polyglot Context]))

(def ^:private resource-file
  "The generated resource, written by `regenerate!` and read by production."
  "resources/vis-shims/capabilities.edn")

(def ^:private builtin-nses
  (delay @(requiring-resolve 'com.blockether.vis.internal.extension/builtin-extension-nses)))

(defn- registered-shims
  "Load every builtin extension ns, then read back the registered shims."
  []
  (run! require @builtin-nses)
  (extension/sandbox-shims))

(def ^:private harvest-python
  "The harvester, run INSIDE a throwaway sandbox context: import each shim, keep
   the members it owns (a stdlib name the module re-exports is not a capability),
   and answer one JSON string."
  "import builtins, inspect, json, types


def _own_doc(obj):
    '''The object's OWN docstring, never one inherited from a base class.'''
    if isinstance(obj, type):
        d = obj.__dict__.get('__doc__')
    else:
        d = getattr(obj, '__doc__', None)
    return d if isinstance(d, str) else ''


def _first_line(name, obj):
    d = _own_doc(obj)
    for line in d.splitlines():
        line = line.strip()
        if not line:
            continue
        # A dataclass writes its own signature as the docstring - that is the
        # signature we already print, not prose.
        if line.startswith(name + '('):
            return ''
        return line
    return ''


def _owned(mod, obj):
    '''True when `obj` belongs to this shim's surface, not a stdlib leak.'''
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
    '''Import every shim module, read what it lends, and answer one JSON string.'''
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

(defn- py-list
  "A Python list literal of strings — Clojure's own printing is valid Python here."
  [xs]
  (str "[" (str/join ", " (map pr-str xs)) "]"))

(defn- member
  [m]
  (cond-> (sorted-map :kind (get m "kind") :name (get m "name") :sig (get m "sig"))
    (not (str/blank? (get m "doc")))
    (assoc :doc (get m "doc"))))

(defn- entry
  [m]
  (cond-> (sorted-map :doc (str (get m "doc")) :kind (get m "kind") :sig (get m "sig"))
    (get m "error")
    (assoc :error (get m "error"))

    (seq (get m "names"))
    (assoc :names (mapv member (get m "names")))))

(defn harvest!
  "Import every shim import and global in ONE throwaway context and read back
   `{name {:kind :sig :doc :names}}`. Slow by nature — it materializes every
   shim's live object graph, which is the only honest way to learn what it lends."
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

    (into (sorted-map)
          (map (fn [[k v]]
                 [k (entry v)]))
          (json/read-json (str/trim (str out)) :key-fn identity))))

(defn- edn-text
  [m]
  (str ";; GENERATED by shim-capabilities-test/regenerate! — never edited by hand.\n"
       ";; Every shim's prose lives in the __doc__ of the Python it installs.\n"
       "{"
       (str/join "\n\n"
                 (map (fn [[k v]]
                        (str (pr-str k) " " (pr-str v)))
                      m))
       "}\n"))

(defn regenerate!
  "Re-harvest and write `resources/vis-shims/capabilities.edn`. Call from a REPL
   after changing what a shim installs; answers the entry count."
  []
  (let [m (harvest!)]
    (spit resource-file (edn-text m))
    (count m)))

(defdescribe
  shim-capabilities-test
  "The generated capability index is the ONLY description of a shim, so it has to
   cover every reachable name and stay equal to what the sandbox really holds."
  (it "covers every shim import and global"
      ;; A name the index misses is a capability `doc()` and `apropos` cannot answer.
      (let [want
            (->> (registered-shims)
                 (mapcat #(concat (:shim/imports %) (:shim/globals %)))
                 (filter string?)
                 set)

            have
            (set (keys (caps/capabilities)))

            missing
            (sort (remove have want))]

        (expect (< 20 (count want)))
        (expect (empty? missing)
                (str "absent from capabilities.edn — run regenerate!: " (pr-str missing)))))
  (it "documents every name in Python, not in Clojure"
      ;; This replaces the old `:shim/description` non-blank rule: a module whose
      ;; own __doc__ is empty ships a capability nothing can describe.
      (let [blank (sort (map key (filter #(str/blank? (:doc (val %))) (caps/capabilities))))]
        (expect (empty? blank) (str "no __doc__ in their Python: " (pr-str blank)))))
  (it "lends members under every importable module"
      ;; A module that harvests zero members is a failed import, not a small library.
      (let [empty-mods (sort (map key
                                  (filter #(and (= "module" (:kind (val %)))
                                                (empty? (:names (val %))))
                                          (caps/capabilities))))]
        (expect (empty? empty-mods) (str "modules lending nothing: " (pr-str empty-mods)))))
  (it "lends every member name to the ranker as a search alias"
      ;; Cycle 2: a reader looks up `read_csv`, never `pandas`. The page is filed
      ;; under the module name, so its members reach it as ALIASES, not as text.
      (let [members (set (caps/member-names "pandas"))]
        (expect (contains? members "read_csv"))
        (expect (contains? members "DataFrame"))
        (expect (empty? (caps/member-names "never-harvested")))))
  (it "matches a live harvest"
      ;; The pin. Changing what a shim installs without regenerating leaves the
      ;; model reading yesterday's surface.
      (let [live
            (harvest!)

            stored
            (caps/capabilities)

            drifted
            (sort (remove #(= (get live %) (get stored %))
                    (distinct (concat (keys live) (keys stored)))))]

        (expect (empty? drifted)
                (str "capabilities.edn is stale for "
                     (pr-str drifted)
                     " — run (shim-capabilities-test/regenerate!)")))))
