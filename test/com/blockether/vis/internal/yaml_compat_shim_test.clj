(ns com.blockether.vis.internal.yaml-compat-shim-test
  "The YAML-compat shim installed into every sandbox context: a PyYAML-compatible
   `yaml` module whose load/dump DELEGATE to the pure-Clojure YAMLStar loader
   (`org.yamlstar/yamlstar`). Published into `sys.modules` (so `import yaml`
   works) AND stapled onto builtins (so `yaml.safe_load(...)` needs no import).
   Results come back as REAL python dict/list (not host proxies); a malformed
   document surfaces as a catchable `yaml.YAMLError`."
  (:require [com.blockether.vis.internal.env-python :as ep]
            [lazytest.core :refer [defdescribe expect it]])
  (:import [org.graalvm.polyglot Context]))

(defn- ev [^Context c code] (ep/->clj (.eval c "python" code)))

(defdescribe
  yaml-load-test
  (it "parses nested mappings/sequences/scalars into REAL python data"
      (let [{:keys [^Context python-context]} (ep/create-python-context {})]
        (.eval python-context "python" "import yaml")
        (expect (= {"a" 1 "b" ["x" "y"] "c" true "nested" {"k" "v"}}
                   (ev python-context
                       "yaml.safe_load('{a: 1, b: [x, y], c: true, nested: {k: v}}')")))
        ;; the result is a genuine dict — isinstance / {**_} / json.dumps all work,
        ;; not an opaque host proxy.
        (expect (true? (ev python-context "isinstance(yaml.safe_load('a: 1'), dict)")))))
  (it "autoloads on builtins so `yaml.safe_load` works with NO import"
      (let [{:keys [^Context python-context]} (ep/create-python-context {})]
        ;; deliberately NO `import yaml` first
        (expect (= {"foo" "bar"} (ev python-context "yaml.safe_load('foo: bar')")))))
  (it "empty input yields None (PyYAML parity)"
      (let [{:keys [^Context python-context]} (ep/create-python-context {})]
        (expect (nil? (ev python-context "yaml.safe_load('')")))))
  (it "accepts a PyYAML Loader= kwarg for signature compatibility"
      (let [{:keys [^Context python-context]} (ep/create-python-context {})]
        (expect (= {"k" 42} (ev python-context "yaml.load('k: 42', Loader=yaml.SafeLoader)")))))
  (it "safe_load_all returns every document in a multi-doc stream"
      (let [{:keys [^Context python-context]} (ep/create-python-context {})]
        (expect (= [{"x" 1} {"y" 2}]
                   (ev python-context "list(yaml.safe_load_all('''x: 1\n---\ny: 2'''))"))))))

(defdescribe
  yaml-dump-test
  (it "dump serializes JSON-compatible data to a YAML string"
      (let [{:keys [^Context python-context]} (ep/create-python-context {})]
        (expect (= "a: 1\nb:\n- x\n- y\nc: true\n"
                   (ev python-context "yaml.dump({'a': 1, 'b': ['x', 'y'], 'c': True})")))))
  (it "dump_all serializes a sequence of documents"
      (let [{:keys [^Context python-context]} (ep/create-python-context {})]
        (expect (= "---\na: 1\n---\nb: 2\n"
                   (ev python-context "yaml.dump_all([{'a': 1}, {'b': 2}])")))))
  (it
    "dump writes to a file-like stream and returns None"
    (let [{:keys [^Context python-context]} (ep/create-python-context {})]
      (expect
        (=
          "z: 9\n"
          (ev
            python-context
            "(lambda s: (yaml.dump({'z': 9}, s), s.getvalue())[1])(__import__('io').StringIO())"))))))

(defdescribe
  yaml-error-test
  (it
    "surfaces a malformed document as a catchable yaml.YAMLError"
    (let [{:keys [^Context python-context]} (ep/create-python-context {})]
      (expect
        (=
          "YAMLError"
          (ev
            python-context
            "\ntry:\n    yaml.safe_load('foo: [1, 2')\n    _r = 'NO-ERROR'\nexcept yaml.YAMLError:\n    _r = 'YAMLError'\n_r")))))
  (it "publishes the module under sys.modules so `import yaml` resolves"
      (let [{:keys [^Context python-context]} (ep/create-python-context {})]
        (expect (true? (ev python-context
                           "import yaml\n__import__('sys').modules.get('yaml') is not None"))))))
