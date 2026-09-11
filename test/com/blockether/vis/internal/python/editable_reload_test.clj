(ns com.blockether.vis.internal.python.editable-reload-test
  "Editable project imports must not evict host-managed extension contexts."
  (:require [clojure.java.io :as io]
            [com.blockether.vis.internal.extension.core :as extension]
            [com.blockether.vis.internal.python.extensions :as pyx]
            [com.blockether.vis.internal.python.extensions-test :as fixtures]
            [lazytest.experimental.interfaces.clojure-test :refer [deftest is testing]]))

(defn- extension-source
  [name]
  (str "import sys\nimport blockether.vis.extension as vis\n"
       "from issue196_package import VALUE\n"
       "original_context = sys.modules[__name__]\n"
       "assert callable(__vis_registration__)\n"
       "def inspect_context():\n"
       "    'Inspect the editable extension context.'\n"
       "    return [VALUE, __file__, sys.modules[__name__] is original_context, "
       "callable(__vis_registration__), callable(__vis_host_live__)]\n"
       "vis.register(vis.Extension(name="
       (pr-str name)
       ", alias="
       (pr-str name)
       ", description='Editable context fixture', symbols=["
       "vis.Symbol(inspect_context, tag='observation', "
       "activity=vis.Activity(label='Inspect editable context', show_start=False))]))\n"))

(deftest editable-project-reload-preserves-extension-contexts
  ;; #196: direct_url.json includes the extension entry in the editable project root.
  ;; The loader binds the host, refreshes imports, executes source and reads registration
  ;; in separate worker calls. All calls must see the same module, including older contexts.
  (#'fixtures/with-shared-packages
   (fn [packages]
     (let [workspace
           (doto (io/file packages "editable-project") .mkdirs)

           entries
           (doto (io/file workspace ".vis/extensions") .mkdirs)

           source
           (io/file workspace "src")

           implementation
           (#'fixtures/write-ext! source "issue196_package/__init__.py" "VALUE = 'v1'\n")

           opts
           {:dirs [(str entries)]}

           names
           ["editable-first" "editable-second"]

           check!
           (fn [name value]
             (let [registered
                   (#'fixtures/registered name)

                   invoke
                   (#'fixtures/symbol-fn registered 'inspect_context)]

               (is (some? invoke))
               (when invoke
                 (let [result (invoke)]
                   (is (extension/envelope-success? result) (pr-str result))
                   (is (= [value (.getCanonicalPath (io/file entries (str name ".py"))) true true
                           true]
                          (:result result))
                       (pr-str result))))))

           reload!
           (fn [expected]
             (let [result (pyx/reload-python-extensions! opts)]
               (is (= expected (:loaded result)) (pr-str (pyx/load-failures)))
               (is (zero? (:failed result)) (pr-str (pyx/load-failures)))))]

       (#'fixtures/write-ext! packages "issue196.pth" (str (.getCanonicalPath source) "\n"))
       (#'fixtures/write-ext!
        packages
        "issue196-0.0.1.dist-info/direct_url.json"
        (str "{\"dir_info\":{\"editable\":true},\"url\":"
             (pr-str (str (.toURI (.getCanonicalFile workspace))))
             "}"))
       (try (#'fixtures/write-ext! entries "editable-first.py" (extension-source (first names)))
            (reload! 1)
            (check! (first names) "v1")
            (testing "loading another extension preserves the first managed context"
              (#'fixtures/write-ext! entries "editable-second.py" (extension-source (second names)))
              (let [result (pyx/load-python-extensions! opts)]
                (is (= 2 (:loaded result)) (pr-str (pyx/load-failures)))
                (is (zero? (:failed result)) (pr-str (pyx/load-failures))))
              (doseq [name names]
                (check! name "v1")))
            (testing "repeated reload preserves globals and reads same-size, same-time edits"
              (doseq [value ["v2" "v3"]]
                (let [mtime (.lastModified implementation)]
                  (spit implementation (str "VALUE = '" value "'\n"))
                  (is (.setLastModified implementation mtime)))
                (reload! 2)
                (doseq [name names]
                  (check! name value)))
              (reload! 2)
              (doseq [name names]
                (check! name "v3")))
            (finally (pyx/reload-python-extensions! {:dirs []})))))))
