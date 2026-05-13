(ns com.blockether.vis.ext.lang-clojure.path-test
  (:require
   [babashka.fs :as fs]
   [com.blockether.vis.ext.lang-clojure.path :as path]
   [lazytest.core :refer [defdescribe expect it throws?]]))

(defdescribe path-guards-test
  (it "recognizes Clojure/EDN extensions case-insensitively"
    (expect (= "clj" (path/file-extension "src/demo/core.CLJ")))
    (expect (= "" (path/file-extension "deps")))
    (expect (nil? (path/ensure-clojure-file-ext! "src/demo/core.cljs")))
    (expect (throws? clojure.lang.ExceptionInfo
              #(path/ensure-clojure-file-ext! "README.md"))))

  (it "resolves safe paths and rejects workspace escapes"
    (let [f (fs/file "target/lang-clojure-path-test/core.clj")]
      (fs/create-dirs (fs/parent f))
      (spit f "(ns demo)\n")
      (expect (= "target/lang-clojure-path-test/core.clj"
                (path/rel-path (path/ensure-existing-file! (path/safe-path (str f))))))
      (expect (throws? clojure.lang.ExceptionInfo #(path/safe-path "../outside.clj"))))))
