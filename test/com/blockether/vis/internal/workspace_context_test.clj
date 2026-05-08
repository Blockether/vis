(ns com.blockether.vis.internal.workspace-context-test
  (:require [clojure.java.io :as io]
            [com.blockether.vis.internal.workspace-context :as wctx]
            [lazytest.core :refer [defdescribe expect it]]))

(defdescribe workspace-context-test
  (it "canonicalizes workspace roots and falls back to process cwd"
    (let [cwd (.getCanonicalPath (io/file (System/getProperty "user.dir")))]
      (expect (= cwd (.getCanonicalPath (wctx/cwd))))
      (expect (= cwd (wctx/workspace-root {:workspace/root "."})))
      (binding [wctx/*workspace-root* cwd]
        (expect (= cwd (.getCanonicalPath (wctx/cwd))))))))
