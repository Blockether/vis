(ns com.blockether.vis.native-image-resources-test
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [lazytest.core :refer [defdescribe expect it]])
  (:import [java.io PushbackReader]))

(defn- native-image-flags
  "The literal resource flags supplied to both native build entrypoints."
  []
  (with-open [reader (PushbackReader. (io/reader "build.clj"))]
    (loop []

      (let [form (read {:eof ::eof} reader)]
        (cond (= ::eof form) (throw (ex-info "native-image-args is missing" {}))
              (and (seq? form) (= 'defn- (first form)) (= 'native-image-args (second form)))
              (filter #(and (string? %) (str/starts-with? % "-H:")) (tree-seq coll? seq form))
              :else (recur))))))

(defdescribe
  native-image-resources-test
  (it
    "excludes ONNX debug symbols while keeping the platform JNI libraries"
    (let [flags
          (native-image-flags)

          exclude
          (some #(when (str/starts-with? % "-H:ExcludeResources=")
                   (subs % (count "-H:ExcludeResources=")))
                flags)]

      (expect (some #(str/starts-with? % "-H:IncludeResources=ai/onnxruntime/native/") flags))
      (expect (string? exclude))
      (when exclude
        (let [pattern (re-pattern exclude)]
          (expect
            (re-matches
              pattern
              "ai/onnxruntime/native/osx-aarch64/libonnxruntime.dylib.dSYM/Contents/Resources/DWARF/libonnxruntime.dylib"))
          (expect (re-matches
                    pattern
                    "ai/onnxruntime/native/osx-x64/libonnxruntime.dylib.dSYM/Contents/Info.plist"))
          (expect (not (re-matches pattern
                                   "ai/onnxruntime/native/osx-aarch64/libonnxruntime.dylib")))
          (expect (not (re-matches pattern
                                   "ai/onnxruntime/native/linux-x64/libonnxruntime.so"))))))))
