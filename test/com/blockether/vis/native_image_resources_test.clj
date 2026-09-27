(ns com.blockether.vis.native-image-resources-test
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [com.blockether.vis.internal.inference.runtime :as runtime]
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
          (some #(when (str/starts-with? % "-H:ExcludeResources=ai/onnxruntime/native/")
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
                                   "ai/onnxruntime/native/linux-x64/libonnxruntime.so")))))))
  (it
    "excludes only Sherpa's redundant runtime on shared release platforms"
    (let [exclusions
          (keep #(when (str/starts-with? % "-H:ExcludeResources=")
                   (subs % (count "-H:ExcludeResources=")))
                (native-image-flags))

          sherpa-pattern
          (some #(when (str/starts-with? % "sherpa-onnx/native/") (re-pattern %)) exclusions)]

      (expect sherpa-pattern)
      (when sherpa-pattern
        (doseq [token runtime/shared-platforms]
          (expect (re-matches sherpa-pattern
                              (str "sherpa-onnx/native/" token
                                   "/libonnxruntime."
                                   (if (str/starts-with? token "osx") "dylib" "so")))))
        (doseq [[token library] [["osx-x64" "libonnxruntime.dylib"] ["win-x64" "onnxruntime.dll"]]]
          (expect (not (re-matches sherpa-pattern (str "sherpa-onnx/native/" token "/" library)))))
        (expect (not (re-matches sherpa-pattern
                                 "sherpa-onnx/native/osx-aarch64/libsherpa-onnx-jni.dylib")))))))
