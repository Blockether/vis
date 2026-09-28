(ns com.blockether.vis.internal.inference.runtime-test
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [com.blockether.vis.internal.inference.runtime :as runtime]
            [lazytest.core :refer [defdescribe expect it]])
  (:import [java.io ByteArrayInputStream File]
           [java.nio.file Files]
           [java.nio.file.attribute FileAttribute]))

(defn- temporary-dir
  []
  (.toFile (Files/createTempDirectory "vis-shared-onnx-test-" (make-array FileAttribute 0))))

(defn- remove-dir!
  [^File dir]
  (doseq [^File file (.listFiles dir)]
    (.delete file))
  (.delete dir))

(defdescribe
  shared-runtime-test
  (it
    "stages one complete Java ONNX Runtime during concurrent start-up"
    (let [^File dir
          (temporary-dir)

          old-path
          (System/getProperty runtime/ort-native-path-property)

          [library jni _]
          (runtime/library-names)]

      (try (System/clearProperty runtime/ort-native-path-property)
           (with-redefs [runtime/native-dir (constantly (str dir))]
             (let [loads (doall (pmap (fn [_]
                                        (runtime/ensure-ort!))
                                      (range 8)))
                   second-load (runtime/ensure-ort!)]

               (expect (= 8 (count loads)))
               (expect (= 1 (count (filter #(= :embedded (:source %)) loads))))
               (expect (= #{:embedded :property} (set (map :source loads))))
               (expect (= :property (:source second-load)))
               (expect (every? #(= (.getAbsolutePath dir) (:dir %)) loads))
               (expect (= (.getAbsolutePath dir) (:dir second-load)))
               (expect (= (.getAbsolutePath dir)
                          (System/getProperty runtime/ort-native-path-property)))
               (expect (= #{library jni} (set (map #(.getName ^File %) (.listFiles dir)))))
               (doseq [name [library jni]]
                 (expect (runtime/installed? (str dir) name))
                 (expect (= (.length (io/file dir name))
                            (with-open [in (io/input-stream (io/resource (str
                                                                           "ai/onnxruntime/native/"
                                                                           (runtime/platform-token)
                                                                           "/" name)))]
                              (count (.readAllBytes in))))))))
           (finally (if old-path
                      (System/setProperty runtime/ort-native-path-property old-path)
                      (System/clearProperty runtime/ort-native-path-property))
                    (remove-dir! dir)))))
  (it "refuses an empty library without leaving a loadable partial file"
      (let [^File dir (temporary-dir)]
        (try (expect (= :inference/native-incomplete
                        (try (runtime/install-stream! (str dir)
                                                      "libonnxruntime.so"
                                                      (ByteArrayInputStream. (byte-array 0)))
                             nil
                             (catch clojure.lang.ExceptionInfo e (:type (ex-data e))))))
             (expect (not (runtime/installed? (str dir) "libonnxruntime.so")))
             (expect (empty? (seq (.listFiles dir))))
             (finally (remove-dir! dir)))))
  (it "does not reuse an incompatible pre-fix Linux native cache"
      (doseq [token ["linux-x64" "linux-aarch64"]]
        (expect (str/includes? (runtime/default-native-dir token)
                               (str "sherpa-" runtime/sherpa-version "-abi2"))))
      (expect (not (str/includes? (runtime/default-native-dir "osx-aarch64") "-abi2"))))
  (it "preserves upstream loading on platforms without a shared native release"
      (let [old-path (System/getProperty runtime/ort-native-path-property)]
        (with-redefs [runtime/platform-token (constantly "osx-x64")]
          (expect (= {:source :upstream :platform "osx-x64"} (runtime/ensure-ort!)))
          (expect (= old-path (System/getProperty runtime/ort-native-path-property)))))))
