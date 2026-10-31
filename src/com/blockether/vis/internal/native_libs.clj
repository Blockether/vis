;; User request 2026-10-31: find and fix slow internal calls of the native binary.
(ns com.blockether.vis.internal.native-libs
  "Stable on-disk copies of the native libraries inside the binary.

   A library inside a jar or a native image has no file path. Its loader copies it
   to a new temp file in each process. macOS checks every new dylib at its first
   `dlopen`: measured 0.8 s for sqlite-jdbc and 1.2 s for fff, against 35 ms for a
   path it has seen. So every native start and the first `ls` paid seconds.

   `stage!` copies each library once per container (the executable or the jar)
   into `~/.vis/native/<id>/<stamp>/`. It then sets the loader's own path
   property. An explicit environment variable or property stays authoritative."
  (:require [clojure.java.io :as io]
            [com.blockether.nativeresolver :as nr]
            [com.blockether.vis.internal.paths :as paths]
            [taoensso.telemere :as tel])
  (:import (java.io File)
           (java.net JarURLConnection URL)
           (java.nio.charset StandardCharsets)
           (java.security MessageDigest)))

(set! *warn-on-reflection* true)

(defn- file-stamp
  "Path, modification time and size of `f`, or nil when it is not a file."
  [^File f]
  (when (and f (.isFile f)) [(.getAbsolutePath f) (.lastModified f) (.length f)]))

(defn- container-stamp
  "Identity of the bytes behind `url`: its jar file, or the running executable.
   Nil when neither is known. One container holds one build of each library."
  [^URL url]
  (if (= "jar" (.getProtocol url))
    (let [^JarURLConnection conn (.openConnection url)]
      (file-stamp (io/file (.toURI (.getJarFileURL conn)))))
    (let [^java.lang.ProcessHandle$Info info (.info (java.lang.ProcessHandle/current))]
      (some-> (.orElse (.command info) nil)
              io/file
              file-stamp))))

(defn stamp-key
  "Short hex key of `url` and its container stamp."
  [url stamp]
  (let [digest (.digest (MessageDigest/getInstance "SHA-256")
                        (.getBytes (pr-str [(str url) stamp]) StandardCharsets/UTF_8))]
    (apply str (map #(format "%02x" (bit-and (long %) 0xff)) (take 8 digest)))))

(defn- root-dir ^File [id] (io/file (System/getProperty "user.home") ".vis" "native" (str id)))

(defn- install!
  "Copy `url` to `target` through a temp file in the same directory. A reader
   never sees a partial file, and a repeated or interrupted copy ends complete."
  [^URL url ^File target]
  (let [^File staging (File/createTempFile "vis-native-" ".tmp" (.getParentFile target))]
    (try (with-open [in (io/input-stream url)]
           (io/copy in staging))
         (when (zero? (.length staging))
           (throw (ex-info "A bundled native library is empty" {:url (str url)})))
         (when-not (or (.renameTo staging target) (pos? (.length target)))
           (throw (ex-info "Could not stage a native library" {:target (str target)})))
         (finally (when (.exists staging) (.delete staging))))))

(defn- delete-tree!
  [^File f]
  (doseq [^File x (reverse (file-seq f))]
    (.delete x)))

(defn- prune-free!
  "Delete sibling stamps of `keep` that no live process claims."
  [^File root ^File keep]
  (doseq [^File d
          (.listFiles root)

          :when (and (.isDirectory d) (not= keep d) (= :free (paths/claim-state d)))]

    (delete-tree! d)))

(defn stage!
  "Stage classpath `resource` as `file-name` for library `id` and answer its File.
   Answer nil when the resource is absent or already a plain file. Never throws."
  [id resource file-name]
  (try (when-let [^URL url (io/resource resource)]
         (when-not (= "file" (.getProtocol url))
           (when-let [stamp (container-stamp url)]
             (let [started (System/nanoTime)
                   root (root-dir id)
                   dir (io/file root (stamp-key url stamp))
                   target (io/file dir (str file-name))
                   fresh? (paths/with-claim-lock
                            (fn []
                              (.mkdirs dir)
                              (paths/claim-dir! dir)
                              (when-not (pos? (.length target)) (install! url target) true)))]

               (when fresh? (prune-free! root dir))
               (tel/log! {:level :debug
                          :id ::staged
                          :data {:id id
                                 :path (str target)
                                 :fresh? (boolean fresh?)
                                 :ms (/ (- (System/nanoTime) started) 1e6)}})
               target))))
       (catch Throwable e
         (tel/log! {:level :warn :id ::stage-failed :data {:id id :error (ex-message e)}})
         nil)))

(def ^:private resolver-libs
  "Libraries that read `<group>.<lib>.native.path` before they copy a bundled file."
  [{:lib "fff"} {:lib "imaging"} {:lib "rift" :lib-base "rift_ffi"}])

(defn- explicit-path?
  "True when the user already set the library's environment variable or property."
  [{:keys [env-prefix prop-prefix]}]
  (boolean (or (System/getenv (str env-prefix "_NATIVE_PATH"))
               (System/getProperty (str prop-prefix ".native.path")))))

(defn stage-resolver-libs!
  "Point fff, imaging and rift at stable copies before their first load. Never throws."
  []
  (try (doseq [spec
               (map nr/spec resolver-libs)

               :when (not (explicit-path? spec))
               :let [res
                     (nr/resource-name spec)

                     ^File f
                     (stage! (:lib spec) res (.getName (io/file res)))]
               :when f]

         (System/setProperty (str (:prop-prefix spec) ".native.path") (.getAbsolutePath f)))
       (catch Throwable e
         (tel/log!
           {:level :warn :id ::stage-failed :data {:id :resolver-libs :error (ex-message e)}}))))

(defn stage-sqlite!
  "Point sqlite-jdbc at a stable copy unless `org.sqlite.lib.path` is set.
   Call it before the first SQLite connection. Never throws."
  []
  (try (when-not (System/getProperty "org.sqlite.lib.path")
         (let [lib-name
               (org.sqlite.util.LibraryLoaderUtil/getNativeLibName)

               folder
               (subs (org.sqlite.util.LibraryLoaderUtil/getNativeLibResourcePath) 1)]

           (when-let [^File f (stage! "sqlite-jdbc" (str folder "/" lib-name) lib-name)]
             (System/setProperty "org.sqlite.lib.path" (.getParent f))
             (System/setProperty "org.sqlite.lib.name" (.getName f)))))
       (catch Throwable e
         (tel/log!
           {:level :warn :id ::stage-failed :data {:id :sqlite-jdbc :error (ex-message e)}}))))
