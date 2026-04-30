(ns com.blockether.vis.internal.extension.source-markers
  "Resolve an extension's namespaces to their on-disk sources and
   compute deterministic markers — paths, max(mtime), SHA-256 over
   concatenated content (entries sorted by path) — used to detect
   whether the extension's source has changed since last load.

   Two source-resolution paths depending on classpath origin (plan
   §5.5):

     File-classpath sources (local-dev `:paths`)
       `(.getResource cl ns-path)` → `file:` URL → `java.io.File` →
       `.lastModified` for mtime, SHA-256 over file content for hash.

     Jar-classpath sources (packaged extensions)
       `(.getResource cl ns-path)` → `jar:file:` URL →
       `JarFile` + `JarEntry` → entry mtime (which is the jar build
       time, identical for every entry — acceptable since jars don't
       change at runtime), SHA-256 over the entry's `InputStream`
       for hash.

     Mixed (one ext spans multiple ns: file paths AND jars)
       max-mtime across all entries; SHA-256 over concatenated
       content sorted by entry name.

   Public surface:

     (resolve-markers ns-syms)
       Returns
         {:source-paths      [\"...\"]              ;; sorted entries, locator strings
          :source-mtime-max  1714403520000          ;; epoch-ms; -1 when nothing resolved
          :source-hash-sha256 \"abc...\"}            ;; hex; nil when nothing resolved

   v1 behavior:
     - Missing source files: skipped (Telemere :warn), excluded from
       the aggregate. Better than throwing — a partially-classpathed
       extension still gets *some* markers.
     - Read errors on individual files: same — skip + warn.
     - Returns nil-ish markers (`:source-paths []`,
       `:source-mtime-max -1`, `:source-hash-sha256 nil`) only when
       NO source resolves at all.

   No I/O caching here; callers are expected to compute markers
   once per `register-extension!` or `reload-extensions!` invocation
   and reuse the cached map."
  (:require
   [clojure.string :as string]
   [taoensso.telemere :as tel])
  (:import
   (java.io ByteArrayOutputStream InputStream)
   (java.net URL)
   (java.security MessageDigest)
   (java.util.jar JarEntry JarFile)))

(set! *warn-on-reflection* true)

;; ---------------------------------------------------------------------------
;; Hash + mtime primitives.
;; ---------------------------------------------------------------------------

(defn- sha256-digest ^MessageDigest []
  (MessageDigest/getInstance "SHA-256"))

(defn- bytes->hex ^String [^bytes b]
  (let [sb (StringBuilder. (* 2 (alength b)))]
    (dotimes [i (alength b)]
      (let [v (bit-and (aget b i) 0xff)]
        (when (< v 16) (.append sb \0))
        (.append sb (Integer/toString v 16))))
    (.toString sb)))

(defn- read-stream-bytes ^bytes [^InputStream in]
  (with-open [out (ByteArrayOutputStream.)]
    (let [buf (byte-array 8192)]
      (loop []
        (let [n (.read in buf)]
          (when (pos? n)
            (.write out buf 0 n)
            (recur)))))
    (.toByteArray out)))

;; ---------------------------------------------------------------------------
;; Resolve one namespace → entry map.
;; ---------------------------------------------------------------------------

(defn- ns->resource-path
  "Convert `clojure.core` to `clojure/core.clj`. Tries .clj first;
   .cljc / .cljs fallback if the .clj resolves to nothing."
  [ns-sym]
  (let [base (-> (str ns-sym)
               (string/replace \- \_)
               (string/replace \. \/))]
    [(str base ".clj") (str base ".cljc")]))

(defn- find-source-resource
  ^URL [^ClassLoader cl ns-sym]
  ;; Locate a resource URL for the namespace, trying .clj before .cljc.
  ;; Returns nil when nothing on the classpath corresponds.
  (let [paths (ns->resource-path ns-sym)]
    (some (fn [p] (.getResource cl ^String p)) paths)))

(defrecord ^:private SourceEntry [^String locator ^long mtime ^bytes content])

(defn- file-entry
  "Build a SourceEntry for a `file:` URL. Reads the file content for
   hashing; mtime from `.lastModified`."
  ^SourceEntry [^URL url]
  (let [f       (java.io.File. (.toURI url))
        path    (.getAbsolutePath f)
        mtime   (.lastModified f)
        content (try (read-stream-bytes (java.io.FileInputStream. f))
                  (catch Throwable t
                    (tel/log! {:level :warn :id ::file-read-failed
                               :data  {:path path :error (ex-message t)}})
                    (byte-array 0)))]
    (->SourceEntry path mtime content)))

(defn- jar-entry-locator
  "Build a stable locator string for a jar entry: `jar-path!entry-path`.
   Same convention as the JDK's `jar:` URL form, more readable."
  [^String jar-path ^String entry-name]
  (str jar-path "!" entry-name))

(defn- jar-entry
  "Build a SourceEntry for a `jar:file:` URL. Opens the jar, reads
   the named entry, hashes its content. mtime is the entry's
   `getTime` (= jar build time for entries that weren't individually
   timestamped). Closes the jar on exit."
  ^SourceEntry [^URL url]
  (let [conn      (.openConnection url)
        ;; The cast is paranoid — `.getJarFileURL` lives on `JarURLConnection`,
        ;; we know URL was a jar: URL when we got here.
        jconn     ^java.net.JarURLConnection conn
        jar-url   (.getJarFileURL jconn)
        jar-file  (java.io.File. (.toURI jar-url))
        jar-path  (.getAbsolutePath jar-file)
        entry-nm  (.getEntryName jconn)]
    (with-open [jar (JarFile. jar-file)]
      (let [^JarEntry e (.getJarEntry jar entry-nm)]
        (if (nil? e)
          (do (tel/log! {:level :warn :id ::jar-entry-missing
                         :data  {:jar jar-path :entry entry-nm}})
            nil)
          (let [mtime   (.getTime e)
                content (try (with-open [in (.getInputStream jar e)]
                               (read-stream-bytes in))
                          (catch Throwable t
                            (tel/log! {:level :warn :id ::jar-entry-read-failed
                                       :data  {:jar jar-path :entry entry-nm
                                               :error (ex-message t)}})
                            (byte-array 0)))]
            (->SourceEntry (jar-entry-locator jar-path entry-nm) mtime content)))))))

(defn- url->entry
  "Dispatch on URL protocol to the right reader. Returns SourceEntry
   or nil on unrecognized protocol."
  [^URL url]
  (try
    (case (some-> url .getProtocol string/lower-case)
      "file" (file-entry url)
      "jar"  (jar-entry url)
      (do (tel/log! {:level :warn :id ::unsupported-protocol
                     :data  {:protocol (.getProtocol url) :url (str url)}})
        nil))
    (catch Throwable t
      (tel/log! {:level :warn :id ::resolve-failed
                 :data  {:url (str url) :error (ex-message t)}})
      nil)))

;; ---------------------------------------------------------------------------
;; Public API.
;; ---------------------------------------------------------------------------

(defn resolve-markers
  "Resolve every namespace in `ns-syms` to its source on the classpath
   and compute aggregate markers.

   Returns
     {:source-paths      [\"...\"]               ;; sorted entry locators
      :source-mtime-max  long                    ;; -1 if nothing resolved
      :source-hash-sha256 \"hex\"}                ;; nil if nothing resolved

   Always returns a map (never throws). Per-namespace failures are
   logged at :warn and skipped; an extension whose nses partially
   resolve still gets markers from the parts that did."
  [ns-syms]
  (let [cl       (.getContextClassLoader (Thread/currentThread))
        urls     (->> ns-syms
                   (map #(find-source-resource cl %))
                   (remove nil?))
        entries  (->> urls
                   (map url->entry)
                   (remove nil?)
                   (sort-by :locator)
                   vec)]
    (if (empty? entries)
      {:source-paths       []
       :source-mtime-max   -1
       :source-hash-sha256 nil}
      (let [paths       (mapv :locator entries)
            mtime-max   (long (reduce max 0 (map :mtime entries)))
            digest      (sha256-digest)
            _           (doseq [^SourceEntry e entries]
                          (let [^bytes c (:content e)]
                            (.update digest c 0 (alength c))))
            hash-bytes  (.digest digest)
            hash-hex    (bytes->hex hash-bytes)]
        {:source-paths       paths
         :source-mtime-max   mtime-max
         :source-hash-sha256 hash-hex}))))

(defn resolve-markers-for-extension
  "Convenience wrapper: pull `:nses` (or `:ext/nses`) off the
   extension map / manifest entry, fall back to `:ext/namespace`
   when no list is provided. Returns the same shape as
   [[resolve-markers]]."
  [ext-or-manifest]
  (let [ns-syms (or (some-> (:nses ext-or-manifest) seq vec)
                  (some-> (:ext/nses ext-or-manifest) seq vec)
                  (when-let [n (:ext/namespace ext-or-manifest)]
                    [n]))]
    (resolve-markers (or ns-syms []))))
