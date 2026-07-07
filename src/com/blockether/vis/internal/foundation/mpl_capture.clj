(ns com.blockether.vis.internal.foundation.mpl-capture
  "Capture the image BYTES behind a `vis-image` fence so the engine can OWN
   them (a `session_iteration_attachment` row) instead of only keeping the
   path.

   The matplotlib shim's `_emit_image` renders each `plt.show()` /
   `plt.savefig()` figure to a HOST temp file under `$TMPDIR/vis-mpl` and
   prints a `vis-image` fence carrying just that PATH — great for painting the
   picture inline in the live TUI/web, useless after a restart because the
   bytes never entered the transcript. At persist time the loop hands each tool
   call's stdout here; we re-read the CONFINED file and return inline
   attachment maps ready for `db-store-iteration!`'s `:attachments`.

   Deliberately dependency-free (no AWT / no `vis.core`): the render side lives
   in `shim-matplotlib`, but capture must be safe to require from the hot
   engine loop without dragging the Java2D toolkit or risking a require cycle."
  (:require [clojure.string :as str])
  (:import [java.io File]
           [java.nio.file Files]
           [java.util Base64]))

(def ^:private mpl-temp-dir
  "The `$TMPDIR/vis-mpl` directory `__vis_mpl_render_file__` writes figures
   into. The confinement guard below only reads files that canonicalize to a
   DIRECT child of this dir, so a crafted `vis-image` fence can never make the
   host read an arbitrary path."
  (delay (.getCanonicalFile (File. (System/getProperty "java.io.tmpdir") "vis-mpl"))))

(defn- confined-mpl-file
  "`path` as a `File` IFF it canonicalizes to a regular file directly inside
   `mpl-temp-dir`, else nil — a path-traversal / foreign-path guard (mirrors
   the web channel's `mpl-confined-file`)."
  ^File [path]
  (try (let [f (.getCanonicalFile (File. (str path)))]
         (when (and (.isFile f)
                    (= @mpl-temp-dir
                       (some-> (.getParentFile f)
                               .getCanonicalFile)))
           f))
       (catch Throwable _ nil)))

(def ^:private vis-image-fence-pattern
  "Matches one whole `vis-image` fence; group 1 is the body (header lines +
   optional ASCII fallback). Same shape the web channel parses."
  #"(?s)````vis-image\r?\n(.*?)\r?\n````")

(defn parse-image-fences
  "Extract every `vis-image` fence from a `python_execution` `stdout` string as
   descriptor maps `{:summary :path :media-type :dims :size-label}` (the ASCII
   fallback dropped). Header layout mirrors what `_emit_image` prints:
   summary / abs-path / mime / WxH / size-label. Empty seq when no fence — the
   cheap common case is a single `includes?` scan."
  [stdout]
  (let [s (str stdout)]
    (if-not (str/includes? s "````vis-image")
      []
      (for [[_ body] (re-seq vis-image-fence-pattern s)
            :let [[summary path mime dims size-label] (str/split (str body) #"\n" -1)]
            :when (and (not (str/blank? (str path))) (not (str/blank? (str mime))))]

        {:summary summary :path path :media-type mime :dims dims :size-label size-label}))))

(defn collect-stdout-images
  "Turn every `vis-image` fence a tool call PRINTED to `stdout` into an inline
   attachment map by reading the confined figure file's bytes:
   `{:kind \"image\" :media-type :filename :base64 :size :dims}` — the shape
   `db-store-iteration!`'s `:attachments` expects, minus `:tool-call-id` which
   the caller stamps from the block that produced the stdout. A fence whose
   path escapes `mpl-temp-dir` or can't be read is skipped. NEVER throws —
   capture must not break a turn."
  [stdout]
  (into []
        (keep (fn [{:keys [path media-type dims]}]
                (try (when-let [f (confined-mpl-file path)]
                       (let [bytes (Files/readAllBytes (.toPath f))]
                         {:kind "image"
                          :media-type (or (not-empty (str media-type)) "image/png")
                          :filename (.getName f)
                          :base64 (.encodeToString (Base64/getEncoder) bytes)
                          :size (alength bytes)
                          :dims dims}))
                     (catch Throwable _ nil))))
        (parse-image-fences stdout)))
