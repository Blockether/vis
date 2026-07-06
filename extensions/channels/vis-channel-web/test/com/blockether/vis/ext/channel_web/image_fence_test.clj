(ns com.blockether.vis.ext.channel-web.image-fence-test
  (:require [clojure.string :as str]
            [com.blockether.vis.ext.channel-web.core :as core]
            [lazytest.core :refer [defdescribe expect it]])
  (:import [java.io File]
           [java.nio.file Files]))

(def ^:private resolve-image-fences @#'core/resolve-image-fences)

;; A 1x1 PNG (valid header bytes are all `mpl-confined-file` cares about — it
;; reads WHATEVER is at a confined path and base64s it).
(def ^:private png-bytes
  (byte-array (map unchecked-byte [0x89 0x50 0x4E 0x47 0x0D 0x0A 0x1A 0x0A])))

(defn- write-mpl-png!
  "Write a throwaway PNG into the SAME vis-mpl temp dir the shim uses, so the
   confinement guard accepts it. Returns its absolute path."
  []
  (let [dir
        (doto (File. (System/getProperty "java.io.tmpdir") "vis-mpl") (.mkdirs))

        f
        (File/createTempFile "test-fig-" ".png" dir)]

    (Files/write (.toPath f) ^bytes png-bytes (make-array java.nio.file.OpenOption 0))
    (.getAbsolutePath f)))

(defn- fence
  [path ascii]
  (str "````vis-image\n[Image #1: demo 640x480, 8 B]\n"
       path
       "\nimage/png\n640x480\n8 B"
       (when ascii (str "\n" ascii))
       "\n````"))

(defdescribe resolve-image-fences-test
             (it "inlines a confined PNG as a base64 data-URI <img> with a caption"
                 (let [out (resolve-image-fences (fence (write-mpl-png!) nil))]
                   (expect (str/includes? out "<figure class=\"mpl-fig\">"))
                   (expect (re-find #"<img src=\"data:image/png;base64,[A-Za-z0-9+/]+=*\"" out))
                   (expect (str/includes? out "width=\"640\""))
                   (expect (str/includes? out "height=\"480\""))
                   (expect (str/includes? out "<figcaption>"))
                   ;; no ASCII plot in the fence → no fallback disclosure
                   (expect (not (str/includes? out "mpl-ascii")))))
             (it "keeps the ASCII plot as a collapsed <details> fallback when present"
                 (let [out (resolve-image-fences (fence (write-mpl-png!) "⠉⠁⠀braille"))]
                   (expect (str/includes? out "<details class=\"mpl-ascii\">"))
                   (expect (str/includes? out "braille"))))
             (it "escapes HTML in the ASCII fallback so a crafted plot can't inject markup"
                 (let [out (resolve-image-fences (fence (write-mpl-png!) "<script>x</script>"))]
                   (expect (not (str/includes? out "<script>")))
                   (expect (str/includes? out "&lt;script&gt;"))))
             (it "leaves a foreign / non-confined path untouched (path-traversal guard)"
                 (let [f (fence "/etc/passwd" nil)]
                   (expect (= f (resolve-image-fences f)))))
             (it "leaves a missing file untouched (degrades to the code-block fallback)"
                 (let [f (fence (str (System/getProperty "java.io.tmpdir") "/vis-mpl/nope-404.png")
                                nil)]
                   (expect (= f (resolve-image-fences f)))))
             (it "is a no-op on markdown without a vis-image fence"
                 (expect (= "plain **text**" (resolve-image-fences "plain **text**")))
                 (expect (nil? (resolve-image-fences nil)))))
