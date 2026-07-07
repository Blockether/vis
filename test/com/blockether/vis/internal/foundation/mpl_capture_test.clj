(ns com.blockether.vis.internal.foundation.mpl-capture-test
  "Capture of `vis-image` fence bytes for the iteration-attachment rail: the
   fence a `plt.show()` prints carries only a temp-file PATH, so at persist
   time we re-read the CONFINED file into inline bytes. These cover the fence
   parse, the base64 round-trip, and the path-traversal guard."
  (:require [clojure.string :as str]
            [com.blockether.vis.internal.foundation.mpl-capture :as cap]
            [lazytest.core :refer [defdescribe describe expect it]])
  (:import [java.io File FileOutputStream]
           [java.util Base64]))

(defn- write-fig!
  "Write `bytes` to a fresh `fig-*.png` inside `$TMPDIR/vis-mpl` (the only dir
   the confinement guard trusts) and return its absolute path."
  ^String [^bytes bytes]
  (let [dir
        (doto (File. (System/getProperty "java.io.tmpdir") "vis-mpl") (.mkdirs))

        f
        (File/createTempFile "fig-" ".png" dir)]

    (with-open [o (FileOutputStream. f)]
      (.write o bytes))
    (.getAbsolutePath f)))

(defn- fence
  "Build a `vis-image` fence body the way the shim's `_emit_image` prints it."
  [path & {:keys [mime dims size ascii] :or {mime "image/png" dims "640x480" size "12 B"}}]
  (str "````vis-image\n[Image #1: demo]\n"
       path
       "\n"
       mime
       "\n"
       dims
       "\n"
       size
       (when ascii (str "\n" ascii))
       "\n````"))

(def ^:private png-bytes
  (byte-array (map unchecked-byte [0x89 0x50 0x4e 0x47 0x0d 0x0a 0x1a 0x0a 1 2 3 4])))

(defdescribe
  mpl-capture-test
  (describe "parse-image-fences"
            (it "pulls header fields and drops the ASCII fallback"
                (let [[d] (cap/parse-image-fences
                            (fence "/tmp/vis-mpl/x.png" :ascii "plot-line-1\nplot-line-2"))]
                  (expect (= "/tmp/vis-mpl/x.png" (:path d)))
                  (expect (= "image/png" (:media-type d)))
                  (expect (= "640x480" (:dims d)))
                  (expect (= "12 B" (:size-label d)))
                  (expect (not (str/includes? (pr-str d) "plot-line")))))
            (it "returns [] when there is no fence"
                (expect (= []
                           (cap/parse-image-fences "just some printed output\nno picture here"))))
            (it "finds every fence in one stdout"
                (let [s (str (fence "/a/x.png") "\nbetween\n" (fence "/b/y.png"))]
                  (expect (= 2 (count (cap/parse-image-fences s)))))))
  (describe
    "collect-stdout-images"
    (it "reads the confined file into inline base64 bytes"
        (let [path
              (write-fig! png-bytes)

              [img]
              (cap/collect-stdout-images (fence path :ascii "ascii"))]

          (expect (= "image" (:kind img)))
          (expect (= "image/png" (:media-type img)))
          (expect (= 12 (:size img)))
          (expect (str/starts-with? (:filename img) "fig-"))
          (expect (= (seq png-bytes) (seq (.decode (Base64/getDecoder) ^String (:base64 img)))))))
    (it "stamps no :tool-call-id (the caller owns that)"
        (let [path
              (write-fig! png-bytes)

              [img]
              (cap/collect-stdout-images (fence path))]

          (expect (not (contains? img :tool-call-id)))))
    (it "rejects a path outside the vis-mpl temp dir (traversal guard)"
        (expect (= [] (cap/collect-stdout-images (fence "/etc/passwd")))))
    (it "skips a fence whose file does not exist"
        (let [dir (System/getProperty "java.io.tmpdir")]
          (expect (= []
                     (cap/collect-stdout-images (fence (str dir
                                                            "/vis-mpl/fig-does-not-exist.png")))))))
    (it "is a no-op on plain output" (expect (= [] (cap/collect-stdout-images "hello world"))))))
