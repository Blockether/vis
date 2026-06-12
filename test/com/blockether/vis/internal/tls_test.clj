(ns com.blockether.vis.internal.tls-test
  "Trust-bootstrap tests.

   These run in CI where no corporate CA is present, so assertions hold
   both with and without an extra CA on disk: `ca-sources` must always
   yield readable files, and `install!` must never throw and must be
   idempotent."
  (:require
   [clojure.string :as str]
   [com.blockether.vis.internal.tls :as tls]
   [lazytest.core :refer [defdescribe expect it]])
  (:import
   (java.io File)))

(defdescribe tls-test
  (it "resolves the drop-in certs dir under ~/.vis/certs"
    (let [p (.getPath (tls/certs-dir))]
      (expect (str/ends-with? p (str File/separator ".vis" File/separator "certs")))))

  (it "ca-sources returns only readable files, de-duplicated by path"
    (let [files (tls/ca-sources)]
      (expect (every? #(and (instance? File %) (.isFile ^File %) (.canRead ^File %)) files))
      (let [paths (map #(.getAbsolutePath ^File %) files)]
        (expect (= (count paths) (count (distinct paths)))))))

  (it "install! never throws and is idempotent"
    (let [first-result  (tls/install!)
          second-result (tls/install!)]
      ;; First call returns a summary map (CA present) or nil (none found).
      (expect (or (nil? first-result) (map? first-result)))
      (when (map? first-result)
        (expect (pos? (:certs first-result)))
        (expect (vector? (:sources first-result))))
      ;; Trust is installed once; a second call is always a no-op.
      (expect (nil? second-result)))))
