(ns com.blockether.vis.internal.attachment-storage-test
  "The storage-offload rail: a registry of storage backends + the pure offload
   DECISION that routes an attachment either INLINE (bytes in the BLOB) or
   EXTERNAL (bytes in a backend, `storage_uri` in the row). Covers the registry
   (validation / priority / scheme dispatch), the `default-offload?` predicate
   (`hot? AND size` — images stay inline even when big), the write-side
   `offload-attachment` (inline vs external, PUT-failure fallback), the
   read-side `hydrate` (inline untouched, external re-fetched), and the
   reference `file://` backend round-trip, and the LOSSLESS compaction every
   store-bound image payload goes through on the way in."
  (:require [com.blockether.imaging :as imaging]
            [com.blockether.vis.internal.attachment-storage :as as]
            [lazytest.core :refer [defdescribe describe expect it throws?]])
  (:import (java.nio.file Files)
           (java.nio.file.attribute FileAttribute)
           (java.util Base64)))

(defn- b64 [^String s] (.encodeToString (Base64/getEncoder) (.getBytes s "UTF-8")))

(defn- unb64 [^String s] (String. (.decode (Base64/getDecoder) s) "UTF-8"))

(defn- b64* [^bytes b] (.encodeToString (Base64/getEncoder) b))

(defn- unb64* ^bytes [^String s] (.decode (Base64/getDecoder) s))

(defn- clear-registry!
  []
  (doseq [b (as/registered-backends)]
    (as/deregister-backend! (:storage/id b))))

(defn- temp-dir [] (str (Files/createTempDirectory "vis-as-test" (make-array FileAttribute 0))))

(def ^:private big-cold
  "A ~300 KiB non-image (cold) artifact — the offload candidate."
  (let [payload (apply str (repeat 300000 "x"))]
    {:kind "file" :media-type "text/csv" :filename "d.csv" :size 300000 :base64 (b64 payload)}))

(def ^:private small-inline
  {:kind "file" :media-type "text/csv" :filename "t.csv" :size 5 :base64 (b64 "hello")})

(def ^:private big-image
  (let [payload (apply str (repeat 300000 "p"))]
    {:kind "image" :media-type "image/png" :filename "fig.png" :size 300000 :base64 (b64 payload)}))

;; A trivial in-memory backend so decisions/round-trips don't need disk.
(defn- mem-backend
  ([] (mem-backend {}))
  ([{:keys [id scheme priority offload? store fail-put?] :or {id :mem scheme "mem" priority 0}}]
   (let [store (or store (atom {}))]
     (cond->
       {:storage/id id
        :storage/scheme scheme
        :storage/priority priority
        :storage/put-fn (fn [{:keys [^bytes bytes]}]
                          (when fail-put? (throw (ex-info "boom" {})))
                          (let [k (str scheme "://" (count @store))]
                            (swap! store assoc k bytes)
                            k))
        :storage/get-fn (fn [uri]
                          (get @store uri))}
       offload?
       (assoc :storage/offload? offload?)))))

(defdescribe
  attachment-storage-test
  (describe "registry"
            (it "validates required descriptor keys"
                (expect (throws? clojure.lang.ExceptionInfo
                                 #(as/attachment-backend {:storage/scheme "x"
                                                          :storage/put-fn identity
                                                          :storage/get-fn identity})))
                (expect (throws? clojure.lang.ExceptionInfo
                                 #(as/attachment-backend {:storage/id :x
                                                          :storage/scheme ""
                                                          :storage/put-fn identity
                                                          :storage/get-fn identity})))
                (expect (throws? clojure.lang.ExceptionInfo
                                 #(as/attachment-backend
                                    {:storage/id :x :storage/scheme "x" :storage/get-fn identity})))
                (expect (throws? clojure.lang.ExceptionInfo
                                 #(as/attachment-backend {:storage/id :x
                                                          :storage/scheme "x"
                                                          :storage/put-fn identity
                                                          :storage/get-fn identity
                                                          :storage/offload? 42}))))
            (it "coerces priority to a long (default 0)"
                (expect (= 0
                           (:storage/priority (as/attachment-backend {:storage/id :x
                                                                      :storage/scheme "x"
                                                                      :storage/put-fn identity
                                                                      :storage/get-fn identity})))))
            (it "register is idempotent by id and active-backend is highest priority"
                (clear-registry!)
                (try (expect (nil? (as/active-backend)))
                     (as/register-backend! (mem-backend {:id :a :scheme "a" :priority 1}))
                     (as/register-backend! (mem-backend {:id :b :scheme "b" :priority 5}))
                     (as/register-backend! (mem-backend {:id :a :scheme "a" :priority 1})) ; dup id
                     (expect (= 2 (count (as/registered-backends))))
                     (expect (= :b (:storage/id (as/active-backend))))
                     (finally (clear-registry!))))
            (it "resolve-bytes dispatches on the URI scheme to the owning backend"
                (clear-registry!)
                (try (let
                       [sa
                        (atom {"a://k" (.getBytes "AAA" "UTF-8")})

                        sb
                        (atom {"b://k" (.getBytes "BBB" "UTF-8")})]

                       (as/register-backend! (mem-backend {:id :a :scheme "a" :store sa}))
                       (as/register-backend! (mem-backend {:id :b :scheme "b" :store sb}))
                       (expect (= "AAA" (String. (as/resolve-bytes "a://k") "UTF-8")))
                       (expect (= "BBB" (String. (as/resolve-bytes "b://k") "UTF-8")))
                       (expect (nil? (as/resolve-bytes "zzz://k")))) ; no backend owns scheme
                     (finally (clear-registry!)))))
  (describe "default-offload? — hot? AND size"
            (it "offloads a big COLD (non-image) artifact"
                (expect (true? (as/default-offload? {:size 300000 :media-type "text/csv"}))))
            (it "keeps a big IMAGE inline (hot: replayed to vision each turn)"
                (expect (false? (as/default-offload? {:size 300000 :media-type "image/png"}))))
            (it "keeps a small artifact inline (below the size floor)"
                (expect (false? (as/default-offload? {:size 100 :media-type "text/csv"}))))
            (it "keeps inline when size is missing"
                (expect (false? (as/default-offload? {:media-type "text/csv"})))))
  (describe "offload? — precedence"
            (it "is false with no backend (inline, zero regression)"
                (expect (false? (as/offload? nil {:size 999999 :media-type "text/csv"}))))
            (it "uses default-offload? when the backend supplies no predicate"
                (let [b (mem-backend)]
                  (expect (true? (as/offload? b {:size 300000 :media-type "text/csv"})))
                  (expect (false? (as/offload? b {:size 300000 :media-type "image/png"})))))
            (it "backend :storage/offload? wins over the default"
                (let
                  [never
                   (mem-backend {:offload? (constantly false)})

                   always
                   (mem-backend {:offload? (constantly true)})]

                  (expect (false? (as/offload? never {:size 300000 :media-type "text/csv"})))
                  (expect (true? (as/offload? always {:size 1 :media-type "image/png"})))))
            (it "falls back to inline when the predicate throws"
                (let
                  [boom (mem-backend {:offload? (fn [_]
                                                  (throw (ex-info "x" {})))})]
                  (expect (false? (as/offload? boom {:size 300000 :media-type "text/csv"}))))))
  (describe "offload-attachment — write side"
            (it "returns the map unchanged with no backend"
                (clear-registry!)
                (expect (= big-cold (as/offload-attachment big-cold))))
            (it "offloads a big cold artifact: storage-uri set, base64 dropped, size fixed"
                (clear-registry!)
                (try (as/register-backend! (mem-backend))
                     (let [off (as/offload-attachment big-cold)]
                       (expect (string? (:storage-uri off)))
                       (expect (not (contains? off :base64)))
                       (expect (= 300000 (:size off)))
                       (expect (= "text/csv" (:media-type off))))
                     (finally (clear-registry!))))
            (it "keeps a small artifact and a big image INLINE even with a backend"
                (clear-registry!)
                (try (as/register-backend! (mem-backend))
                     (expect (contains? (as/offload-attachment small-inline) :base64))
                     (expect (contains? (as/offload-attachment big-image) :base64))
                     (finally (clear-registry!))))
            (it "falls back to inline when the backend PUT fails"
                (clear-registry!)
                (try (as/register-backend! (mem-backend {:fail-put? true}))
                     (let [off (as/offload-attachment big-cold)]
                       (expect (contains? off :base64))
                       (expect (not (contains? off :storage-uri))))
                     (finally (clear-registry!)))))
  (describe "hydrate — read side"
            (it "leaves an inline envelope (already has base64) untouched"
                (clear-registry!)
                (let [row {:id "r" :media-type "text/csv" :base64 (b64 "hi")}]
                  (expect (= row (as/hydrate row)))))
            (it "leaves a row with neither base64 nor storage-uri untouched"
                (let [row {:id "r" :media-type "text/csv"}]
                  (expect (= row (as/hydrate row)))))
            (it "re-fetches an external row's bytes into :base64"
                (clear-registry!)
                (try
                  (as/register-backend! (mem-backend))
                  (let
                    [off
                     (as/offload-attachment big-cold)

                     ; external
                     row
                     {:id "r" :source :tool :media-type "text/csv" :storage-uri (:storage-uri off)}

                     hyd
                     (as/hydrate row)]

                    (expect (string? (:base64 hyd)))
                    (expect (= (:base64 big-cold) (:base64 hyd)))) ; byte-exact round trip
                  (finally (clear-registry!))))
            (it "leaves the row alone (keeps storage-uri) when no backend owns the scheme"
                (clear-registry!)
                (let [row {:id "r" :storage-uri "gone://x" :media-type "text/csv"}]
                  (expect (= row (as/hydrate row))))))
  (describe
    "file-backend — reference local-disk implementation"
    (it "round-trips bytes through file:// PUT then GET"
        (let
          [dir
           (temp-dir)

           backend
           (as/file-backend {:dir dir})

           uri
           ((:storage/put-fn backend) {:bytes (.getBytes "payload-123" "UTF-8")})]

          (expect (= "file" (:storage/scheme backend)))
          (expect (as/scheme-of uri))
          (expect (= "file" (as/scheme-of uri)))
          (expect (= "payload-123" (String. ^bytes ((:storage/get-fn backend) uri) "UTF-8")))))
    (it "requires :dir" (expect (throws? clojure.lang.ExceptionInfo #(as/file-backend {}))))
    (it "offloads + hydrates a cold artifact end-to-end via the registry"
        (clear-registry!)
        (try (as/register-backend! (as/file-backend {:dir (temp-dir)}))
             (let
               [off
                (as/offload-attachment big-cold)

                hyd
                (as/hydrate {:storage-uri (:storage-uri off) :media-type "text/csv"})]

               (expect (not (contains? off :base64)))
               (expect (= (unb64 (:base64 big-cold)) (unb64 (:base64 hyd)))))
             (finally (clear-registry!))))
    (it "honors an :offload? override on the backend descriptor"
        (clear-registry!)
        (try (as/register-backend! (as/file-backend {:dir (temp-dir) :offload? (constantly true)}))
             (let [off (as/offload-attachment small-inline)] ; small, but override forces external
               (expect (string? (:storage-uri off)))
               (expect (not (contains? off :base64))))
             (finally (clear-registry!))))))

(defn- gradient-png
  "A REAL 300x200 PNG straight out of the imaging encoder — the shape of every
   figure the sandbox produces (matplotlib/PIL): correct, and fatter than it
   needs to be."
  ^bytes []
  (let [img (imaging/blank 300 200 "white")]
    (dotimes [x 300]
      (dotimes [y 200]
        (imaging/put-pixel! img x y [(mod (* x 7) 256) (mod (* y 11) 256) (mod (+ x y) 256) 255])))
    (imaging/encode img :png)))

(defn- pixels-of ^bytes [^bytes encoded] (imaging/pixels (imaging/decode encoded)))

(defdescribe
  storage-compaction-test
  "What the DB actually keeps. Every image payload bound for the
   `session_attachment` BLOB (or for a backend PUT) is re-compressed by the
   imaging library's real optimisers first — oxipng for PNG, jpegtran-style
   marker stripping for JPEG, gifsicle differencing for GIF. LOSSLESS is the
   whole contract: fewer bytes, the same picture, so replay and the vision wire
   see exactly what the producer drew."
  (describe
    "inline (the BLOB)"
    (it "shrinks a store-bound PNG and reports the size actually stored"
        (let
          [png
           (gradient-png)

           att
           {:kind "image"
            :media-type "image/png"
            :filename "fig.png"
            :size (alength png)
            :base64 (b64* png)}

           [out]
           (as/offload-attachments [att])

           stored
           (unb64* (:base64 out))]

          (expect (< (alength stored) (alength png)))
          ;; `:size` describes the row that exists, not the payload we were handed —
          ;; the offload decision and every size label read it.
          (expect (= (alength stored) (:size out)))
          ;; Same picture, bit for bit.
          (expect (java.util.Arrays/equals (pixels-of png) (pixels-of stored)))
          (expect (= "png" (:format (imaging/probe stored))))
          (expect (= [300 200] [(:width (imaging/probe stored)) (:height (imaging/probe stored))]))
          ;; Third decoder (neither our encoder nor our optimiser agreeing with itself).
          (let [bi (javax.imageio.ImageIO/read (java.io.ByteArrayInputStream. stored))]
            (expect (= [300 200] [(.getWidth bi) (.getHeight bi)])))))
    (it "leaves a non-image payload byte-identical"
        (expect (= small-inline (as/offload-attachment small-inline)))
        (expect (= big-cold (as/offload-attachment big-cold))))
    (it "leaves bytes that only CLAIM to be an image untouched"
        ;; `big-image` is 300 KB of \"p\" labelled image/png: the optimiser cannot read
        ;; it, and a payload we cannot prove is the same picture is never rewritten.
        (expect (= big-image (as/offload-attachment big-image))))
    (it "keeps the original when there is nothing left to win"
        (let
          [tight
           (imaging/optimize (gradient-png))

           att
           {:kind "image" :media-type "image/png" :size (alength tight) :base64 (b64* tight)}]

          (expect (= att (as/offload-attachment att))))))
  (describe
    "external (a backend PUT)"
    (it "hands the backend the compacted bytes, and hydrate gives the same picture back"
        (clear-registry!)
        (try (let
               [png
                (gradient-png)

                store
                (atom {})

                _
                (as/register-backend! (mem-backend {:store store :offload? (constantly true)}))

                off
                (as/offload-attachment
                  {:kind "image" :media-type "image/png" :size (alength png) :base64 (b64* png)})

                ^bytes put
                (first (vals @store))]

               (expect (string? (:storage-uri off)))
               (expect (not (contains? off :base64)))
               (expect (< (alength put) (alength png)))
               (expect (= (alength put) (:size off)))
               (expect (java.util.Arrays/equals (pixels-of png) (pixels-of put)))
               (let [hyd (as/hydrate {:storage-uri (:storage-uri off) :media-type "image/png"})]
                 (expect (java.util.Arrays/equals (pixels-of png)
                                                  (pixels-of (unb64* (:base64 hyd)))))))
             (finally (clear-registry!))))))
