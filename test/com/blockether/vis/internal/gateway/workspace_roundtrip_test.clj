(ns com.blockether.vis.internal.gateway.workspace-roundtrip-test
  "END-TO-END guard for the filesystem-root add the TUI picker / web footer show.

   The gateway serves the workspace in THE canonical string-keyed wire shape
   (`wire/canonical`) on BOTH transports, and `client/decode-workspace` passes
   it through VERBATIM — one representation, no re-hydration. This exercises
   the REAL chain a C-a keystroke drives — `workspace/add-filesystem-root!`
   (server DB) → the `state/session-workspace-info` map shape → `wire/json-str`
   (server encoder) → `wire/parse-json` (client decoder) → the REAL, private
   `client/decode-workspace` — and asserts the added root arrives in the
   canonical snake_case STRING-key shape every channel reads."
  (:require [clojure.java.io :as io]
            [com.blockether.vis.ext.persistance-sqlite.core :as ps]
            [com.blockether.vis.internal.gateway.client :as client]
            [com.blockether.vis.internal.gateway.wire :as wire]
            [com.blockether.vis.internal.workspace :as ws]
            [lazytest.core :refer [defdescribe expect it]]))

(def ^:private decode-workspace
  "The real, private client decoder — the seam under test."
  (deref #'client/decode-workspace))

(defn- with-store
  [f]
  (let [store (assoc (ps/db-open! :memory) :backend :sqlite)]
    (try (f store) (finally (ps/db-close! store)))))

(defn- temp-dir
  [prefix]
  (.getCanonicalPath (.toFile (java.nio.file.Files/createTempDirectory
                                prefix
                                (make-array java.nio.file.attribute.FileAttribute 0)))))

(defn- delete-tree!
  [root]
  (doseq [f (reverse (file-seq (io/file root)))]
    (io/delete-file f true)))

(defn- session-workspace-shape
  "The exact map `state/session-workspace-info` emits for a workspace."
  [ws-rec]
  {:id (:id ws-rec)
   :draft? (ws/draft? ws-rec)
   :root (:root ws-rec)
   :repo-root (:repo-root ws-rec)
   :label (:label ws-rec)
   :fork-ms (:fork-ms ws-rec)
   :filesystem-roots (ws/filesystem-roots ws-rec)})

(defn- hop
  "Server-encode then client-decode, exactly like the gateway HTTP boundary."
  [info]
  (decode-workspace (wire/parse-json (wire/json-str info))))

(defdescribe
  filesystem-add-survives-the-wire-test
  (it
    "an added root reaches every channel in the canonical string-keyed shape"
    (with-store
      (fn [store]
        (let [base
              (temp-dir "vis-rt-base")

              extra
              (temp-dir "vis-rt-extra")]

          (try
            (let [seed
                  (ps/db-workspace-insert! store
                                           {:id (str (random-uuid))
                                            :repo-id "rt"
                                            :repo-root base
                                            :root base
                                            :state :active
                                            :fork-ms 0})

                  wid
                  (:id seed)]

              ;; C-a: add `extra` as an extra filesystem root (trunk session → live).
              (ws/add-filesystem-root! store wid extra)
              (let [info
                    (session-workspace-shape (ws/get store wid))

                    decoded
                    (hop info)

                    raw
                    (wire/parse-json (wire/json-str info))]

                ;; The decode is a VERBATIM passthrough — one canonical shape.
                (expect (= raw decoded))
                ;; Channels read these snake_case STRING keys off `@ws-info`:
                (expect (= [(ws/normalize-root extra)]
                           (mapv #(get % "trunk") (get decoded "filesystem_roots"))))
                (expect (= 1 (count (get decoded "filesystem_roots"))))
                (expect (= base (get decoded "root")))
                (expect (= base (get decoded "repo_root")))
                ;; Keyword VALUES ride the wire as strings — never keywords.
                (expect (string? (get (first (get decoded "filesystem_roots")) "backend")))
                ;; The `?` boolean rides as `is_draft` — no kebab / `?` key survives.
                (expect (contains? decoded "is_draft"))
                (expect (nil? (get decoded "draft?")))
                (expect (nil? (:filesystem-roots decoded)))))
            (finally (delete-tree! base) (delete-tree! extra))))))))
