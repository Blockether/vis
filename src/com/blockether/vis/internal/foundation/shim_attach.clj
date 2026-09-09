(ns com.blockether.vis.internal.foundation.shim-attach
  "Built-in sandbox SHIM: `attach` — the GENERIC
   producer twin of the matplotlib capture. A tool running in `python_execution`
   writes any artifact (a PNG it rendered, a CSV/JSON/PDF/wav it built, whatever)
   and hands it to `attach(path)` (or `attach(data, filename)` for bytes it
   never wrote out), getting back the stored artifact's DESCRIPTOR; the
   engine then OWNS the bytes as a durable `session_iteration_attachment` row,
   exactly like a matplotlib figure — surviving a web/TUI restart and (for image
   media-types) replayable to a vision model cross-turn.

   No parsing, no round-trip through the model-facing stdout: we control the whole
   boundary. The Python side reads the file through the sandbox's OWN confined
   `open` (so filesystem-root confinement is enforced for free — a path outside
   the roots raises the normal sandbox error), sniffs the media-type (magic bytes
   then extension then utf-8 probe), base64-encodes, and calls the tiny host
   bridge `__vis_record_attachment__`, which appends the attachment map to the
   per-block `*image-sink*` (`mpl-capture/record-attachment!`). `run-python-block`
   drains that sink into the block outcome's `:attachments`; the loop stamps each with
   the producing block's tool-call-id and hands them to `db-store-iteration!`'s
   `:attachments`. The artifact's `:id` and `:version` are minted at the sink, so
   the block that produced it can address it immediately.

   Registered unconditionally as a foundation shim (like shim-yaml /
   shim-matplotlib): its `:ext/sandbox-shims` entry autoloads `attach` into
   every sandbox."
  (:require [com.blockether.imaging :as imaging]
            [com.blockether.vis.internal.attachment.core :as attachments]
            [com.blockether.vis.core :as vis]
            [com.blockether.vis.internal.foundation.mpl-capture :as mpl-capture]
            [com.blockether.vis.internal.extension.core :as extension]
            [charred.api :as json]
            [clojure.string :as str]))

(defn- display-info
  "Write an attachment's decoded bytes to a HOST temp file and describe what a
   surface needs in order to SHOW it: `[abs-path width height]`.

   For an image the pixel dimensions are probed and the attach shim prints a
   `vis-image` display fence so a graphical TUI/web paints the picture inline
   (the same fence matplotlib's `plt.show()` emits). For a PDF or an HTML page
   ([[attachments/human-only-media-type?]]) there are no pixels to probe: the
   dimensions are 0 and the shim prints a `vis-doc` fence instead, which the TUI
   hands to the system viewer and the companion renders inside a sandboxed
   frame. Either way the bytes are written HOST-side (like
   `__vis_mpl_render_file__`), so display works even when the sandbox's own
   Python filesystem is denied.

   Returns nil for any other media-type, and for bytes that cannot be decoded as
   an image (an SVG, or a format `com.blockether/imaging` cannot probe) — the
   caller then records the attachment with no inline fence and the renderer
   keeps its text placeholder. Never throws: a temp-file/decoding hiccup must
   not break `attach`."
  [^String media-type ^String b64]
  (try (let [mt (str/lower-case (str/trim (str media-type)))]
         (cond (attachments/human-only-media-type? mt)
               (let [bytes (.decode (java.util.Base64/getDecoder) b64)
                     ext (if (str/includes? mt "pdf") "pdf" "html")
                     f (mpl-capture/display-cache-file "doc-" ext bytes)]

                 [(.getAbsolutePath f) 0 0])
               (str/starts-with? mt "image/")
               (let [bytes (.decode (java.util.Base64/getDecoder) b64)
                     info
                     ;; imaging probes SVG too, but the inline fence is for RASTER bytes a
                     ;; viewer can paint as-is — an SVG stays a text placeholder.
                     (let [i (imaging/probe bytes)]
                       (when-not (= "svg"
                                    (some-> (:format i)
                                            name))
                         i))]

                 (when info
                   (let [w (:width info)
                         h (:height info)
                         ext (or (some-> mt
                                         (str/split #"/")
                                         second
                                         (str/replace #"[^a-z0-9]" ""))
                                 "img")
                         f (mpl-capture/display-cache-file "att-" ext bytes)]

                     [(.getAbsolutePath f) w h])))))
       (catch Throwable _ nil)))

(defn- pending-descriptor
  "The `loop/attachment-descriptor` shape for ONE artifact the RUNNING block has
   attached but the loop has not persisted yet. Same keys, same meaning, with
   `:is-pending true` — the row is real, addressable and already carries the id
   its database row will be inserted under; it simply is not in the database
   until this iteration is stored."
  [position att]
  {:id (:id att)
   :source "tool"
   :filename (:filename att)
   :version (:version att)
   :media-type (:media-type att)
   :kind (:kind att)
   :size (:size att)
   :position position
   :is-pending true
   :audience (attachments/attachment-audience att)})

(defn- pending-descriptors
  "Descriptors for everything the RUNNING block has attached so far, in call
   order — what `list_attachments()` must show ON TOP of the stored rows, so a
   producer never has to wait for the next iteration to see its own artifact."
  []
  (into [] (map-indexed pending-descriptor) (mpl-capture/pending-attachments)))

(defn- attachment-descriptors
  "Stored and pending descriptors, in attachment order."
  []
  (let [reader mpl-capture/*attachment-reader*]
    (when-not (or reader mpl-capture/*attachment-sink*)
      (throw (ex-info "list_attachments: call inside python_execution." {})))
    (into (vec (when reader (or ((:list reader)) []))) (pending-descriptors))))

(defn- locate-attachment
  "Resolve an id, filename or relative version against this session's descriptors."
  [caller target version]
  (let [rows
        (attachment-descriptors)

        wanted
        (str target)

        by-id
        (when (nil? version) (some #(when (= wanted (str (:id %))) %) rows))

        versions
        (sort-by #(long (or (:version %) 1)) (filter #(= wanted (:filename %)) rows))

        n
        (count versions)]

    (or by-id
        (when (pos? n)
          (cond (nil? version) (last versions)
                (<= (- n) (long version) -1) (nth (vec versions) (+ n (long version)))
                :else (some #(when (= (long version) (long (or (:version %) 1))) %) versions)))
        (throw (ex-info (if (zero? n)
                          (str caller
                               ": unknown id or filename "
                               (pr-str wanted)
                               (if (= caller "show_attachment")
                                 "; local image: show_attachment(attach(path))."
                                 "; use list_attachments()."))
                          (str caller
                               ": "
                               (pr-str wanted)
                               " has no version "
                               version
                               " (versions: "
                               (str/join ", " (map #(or (:version %) 1) versions))
                               ")."))
                        {:type ::attachment-not-found :target wanted :version version})))))

(defn- pending-by-id
  "The raw sink entry (BYTES included) this block recorded under `id`, or nil."
  [id]
  (first (filter #(= (str id) (str (:id %))) (mpl-capture/pending-attachments))))

(defn- reinspect-pending
  "`show_attachment` for an artifact the RUNNING block just attached. Its bytes
   are already in this iteration's sink, so nothing is re-stored and nothing is
   re-read: when the artifact's audience does not reach the model on its own
   (`audience=\"user\"`) it is queued for exactly the next request; otherwise it
   is on the wire already and this is a no-op. A non-image is refused exactly as
   the stored path refuses one."
  [att]
  (if-not (and (str/starts-with? (str (:media-type att)) "image/")
               (not (str/blank? (str (:base64 att)))))
    (throw (ex-info (str "show_attachment: " (pr-str (:filename att)) " is not an image.") {}))
    (do (when (= "user" (attachments/attachment-audience att))
          (mpl-capture/queue-reinspection! att))
        [(str (:id att)) (str (:filename att)) (str (:media-type att)) (long (or (:size att) 0))])))

(defn- record-attachment-call
  "Body of `__vis_record_attachment__`: validate the already-decided attachment
   fields, append the map to the active per-block artifact sink, and hand the
   shim back the stored artifact's DESCRIPTOR as JSON.

   The image probe runs ONCE: its `[path w h]` rides that descriptor under
   `:display`, for the inline `vis-image` fence. Everything else is the identity
   `record-attachment!` minted — `:id` and `:version` — so `attach` returns a
   HANDLE to what it just stored and every read verb can address it inside the
   same block."
  [kind media-type b64 filename size audience label]
  (cond (str/blank? (str b64)) (throw (ex-info "attach: empty payload." {}))
        (str/blank? (str media-type)) (throw (ex-info "attach: missing media type" {}))
        (> (long (or size 0)) mpl-capture/max-capture-bytes)
        (throw (ex-info (str "attach: payload "
                             (long (or size 0))
                             " bytes exceeds the "
                             (quot mpl-capture/max-capture-bytes (* 1024 1024))
                             " MiB attachment limit")
                        {}))
        (nil? mpl-capture/*attachment-sink*) (throw (ex-info "attach: call inside python_execution."
                                                             {}))
        :else (let [info
                    (display-info (str media-type) (str b64))

                    recorded
                    (mpl-capture/record-attachment!
                      (cond-> {:kind (or (not-empty (str kind)) "file")
                               :media-type (str media-type)
                               :base64 (str b64)
                               :size (long (or size 0))
                               ;; One funnel: a PDF/HTML document is clamped to "user" by
                               ;; `attachment-audience` itself, so no caller can put a
                               ;; document on the wire as an image block.
                               :audience (attachments/attachment-audience
                                           {:media-type (str media-type) :audience audience})}
                        (not (str/blank? (str filename)))
                        (assoc :filename (str filename))

                        (not (str/blank? (str label)))
                        (assoc :label (str/trim (str label)))))]

                (json/write-json-str (cond-> (dissoc (pending-descriptor 0 recorded) :position)
                                       (some? info)
                                       (assoc :display (vec info)))))))

(defn- attachment-tool
  "Wrap an attachment operation with the standard error hook."
  [symbol tag f]
  (let [entry {:ext.symbol/symbol symbol
               :ext.symbol/tag tag
               :ext.symbol/on-error-fn extension/tool-failure-on-error
               :ext.symbol/fn (fn [& args]
                                (extension/success {:result (apply f args)}))}]
    (fn [& args]
      ;; Attachment presentation already records descriptors. Do not duplicate raw
      ;; upload/download bytes in tool Activity when applying the error hook.
      (binding [extension/*tool-event-sink* nil]
        (extension/invoke-symbol-wrapper {:ext/name "foundation-shim-attach"}
                                         entry
                                         args
                                         extension/*current-environment*)))))

(defn- attach-bridge-bindings
  "Attachment host operations. Values cross directly; failures use the shared
   observed-symbol hook and remain catchable host errors in Python."
  []
  {"__vis_record_attachment__" (attachment-tool 'attach :mutation record-attachment-call)
   "__vis_list_attachments__"
   (attachment-tool 'list_attachments :observation #(json/write-json-str (attachment-descriptors)))
   "__vis_get_attachment__"
   (attachment-tool 'get_attachment
                    :observation
                    (fn [target version]
                      (json/write-json-str (locate-attachment "get_attachment" target version))))
   "__vis_read_attachment__"
   (attachment-tool
     'read_attachment
     :observation
     (fn [target version]
       (let [id
             (:id (locate-attachment "read_attachment" target version))

             attachment
             (or (pending-by-id id)
                 (when-let [reader mpl-capture/*attachment-reader*]
                   ((:read reader) (str id))))]

         (if attachment
           (:base64 attachment)
           (throw (ex-info (str "read_attachment: bytes unavailable for " (pr-str (str target)) ".")
                           {:type ::attachment-not-found}))))))
   "__vis_reinspect_attachment__"
   (attachment-tool
     'show_attachment
     :observation
     (fn [target version]
       (let [row
             (locate-attachment "show_attachment" target version)

             id
             (:id row)]

         (if-let [pending (pending-by-id id)]
           (reinspect-pending pending)
           (if-let [a (when-let [reader mpl-capture/*attachment-reader*]
                        ((:reinspect reader) (str id)))]
             [(str (:id a)) (str (:filename a)) (str (:media-type a)) (long (or (:size a) 0))]
             (throw (ex-info (str "show_attachment: " (pr-str (:filename row)) " is not an image.")
                             {:type ::not-an-image})))))))})

(def vis-extension
  (vis/extension
    {:ext/name "foundation-shim-attach"
     :ext/description
     (str "Sandbox `attach(source)` — a confined path, in-memory bytes, a PIL image or a "
          "matplotlib figure: "
          "persists any artifact (image, CSV/TSV, JSON, PDF, wav) as a durable session "
          "attachment. Survives restart; `image/*` replays to vision models; a CSV/TSV becomes "
          "a transcript table whose rows never reach the model. "
          "SAME DOCUMENT, SAME NAME: a revision goes back under the filename it already had, "
          "as that artifact's next VERSION; a new name is a different document. "
          "`attach` returns that artifact's descriptor; `list_attachments()`, `get_attachment` "
          "and `read_attachment` take the same target — the filename, or an id out of a "
          "descriptor — including an artifact attached in the very same block.")
     :ext/version "0.1.0"
     :ext/author "Blockether"
     :ext/owner "vis"
     :ext/license "Apache-2.0"
     :ext/kind "foundation"
     :ext/sandbox-shims
     [{:shim/name "attachments"
       :shim/globals ["attach" "list_attachments" "get_attachment" "read_attachment"
                      "show_attachment"]
       :shim/docs
       (str "`attach` persists artifacts (images, CSV/TSV tables, JSON, PDF, audio) as durable "
            "DB-owned iteration attachments with sniffed media types, surviving restarts. "
            "SAME DOCUMENT, SAME NAME — a revision goes back under its OWN filename as that "
            "artifact's next VERSION, never `report_v2.png` beside `report.png`; a fresh name "
            "is a different document, and `list_attachments(name)` walks the thread. Compose "
            "many images into one sheet per call; `audience='both'|'user'|'model'` routes who "
            "sees it. ONE ADDRESSING RULE on the read side: `get_attachment(target, "
            "version=None)`, `read_attachment` and `show_attachment` take the FILENAME (latest "
            "cut unless you name a version) or an `id` from a descriptor — `attach` RETURNS "
            "that descriptor, and an artifact this block just attached is addressable at once. "
            "`read_attachment` is "
            "the only door to the BYTES; `show_attachment` puts a stored image back in front of "
            "the MODEL for the next request. Vis-native; no upstream library.")
       :shim/bindings attach-bridge-bindings
       :shim/source "vis-shims/attach.py"}]}))

(defn register! [] (vis/register-extension! vis-extension))
