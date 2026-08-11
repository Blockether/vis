(ns com.blockether.vis.internal.foundation.shim-attach
  "Built-in sandbox SHIM: `attach` — the GENERIC
   producer twin of the matplotlib capture. A tool running in `python_execution`
   writes any artifact (a PNG it rendered, a CSV/JSON/PDF/wav it built, whatever)
   and hands it to `attach(path)` (or `attach(data, name)` for bytes it
   never wrote out); the
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
   `:attachments`.

   Registered unconditionally as a foundation shim (like shim-yaml /
   shim-matplotlib): its `:ext/sandbox-shims` entry autoloads `attach` into
   every sandbox (main + every `sub_loop` fork)."
  (:require [com.blockether.imaging :as imaging]
            [com.blockether.vis.internal.attachments :as attachments]
            [com.blockether.vis.core :as vis]
            [com.blockether.vis.internal.foundation.mpl-capture :as mpl-capture]
            [charred.api :as json]
            [clojure.string :as str]))

(defn- attach-envelope
  "Run thunk `f`, returning the 2-vector the attach shim expects: [true result]
   on success, [false message] on any Throwable. Errors cross the boundary as
   DATA so the Python shim can raise a catchable `RuntimeError` instead of a raw
   host `PolyglotException` (GraalPy does not route host exceptions through
   Python `except`)."
  [f]
  (try [true (f)] (catch Throwable t [false (str (or (.getMessage t) t))])))

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
               (let
                 [bytes (.decode (java.util.Base64/getDecoder) b64)
                  ext (if (str/includes? mt "pdf") "pdf" "html")
                  f (mpl-capture/display-cache-file "doc-" ext bytes)]

                 [(.getAbsolutePath f) 0 0])
               (str/starts-with? mt "image/")
               (let
                 [bytes (.decode (java.util.Base64/getDecoder) b64)
                  info
                  ;; imaging probes SVG too, but the inline fence is for RASTER bytes a
                  ;; viewer can paint as-is — an SVG stays a text placeholder.
                  (let [i (imaging/probe bytes)]
                    (when-not (= "svg"
                                 (some-> (:format i)
                                         name))
                      i))]

                 (when info
                   (let
                     [w (:width info)
                      h (:height info)
                      ext (or (some-> mt
                                      (str/split #"/")
                                      second
                                      (str/replace #"[^a-z0-9]" ""))
                              "img")
                      f (mpl-capture/display-cache-file "att-" ext bytes)]

                     [(.getAbsolutePath f) w h])))))
       (catch Throwable _ nil)))

(defn- record-attachment-call
  "Body of `__vis_record_attachment__`: validate the already-decided attachment
   fields and append the map to the active per-block artifact sink.

   The image probe runs ONCE: its `[path w h]` is returned to the shim for the
   inline `vis-image` fence."
  [kind media-type b64 filename size audience label]
  (attach-envelope
    #(cond (str/blank? (str b64)) (throw (ex-info "attach: empty payload (no bytes to persist)" {}))
           (str/blank? (str media-type)) (throw (ex-info "attach: missing media type" {}))
           (> (long (or size 0)) mpl-capture/max-capture-bytes)
           (throw (ex-info (str "attach: payload "
                                (long (or size 0))
                                " bytes exceeds the "
                                (quot mpl-capture/max-capture-bytes (* 1024 1024))
                                " MiB attachment limit")
                           {}))
           (nil? mpl-capture/*attachment-sink*)
           (throw (ex-info (str "attach: no active capture sink — call it inside a "
                                "python_execution block so the produced artifact can be "
                                "attached to that iteration")
                           {}))
           :else (let [info (display-info (str media-type) (str b64))]
                   (mpl-capture/record-attachment! (cond->
                                                     {:kind (or (not-empty (str kind)) "file")
                                                      :media-type (str media-type)
                                                      :base64 (str b64)
                                                      :size (long (or size 0))
                                                      ;; One funnel: a PDF/HTML document is clamped to "user" by
                                                      ;; `attachment-audience` itself, so no caller can put a
                                                      ;; document on the wire as an image block.
                                                      :audience (attachments/attachment-audience
                                                                  {:media-type (str media-type)
                                                                   :audience audience})}
                                                     (not (str/blank? (str filename)))
                                                     (assoc :filename (str filename))

                                                     (not (str/blank? (str label)))
                                                     (assoc :label (str/trim (str label)))))
                   info))))

(defn- attach-bridge-bindings
  "Host callable the `attach` shim delegates to. `__vis_record_attachment__`
   takes the already-decided attachment fields (kind / media-type / base64 /
   filename / size / audience / label) and appends the map to the
   active per-block artifact sink via `mpl-capture/record-attachment!`. Returns
   [true display-info] once recorded, or [false message] when there is no active
   capture sink (called outside a driven `python_execution` block) or a field is
   missing — surfaced to the model as a `RuntimeError`, never silently dropped.

   `audience` is WHO the artifact is for (`attachments/audiences`): `\"both\"`,
   `\"user\"` (the human sees it, the bytes never reach the wire — an image
   replays IN FULL on every later request, so a screenshot the model does not
   need is re-billed forever) or `\"model\"` (the model gets it, the human's
   transcript stays clean)."
  []
  {"__vis_record_attachment__"
   (fn record-attachment [kind media-type b64 filename size audience label]
     (record-attachment-call kind media-type b64 filename size audience label))
   "__vis_list_attachments__"
   (fn []
     (attach-envelope
       #(if-let [r mpl-capture/*attachment-reader*] (json/write-json-str (vec (or ((:list r)) [])))
          (throw (ex-info (str "list_attachments: no active attachment reader — call it "
                               "inside a python_execution block")
                          {})))))
   "__vis_read_attachment__"
   (fn [id]
     (attach-envelope
       #(if-let [r mpl-capture/*attachment-reader*]
          (if-let [a ((:read r) (str id))]
            (:base64 a)
            (throw (ex-info (str "read_attachment: no attachment with id " id " in this session")
                            {})))
          (throw (ex-info (str "read_attachment: no active attachment reader — call it "
                               "inside a python_execution block")
                          {})))))
   "__vis_reinspect_attachment__"
   (fn [id]
     (attach-envelope
       #(if-let [r mpl-capture/*attachment-reader*]
          (if-let [a ((:reinspect r) (str id))]
            [(str (:id a)) (str (:filename a)) (str (:media-type a)) (long (or (:size a) 0))]
            (throw (ex-info
                     (str "show_attachment: no image attachment with id " id " in this session")
                     {})))
          (throw (ex-info (str "show_attachment: no active attachment reader — call it "
                               "inside a python_execution block")
                          {})))))})


(def vis-extension
  (vis/extension
    {:ext/name "foundation-shim-attach"
     :ext/description
     (str "Sandbox `attach(source)` — a confined path, in-memory bytes, or a figure: "
          "persists any artifact (image, CSV/TSV, JSON, PDF, wav) as a durable session "
          "attachment. Survives restart; `image/*` replays to vision models; a CSV/TSV becomes "
          "a transcript table whose rows never reach the model. "
          "SAME DOCUMENT, SAME NAME: a revision goes back under the filename it already had, "
          "as that artifact's next VERSION; a new name is a different document. "
          "`list_attachments()`, `get_attachment` and `read_attachment` take the same target — "
          "the filename, or an id out of a descriptor.")
     :ext/version "0.1.0"
     :ext/author "Blockether"
     :ext/owner "vis"
     :ext/license "Apache-2.0"
     :ext/kind "foundation"
     :ext/sandbox-shims
     [{:shim/name "attachments"
       :shim/globals ["attach" "list_attachments" "get_attachment" "read_attachment"
                      "show_attachment"]
       :shim/description
       (str "Persists artifacts (images, tables, JSON, PDF, audio) as durable attachments. SAME "
            "DOCUMENT, SAME NAME — a revision goes back under its OWN filename as that artifact's "
            "next VERSION, never `report_v2.png`; a fresh name is a different document. "
            "`read_attachment` is the only door to the bytes. Addressing and versions: "
            "`doc(\"attach\")`.")
       :shim/docs
       (str "`attach` persists artifacts (images, CSV/TSV tables, JSON, PDF, audio) as durable "
            "DB-owned iteration attachments with sniffed media types, surviving restarts. "
            "SAME DOCUMENT, SAME NAME — a revision goes back under its OWN filename as that "
            "artifact's next VERSION, never `report_v2.png` beside `report.png`; a fresh name "
            "is a different document, and `list_attachments(name)` walks the thread. Compose "
            "many images into one sheet per call; `audience='both'|'user'|'model'` routes who "
            "sees it. ONE ADDRESSING RULE on the read side: `get_attachment(target, "
            "version=None)`, `read_attachment` and `show_attachment` take the FILENAME (latest "
            "cut unless you name a version) or an `id` from a descriptor. `read_attachment` is "
            "the only door to the BYTES; `show_attachment` puts a stored image back in front of "
            "the MODEL for the next request. Vis-native; no upstream library.")
       :shim/bindings attach-bridge-bindings
       :shim/source "vis-shims/attach.py"}]}))

(vis/register-extension! vis-extension)
