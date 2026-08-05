(ns com.blockether.vis.internal.foundation.shim-attach
  "Built-in sandbox SHIM: `vis_attach` / `vis_attach_bytes` — the GENERIC
   producer twin of the matplotlib capture. A tool running in `python_execution`
   writes any artifact (a PNG it rendered, a CSV/JSON/PDF/wav it built, whatever)
   and hands it to `vis_attach(path)` (or `vis_attach_bytes(data, name)`); the
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
   shim-matplotlib): its `:ext/sandbox-shims` entry autoloads `vis_attach` into
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
   not break `vis_attach`."
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

(defn- truthy-flag?
  "Tolerant reading of a boolean argument crossing the shim bridge: Python hands
   us a bool, but a hand-rolled caller may send 0/1, \"\" or nil, and a mis-read
   here would silently start (or stop) showing someone's screenshots."
  [v]
  (boolean (and (some? v) (not (false? v)) (not= 0 v) (not= 0.0 v) (not= "" v))))

(defn- record-attachment-call
  "Body of `__vis_record_attachment__`: validate the already-decided attachment
   fields and append the map to the active per-block artifact sink.

   The image probe runs ONCE: its `[path w h]` is returned to the shim (which
   prints the inline `vis-image` fence) and, for an `in_answer` artifact the shim
   deliberately does NOT print, stamped onto the row so the answer's own gallery
   can paint it later without re-decoding the bytes."
  [kind media-type b64 filename size audience in-answer label]
  (attach-envelope
    #(cond (str/blank? (str b64)) (throw (ex-info "vis_attach: empty payload (no bytes to persist)"
                                                  {}))
           (str/blank? (str media-type)) (throw (ex-info "vis_attach: missing media type" {}))
           (> (long (or size 0)) mpl-capture/max-capture-bytes)
           (throw (ex-info (str "vis_attach: payload "
                                (long (or size 0))
                                " bytes exceeds the "
                                (quot mpl-capture/max-capture-bytes (* 1024 1024))
                                " MiB attachment limit")
                           {}))
           (nil? mpl-capture/*attachment-sink*)
           (throw (ex-info (str "vis_attach: no active capture sink — call it inside a "
                                "python_execution block so the produced artifact can be "
                                "attached to that iteration")
                           {}))
           :else (let
                   [info
                    (display-info (str media-type) (str b64))

                    answer?
                    (truthy-flag? in-answer)]

                   (mpl-capture/record-attachment!
                     (cond->
                       {:kind (or (not-empty (str kind)) "file")
                        :media-type (str media-type)
                        :base64 (str b64)
                        :size (long (or size 0))
                        ;; One funnel: a PDF/HTML document is clamped to "user" by
                        ;; `attachment-audience` itself, so no caller can put a
                        ;; document on the wire as an image block.
                        :audience (attachments/attachment-audience {:media-type (str media-type)
                                                                    :audience audience})}
                       (not (str/blank? (str filename)))
                       (assoc :filename (str filename))

                       (not (str/blank? (str label)))
                       (assoc :label (str/trim (str label)))

                       answer?
                       (assoc :is-in-answer true)

                       (and answer? info)
                       (assoc :display-path
                         (nth info 0) :display-width
                         (nth info 1) :display-height
                         (nth info 2))))
                   info))))

(defn- attach-bridge-bindings
  "Host callable the `vis_attach` shim delegates to. `__vis_record_attachment__`
   takes the already-decided attachment fields (kind / media-type / base64 /
   filename / size / audience / in-answer / label) and appends the map to the
   active per-block artifact sink via `mpl-capture/record-attachment!`. Returns
   [true display-info] once recorded, or [false message] when there is no active
   capture sink (called outside a driven `python_execution` block) or a field is
   missing — surfaced to the model as a `RuntimeError`, never silently dropped.

   `audience` is WHO the artifact is for (`attachments/audiences`): `\"both\"`,
   `\"user\"` (the human sees it, the bytes never reach the wire — an image
   replays IN FULL on every later request, so a screenshot the model does not
   need is re-billed forever) or `\"model\"` (the model gets it, the human's
   transcript stays clean). `in-answer` defers the artifact to the gallery under
   the final answer, which is where a human actually reviews figures."
  []
  {"__vis_record_attachment__"
   (fn record-attachment [kind media-type b64 filename size audience in-answer label]
     (record-attachment-call kind media-type b64 filename size audience in-answer label))
   "__vis_list_attachments__"
   (fn []
     (attach-envelope
       #(if-let [r mpl-capture/*attachment-reader*] (json/write-json-str (vec (or ((:list r)) [])))
          (throw (ex-info (str "vis_attachments: no active attachment reader — call it "
                               "inside a python_execution block")
                          {})))))
   "__vis_read_attachment__"
   (fn [id]
     (attach-envelope
       #(if-let [r mpl-capture/*attachment-reader*]
          (if-let [a ((:read r) (str id))]
            [(:base64 a) (:media-type a) (:filename a) (:kind a) (long (or (:size a) 0))
             (str (:id a)) (:storage-uri a)]
            (throw (ex-info
                     (str "vis_read_attachment: no attachment with id " id " in this session")
                     {})))
          (throw (ex-info (str "vis_read_attachment: no active attachment reader — call it "
                               "inside a python_execution block")
                          {})))))
   "__vis_reinspect_attachment__"
   (fn [id detail]
     (attach-envelope
       #(if-let [r mpl-capture/*attachment-reader*]
          (if-let [a ((:reinspect r) (str id) (str (or detail "auto")))]
            [(str (:id a)) (str (:filename a)) (str (:media-type a)) (long (or (:size a) 0))]
            (throw (ex-info (str "vis_reinspect_attachment: no image attachment with id "
                                 id
                                 " in this session")
                            {})))
          (throw (ex-info (str "vis_reinspect_attachment: no active attachment reader — call it "
                               "inside a python_execution block")
                          {})))))})


(def vis-extension
  (vis/extension
    {:ext/name "foundation-shim-attach"
     :ext/description
     "Sandbox `vis_attach(path)`/`vis_attach_bytes(data, filename)`: persists any artifact — image, CSV/TSV, JSON, PDF, wav — as a durable session attachment without stdout parsing. Survives restart; `image/*` replays to vision models across turns; a CSV/TSV becomes a live transcript table whose rows never reach the model. SAME DOCUMENT, SAME NAME: a revision goes back under the filename it already had and is stored as that artifact's next VERSION, so one document stays one continuous thread. ONE OR TWO artifacts per turn: `audience` routes each one to `both`/`user`/`model` and `in_answer=True` collects the figures into a single gallery under the final answer."
     :ext/version "0.1.0"
     :ext/author "Blockether"
     :ext/owner "vis"
     :ext/license "Apache-2.0"
     :ext/kind "foundation"
     :ext/sandbox-shims
     [{:shim/name "attach"
       :shim/globals ["vis_attach" "vis_attach_bytes" "vis_attachments" "vis_read_attachment"
                      "vis_reinspect_attachment" "vis_attachment_versions" "vis_attachment_version"]
       :shim/description
       "`vis_attach`/`vis_attach_bytes`: persist artifacts (images, CSV/TSV tables, JSON, PDF, audio) as durable DB-owned iteration attachments with sniffed media types. SAME DOCUMENT, SAME NAME — a new revision of an artifact you already attached goes back under its OWN filename and is stored as that artifact's next VERSION, never `report_v2.png` beside `report.png`; a fresh name is for a genuinely different document, so `vis_attachment_versions(name)` walks the whole thread and `vis_attachment_version(name, n)` picks one cut. ATTACH ONE OR TWO ARTIFACTS PER TURN — compose many images into ONE sheet; `audience='both'|'user'|'model'` decides who sees it, `in_answer=True` paints it once in the FINAL ANSWER's gallery. Vis-native; no upstream library."
       :shim/bindings attach-bridge-bindings
       :shim/source "vis-shims/attach.py"}]}))

(vis/register-extension! vis-extension)
