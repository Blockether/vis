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
  (:require [com.blockether.vis.core :as vis]
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

(defn- image-display-info
  "For an image attachment, write the decoded bytes to a HOST temp file and read
   back its pixel dimensions, returning `[abs-path width height]` — the attach
   shim prints these as a `vis-image` display fence so a graphical TUI/web paints
   the picture inline (the same fence matplotlib's `plt.show()` emits). The bytes
   are written HOST-side (like `__vis_mpl_render_file__`), so inline display works
   even when the sandbox's own Python filesystem is denied. Returns nil for a
   non-image media-type, or when the bytes can't be decoded as an image (e.g. an
   SVG or a format with no ImageIO reader) — the caller then records the
   attachment with no inline fence and the renderer keeps its text placeholder.
   Never throws: a temp-file/decoding hiccup must not break `vis_attach`."
  [^String media-type ^String b64]
  (try (when (str/starts-with? (str media-type) "image/")
         (let
           [bytes
            (.decode (java.util.Base64/getDecoder) b64)

            img
            (javax.imageio.ImageIO/read (java.io.ByteArrayInputStream. bytes))]

           (when img
             (let
               [w
                (.getWidth img)

                h
                (.getHeight img)

                ext
                (or (some-> media-type
                            (str/split #"/")
                            second
                            (str/replace #"[^a-z0-9]" ""))
                    "img")

                f
                (mpl-capture/display-cache-file "att-" ext bytes)]

               [(.getAbsolutePath f) w h]))))
       (catch Throwable _ nil)))

(defn- attach-bridge-bindings
  "Host callable the `vis_attach` shim delegates to. `__vis_record_attachment__`
   takes the already-decided attachment fields (kind / media-type / base64 /
   filename / size) and appends the map to the active per-block artifact sink via
   `mpl-capture/record-attachment!`. Returns [true nil] once recorded, or
   [false message] when there is no active capture sink (called outside a driven
   `python_execution` block) or a field is missing — surfaced to the model as a
   `RuntimeError`, never silently dropped."
  []
  {"__vis_record_attachment__"
   (fn [kind media-type b64 filename size]
     (attach-envelope
       #(cond (str/blank? (str b64))
              (throw (ex-info "vis_attach: empty payload (no bytes to persist)" {}))
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
              :else (do (mpl-capture/record-attachment! (cond->
                                                          {:kind (or (not-empty (str kind)) "file")
                                                           :media-type (str media-type)
                                                           :base64 (str b64)
                                                           :size (long (or size 0))}
                                                          (not (str/blank? (str filename)))
                                                          (assoc :filename (str filename))))
                        (image-display-info (str media-type) (str b64))))))
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
     "Sandbox shim: vis_attach(path) / vis_attach_bytes(data, filename) — persist any artifact a tool produces (image/csv/json/pdf/wav/…) as a durable session_iteration_attachment DB row, captured at the source with no stdout parsing. Survives restart; image/* media-types replay to vision models cross-turn."
     :ext/version "0.1.0"
     :ext/author "Blockether"
     :ext/owner "vis"
     :ext/license "Apache-2.0"
     :ext/kind "foundation"
     :ext/sandbox-shims
     [{:shim/name "attach"
       :shim/globals ["vis_attach" "vis_attach_bytes" "vis_attachments" "vis_read_attachment"
                      "vis_reinspect_attachment"]
       :shim/description
       "vis_attach / vis_attach_bytes: persist a produced artifact as a durable iteration attachment (DB-owned bytes, media-type sniffed). vis-native helper — no upstream Python library."
       :shim/bindings attach-bridge-bindings
       :shim/source "vis-shims/attach.py"}]}))

(vis/register-extension! vis-extension)
