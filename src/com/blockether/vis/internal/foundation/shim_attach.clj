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

                dir
                (doto (java.io.File. (System/getProperty "java.io.tmpdir") "vis-attach") (.mkdirs))

                ext
                (or (some-> media-type
                            (str/split #"/")
                            second
                            (str/replace #"[^a-z0-9]" ""))
                    "img")

                f
                (java.io.File/createTempFile "att-" (str "." ext) dir)]

               (with-open [o (java.io.FileOutputStream. f)]
                 (.write o ^bytes bytes))
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
                          {})))))})

(def ^:private attach-shim-src
  "Pure-Python preamble defining `vis_attach(path)` and
   `vis_attach_bytes(data, filename)` as sandbox globals (so they show up in
   `apropos`/`doc` as tools, and are hidden from the live-vars view by the
   baseline snapshot). Both funnel through the host bridge
   `__vis_record_attachment__` (looked up in `globals()` at CALL time so the shim
   is backend-agnostic). Media-type is sniffed by magic bytes, then extension,
   then a utf-8 probe. INLINED so it ships in-jar with no separate `.py` resource.

   CONSTRAINT: this whole string is a Clojure double-quoted literal, so the Python
   below uses ONLY single-quoted string literals and NO backslash escapes (magic
   bytes are built with `bytes([...])`)."
  "# vis sandbox attachment shim: vis_attach / vis_attach_bytes.
#
# A tool that PRODUCES an artifact (image/csv/json/pdf/wav/...) persists it as a
# durable iteration attachment (a session_iteration_attachment DB row) so it
# survives a restart and, for image media-types, replays to a vision model. The
# bytes are read through the sandbox's own CONFINED open, so a path outside the
# filesystem roots raises the normal sandbox error.

def __vis_install_attach__():
    import os as _os
    import base64 as _b64

    def __vis_kind_for(mt):
        return 'image' if str(mt or '').startswith('image/') else 'file'

    def __vis_guess_media_type(name, data):
        head = bytes(data[:16])

        def starts(sig):
            s = bytes(sig)
            return head[:len(s)] == s

        if starts([0x89, 0x50, 0x4E, 0x47, 0x0D, 0x0A, 0x1A, 0x0A]):
            return 'image/png'
        if starts([0xFF, 0xD8, 0xFF]):
            return 'image/jpeg'
        if starts([0x47, 0x49, 0x46, 0x38]):
            return 'image/gif'
        if starts([0x42, 0x4D]):
            return 'image/bmp'
        if starts([0x25, 0x50, 0x44, 0x46]):
            return 'application/pdf'
        if starts([0x50, 0x4B, 0x03, 0x04]) or starts([0x50, 0x4B, 0x05, 0x06]):
            return 'application/zip'
        if starts([0x1F, 0x8B]):
            return 'application/gzip'
        if starts([0x52, 0x49, 0x46, 0x46]) and head[8:12] == bytes([0x57, 0x45, 0x42, 0x50]):
            return 'image/webp'
        if starts([0x52, 0x49, 0x46, 0x46]) and head[8:12] == bytes([0x57, 0x41, 0x56, 0x45]):
            return 'audio/wav'
        if starts([0x4F, 0x67, 0x67, 0x53]):
            return 'audio/ogg'
        if starts([0x49, 0x44, 0x33]) or starts([0xFF, 0xFB]):
            return 'audio/mpeg'
        import mimetypes
        mt = mimetypes.guess_type(str(name))[0]
        if mt:
            return mt
        try:
            bytes(data).decode('utf-8')
            return 'text/plain'
        except Exception:
            return 'application/octet-stream'

    def __vis_emit_image_fence(disp, name, mt, nbytes):
        # A `vis-image` fence (the same shape plt.show() emits): 5 header lines
        # (summary / host path / mime / WxH / size) a graphical TUI/web reads to
        # paint the picture inline, with the closing fence. No backslash escapes
        # in this shim, so the lines are joined with chr(10).
        try:
            path = str(disp[0])
            w = int(disp[1])
            h = int(disp[2])
        except Exception:
            return
        def _human(n):
            n = float(n)
            for unit in ('B', 'KB', 'MB'):
                if n < 1024.0 or unit == 'MB':
                    return (str(int(n)) + ' B') if unit == 'B' else ('%.1f %s' % (n, unit))
                n = n / 1024.0
        size = _human(nbytes)
        summary = '[Image: ' + str(name) + ' ' + str(w) + '×' + str(h) + ', ' + size + ']'
        fence = '`' * 4
        lines = [fence + 'vis-image', summary, path, str(mt),
                 str(w) + 'x' + str(h), size, fence]
        print(chr(10).join(lines))

    def vis_attach_bytes(data, filename, kind=None, media_type=None):
        if isinstance(data, str):
            data = data.encode('utf-8')
        data = bytes(data)
        name = str(filename) if filename else 'artifact'
        mt = media_type or __vis_guess_media_type(name, data)
        knd = kind or __vis_kind_for(mt)
        b64 = _b64.b64encode(data).decode('ascii')
        rec = globals().get('__vis_record_attachment__')
        if rec is None:
            raise RuntimeError('vis_attach: capture bridge not bound in this sandbox')
        env = rec(knd, mt, b64, name, len(data))
        if not env[0]:
            raise RuntimeError('vis_attach: ' + str(env[1]))
        disp = env[1] if len(env) > 1 else None
        if disp:
            __vis_emit_image_fence(disp, name, mt, len(data))
        return {'filename': name, 'media_type': mt, 'kind': knd, 'size': len(data)}

    def vis_attach(path, kind=None, media_type=None, filename=None):
        with open(path, 'rb') as f:
            data = f.read()
        name = filename or _os.path.basename(str(path)) or 'artifact'
        return vis_attach_bytes(data, name, kind=kind, media_type=media_type)

    def vis_attachments():
        lst = globals().get('__vis_list_attachments__')
        if lst is None:
            raise RuntimeError('vis_attachments: reader bridge not bound in this sandbox')
        env = lst()
        if not env[0]:
            raise RuntimeError('vis_attachments: ' + str(env[1]))
        import json as _json
        rows = _json.loads(env[1])
        return [{str(k).replace('-', '_'): v for k, v in r.items()} for r in rows]

    def vis_read_attachment(attachment_id):
        rd = globals().get('__vis_read_attachment__')
        if rd is None:
            raise RuntimeError('vis_read_attachment: reader bridge not bound in this sandbox')
        env = rd(str(attachment_id))
        if not env[0]:
            raise RuntimeError('vis_read_attachment: ' + str(env[1]))
        row = env[1]
        b64 = row[0]
        data = _b64.b64decode(b64) if b64 else None
        return {'bytes': data, 'media_type': row[1], 'filename': row[2],
                'kind': row[3], 'size': row[4], 'id': row[5], 'storage_uri': row[6]}

    vis_attach.__doc__ = (
        'Persist a file this tool produced as a durable iteration attachment. '
        'Reads path through the sandbox-confined filesystem (a path outside the '
        'roots raises), sniffs the media-type (magic bytes / extension / utf-8 '
        'probe), and hands the bytes to the engine so they land in the DB as a '
        'session_iteration_attachment row - surviving a web/TUI restart and, for '
        'image/* media-types, replayable to a vision model on later turns. '
        'kind / media_type / filename override the guesses. '
        'Returns a summary dict {filename, media_type, kind, size}. '
        'Use vis_attach_bytes(data, filename, ...) for in-memory bytes/str.'
    )
    vis_attach_bytes.__doc__ = (
        'Persist in-memory bytes (or a str, utf-8 encoded) as a durable iteration '
        'attachment - the no-temp-file twin of vis_attach. filename gives the '
        'artifact its name and drives extension-based media-type guessing. '
        'Returns a summary dict {filename, media_type, kind, size}.'
    )

    g = globals()
    g['vis_attach'] = vis_attach
    g['vis_attach_bytes'] = vis_attach_bytes
    g['vis_attachments'] = vis_attachments
    g['vis_read_attachment'] = vis_read_attachment
    g['__vis_guess_media_type'] = __vis_guess_media_type
    g['__vis_kind_for'] = __vis_kind_for

    docs = g.setdefault('__vis_docs__', {})
    docs['vis_attach'] = (
        'vis_attach(path, kind=None, media_type=None, filename=None): persist a '
        'produced file as a durable DB iteration attachment (survives restart; '
        'image/* replays to vision models).'
    )
    docs['vis_attach_bytes'] = (
        'vis_attach_bytes(data, filename, kind=None, media_type=None): persist '
        'in-memory bytes/str as a durable DB iteration attachment.'
    )
    docs['vis_attachments'] = (
        'vis_attachments(): list artifacts already persisted in THIS session '
        '(id, filename, media_type, kind, size, position, tool_call_id, '
        'iteration_id) - metadata only, newest turns included. Pick an id and '
        'read the bytes with vis_read_attachment(id).'
    )
    docs['vis_read_attachment'] = (
        'vis_read_attachment(id): fetch one persisted artifact by id as '
        '{bytes, media_type, filename, kind, size, id, storage_uri} - the '
        'read-back twin of vis_attach, so a tool can reuse an artifact it (or '
        'an earlier turn) produced.'
    )

__vis_install_attach__()
del __vis_install_attach__
")

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
       :shim/description
       "vis_attach / vis_attach_bytes: persist a produced artifact as a durable iteration attachment (DB-owned bytes, media-type sniffed). vis-native helper — no upstream Python library."
       :shim/bindings attach-bridge-bindings
       :shim/preamble attach-shim-src}]}))

(vis/register-extension! vis-extension)
