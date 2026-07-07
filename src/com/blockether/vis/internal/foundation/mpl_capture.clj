(ns com.blockether.vis.internal.foundation.mpl-capture
  "SINK for image artifacts the sandbox PRODUCES (matplotlib `plt.show()` /
   `plt.savefig()`) — so the engine OWNS the bytes (a `session_iteration_attachment`
   row) captured AT THE SOURCE, with NO re-parsing of the model-facing stdout fence.

   The old flow rendered each figure to a `$TMPDIR/vis-mpl` temp file, printed a
   `vis-image` fence carrying just that PATH, then at persist time re-parsed the
   fence out of stdout and re-read the (possibly already-gone) file. We control the
   whole boundary, so that round-trip is gone: the matplotlib shim renders the PNG
   HOST-side (`__vis_mpl_render_file__`, Java2D) and, right where it already holds
   the bytes, calls `record-image!`. `run-python-block` binds `*image-sink*` to a
   fresh collector around each block's eval and drains it into the block outcome's
   `:images`; the loop stamps each with the block's tool-call-id and hands them to
   `db-store-iteration!`'s `:attachments`. The stdout fence now serves ONLY the
   inline TUI/web display + ASCII fallback — never persistence.

   Deliberately dependency-free (no AWT, no `vis.core`): safe to require from BOTH
   the render shim and the hot engine loop without dragging Java2D or a require
   cycle.")

(def ^:dynamic *image-sink*
  "Per-block image collector: an atom holding a vector of attachment maps, bound by
   `run-python-block` around ONE block's eval (else nil). The host render fn appends
   into it via `record-image!`; the block drains `@*image-sink*` into its `:images`."
  nil)

(defn record-image!
  "Append ONE produced-image attachment map to the active per-block `*image-sink*`
   (a silent no-op when unbound — e.g. a render call outside a driven block). Shape
   mirrors ONE element of `db-store-iteration!`'s `:attachments`, minus
   `:tool-call-id` which the loop stamps from the block that produced it:
   `{:kind \"image\" :media-type <mime> :base64 <b64> :size <bytes> :filename <name>
     :dims <\"WxH\">}`. NEVER throws — capture must not break a turn."
  [m]
  (when-let [sink *image-sink*]
    (try (swap! sink conj m) (catch Throwable _ nil)))
  nil)

(defn drain
  "The images collected in `sink` (an atom vector) as a plain vector, or nil when
   empty — the value `run-python-block` folds into a block outcome's `:images`."
  [sink]
  (not-empty (vec (some-> sink
                          deref))))
