(ns com.blockether.vis.internal.foundation.mpl-capture
  "SINK for artifacts the sandbox PRODUCES — matplotlib figures (`plt.show()` /
   `plt.savefig()`) AND, generically, anything a tool hands to `vis_attach` — so the
   engine OWNS the bytes (a `session_iteration_attachment` row) captured AT THE
   SOURCE, with NO re-parsing of the model-facing stdout fence.

   The old flow rendered each figure to a `$TMPDIR/vis-mpl` temp file, printed a
   `vis-image` fence carrying just that PATH, then at persist time re-parsed the
   fence out of stdout and re-read the (possibly already-gone) file. We control the
   whole boundary, so that round-trip is gone: a producer renders/reads the bytes
   HOST-side (matplotlib's `__vis_mpl_render_file__` Java2D backend; `vis_attach`'s
   sandbox-confined `open`) and, right where it already holds the bytes, calls
   `record-attachment!`. `run-python-block` binds `*image-sink*` to a fresh
   collector around each block's eval and drains it into the block outcome's
   `:images`; the loop stamps each with the block's tool-call-id and hands them to
   `db-store-iteration!`'s `:attachments`. The stdout fence now serves ONLY the
   inline TUI/web display + ASCII fallback — never persistence.

   Deliberately dependency-free (no AWT, no `vis.core`): safe to require from BOTH
   a render shim and the hot engine loop without dragging Java2D or a require
   cycle.")

(def ^:dynamic *image-sink*
  "Per-block artifact collector: an atom holding a vector of attachment maps, bound
   by `run-python-block` around ONE block's eval (else nil). Producers append into
   it via `record-attachment!`; the block drains `@*image-sink*` into its `:images`."
  nil)

(defn record-attachment!
  "Append ONE produced-artifact attachment map to the active per-block `*image-sink*`
   (a silent no-op when unbound — e.g. a call outside a driven block). Shape mirrors
   ONE element of `db-store-iteration!`'s `:attachments`, minus `:tool-call-id`
   which the loop stamps from the block that produced it:
   `{:kind <\"image\"|\"file\"|…> :media-type <mime> :base64 <b64> :size <bytes>
     :filename <name> :dims <\"WxH\", images only>}`. NEVER throws — capture must not
   break a turn."
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
