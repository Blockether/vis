(ns com.blockether.vis.internal.fonttools-compat-shim-test
  "The fonttools-compat shim installed into every sandbox context via the generic
   sandbox-shim mechanism (`extension/sandbox-shims`): `fontTools.ttLib.woff2` and
   `brotli` modules published into `sys.modules` (so `import fontTools` /
   `import brotli` work). WOFF2 -> TTF via the vendored pure-Python brotlidecpy
   Brotli decoder plus an inlined WOFF2 reader. No host bridge."
  (:require [com.blockether.vis.internal.env-python :as ep]
            [lazytest.core :refer [defdescribe expect it]])
  (:import [org.graalvm.polyglot Context]))

(defn- ev [^Context c code] (ep/->clj (.eval c "python" code)))

;; A namespace-local context avoids paying GraalPy + shim bootstrap per assertion.
(defonce ^:private python-context* (delay (ep/create-python-context {})))

(defmacro with-python-context
  [& body]
  `(let [~(with-meta 'python-context {:tag `Context}) (:python-context @python-context*)]
     ~@body))

(defdescribe
  fonttools-module-test
  (it
    "publishes fontTools and brotli under sys.modules, exposing decompress"
    (with-python-context
      (expect
        (true?
          (ev
            python-context
            "import fontTools, brotli
__import__('sys').modules.get('fontTools') is not None and __import__('sys').modules.get('brotli') is not None and hasattr(fontTools.ttLib.woff2, 'decompress')"))))))

(defdescribe
  fonttools-woff2-roundtrip-test
  ;; Builds a minimal WOFF2 with one untransformed table over a store-mode Brotli
  ;; stream (round-trips through the vendored decoder), decompresses it, and
  ;; checks the rebuilt sfnt preserves the table bytes. Self-contained: no
  ;; network, no filesystem, no external font blob. It also asserts brotli is
  ;; decompress-only (compress raises NotImplementedError).
  (it
    "decodes brotli and rebuilds a valid sfnt from a WOFF2"
    (with-python-context
      (expect
        (true?
          (ev
            python-context
            "
def _run():
    import io, struct
    import fontTools, brotli
    from fontTools.ttLib.woff2 import decompress
    checks = []
    checks.append(hasattr(brotli, 'decompress'))
    checks.append(fontTools.ttLib.woff2.decompress is decompress)
    # brotli is decompress-only
    try:
        brotli.compress(bytes([1,2,3]))
        checks.append(False)
    except NotImplementedError:
        checks.append(True)
    # store-mode brotli stream (round-trips through the vendored decoder)
    def store(data):
        out = bytearray(); acc = 0; nacc = 0
        def wb(val, n):
            nonlocal acc, nacc
            for i in range(n):
                acc |= ((val >> i) & 1) << nacc; nacc += 1
                if nacc == 8:
                    out.append(acc); acc = 0; nacc = 0
        def align():
            nonlocal acc, nacc
            if nacc:
                out.append(acc); acc = 0; nacc = 0
        wb(0, 1)
        wb(0, 1); wb(0, 2); wb(len(data) - 1, 16); wb(1, 1); align()
        out.extend(data)
        wb(1, 1); wb(1, 1); align()
        return bytes(out)
    payload = bytes(range(60, 72))
    checks.append(brotli.decompress(store(payload)) == payload)
    # UIntBase128
    def u128(n):
        if n == 0:
            return bytes([0])
        b = bytearray()
        while n > 0:
            b.insert(0, n & 0x7f); n >>= 7
        for i in range(len(b) - 1):
            b[i] |= 0x80
        return bytes(b)
    # minimal woff2: 1 untransformed table (tag idx 0 = cmap, tv 0), store-compressed
    comp = store(payload)
    hdr = struct.pack('>4sIIHHIIHHIIIII', bytes([119, 79, 70, 50]),
                      0x00010000, 0, 1, 0, 0, len(comp), 0, 0, 0, 0, 0, 0, 0)
    woff2 = hdr + bytes([0x00]) + u128(len(payload)) + comp
    out = io.BytesIO(); decompress(io.BytesIO(woff2), out); ttf = out.getvalue()
    flavor, num = struct.unpack('>IH', ttf[:6])
    checks.append(flavor == 0x00010000 and num == 1)
    tag, csum, off, ln = struct.unpack('>4sLLL', ttf[12:28])
    checks.append(tag == bytes([99, 109, 97, 112]) and ttf[off:off + ln] == payload)
    return all(checks) and len(checks) == 6

_run()
"))))))

(defdescribe
  fonttools-unsupported-surface-test
  "The shim is a WOFF2 subset, so `from fontTools.ttLib import TTFont` used to fail
   with a bare `cannot import name`, which reads like a broken install. Imports now
   succeed and the unsupported entry points raise messages naming what IS covered."
  (it
    "TTFont imports and raises a NotImplementedError that names the supported surface"
    (with-python-context
      (expect
        (true?
          (ev
            python-context
            "
def _run():
    checks = []
    from fontTools.ttLib import TTFont, TTLibError
    checks.append(issubclass(TTLibError, Exception))
    try:
        TTFont('some.ttf')
        checks.append(False)
    except NotImplementedError as e:
        checks.append('woff2' in str(e) and 'TTFont' in str(e))
    from fontTools.ttLib.ttFont import TTFont as TTFont2
    checks.append(TTFont2 is TTFont)
    try:
        from fontTools.ttLib import somethingUnsupported
        checks.append(False)
    except ImportError as e:
        checks.append('woff2' in str(e))
    try:
        from fontTools import subset
        checks.append(False)
    except ImportError as e:
        checks.append('woff2' in str(e))
    import fontTools
    checks.append(hasattr(fontTools.ttLib.woff2, 'decompress'))
    return all(checks)
_run()
"))))))
