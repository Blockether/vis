# vis sandbox PIL/Pillow-compat shim, backed by the host com.blockether/imaging renderer.
#
# The agent sandbox ships no CPython Pillow wheel. This shim publishes a
# Pillow-compatible PIL package whose Image/ImageDraw/ImageFilter/ImageOps/
# ImageColor/ImageEnhance/ImageChops/ImageFont operations DELEGATE to host
# callables (__vis_pil_*), looked up in globals() at CALL time so the shim is
# backend-agnostic. Images live host-side as imaging rasters keyed by an integer
# handle; the Python Image is a thin wrapper. Published into sys.modules (so
# `from PIL import Image` works) and stapled onto builtins (so PIL.Image /
# Image.new work with NO import). Single-quoted string literals throughout so
# the enclosing Clojure string needs no escaping.


def __vis_install_pil__():
    import sys, types, base64, math, re, struct, os as _os, builtins as _builtins

    def _H(name, *args):
        if _draw_queue:
            _flush_draws()
        fn = globals().get(name)
        if fn is None:
            raise OSError("vis: the PIL host backend is not bound in this sandbox")
        ok, value = _cross(fn, name, args)
        if not ok and _out_of_memory(value) and _sweep(True):
            # The heap an image op exhausts is mostly rasters whose Python owners
            # are already unreachable, which the host cannot see. Freeing them and
            # retrying ONCE beats handing the block an error it cannot act on.
            ok, value = _cross(fn, name, args)
        if ok:
            return value
        raise OSError(str(value))

    def _cross(fn, name, args):
        try:
            env = fn(*args)
        except (KeyboardInterrupt, SystemExit):
            raise
        except OSError:
            raise
        except BaseException as _e:
            # Host failures (a foreign throwable, e.g. an arity mismatch when the
            # running binary is older than this shim) are NOT Python exceptions:
            # untranslated they escape every `except Exception:` a caller writes.
            raise OSError("vis: PIL host call " + str(name) + " failed: " + str(_e))
        try:
            return env[0], env[1]
        except (KeyboardInterrupt, SystemExit):
            raise
        except BaseException:
            raise OSError("vis: PIL host call " + str(name) + " returned no result")

    def _out_of_memory(value):
        text = str(value)
        return (
            "OutOfMemory" in text or "Java heap space" in text or "GC overhead" in text
        )

    def _lst(x):
        try:
            return list(x)
        except Exception:
            return x

    # -- host raster ownership -------------------------------------------------
    # A dropped Image frees NOTHING by itself: GraalPy does not refcount, and the
    # handle is a plain host id that OUTLIVES its owner, so the raster -- a packed
    # int[], 4 bytes per pixel, ~12 MB for one phone screenshot -- sat in the
    # per-JVM registry for the life of the process (measured: 20 handles still
    # live after 20 dropped `Image.new`s and a `gc.collect()`), and a loop over a
    # directory of screenshots walked the sandbox into `Java heap space`.
    #
    # Reclaiming it is NOT this shim's to invent: the runtime keeps ONE registry
    # for every host handle any shim hands out, with one sweep policy and one
    # boundary schedule (`vis-python/async_runtime.py`). All this shim says is how
    # a raster is freed and who owns one -- and `exif_transpose(in_place=True)`
    # genuinely lets two Images share a handle, so the runtime frees it with the
    # LAST of them, never the first.
    _KIND = "PIL.Image"

    def _rt(name):
        # The runtime's handle registry, resolved at CALL time in the sandbox
        # globals -- the same scope `_H` resolves the host bridge from -- with the
        # builtins mirror (`__vis_pin_runtime__`) as its second door.
        fn = globals().get(name)
        if fn is None:
            fn = getattr(_builtins, name, None)
        if fn is None:
            raise OSError("vis: the sandbox handle registry is missing " + str(name))
        return fn

    def _own(image):
        _rt("__vis_own__")(
            image, _KIND, image._handle, max(0, int(image._w) * int(image._h) * 4)
        )

    def _disown(image, handle):
        # Frees the raster host-side unless another Image still holds it.
        return _rt("__vis_disown__")(image, _KIND, handle)

    def _sweep(collect=False):
        return _rt("__vis_reclaim_handles__")(collect)

    _rt("__vis_handle_kind__")(_KIND, lambda h: _H("__vis_pil_free__", h))

    # -- ImageDraw batching --------------------------------------------------
    # Crossing to the host dominates a draw: marshalling ONE nested list+dict
    # costs ~35 us, a flat scalar record ~1 us. So draw ops are QUEUED here as
    # flat records -- name, n-coords, coords..., n-opts, key/value pairs, with
    # colours pre-packed as 0xAARRGGBB -- and a whole run crosses as ONE call.
    # `_H` flushes the queue first, so no other host call sees stale pixels.
    _draw_queue = {}
    _draw_queued = [0]
    _MAX_QUEUED_DRAWS = 4096

    def _flush_draws():
        if not _draw_queue:
            return
        batch = []
        for handle, entry in _draw_queue.items():
            batch.append(handle)
            batch.append(entry[0])
            batch.extend(entry[1])
        _draw_queue.clear()
        _draw_queued[0] = 0
        _H("__vis_pil_draws__", batch)

    def _queue_draw(handle, record):
        entry = _draw_queue.get(handle)
        if entry is None:
            entry = [0, []]
            _draw_queue[handle] = entry
        entry[0] += 1
        entry[1].extend(record)
        _draw_queued[0] += 1
        if _draw_queued[0] >= _MAX_QUEUED_DRAWS:
            _flush_draws()

    _color_cache = {}

    def _packcol(color, mode):
        # A colour RESOLVED in `mode` and packed into ONE 0xAARRGGBB scalar,
        # memoised: draw loops reuse a handful of colours, and packing here beats
        # marshalling a list per op.
        if color is None:
            return None
        try:
            key = (str(mode), color if isinstance(color, (str, int)) else tuple(color))
        except TypeError:
            key = None
        if key is not None:
            packed = _color_cache.get(key)
            if packed is not None:
                return packed
        r, g, b, a = _ink(color, mode)
        packed = ((a & 255) << 24) | ((r & 255) << 16) | ((g & 255) << 8) | (b & 255)
        if key is not None and len(_color_cache) < 1024:
            _color_cache[key] = packed
        return packed

    # -- resampling / transpose / mode constants -----------------------------
    NEAREST = 0
    LANCZOS = 1
    ANTIALIAS = 1
    BILINEAR = 2
    BICUBIC = 3
    BOX = 4
    HAMMING = 5

    class Resampling:
        NEAREST = 0
        LANCZOS = 1
        BILINEAR = 2
        BICUBIC = 3
        BOX = 4
        HAMMING = 5

    FLIP_LEFT_RIGHT = 0
    FLIP_TOP_BOTTOM = 1
    ROTATE_90 = 2
    ROTATE_180 = 3
    ROTATE_270 = 4
    TRANSPOSE = 5
    TRANSVERSE = 6

    class Transpose:
        FLIP_LEFT_RIGHT = 0
        FLIP_TOP_BOTTOM = 1
        ROTATE_90 = 2
        ROTATE_180 = 3
        ROTATE_270 = 4
        TRANSPOSE = 5
        TRANSVERSE = 6

    # -- quantize / dither / palette constants --------------------------------
    # Pillow stapes every enum member onto the module too, so `Image.MEDIANCUT`
    # and `Image.Quantize.MEDIANCUT` are the same value.
    class Quantize:
        MEDIANCUT = 0
        MAXCOVERAGE = 1
        FASTOCTREE = 2
        LIBIMAGEQUANT = 3

    MEDIANCUT = 0
    MAXCOVERAGE = 1
    FASTOCTREE = 2
    LIBIMAGEQUANT = 3

    class Dither:
        NONE = 0
        ORDERED = 1
        RASTERIZE = 2
        FLOYDSTEINBERG = 3

    NONE = 0
    ORDERED = 1
    RASTERIZE = 2
    FLOYDSTEINBERG = 3

    class Palette:
        WEB = 0
        ADAPTIVE = 1

    WEB = 0
    ADAPTIVE = 1

    # -- geometric-transform method constants -------------------------------
    AFFINE = 0
    EXTENT = 1
    PERSPECTIVE = 2
    QUAD = 3
    MESH = 4

    class Transform:
        AFFINE = 0
        EXTENT = 1
        PERSPECTIVE = 2
        QUAD = 3
        MESH = 4

    def _solve_linear(matrix, rhs):
        n = len(rhs)
        m = [matrix[i][:] + [rhs[i]] for i in range(n)]
        for col in range(n):
            piv = max(range(col, n), key=lambda r: abs(m[r][col]))
            m[col], m[piv] = m[piv], m[col]
            d = m[col][col] or 1e-12
            for j in range(col, n + 1):
                m[col][j] /= d
            for r in range(n):
                if r != col and m[r][col]:
                    fr = m[r][col]
                    for j in range(col, n + 1):
                        m[r][j] -= fr * m[col][j]
        return [m[i][n] for i in range(n)]

    def _quad_to_perspective(w, h, quad):
        sx = [float(quad[0]), float(quad[2]), float(quad[4]), float(quad[6])]
        sy = [float(quad[1]), float(quad[3]), float(quad[5]), float(quad[7])]
        tx = [0.0, 0.0, float(w), float(w)]
        ty = [0.0, float(h), float(h), 0.0]
        a = []
        b = []
        for i in range(4):
            a.append([tx[i], ty[i], 1.0, 0.0, 0.0, 0.0, -tx[i] * sx[i], -ty[i] * sx[i]])
            b.append(sx[i])
            a.append([0.0, 0.0, 0.0, tx[i], ty[i], 1.0, -tx[i] * sy[i], -ty[i] * sy[i]])
            b.append(sy[i])
        return _solve_linear(a, b)

    # -- ImageColor ----------------------------------------------------------
    _NAMED = {
        # The 147 CSS3 colour keywords plus CSS4's rebeccapurple, valued as the
        # W3C tables list them. Pillow answers every one of these, so a table
        # that knows only the popular fifty refuses names real code passes --
        # `darkslategray`, `lightgoldenrodyellow`, `rebeccapurple`.
        "aliceblue": (240, 248, 255),
        "antiquewhite": (250, 235, 215),
        "aqua": (0, 255, 255),
        "aquamarine": (127, 255, 212),
        "azure": (240, 255, 255),
        "beige": (245, 245, 220),
        "bisque": (255, 228, 196),
        "black": (0, 0, 0),
        "blanchedalmond": (255, 235, 205),
        "blue": (0, 0, 255),
        "blueviolet": (138, 43, 226),
        "brown": (165, 42, 42),
        "burlywood": (222, 184, 135),
        "cadetblue": (95, 158, 160),
        "chartreuse": (127, 255, 0),
        "chocolate": (210, 105, 30),
        "coral": (255, 127, 80),
        "cornflowerblue": (100, 149, 237),
        "cornsilk": (255, 248, 220),
        "crimson": (220, 20, 60),
        "cyan": (0, 255, 255),
        "darkblue": (0, 0, 139),
        "darkcyan": (0, 139, 139),
        "darkgoldenrod": (184, 134, 11),
        "darkgray": (169, 169, 169),
        "darkgreen": (0, 100, 0),
        "darkgrey": (169, 169, 169),
        "darkkhaki": (189, 183, 107),
        "darkmagenta": (139, 0, 139),
        "darkolivegreen": (85, 107, 47),
        "darkorange": (255, 140, 0),
        "darkorchid": (153, 50, 204),
        "darkred": (139, 0, 0),
        "darksalmon": (233, 150, 122),
        "darkseagreen": (143, 188, 143),
        "darkslateblue": (72, 61, 139),
        "darkslategray": (47, 79, 79),
        "darkslategrey": (47, 79, 79),
        "darkturquoise": (0, 206, 209),
        "darkviolet": (148, 0, 211),
        "deeppink": (255, 20, 147),
        "deepskyblue": (0, 191, 255),
        "dimgray": (105, 105, 105),
        "dimgrey": (105, 105, 105),
        "dodgerblue": (30, 144, 255),
        "firebrick": (178, 34, 34),
        "floralwhite": (255, 250, 240),
        "forestgreen": (34, 139, 34),
        "fuchsia": (255, 0, 255),
        "gainsboro": (220, 220, 220),
        "ghostwhite": (248, 248, 255),
        "gold": (255, 215, 0),
        "goldenrod": (218, 165, 32),
        "gray": (128, 128, 128),
        "green": (0, 128, 0),
        "greenyellow": (173, 255, 47),
        "grey": (128, 128, 128),
        "honeydew": (240, 255, 240),
        "hotpink": (255, 105, 180),
        "indianred": (205, 92, 92),
        "indigo": (75, 0, 130),
        "ivory": (255, 255, 240),
        "khaki": (240, 230, 140),
        "lavender": (230, 230, 250),
        "lavenderblush": (255, 240, 245),
        "lawngreen": (124, 252, 0),
        "lemonchiffon": (255, 250, 205),
        "lightblue": (173, 216, 230),
        "lightcoral": (240, 128, 128),
        "lightcyan": (224, 255, 255),
        "lightgoldenrodyellow": (250, 250, 210),
        "lightgray": (211, 211, 211),
        "lightgreen": (144, 238, 144),
        "lightgrey": (211, 211, 211),
        "lightpink": (255, 182, 193),
        "lightsalmon": (255, 160, 122),
        "lightseagreen": (32, 178, 170),
        "lightskyblue": (135, 206, 250),
        "lightslategray": (119, 136, 153),
        "lightslategrey": (119, 136, 153),
        "lightsteelblue": (176, 196, 222),
        "lightyellow": (255, 255, 224),
        "lime": (0, 255, 0),
        "limegreen": (50, 205, 50),
        "linen": (250, 240, 230),
        "magenta": (255, 0, 255),
        "maroon": (128, 0, 0),
        "mediumaquamarine": (102, 205, 170),
        "mediumblue": (0, 0, 205),
        "mediumorchid": (186, 85, 211),
        "mediumpurple": (147, 112, 219),
        "mediumseagreen": (60, 179, 113),
        "mediumslateblue": (123, 104, 238),
        "mediumspringgreen": (0, 250, 154),
        "mediumturquoise": (72, 209, 204),
        "mediumvioletred": (199, 21, 133),
        "midnightblue": (25, 25, 112),
        "mintcream": (245, 255, 250),
        "mistyrose": (255, 228, 225),
        "moccasin": (255, 228, 181),
        "navajowhite": (255, 222, 173),
        "navy": (0, 0, 128),
        "oldlace": (253, 245, 230),
        "olive": (128, 128, 0),
        "olivedrab": (107, 142, 35),
        "orange": (255, 165, 0),
        "orangered": (255, 69, 0),
        "orchid": (218, 112, 214),
        "palegoldenrod": (238, 232, 170),
        "palegreen": (152, 251, 152),
        "paleturquoise": (175, 238, 238),
        "palevioletred": (219, 112, 147),
        "papayawhip": (255, 239, 213),
        "peachpuff": (255, 218, 185),
        "peru": (205, 133, 63),
        "pink": (255, 192, 203),
        "plum": (221, 160, 221),
        "powderblue": (176, 224, 230),
        "purple": (128, 0, 128),
        "rebeccapurple": (102, 51, 153),
        "red": (255, 0, 0),
        "rosybrown": (188, 143, 143),
        "royalblue": (65, 105, 225),
        "saddlebrown": (139, 69, 19),
        "salmon": (250, 128, 114),
        "sandybrown": (244, 164, 96),
        "seagreen": (46, 139, 87),
        "seashell": (255, 245, 238),
        "sienna": (160, 82, 45),
        "silver": (192, 192, 192),
        "skyblue": (135, 206, 235),
        "slateblue": (106, 90, 205),
        "slategray": (112, 128, 144),
        "slategrey": (112, 128, 144),
        "snow": (255, 250, 250),
        "springgreen": (0, 255, 127),
        "steelblue": (70, 130, 180),
        "tan": (210, 180, 140),
        "teal": (0, 128, 128),
        "thistle": (216, 191, 216),
        "tomato": (255, 99, 71),
        "turquoise": (64, 224, 208),
        "violet": (238, 130, 238),
        "wheat": (245, 222, 179),
        "white": (255, 255, 255),
        "whitesmoke": (245, 245, 245),
        "yellow": (255, 255, 0),
        "yellowgreen": (154, 205, 50),
        # Not a CSS keyword: Pillow carries `transparent` as fully clear black,
        # and callers hand it straight to `Image.new`.
        "transparent": (0, 0, 0, 0),
    }

    def _getrgb(color):
        if isinstance(color, (list, tuple)):
            return tuple(int(c) for c in color)
        if isinstance(color, int):
            return (color, color, color)
        s = str(color).strip().lower()
        if s in _NAMED:
            return _NAMED[s]
        if s.startswith("#"):
            h = s[1:]
            if len(h) == 3:
                return tuple(int(c * 2, 16) for c in h)
            if len(h) == 4:
                return tuple(int(c * 2, 16) for c in h)
            if len(h) == 6:
                return (int(h[0:2], 16), int(h[2:4], 16), int(h[4:6], 16))
            if len(h) == 8:
                return (
                    int(h[0:2], 16),
                    int(h[2:4], 16),
                    int(h[4:6], 16),
                    int(h[6:8], 16),
                )
        if s.startswith("rgb"):
            inside = s[s.index("(") + 1 : s.index(")")]
            vals = []
            for p in inside.split(","):
                p = p.strip()
                if p.endswith("%"):
                    vals.append(int(round(float(p[:-1]) * 255 / 100)))
                else:
                    vals.append(int(float(p)))
            return tuple(vals)
        if s.startswith("hsl(") or s.startswith("hsv(") or s.startswith("hsb("):
            # CSS's own colour wheel, which Pillow accepts wherever a colour
            # string is accepted: `hsl(hue, saturation%, lightness%)` and the
            # `hsv`/`hsb` spelling of the same three numbers.
            import colorsys

            inside = s[s.index("(") + 1 : s.index(")")]
            parts = [q.strip().rstrip("%") for q in inside.split(",")]
            if len(parts) != 3:
                raise ValueError("unknown color specifier: " + repr(color))
            hue = (float(parts[0]) % 360.0) / 360.0
            second = float(parts[1]) / 100.0
            third = float(parts[2]) / 100.0
            if s.startswith("hsl("):
                rgb = colorsys.hls_to_rgb(hue, third, second)
            else:
                rgb = colorsys.hsv_to_rgb(hue, second, third)
            return tuple(int(v * 255.0 + 0.5) for v in rgb)
        raise ValueError("unknown color specifier: " + repr(color))

    def _getcolor(color, mode):
        # PIL's `ImageColor.getcolor`: a colour NAME resolved IN the target mode --
        # a grayscale-base mode answers the ITU-R 601-2 luma (scaled to 24 bits, the
        # way `convert` does it), and a mode whose last band is alpha keeps the
        # spec's alpha, 255 when the spec carried none.
        rgb = _getrgb(color)
        alpha = 255
        if len(rgb) == 4:
            rgb, alpha = rgb[:3], rgb[3]
        m = str(mode)
        if m in ("1", "L", "LA", "La", "I", "F"):
            r, g, b = rgb[0], rgb[1], rgb[2]
            lum = (r * 19595 + g * 38470 + b * 7471 + 0x8000) >> 16
            return (lum, alpha) if m[-1] == "A" else lum
        return tuple(rgb) + (alpha,) if m[-1] == "A" else tuple(rgb)

    def _ink_message(bands, is_tuple):
        if bands == 1:
            return "color must be int or single-element tuple"
        if not is_tuple:
            return "color must be int or tuple"
        if bands == 2:
            return "color must be int, or tuple of one or two elements"
        return "color must be int, or tuple of one, three or four elements"

    def _ink(color, mode, names=True):
        # PIL's `getink`: a colour SPEC becomes pixel bytes by the mode's BAND COUNT,
        # never by the four bytes the host happens to store. An INT is PIL's packed
        # 0xAABBGGRR compatibility form for a multi-band mode -- so `Image.new('RGB',
        # size, 255)` is RED and `Image.new('RGBA', size)` is TRANSPARENT, not opaque
        # black -- and a byte clipped to 0..255 for a single-band one. A colour NAME
        # is resolved in the target mode; `putpixel` resolves none, exactly as PIL
        # refuses one there. Answers [r, g, b, a], which the host packs.
        bands = _MODEBANDS.get(str(mode), 3)
        if isinstance(color, str):
            if not names:
                raise TypeError(_ink_message(bands, False))
            color = _getcolor(color, mode)
        if isinstance(color, (list, tuple)) and len(color) == 1:
            color = color[0]
        if isinstance(color, (list, tuple)):
            parts = [int(c) for c in color]
            n = len(parts)
            if bands == 1 or (bands == 2 and n != 2) or (bands > 2 and n not in (3, 4)):
                raise TypeError(_ink_message(bands, True))
            if bands == 2:
                v, a = parts
                return [v, v, v, a]
            a = parts[3] if n == 4 and bands == 4 else 255
            return [parts[0], parts[1], parts[2], a]
        try:
            v = int(color)
        except (TypeError, ValueError):
            raise TypeError(_ink_message(bands, False))
        if bands == 1:
            v = 0 if v < 0 else (255 if v > 255 else v)
            return [v, v, v, 255]
        r, g, b, a = v & 255, (v >> 8) & 255, (v >> 16) & 255, (v >> 24) & 255
        if bands == 2:
            return [r, r, r, a]
        return [r, g, b, a if bands == 4 else 255]

    ImageColor = types.ModuleType("PIL.ImageColor")
    ImageColor.getrgb = _getrgb
    ImageColor.getcolor = _getcolor
    ImageColor.colormap = dict(_NAMED)

    # A paste/composite mask is read from its ALPHA band ("RGBA"/"LA"), its gray
    # value ("L") or as a bitmap ("1"). Any other mode is PIL's own `bad
    # transparency mask`: refused, never blended by whichever band happens to be
    # there -- that silent blend washed every `im.paste(im, box, im)` composite.
    _MASK_MODES = ("1", "L", "LA", "RGBA")

    def _check_mask(mask):
        if mask is not None and getattr(mask, "mode", None) not in _MASK_MODES:
            raise ValueError("bad transparency mask")

    # -- the Image class -----------------------------------------------------
    def _wrap(meta):
        m = _lst(meta)
        return Image(int(m[0]), int(m[1]), int(m[2]), str(m[3]))

    class Image:
        def __init__(self, handle, w, h, mode):
            self._handle = int(handle)
            self._w = int(w)
            self._h = int(h)
            self.mode = str(mode)
            self.info = {}
            self.palette = None
            self.format = None
            self._pos = 0
            self._n_frames = 1
            self._delays = []
            self._exif = None
            _own(self)

        @property
        def size(self):
            return (self._w, self._h)

        @property
        def width(self):
            return self._w

        @property
        def height(self):
            return self._h

        def __repr__(self):
            return "<PIL.Image.Image mode=%s size=%dx%d at handle %d>" % (
                self.mode,
                self._w,
                self._h,
                self._handle,
            )

        def __enter__(self):
            return self

        def __exit__(self, *a):
            self.close()
            return False

        def _set(self, meta):
            m = _lst(meta)
            previous = self._handle
            self._handle, self._w, self._h, self.mode = (
                int(m[0]),
                int(m[1]),
                int(m[2]),
                str(m[3]),
            )
            if self._handle != previous:
                # An in-place op (`thumbnail`, `putdata`, `putpalette`, `putalpha`,
                # `frombytes`) REPLACES the raster: the old one is unreachable from
                # Python the moment this returns, so free it now rather than pin it
                # for as long as this image lives.
                _own(self)
                _disown(self, previous)
            return self

        def copy(self):
            return _wrap(_H("__vis_pil_copy__", self._handle))

        def close(self):
            # Idempotent, and never frees a raster another Image still holds.
            _disown(self, self._handle)

        def load(self):
            img = self

            class _Access:
                def __getitem__(self, xy):
                    return img.getpixel(xy)

                def __setitem__(self, xy, value):
                    img.putpixel(xy, value)

            return _Access()

        def resize(self, size, resample=BICUBIC, box=None, reducing_gap=None):
            w, h = size
            # `box` is the SOURCE rectangle Pillow scales FROM, not a hint: dropping
            # it scaled the whole image and answered the wrong picture in silence.
            src = self if box is None else self.crop(box)
            return _wrap(
                _H("__vis_pil_resize__", src._handle, int(w), int(h), int(resample))
            )

        def thumbnail(self, size, resample=BICUBIC, reducing_gap=2.0):
            # Pillow's own aspect arithmetic: an image that ALREADY fits the box is
            # left untouched, and the fitted pair is the rounding whose ratio is
            # closest to the source's -- never zero. `reducing_gap` only trades
            # speed for accuracy in Pillow, so it is accepted and ignored here.
            def round_aspect(number, key):
                return max(min(math.floor(number), math.ceil(number), key=key), 1)

            x, y = (int(math.floor(v)) for v in size)
            w, h = self._w, self._h
            if x >= w and y >= h:
                return None
            aspect = w / h
            if x / y >= aspect:
                x = round_aspect(y * aspect, key=lambda n: abs(aspect - n / y))
            else:
                y = round_aspect(
                    x / aspect, key=lambda n: 0 if n == 0 else abs(aspect - x / n)
                )
            if (x, y) != (w, h):
                self._set(_H("__vis_pil_resize__", self._handle, x, y, int(resample)))
            return None

        def crop(self, box):
            l, t, r, b = box
            return _wrap(
                _H("__vis_pil_crop__", self._handle, int(l), int(t), int(r), int(b))
            )

        def rotate(
            self,
            angle,
            resample=NEAREST,
            expand=0,
            center=None,
            translate=None,
            fillcolor=None,
        ):
            fc = None if fillcolor is None else _ink(fillcolor, self.mode)
            if center is None and not translate:
                return _wrap(
                    _H(
                        "__vis_pil_rotate__",
                        self._handle,
                        float(angle),
                        bool(expand),
                        fc,
                    )
                )
            # A rotation about any point OTHER than the centre is Pillow's own affine
            # composition -- the host op only knows the centred one, and ignoring
            # `center`/`translate` answered a differently-placed picture in silence.
            w, h = self._w, self._h
            cx, cy = (w / 2.0, h / 2.0) if center is None else (center[0], center[1])
            tx, ty = (0.0, 0.0) if not translate else (translate[0], translate[1])
            a = -math.radians(float(angle))
            m = [math.cos(a), math.sin(a), 0.0, -math.sin(a), math.cos(a), 0.0]

            def _xf(x, y):
                return (m[0] * x + m[1] * y + m[2], m[3] * x + m[4] * y + m[5])

            m[2], m[5] = _xf(-cx - tx, -cy - ty)
            m[2] += cx
            m[5] += cy
            if expand:
                xs, ys = [], []
                for px, py in ((0, 0), (w, 0), (w, h), (0, h)):
                    nx, ny = _xf(px, py)
                    xs.append(nx)
                    ys.append(ny)
                nw = int(math.ceil(max(xs)) - math.floor(min(xs)))
                nh = int(math.ceil(max(ys)) - math.floor(min(ys)))
                m[2] += (nw - w) / 2.0
                m[5] += (nh - h) / 2.0
                w, h = nw, nh
            return self.transform((w, h), AFFINE, m, resample, fillcolor=fillcolor)

        def transpose(self, method):
            return _wrap(_H("__vis_pil_transpose__", self._handle, int(method)))

        def convert(self, mode=None, matrix=None, dither=None, palette=WEB, colors=256):
            if mode is None or (str(mode) == self.mode and matrix is None):
                return self.copy()
            if str(mode) not in (
                "1",
                "L",
                "LA",
                "La",
                "I",
                "F",
                "P",
                "RGB",
                "RGBA",
                "RGBX",
                "RGBa",
                "CMYK",
                "YCbCr",
                "HSV",
                "I;16",
                "I;16B",
            ):
                raise ValueError(
                    "conversion from "
                    + str(self.mode)
                    + " to "
                    + str(mode)
                    + " not supported"
                )
            if matrix is not None:
                return self._convert_matrix(str(mode), matrix)
            if str(mode) == "P" and int(colors) < 256:
                # Pillow's `colors` caps the adaptive palette; the host quantiser
                # always built 256, so an 8-colour request came back with 256.
                return self.quantize(colors=int(colors))
            out = _wrap(
                _H(
                    "__vis_pil_convert__",
                    self._handle,
                    str(mode),
                    FLOYDSTEINBERG if dither is None else int(dither),
                )
            )
            return _attach_palette(out) if out.mode == "P" else out

        def _convert_matrix(self, mode, matrix):
            """Pillow's `convert(mode, matrix)`: a 4- or 12-entry colour matrix over
            an RGB source. It used to be swallowed with the rest of `**k`, so the
            caller got an ordinary luminance conversion and never knew."""
            if self.mode != "RGB":
                raise ValueError("illegal conversion")
            data = self.getdata()
            if len(matrix) == 4:
                a, b, c, d = [float(v) for v in matrix]
                res = new("L", self.size)
                res.putdata([_clip255(a * r + b * g + c * bl + d) for r, g, bl in data])
                return res if mode == "L" else res.convert(mode)
            if len(matrix) == 12:
                m = [float(v) for v in matrix]
                px = []
                for r, g, bl in data:
                    px.append(
                        tuple(
                            _clip255(m[i] * r + m[i + 1] * g + m[i + 2] * bl + m[i + 3])
                            for i in (0, 4, 8)
                        )
                    )
                res = new("RGB", self.size)
                res.putdata(px)
                return res if mode == "RGB" else res.convert(mode)
            raise ValueError("wrong number of matrix entries")

        def getpixel(self, xy):
            x, y = int(xy[0]), int(xy[1])
            if not (0 <= x < self._w and 0 <= y < self._h):
                # Pillow's own refusal for an off-canvas read; the host used to leak
                # a Java bounds message wrapped in OSError.
                raise IndexError("image index out of range")
            v = _H("__vis_pil_getpixel__", self._handle, x, y)
            if isinstance(v, (list, tuple)):
                return tuple(int(c) for c in v)
            return float(v) if self.mode == "F" else int(v)

        def putpixel(self, xy, value):
            _H(
                "__vis_pil_putpixel__",
                self._handle,
                int(xy[0]),
                int(xy[1]),
                _ink(value, self.mode, False),
            )

        def paste(self, im, box=None, mask=None):
            if not isinstance(im, Image):
                # color paste: fill the region (box) with a solid colour
                if box is None:
                    box = (0, 0, self._w, self._h)
                if len(box) == 2:
                    box = (box[0], box[1], self._w, self._h)
                tmp = new(self.mode, (box[2] - box[0], box[3] - box[1]), im)
                im = tmp
                box = (box[0], box[1])
            _check_mask(mask)
            x, y = 0, 0
            if box is not None:
                x, y = int(box[0]), int(box[1])
            mh = mask._handle if isinstance(mask, Image) else -1
            _H("__vis_pil_paste__", self._handle, im._handle, x, y, int(mh))

        def save(self, fp, format=None, **kw):
            # A path is a path in ANY spelling: an os.PathLike (pathlib.Path,
            # os.DirEntry) becomes its filesystem string HERE, so the extension
            # still picks the format and the bytes still reach the file. Left an
            # object it fell to the file-object branch and died on `.write`.
            if hasattr(fp, "__fspath__"):
                fp = _os.fspath(fp)
            fmt = (format or "").upper()
            name = fp if isinstance(fp, str) else getattr(fp, "name", "")
            if not fmt and isinstance(name, str) and "." in name:
                fmt = name.rsplit(".", 1)[1].upper()
            if not fmt:
                fmt = "PNG"
            quality = kw.get("quality")
            optimize = bool(kw.get("optimize"))
            if isinstance(quality, str):
                quality = _QUALITY_PRESETS.get(quality.lower())
            if quality is not None:
                quality = max(1, min(100, int(quality)))
            if kw.get("save_all"):
                # one multi-frame file: this image is frame 0, append_images the rest.
                handles = [self._handle] + [
                    im._handle for im in (kw.get("append_images") or [])
                ]
                duration = kw.get("duration", self.info.get("duration"))
                if isinstance(duration, (list, tuple)):
                    duration = [int(d) for d in duration]
                elif duration is not None:
                    duration = int(duration)
                loop = kw.get("loop", self.info.get("loop"))
                b64 = _H(
                    "__vis_pil_save_all__",
                    handles,
                    fmt,
                    duration,
                    None if loop is None else int(loop),
                    optimize,
                )
            else:
                b64 = _H("__vis_pil_save__", self._handle, fmt, quality, optimize)
            data = base64.b64decode(b64)
            if hasattr(fp, "write"):
                fp.write(data)
            else:
                with open(fp, "wb") as f:
                    f.write(data)

        def show(self, title=None, **kw):
            try:
                _H("__vis_pil_save_temp__", self._handle, "PNG")
            except Exception:
                pass

        def _rawbytes(self):
            """The host raster's samples, ONE byte each -- what the shim's own readers
            want. `tobytes` reshapes these into Pillow's public layout."""
            return base64.b64decode(_H("__vis_pil_tobytes__", self._handle))

        def tobytes(self, encoder_name="raw", *args):
            # Pillow's buffer layout is NOT one byte per sample for every mode: I and
            # F carry 32-bit samples, so a raw host buffer handed straight back here
            # was a quarter of the length its own mode promises. ('1' is already
            # bit-packed by the host.)
            return _pack_mode_bytes(self.mode, self._rawbytes(), self._w, self._h)

        def tobitmap(self, name="image"):
            if self.mode != "1":
                raise ValueError("not a bitmap")
            px = self.getdata()
            stride = (self._w + 7) // 8
            data = bytearray(stride * self._h)
            for y in range(self._h):
                for x in range(self._w):
                    if px[y * self._w + x]:
                        # XBM packs LSB first, unlike a '1' raster's own bytes.
                        data[y * stride + (x >> 3)] |= 1 << (x & 7)
            return (
                "#define %s_width %d\n#define %s_height %d\n"
                "static char %s_bits[] = {%s};\n"
                % (
                    name,
                    self._w,
                    name,
                    self._h,
                    name,
                    ",".join("0x%02x" % b for b in data),
                )
            ).encode("ascii")

        def getdata(self, band=None):
            if self.mode == "1":
                # a bilevel image's BYTES are bit-packed, but its PIXELS are the raw
                # values PIL stored (`Image.new('1', size, 1)` reads back as 1, not
                # 255), so read the band, never `tobytes`.
                return list(self.split()[0]._rawbytes())
            raw = self._rawbytes()
            if self.mode == "F":
                # an F band is FLOAT-valued in Pillow, even where the sample fits a byte.
                return [float(v) for v in raw]
            if self.mode in ("L", "I", "P"):
                data = list(raw)
            elif self.mode == "LA":
                # TWO bands, not the four the host raster stores: Pillow's (L, A).
                data = [tuple(raw[i : i + 2]) for i in range(0, len(raw), 2)]
            elif self.mode in ("RGBA", "CMYK"):
                data = [tuple(raw[i : i + 4]) for i in range(0, len(raw), 4)]
            else:
                data = [tuple(raw[i : i + 3]) for i in range(0, len(raw), 3)]
            if band is not None:
                return [px[band] for px in data]
            return data

        def __array__(self, dtype=None):
            w, h = self._w, self._h
            data = self.getdata()
            if self.mode in ("L", "1", "I", "F", "P"):
                rows = [list(data[y * w : (y + 1) * w]) for y in range(h)]
            else:
                rows = [
                    [list(px) for px in data[y * w : (y + 1) * w]] for y in range(h)
                ]
            return rows

        def putdata(self, data, scale=1.0, offset=0.0):
            ba = bytearray()
            for px in data:
                if isinstance(px, (list, tuple)):
                    for c in px:
                        ba.append(int(c * scale + offset) & 255)
                else:
                    ba.append(int(px * scale + offset) & 255)
            b64 = base64.b64encode(bytes(ba)).decode("ascii")
            self._set(_H("__vis_pil_frombytes__", self.mode, self._w, self._h, b64))

        def point(self, lut, mode=None):
            nb = len(self.getbands())
            if callable(lut):
                if self.mode in ("I", "F"):
                    # Pillow cannot table a 32-bit band: it reads a LINEAR transform
                    # off the callable instead. Tabulating it clamped I/F at 255.
                    base = lut(0)
                    step = lut(1) - base
                    lut = [step * i + base for i in range(256)]
                else:
                    lut = [lut(i) for i in range(256)]
                # Pillow tables a callable once PER BAND.
                lut = list(lut) * nb
            lut = [int(v) for v in lut]
            if str(mode) == "1":
                # `point(f, "1")` is Pillow's threshold idiom (`v > n and 255`): the
                # LUT's own truthiness decides the bit, not a luminance conversion.
                lut = [255 if v else 0 for v in lut]
            if len(lut) != 256 * nb:
                # Pillow's own C error: 256 entries for EVERY band, no padding.
                raise ValueError("wrong number of lut entries")
            if nb > 1 and any(
                lut[b * 256 : (b + 1) * 256] != lut[:256] for b in range(1, nb)
            ):
                # Per-BAND tables: the host op carries one table, so the bands are
                # mapped separately and merged back. Truncating to the first 256
                # silently applied the red table to green and blue as well.
                parts = [
                    band.point(lut[i * 256 : (i + 1) * 256])
                    for i, band in enumerate(self.split())
                ]
                res = merge(self.mode, parts)
            else:
                res = _wrap(_H("__vis_pil_point__", self._handle, lut[:256]))
            # Pillow's second argument is the mode the RESULT is read in; it used to
            # be accepted and dropped, answering the source mode.
            return res if mode is None or str(mode) == res.mode else res.convert(mode)

        def histogram(self, mask=None, extrema=None):
            # PIL's mask rule: with a mask, ONLY the pixels it marks non-zero are
            # counted -- what `ImageStat.Stat(im, mask)` measures. The mask used to
            # be dropped, so every masked statistic described the whole image.
            return [
                int(x)
                for x in _lst(
                    _H(
                        "__vis_pil_histogram__",
                        self._handle,
                        None if mask is None else mask._handle,
                    )
                )
            ]

        def getbbox(self, *a, **k):
            v = _H("__vis_pil_getbbox__", self._handle)
            if v is None:
                return None
            v = _lst(v)
            return (int(v[0]), int(v[1]), int(v[2]), int(v[3]))

        def getextrema(self):
            hist = self.histogram()
            out = []
            nb = len(hist) // 256
            for c in range(nb):
                band = hist[c * 256 : (c + 1) * 256]
                lo = next((i for i in range(256) if band[i] > 0), 0)
                hi = next((i for i in range(255, -1, -1) if band[i] > 0), 0)
                out.append((lo, hi))
            return out[0] if len(out) == 1 else tuple(out)

        def getbands(self):
            return _MODEBANDNAMES.get(self.mode, ("L",))

        def split(self):
            res = _lst(_H("__vis_pil_split__", self._handle))
            out = [_wrap(m) for m in res]
            if self.mode in ("I", "F") and len(out) == 1:
                # a single-band I/F image splits into ITSELF, never into an L band.
                out[0].mode = self.mode
            return tuple(out)

        def getchannel(self, channel):
            bands = self.getbands()
            if isinstance(channel, str):
                channel = bands.index(channel)
            return self.split()[channel]

        def putalpha(self, alpha):
            if isinstance(alpha, Image):
                a = alpha if alpha.mode == "L" else alpha.convert("L")
            else:
                a = new("L", self.size, int(alpha))
            if self.mode in ("L", "LA"):
                # a gray image keeps its gray band -- L/LA plus alpha is LA, not RGBA.
                gray = self if self.mode == "L" else self.split()[0]
                self._set(_H("__vis_pil_merge__", "LA", [gray._handle, a._handle]))
                return
            rgb = self if self.mode == "RGB" else self.convert("RGB")
            r, g, b = rgb.split()
            self._set(
                _H(
                    "__vis_pil_merge__",
                    "RGBA",
                    [r._handle, g._handle, b._handle, a._handle],
                )
            )

        def filter(self, filt):
            if isinstance(filt, type):
                filt = filt()
            return filt.filter(self)

        def transform(
            self, size, method, data=None, resample=NEAREST, fill=1, fillcolor=None
        ):
            if hasattr(method, "method") and hasattr(method, "data"):
                data = method.data
                method = method.method
            w, h = int(size[0]), int(size[1])
            fc = None if fillcolor is None else _ink(fillcolor, self.mode)
            if method == EXTENT:
                x0, y0, x1, y1 = data
                sx = (x1 - x0) / float(w) if w else 1.0
                sy = (y1 - y0) / float(h) if h else 1.0
                return _wrap(
                    _H(
                        "__vis_pil_transform__",
                        self._handle,
                        w,
                        h,
                        "AFFINE",
                        [sx, 0.0, float(x0), 0.0, sy, float(y0)],
                        fc,
                    )
                )
            if method == AFFINE:
                return _wrap(
                    _H(
                        "__vis_pil_transform__",
                        self._handle,
                        w,
                        h,
                        "AFFINE",
                        [float(c) for c in data],
                        fc,
                    )
                )
            if method == PERSPECTIVE:
                return _wrap(
                    _H(
                        "__vis_pil_transform__",
                        self._handle,
                        w,
                        h,
                        "PERSPECTIVE",
                        [float(c) for c in data],
                        fc,
                    )
                )
            if method == QUAD:
                return _wrap(
                    _H(
                        "__vis_pil_transform__",
                        self._handle,
                        w,
                        h,
                        "PERSPECTIVE",
                        _quad_to_perspective(w, h, data),
                        fc,
                    )
                )
            if method == MESH:
                out = new(self.mode, (w, h), fillcolor if fillcolor is not None else 0)
                for box, quad in data:
                    bx0, by0, bx1, by1 = [int(v) for v in box]
                    bw, bh = bx1 - bx0, by1 - by0
                    if bw <= 0 or bh <= 0:
                        continue
                    coeffs = _quad_to_perspective(bw, bh, quad)
                    piece = _wrap(
                        _H(
                            "__vis_pil_transform__",
                            self._handle,
                            bw,
                            bh,
                            "PERSPECTIVE",
                            coeffs,
                            fc,
                        )
                    )
                    out.paste(piece, (bx0, by0))
                return out
            raise ValueError("unsupported transform method: %r" % (method,))

        def reduce(self, factor, box=None):
            if isinstance(factor, (tuple, list)):
                fx, fy = int(factor[0]), int(factor[1])
            else:
                fx = fy = int(factor)
            # `box` names the source region, exactly as it does for `resize`.
            src = self if box is None else self.crop(box)
            nw = max(1, src._w // max(1, fx))
            nh = max(1, src._h // max(1, fy))
            return _wrap(_H("__vis_pil_resize__", src._handle, nw, nh, BOX))

        def alpha_composite(self, im, dest=(0, 0), source=(0, 0)):
            if self.mode != "RGBA" or im.mode != "RGBA":
                # Pillow refuses; the host happily composited whatever it was handed.
                raise ValueError("image mode must be RGBA")
            src = im
            if tuple(source) != (0, 0):
                if len(source) == 2:
                    sbox = (source[0], source[1], im._w, im._h)
                else:
                    sbox = source
                src = im.crop(sbox)
            self._set(
                _H(
                    "__vis_pil_alpha_composite__",
                    self._handle,
                    src._handle,
                    int(dest[0]),
                    int(dest[1]),
                )
            )

        def entropy(self, mask=None, extrema=None):
            hist = self.histogram()
            total = float(sum(hist)) or 1.0
            ent = 0.0
            for c in hist:
                if c > 0:
                    p = c / total
                    ent -= p * math.log(p, 2)
            return ent

        def getprojection(self):
            w, h = self._w, self._h
            g = self if self.mode == "L" else self.convert("L")
            data = g.getdata()
            xp = [0] * w
            yp = [0] * h
            for y in range(h):
                row = data[y * w : (y + 1) * w]
                for x in range(w):
                    if row[x]:
                        xp[x] = 1
                        yp[y] = 1
            return (xp, yp)

        def getcolors(self, maxcolors=256):
            counts = {}
            for px in self.getdata():
                counts[px] = counts.get(px, 0) + 1
                if len(counts) > maxcolors:
                    return None
            return [(v, k) for k, v in counts.items()]

        def getpalette(self, rawmode="RGB"):
            pal = _H("__vis_pil_getpalette__", self._handle)
            pal = _lst(pal) if pal is not None else None
            return [int(v) for v in pal] if pal else None

        def putpalette(self, data, rawmode="RGB"):
            if hasattr(data, "palette"):
                data = data.palette
            flat = [int(v) for v in _lst(data)]
            self._set(_H("__vis_pil_putpalette__", self._handle, flat))
            self.palette = _Palette("RGB", flat)

        def remap_palette(self, dest_map, source_palette=None):
            return self.copy()

        def quantize(self, colors=256, method=None, kmeans=0, palette=None, dither=1):
            out = _wrap(
                _H("__vis_pil_quantize__", self._handle, int(colors), bool(dither))
            )
            return _attach_palette(out)

        def apply_transparency(self):
            return None

        def draft(self, mode, size):
            return None

        def verify(self):
            return None

        def seek(self, frame):
            frame = int(frame)
            if frame == self.tell():
                return
            if frame < 0 or frame >= self.n_frames:
                raise EOFError("attempt to seek beyond the last frame")
            self._set(_H("__vis_pil_seek__", self._handle, frame))
            self._pos = frame
            delays = getattr(self, "_delays", None)
            if delays:
                self.info["duration"] = delays[frame]

        def tell(self):
            return int(getattr(self, "_pos", 0))

        @property
        def n_frames(self):
            return int(getattr(self, "_n_frames", 1) or 1)

        @property
        def is_animated(self):
            return self.n_frames > 1

        def getexif(self):
            ex = getattr(self, "_exif", None)
            if ex is None:
                blob = self.info.get("exif") or getattr(self, "_raw", None)
                top, ifds = _parse_exif_tiff(_exif_tiff_block(blob) or b"")
                ex = Exif(top, ifds)
                self._exif = ex
            return ex

        def _getexif(self):
            # the legacy accessor is FLAT: sub-IFD tags merged into one dict.
            ex = self.getexif()
            merged = dict(ex)
            for sub in ex._ifds.values():
                merged.update(sub)
            return merged or None

        def effect_spread(self, distance):
            """Pillow's random pixel spread: every pixel takes the value of one drawn
            from its `distance` neighbourhood. This used to answer an untouched copy,
            so the effect silently did nothing."""
            import random

            d = max(0, int(distance))
            w, h = self._w, self._h
            data = list(self.getdata())
            out = list(data)
            for y in range(h):
                for x in range(w):
                    sx = min(w - 1, max(0, x + random.randint(-d, d)))
                    sy = min(h - 1, max(0, y + random.randint(-d, d)))
                    out[y * w + x] = data[sy * w + sx]
            res = self.copy()
            res.putdata(out)
            return res

        def frombytes(self, data, decoder_name="raw", *args):
            raw = _unpack_mode_bytes(self.mode, bytes(data), self._w, self._h)
            b64 = base64.b64encode(raw).decode("ascii")
            self._set(_H("__vis_pil_frombytes__", self.mode, self._w, self._h, b64))

    # -- module-level Image constructors ------------------------------------
    def new(mode, size, color=0):
        _check_size(size)
        if str(mode) not in _MODEBANDS:
            # Pillow refuses an unknown mode; the host used to build a raster for
            # any string at all, so a typo produced a picture nobody could read.
            raise ValueError("unrecognized image mode")
        w, h = size
        fill = None if color is None else _ink(color, mode)
        return _wrap(_H("__vis_pil_new__", str(mode), int(w), int(h), fill))

    # -- palette / quality / Exif helpers ------------------------------------
    _QUALITY_PRESETS = {
        "web_low": 10,
        "web_medium": 30,
        "web_high": 60,
        "web_very_high": 85,
        "web_maximum": 95,
        "low": 10,
        "medium": 30,
        "high": 60,
        "maximum": 95,
    }

    def _attach_palette(im):
        pal = im.getpalette()
        if pal:
            im.palette = _Palette("RGB", list(pal))
        return im

    # type -> (bytes per component, struct code); None means "decode by hand".
    _EXIF_FMT = {
        1: (1, "B"),
        2: (1, None),
        3: (2, "H"),
        4: (4, "I"),
        5: (8, None),
        6: (1, "b"),
        7: (1, None),
        8: (2, "h"),
        9: (4, "i"),
        10: (8, None),
        11: (4, "f"),
        12: (8, "d"),
    }

    def _exif_tiff_block(data):
        """The raw TIFF block carrying an image's Exif, or None: from a JPEG APP1
        segment, a PNG eXIf chunk, a bare TIFF file, or an already-extracted
        Exif-prefixed blob (what Pillow keeps in info['exif'])."""
        if not data:
            return None
        data = bytes(data)
        if data[:6] == b"Exif\x00\x00":
            return data[6:]
        if data[:3] == bytes([255, 216, 255]):
            i, n = 2, len(data)
            while i + 4 <= n and data[i] == 0xFF:
                marker = data[i + 1]
                if marker in (0xD8, 0x01) or 0xD0 <= marker <= 0xD7:
                    i += 2
                    continue
                if marker == 0xDA:  # start of scan: no metadata past here
                    break
                seglen = (data[i + 2] << 8) | data[i + 3]
                if seglen < 2:
                    break
                seg = data[i + 4 : i + 2 + seglen]
                if marker == 0xE1 and seg[:6] == b"Exif\x00\x00":
                    return seg[6:]
                i += 2 + seglen
            return None
        if data[:8] == bytes([137, 80, 78, 71, 13, 10, 26, 10]):
            i, n = 8, len(data)
            while i + 8 <= n:
                ln = int.from_bytes(data[i : i + 4], "big")
                kind = data[i + 4 : i + 8]
                if kind == b"eXIf":
                    return data[i + 8 : i + 8 + ln]
                if kind == b"IEND":
                    break
                i += 12 + ln
            return None
        if data[:2] in (b"II", b"MM"):
            return data
        return None

    def _exif_value(order, typ, count, raw):
        if typ == 2:
            return raw.split(b"\x00")[0].decode("utf-8", "replace")
        if typ == 7:
            return bytes(raw)
        if typ in (5, 10):
            c = "I" if typ == 5 else "i"
            nums = struct.unpack(order + c * (2 * count), raw[: 8 * count])
            vals = tuple(
                (nums[i * 2] / nums[i * 2 + 1]) if nums[i * 2 + 1] else 0.0
                for i in range(count)
            )
            return vals[0] if count == 1 else vals
        code = _EXIF_FMT.get(typ, (0, None))[1]
        if code is None:
            return bytes(raw)
        fmt = order + code * count
        vals = struct.unpack(fmt, raw[: struct.calcsize(fmt)])
        return vals[0] if count == 1 else vals

    def _parse_exif_tiff(tiff):
        """(top-level tags, {pointer-tag: sub-IFD tags}) of one Exif TIFF block."""
        if not tiff or len(tiff) < 8:
            return {}, {}
        head = bytes(tiff[:2])
        order = "<" if head == b"II" else ">" if head == b"MM" else None
        if order is None or struct.unpack(order + "H", tiff[2:4])[0] != 42:
            return {}, {}
        top, ifds, seen = {}, {}, set()

        def rd(off, target, depth):
            if off <= 0 or off + 2 > len(tiff) or off in seen or depth > 4:
                return
            seen.add(off)
            (count,) = struct.unpack(order + "H", tiff[off : off + 2])
            p = off + 2
            for _ in range(count):
                if p + 12 > len(tiff):
                    return
                tag, typ, n = struct.unpack(order + "HHI", tiff[p : p + 8])
                size = _EXIF_FMT.get(typ, (0, None))[0]
                p += 12
                if not size or n > 0x10000:
                    continue
                nb = size * n
                if nb <= 4:
                    raw = tiff[p - 4 : p - 4 + nb]
                else:
                    (vo,) = struct.unpack(order + "I", tiff[p - 4 : p])
                    raw = tiff[vo : vo + nb]
                if len(raw) < nb:
                    continue
                try:
                    v = _exif_value(order, typ, n, raw)
                except Exception:
                    continue
                target[tag] = v
                if tag in (0x8769, 0x8825, 0xA005) and isinstance(v, int):
                    sub = {}
                    rd(v, sub, depth + 1)
                    ifds[tag] = sub
            if p + 4 <= len(tiff):
                (nxt,) = struct.unpack(order + "I", tiff[p : p + 4])
                if nxt:
                    rd(nxt, target, depth + 1)

        (first,) = struct.unpack(order + "I", tiff[4:8])
        rd(first, top, 0)
        return top, ifds

    class Exif(dict):
        """Pillow's `Image.Exif`: tag -> value, plus `get_ifd` for the sub-IFDs."""

        def __init__(self, top=None, ifds=None):
            dict.__init__(self, top or {})
            self._ifds = dict(ifds or {})

        def load(self, data):
            top, ifds = _parse_exif_tiff(_exif_tiff_block(data) or b"")
            self.clear()
            self.update(top)
            self._ifds = ifds
            return self

        def get_ifd(self, tag):
            return dict(self._ifds.get(int(tag), {}))

        def tobytes(self, offset=8):
            raise NotImplementedError("vis PIL shim: Exif.tobytes() is not supported")

    def _sniff_format(data):
        if data[:8] == bytes([137, 80, 78, 71, 13, 10, 26, 10]):
            return "PNG"
        if data[:3] == bytes([255, 216, 255]):
            return "JPEG"
        if data[:6] in (b"GIF87a", b"GIF89a"):
            return "GIF"
        if data[:2] == b"BM":
            return "BMP"
        if data[:4] == b"RIFF" and data[8:12] == b"WEBP":
            return "WEBP"
        if data[:2] in (b"II", b"MM"):
            return "TIFF"
        if data[:4] == b"qoif":
            return "QOI"
        if data[:4] == bytes([0, 0, 1, 0]):
            return "ICO"
        # netpbm: "P1".."P7" then whitespace. Pillow calls every flavour PPM.
        if (
            len(data) > 2
            and data[:1] == b"P"
            and data[1:2] in b"1234567"
            and data[2:3].isspace()
        ):
            return "PPM"
        return None

    def _open(fp, mode="r", formats=None):
        if hasattr(fp, "read"):
            data = fp.read()
        elif isinstance(fp, (bytes, bytearray)):
            data = bytes(fp)
        else:
            with open(fp, "rb") as f:
                data = f.read()
        raw = bytes(data)
        b64 = base64.b64encode(raw).decode("ascii")
        try:
            _im = _wrap(_H("__vis_pil_open__", b64))
        except OSError as exc:
            # Pillow's own error for a file no plugin claims -- an OSError subclass,
            # so `except OSError` still catches it, but `except UnidentifiedImageError`
            # now works too.
            raise UnidentifiedImageError(str(exc)) from None
        _im.format = _sniff_format(raw)
        _im._raw = raw
        tiff = _exif_tiff_block(raw)
        if tiff:
            _im.info["exif"] = b"Exif\x00\x00" + tiff
        try:
            fr = _lst(_H("__vis_pil_frames__", _im._handle))
        except Exception:
            fr = None
        if fr:
            _im._n_frames = int(fr[0])
            _im._delays = [int(d) for d in _lst(fr[2])]
            if fr[1] is not None:
                # the host reports -1 for "loop forever"; PIL's GIF info says 0.
                _im.info["loop"] = max(0, int(fr[1]))
            if _im._delays:
                _im.info["duration"] = _im._delays[0]
        if _im.mode == "P":
            _attach_palette(_im)
        return _im

    def frombytes(mode, size, data, decoder_name="raw", *args):
        _check_size(size)
        w, h = size
        raw = _unpack_mode_bytes(str(mode), bytes(data), int(w), int(h))
        b64 = base64.b64encode(raw).decode("ascii")
        return _wrap(_H("__vis_pil_frombytes__", str(mode), int(w), int(h), b64))

    frombuffer = frombytes

    def merge(mode, bands):
        # A wrong band count used to reach the host and come back as a garbage
        # pixel; "LA" takes two bands, not RGBA's four.
        if len(bands) != _MODEBANDS.get(str(mode), 3):
            raise ValueError("wrong number of bands")
        hs = [b._handle for b in bands]
        return _wrap(_H("__vis_pil_merge__", str(mode), hs))

    def blend(im1, im2, alpha):
        return _wrap(_H("__vis_pil_blend__", im1._handle, im2._handle, float(alpha)))

    def composite(image1, image2, mask):
        _check_mask(mask)
        return _wrap(
            _H("__vis_pil_composite__", image1._handle, image2._handle, mask._handle)
        )

    def fromarray(obj, mode=None):
        # minimal numpy-array support: obj is a nested list / has tolist()
        arr = obj.tolist() if hasattr(obj, "tolist") else obj
        rows = list(arr)
        h = len(rows)
        first = list(rows[0])
        w = len(first)
        px0 = first[0]
        if isinstance(px0, (list, tuple)):
            nb = len(px0)
            m = mode or ("RGBA" if nb == 4 else "RGB")
        else:
            nb = 1
            m = mode or "L"
        ba = bytearray()
        for row in rows:
            for px in row:
                if isinstance(px, (list, tuple)):
                    for c in px:
                        ba.append(int(c) & 255)
                else:
                    ba.append(int(px) & 255)
        b64 = base64.b64encode(bytes(ba)).decode("ascii")
        return _wrap(_H("__vis_pil_frombytes__", m, w, h, b64))

    # -- Image submodule -----------------------------------------------------
    Image_mod = types.ModuleType("PIL.Image")
    Image_mod.Image = Image
    Image_mod.new = new
    Image_mod.open = _open
    Image_mod.frombytes = frombytes
    Image_mod.frombuffer = frombuffer
    Image_mod.merge = merge
    Image_mod.blend = blend
    Image_mod.composite = composite
    Image_mod.fromarray = fromarray

    def _module_alpha_composite(im1, im2, dest=None, source=None):
        if im1.mode != "RGBA" or im2.mode != "RGBA":
            raise ValueError("image mode must be RGBA")
        return _wrap(_H("__vis_pil_alpha_composite__", im1._handle, im2._handle, 0, 0))

    def _module_eval(image, *args):
        return image.point(args[0])

    def _linear_gradient(mode="L"):
        ba = bytearray()
        for y in range(256):
            ba.extend([y] * 256)
        b64 = base64.b64encode(bytes(ba)).decode("ascii")
        g = _wrap(_H("__vis_pil_frombytes__", "L", 256, 256, b64))
        return g if mode == "L" else g.convert(mode)

    def _radial_gradient(mode="L"):
        ba = bytearray()
        for y in range(256):
            for x in range(256):
                d = math.hypot(x - 127.5, y - 127.5) / 127.5 * 255.0
                ba.append(min(255, int(d)))
        b64 = base64.b64encode(bytes(ba)).decode("ascii")
        g = _wrap(_H("__vis_pil_frombytes__", "L", 256, 256, b64))
        return g if mode == "L" else g.convert(mode)

    def _effect_noise(size, sigma):
        import random

        w, h = int(size[0]), int(size[1])
        ba = bytearray()
        for _ in range(w * h):
            ba.append(max(0, min(255, int(random.gauss(128, sigma)))))
        b64 = base64.b64encode(bytes(ba)).decode("ascii")
        return _wrap(_H("__vis_pil_frombytes__", "L", w, h, b64))

    def _effect_mandelbrot(size, extent, quality):
        w, h = int(size[0]), int(size[1])
        x0, y0, x1, y1 = extent
        ba = bytearray()
        for py in range(h):
            for px in range(w):
                cx = x0 + (x1 - x0) * px / float(w)
                cy = y0 + (y1 - y0) * py / float(h)
                zx = zy = 0.0
                i = 0
                while zx * zx + zy * zy <= 4.0 and i < quality:
                    zx, zy = zx * zx - zy * zy + cx, 2.0 * zx * zy + cy
                    i += 1
                ba.append(int(255 * i / quality))
        b64 = base64.b64encode(bytes(ba)).decode("ascii")
        return _wrap(_H("__vis_pil_frombytes__", "L", w, h, b64))

    _MODEBANDS = {
        "1": 1,
        "L": 1,
        "La": 2,
        "P": 1,
        "I": 1,
        "F": 1,
        "I;16": 1,
        "I;16B": 1,
        "RGB": 3,
        "RGBA": 4,
        "RGBa": 4,
        "CMYK": 4,
        "YCbCr": 3,
        "LAB": 3,
        "HSV": 3,
        "LA": 2,
        "RGBX": 4,
    }
    _MODEBANDNAMES = {
        "1": ("1",),
        "L": ("L",),
        "La": ("L", "a"),
        "P": ("P",),
        "I": ("I",),
        "F": ("F",),
        "I;16": ("I",),
        "I;16B": ("I",),
        "RGB": ("R", "G", "B"),
        "RGBA": ("R", "G", "B", "A"),
        "RGBa": ("R", "G", "B", "a"),
        "RGBX": ("R", "G", "B", "X"),
        "LA": ("L", "A"),
        "CMYK": ("C", "M", "Y", "K"),
        "YCbCr": ("Y", "Cb", "Cr"),
        "LAB": ("L", "A", "B"),
        "HSV": ("H", "S", "V"),
    }

    def _clip255(v):
        """One sample, clipped to a byte the way Pillow's CLIP8 does."""
        n = int(v)
        return 0 if n < 0 else (255 if n > 255 else n)

    def _check_size(size):
        """Pillow's own `_check_size` refusals. A bad size used to reach the host and
        come back as a Java message wrapped in OSError."""
        if not isinstance(size, (list, tuple)):
            raise ValueError("Size must be a tuple")
        if len(size) != 2:
            raise ValueError("Size must be a tuple of length 2")
        if int(size[0]) < 0 or int(size[1]) < 0:
            raise ValueError("Width and height must be >= 0")

    def _pack_mode_bytes(mode, raw, w, h):
        """Host samples -> Pillow's `tobytes` layout: 'I' and 'F' carry 32-bit
        samples. Every other mode, '1' included, already leaves the host in
        Pillow's own layout."""
        mode = str(mode)
        if mode == "I":
            return struct.pack("<%di" % len(raw), *raw)
        if mode == "F":
            return struct.pack("<%df" % len(raw), *raw)
        return raw

    def _unpack_mode_bytes(mode, data, w, h):
        """Pillow's `frombytes` layout -> the layout the host reads, refusing a short
        buffer the way Pillow does instead of leaking a host bounds error.

        The host raster is 8-bit per sample, so an I or F buffer is read at its
        Pillow WIDTH and each sample clipped to a byte."""
        mode = str(mode)
        if mode == "1":
            if len(data) < ((w + 7) // 8) * h:
                raise ValueError("not enough image data")
            return bytes(data)
        if mode in ("I", "F"):
            if len(data) < w * h * 4:
                raise ValueError("not enough image data")
            code = "i" if mode == "I" else "f"
            vals = struct.unpack("<%d%s" % (w * h, code), bytes(data[: w * h * 4]))
            return bytes(_clip255(v) for v in vals)
        bands = _MODEBANDS.get(mode, 3)
        if len(data) < w * h * bands:
            raise ValueError("not enough image data")
        return bytes(data[: w * h * bands])

    Image_mod.alpha_composite = _module_alpha_composite
    Image_mod.eval = _module_eval
    Image_mod.linear_gradient = _linear_gradient
    Image_mod.radial_gradient = _radial_gradient
    Image_mod.effect_noise = _effect_noise
    Image_mod.effect_mandelbrot = _effect_mandelbrot
    Image_mod.getmodebands = lambda mode: _MODEBANDS.get(mode, 3)
    Image_mod.getmodebandnames = lambda mode: _MODEBANDNAMES.get(mode, ("R", "G", "B"))
    Image_mod.AFFINE = AFFINE
    Image_mod.EXTENT = EXTENT
    Image_mod.PERSPECTIVE = PERSPECTIVE
    Image_mod.QUAD = QUAD
    Image_mod.MESH = MESH
    Image_mod.Transform = Transform
    Image_mod.NEAREST = NEAREST
    Image_mod.LANCZOS = LANCZOS
    Image_mod.ANTIALIAS = ANTIALIAS
    Image_mod.BILINEAR = BILINEAR
    Image_mod.BICUBIC = BICUBIC
    Image_mod.BOX = BOX
    Image_mod.HAMMING = HAMMING
    Image_mod.Resampling = Resampling
    Image_mod.FLIP_LEFT_RIGHT = FLIP_LEFT_RIGHT
    Image_mod.FLIP_TOP_BOTTOM = FLIP_TOP_BOTTOM
    Image_mod.ROTATE_90 = ROTATE_90
    Image_mod.ROTATE_180 = ROTATE_180
    Image_mod.ROTATE_270 = ROTATE_270
    Image_mod.TRANSPOSE = TRANSPOSE
    Image_mod.TRANSVERSE = TRANSVERSE
    Image_mod.Transpose = Transpose
    Image_mod.Quantize = Quantize
    Image_mod.MEDIANCUT = MEDIANCUT
    Image_mod.MAXCOVERAGE = MAXCOVERAGE
    Image_mod.FASTOCTREE = FASTOCTREE
    Image_mod.LIBIMAGEQUANT = LIBIMAGEQUANT
    Image_mod.Dither = Dither
    Image_mod.NONE = NONE
    Image_mod.ORDERED = ORDERED
    Image_mod.RASTERIZE = RASTERIZE
    Image_mod.FLOYDSTEINBERG = FLOYDSTEINBERG
    Image_mod.Palette = Palette
    Image_mod.WEB = WEB
    Image_mod.ADAPTIVE = ADAPTIVE
    Image_mod.__version__ = "10.0-vis-imaging"

    class UnidentifiedImageError(OSError):
        """Pillow's error for a file no plugin can identify. An OSError subclass, so
        code that catches either one keeps working."""

    class DecompressionBombWarning(RuntimeWarning):
        """Pillow's warning for an image over `MAX_IMAGE_PIXELS`."""

    class DecompressionBombError(Exception):
        """Pillow's error for an image far over `MAX_IMAGE_PIXELS`."""

    # The extensions Pillow's registry would carry for the formats this host really
    # decodes -- `registered_extensions()` answers exactly these.
    _EXTENSION = {
        ".bmp": "BMP",
        ".gif": "GIF",
        ".ico": "ICO",
        ".jpe": "JPEG",
        ".jpeg": "JPEG",
        ".jpg": "JPEG",
        ".pbm": "PPM",
        ".pgm": "PPM",
        ".png": "PNG",
        ".pnm": "PPM",
        ".ppm": "PPM",
        ".qoi": "QOI",
        ".tga": "TGA",
        ".tif": "TIFF",
        ".tiff": "TIFF",
        ".webp": "WEBP",
    }

    def _init_plugins():
        """Pillow's lazy plugin import. Every format the host knows is registered
        from the start, so this only reports that the registry is loaded."""
        return 1

    Image_mod.MAX_IMAGE_PIXELS = 89478485
    Image_mod.UnidentifiedImageError = UnidentifiedImageError
    Image_mod.DecompressionBombWarning = DecompressionBombWarning
    Image_mod.DecompressionBombError = DecompressionBombError
    Image_mod.isImageType = lambda t: isinstance(t, Image)
    Image_mod.EXTENSION = _EXTENSION
    Image_mod.registered_extensions = lambda: dict(_EXTENSION)
    Image_mod.init = _init_plugins
    Image_mod.preinit = _init_plugins

    def _fontname(font):
        # What `ImageFont.truetype` was handed -- a font file path or a family
        # name. The host resolves it against the shared font database; an empty
        # string means "whatever the host falls back to".
        return str(getattr(font, "path", "") or "")

    # -- ImageDraw -----------------------------------------------------------
    class _Draw:
        def __init__(self, im, mode=None):
            self._im = im
            self.mode = im.mode

        def _emit(self, name, xy, keys, values):
            # ONE flat scalar record per op, appended to the shared draw queue. A
            # colour key is resolved HERE, in the image's OWN mode: how many bands a
            # spec must carry is the mode's business, not the caller's.
            record = [name]
            points = self._flat(xy)
            record.append(len(points))
            record.extend(points)
            tail = []
            for i in range(len(keys)):
                value = values[i]
                if keys[i] in ("fill", "outline"):
                    value = _packcol(value, self.mode)
                if value is not None:
                    tail.append(keys[i])
                    tail.append(value)
            record.append(len(tail) // 2)
            record.extend(tail)
            _queue_draw(self._im._handle, record)

        def _flat(self, xy):
            xy = list(xy)
            if len(xy) == 0:
                return []
            if isinstance(xy[0], (int, float)):
                return [float(v) for v in xy]
            out = []
            for p in xy:
                out.append(float(p[0]))
                out.append(float(p[1]))
            return out

        def point(self, xy, fill=None):
            self._emit("point", xy, ("fill",), (fill,))

        def line(self, xy, fill=None, width=1, joint=None):
            self._emit("line", xy, ("fill", "width"), (fill, int(width)))

        def rectangle(self, xy, fill=None, outline=None, width=1):
            self._emit(
                "rectangle",
                xy,
                ("fill", "outline", "width"),
                (fill, outline, int(width)),
            )

        def rounded_rectangle(
            self, xy, radius=0, fill=None, outline=None, width=1, corners=None, **k
        ):
            f = self._flat(xy)
            x0, y0, x1, y1 = f[0], f[1], f[2], f[3]
            r = min(float(radius), (x1 - x0) / 2.0, (y1 - y0) / 2.0)
            if r <= 0:
                self.rectangle(
                    (x0, y0, x1, y1), fill=fill, outline=outline, width=width
                )
                return
            d = 2 * r
            if fill is not None:
                self.rectangle((x0 + r, y0, x1 - r, y1), fill=fill)
                self.rectangle((x0, y0 + r, x1, y1 - r), fill=fill)
                self.pieslice((x0, y0, x0 + d, y0 + d), 180, 270, fill=fill)
                self.pieslice((x1 - d, y0, x1, y0 + d), 270, 360, fill=fill)
                self.pieslice((x0, y1 - d, x0 + d, y1), 90, 180, fill=fill)
                self.pieslice((x1 - d, y1 - d, x1, y1), 0, 90, fill=fill)
            if outline is not None and width > 0:
                self.line((x0 + r, y0, x1 - r, y0), fill=outline, width=width)
                self.line((x0 + r, y1, x1 - r, y1), fill=outline, width=width)
                self.line((x0, y0 + r, x0, y1 - r), fill=outline, width=width)
                self.line((x1, y0 + r, x1, y1 - r), fill=outline, width=width)
                self.arc((x0, y0, x0 + d, y0 + d), 180, 270, fill=outline, width=width)
                self.arc((x1 - d, y0, x1, y0 + d), 270, 360, fill=outline, width=width)
                self.arc((x0, y1 - d, x0 + d, y1), 90, 180, fill=outline, width=width)
                self.arc((x1 - d, y1 - d, x1, y1), 0, 90, fill=outline, width=width)

        def ellipse(self, xy, fill=None, outline=None, width=1):
            self._emit(
                "ellipse",
                xy,
                ("fill", "outline", "width"),
                (fill, outline, int(width)),
            )

        def polygon(self, xy, fill=None, outline=None, width=1):
            self._emit(
                "polygon",
                xy,
                ("fill", "outline", "width"),
                (fill, outline, int(width)),
            )

        def arc(self, xy, start, end, fill=None, width=1):
            self._emit(
                "arc",
                xy,
                ("fill", "start", "end", "width"),
                (fill, float(start), float(end), int(width)),
            )

        def chord(self, xy, start, end, fill=None, outline=None, width=1):
            self._emit(
                "chord",
                xy,
                ("fill", "outline", "start", "end", "width"),
                (
                    fill,
                    outline,
                    float(start),
                    float(end),
                    int(width),
                ),
            )

        def pieslice(self, xy, start, end, fill=None, outline=None, width=1):
            self._emit(
                "pieslice",
                xy,
                ("fill", "outline", "start", "end", "width"),
                (
                    fill,
                    outline,
                    float(start),
                    float(end),
                    int(width),
                ),
            )

        def text(self, xy, text, fill=None, font=None, anchor=None, **kw):
            size = getattr(font, "size", 12) if font is not None else 12
            self._emit(
                "text",
                xy,
                ("fill", "font_size", "font", "text"),
                (fill, int(size), _fontname(font), str(text)),
            )

        def multiline_text(self, xy, text, fill=None, font=None, spacing=4, **kw):
            size = getattr(font, "size", 12) if font is not None else 12
            x, y = float(xy[0]), float(xy[1])
            for i, line in enumerate(str(text).split(chr(10))):
                self.text((x, y + i * (size + spacing)), line, fill=fill, font=font)

        def textbbox(self, xy, text, font=None, **kw):
            size = getattr(font, "size", 12) if font is not None else 12
            b = _lst(_H("__vis_pil_textbbox__", str(text), int(size), _fontname(font)))
            x, y = xy[0], xy[1]
            return (x + b[0], y + b[1], x + b[2], y + b[3])

        def textlength(self, text, font=None, **kw):
            size = getattr(font, "size", 12) if font is not None else 12
            b = _lst(_H("__vis_pil_textbbox__", str(text), int(size), _fontname(font)))
            # Pillow answers a FLOAT advance -- text layout adds fractional widths.
            return float(b[2] - b[0])

        def textsize(self, text, font=None, **kw):
            size = getattr(font, "size", 12) if font is not None else 12
            b = _lst(_H("__vis_pil_textbbox__", str(text), int(size), _fontname(font)))
            return (b[2] - b[0], b[3] - b[1])

        def regular_polygon(
            self, bounding_circle, n_sides, rotation=0, fill=None, outline=None, width=1
        ):
            bc = bounding_circle
            if len(bc) == 2 and isinstance(bc[0], (list, tuple)):
                cx, cy = bc[0]
                r = bc[1]
            else:
                cx, cy, r = bc[0], bc[1], bc[2]
            pts = []
            for i in range(int(n_sides)):
                ang = math.radians(float(rotation) - 90.0) + 2.0 * math.pi * i / float(
                    n_sides
                )
                pts.append((cx + r * math.cos(ang), cy + r * math.sin(ang)))
            self.polygon(pts, fill=fill, outline=outline, width=width)

        def circle(self, xy, radius, fill=None, outline=None, width=1):
            x, y = xy[0], xy[1]
            self.ellipse(
                (x - radius, y - radius, x + radius, y + radius),
                fill=fill,
                outline=outline,
                width=width,
            )

        def multiline_textbbox(self, xy, text, font=None, anchor=None, spacing=4, **kw):
            size = getattr(font, "size", 12) if font is not None else 12
            x, y = float(xy[0]), float(xy[1])
            lines = str(text).split(chr(10))
            x0 = y0 = 1e18
            x1 = y1 = -1e18
            for i, line in enumerate(lines):
                bb = self.textbbox((x, y + i * (size + spacing)), line, font=font)
                x0 = min(x0, bb[0])
                y0 = min(y0, bb[1])
                x1 = max(x1, bb[2])
                y1 = max(y1, bb[3])
            return (x0, y0, x1, y1)

        def multiline_textsize(self, text, font=None, spacing=4, **kw):
            bb = self.multiline_textbbox((0, 0), text, font=font, spacing=spacing)
            return (bb[2] - bb[0], bb[3] - bb[1])

        def getfont(self):
            return _Font(12)

        def bitmap(self, xy, bitmap, fill=None):
            self._im.paste(bitmap, (int(xy[0]), int(xy[1])), bitmap)

    ImageDraw = types.ModuleType("PIL.ImageDraw")
    ImageDraw.ImageDraw = _Draw
    ImageDraw.Draw = lambda im, mode=None: _Draw(im, mode)

    def _floodfill(image, xy, value, border=None, thresh=0):
        x, y = int(xy[0]), int(xy[1])
        w, h = image.size
        if x < 0 or y < 0 or x >= w or y >= h:
            return
        px = image.load()
        bg = px[x, y]
        fill_val = _getrgb(value) if isinstance(value, str) else value
        if isinstance(fill_val, (list, tuple)):
            fill_val = tuple(int(c) for c in fill_val)
        bd = None
        if border is not None:
            bd = _getrgb(border) if isinstance(border, str) else border
            if isinstance(bd, (list, tuple)):
                bd = tuple(int(c) for c in bd)

        def _same(a, b):
            if isinstance(a, (list, tuple)):
                return all(abs(a[i] - b[i]) <= thresh for i in range(len(a)))
            return abs(a - b) <= thresh

        stack = [(x, y)]
        seen = set()
        while stack:
            cx, cy = stack.pop()
            if (cx, cy) in seen or cx < 0 or cy < 0 or cx >= w or cy >= h:
                continue
            seen.add((cx, cy))
            cur = px[cx, cy]
            if bd is None:
                if not _same(cur, bg):
                    continue
            else:
                if _same(cur, bd) or cur == fill_val:
                    continue
            px[cx, cy] = fill_val
            stack.extend([(cx + 1, cy), (cx - 1, cy), (cx, cy + 1), (cx, cy - 1)])

    ImageDraw.floodfill = _floodfill
    ImageDraw.getdraw = lambda im=None, hints=None: (
        _Draw(im) if im is not None else None,
        None,
    )

    # -- ImageFont -----------------------------------------------------------
    class _Font:
        def __init__(self, size=10, name=""):
            self.size = int(size)
            self.path = name

        def getbbox(self, text, *a, **k):
            b = _lst(_H("__vis_pil_textbbox__", str(text), int(self.size), self.path))
            return (b[0], b[1], b[2], b[3])

        def getsize(self, text, *a, **k):
            b = _lst(_H("__vis_pil_textbbox__", str(text), int(self.size), self.path))
            return (b[2] - b[0], b[3] - b[1])

        def getlength(self, text, *a, **k):
            b = _lst(_H("__vis_pil_textbbox__", str(text), int(self.size), self.path))
            return float(b[2] - b[0])

        def getmask(self, text, mode="L", *a, **k):
            """Pillow rasterises the glyphs into a bitmap. The host draws text rather
            than exposing glyph coverage, so the mask is that drawing on black."""
            b = self.getbbox(text)
            w = max(1, int(b[2] - b[0]))
            h = max(1, int(b[3] - b[1]))
            img = new("L", (w, h), 0)
            _Draw(img).text((-b[0], -b[1]), text, fill=255, font=self)
            return img

        def font_variant(self, font=None, size=None, index=None, encoding=None, **k):
            return _Font(self.size if size is None else size, font or self.path)

    ImageFont = types.ModuleType("PIL.ImageFont")
    ImageFont.FreeTypeFont = _Font
    ImageFont.ImageFont = _Font
    ImageFont.truetype = lambda font=None, size=10, *a, **k: _Font(
        size, str(font) if font else ""
    )
    ImageFont.load_default = lambda size=None: _Font(size or 10)
    ImageFont.load = lambda filename: _Font(10, str(filename))

    # -- ImageFilter ---------------------------------------------------------
    ImageFilter = types.ModuleType("PIL.ImageFilter")

    class _Kernel:
        name = "Kernel"

        def __init__(self, size, kernel, scale=None, offset=0):
            self.size = size if isinstance(size, (tuple, list)) else (size, size)
            self.kernel = list(kernel)
            self.scale = scale if scale is not None else (sum(self.kernel) or 1)
            self.offset = offset

        def filter(self, image):
            s = self.size[0]
            m = _H(
                "__vis_pil_conv__",
                image._handle,
                int(s),
                [float(x) for x in self.kernel],
                float(self.scale),
                float(self.offset),
            )
            return _wrap(m)

    class _BuiltinFilter(_Kernel):
        def __init__(self):
            _Kernel.__init__(self, self._size, self._kernel, self._scale, self._offset)

    def _mk_builtin(nm, size, scale, offset, kernel):
        return type(
            nm,
            (_BuiltinFilter,),
            {
                "name": nm,
                "_size": size,
                "_scale": scale,
                "_offset": offset,
                "_kernel": kernel,
            },
        )

    ImageFilter.Kernel = _Kernel
    ImageFilter.BuiltinFilter = _BuiltinFilter
    # Pillow's abstract base -- `isinstance(f, ImageFilter.Filter)` is a real check
    # callers make, and every filter here already derives from the kernel class.
    ImageFilter.Filter = _Kernel
    ImageFilter.BLUR = _mk_builtin(
        "BLUR",
        (5, 5),
        16,
        0,
        [1, 1, 1, 1, 1, 1, 0, 0, 0, 1, 1, 0, 0, 0, 1, 1, 0, 0, 0, 1, 1, 1, 1, 1, 1],
    )
    ImageFilter.SMOOTH = _mk_builtin(
        "SMOOTH", (3, 3), 13, 0, [1, 1, 1, 1, 5, 1, 1, 1, 1]
    )
    ImageFilter.SMOOTH_MORE = _mk_builtin(
        "SMOOTH_MORE",
        (5, 5),
        100,
        0,
        [1, 1, 1, 1, 1, 1, 5, 5, 5, 1, 1, 5, 44, 5, 1, 1, 5, 5, 5, 1, 1, 1, 1, 1, 1],
    )
    ImageFilter.SHARPEN = _mk_builtin(
        "SHARPEN", (3, 3), 16, 0, [-2, -2, -2, -2, 32, -2, -2, -2, -2]
    )
    ImageFilter.DETAIL = _mk_builtin(
        "DETAIL", (3, 3), 6, 0, [0, -1, 0, -1, 10, -1, 0, -1, 0]
    )
    ImageFilter.EDGE_ENHANCE = _mk_builtin(
        "EDGE_ENHANCE", (3, 3), 2, 0, [-1, -1, -1, -1, 10, -1, -1, -1, -1]
    )
    ImageFilter.EDGE_ENHANCE_MORE = _mk_builtin(
        "EDGE_ENHANCE_MORE", (3, 3), 1, 0, [-1, -1, -1, -1, 9, -1, -1, -1, -1]
    )
    ImageFilter.FIND_EDGES = _mk_builtin(
        "FIND_EDGES", (3, 3), 1, 0, [-1, -1, -1, -1, 8, -1, -1, -1, -1]
    )
    ImageFilter.EMBOSS = _mk_builtin(
        "EMBOSS", (3, 3), 1, 128, [-1, 0, 0, 0, 1, 0, 0, 0, 0]
    )
    ImageFilter.CONTOUR = _mk_builtin(
        "CONTOUR", (3, 3), 1, 255, [-1, -1, -1, -1, 8, -1, -1, -1, -1]
    )

    class GaussianBlur:
        name = "GaussianBlur"

        def __init__(self, radius=2):
            self.radius = radius

        def filter(self, image):
            r = self.radius
            if isinstance(r, (tuple, list)):
                r = r[0]
            r = float(r)
            rad = max(1, int(math.ceil(r * 2)))
            sigma = r if r > 0 else 1e-6
            ker = []
            s = 0.0
            for j in range(-rad, rad + 1):
                for i in range(-rad, rad + 1):
                    v = math.exp(-(i * i + j * j) / (2 * sigma * sigma))
                    ker.append(v)
                    s += v
            size = 2 * rad + 1
            return _wrap(_H("__vis_pil_conv__", image._handle, size, ker, s, 0.0))

    class BoxBlur:
        name = "BoxBlur"

        def __init__(self, radius=1):
            self.radius = radius

        def filter(self, image):
            r = self.radius
            if isinstance(r, (tuple, list)):
                r = r[0]
            rad = max(1, int(round(float(r))))
            size = 2 * rad + 1
            n = size * size
            ker = [1.0] * n
            return _wrap(
                _H("__vis_pil_conv__", image._handle, size, ker, float(n), 0.0)
            )

    class RankFilter:
        """Pillow's `RankFilter(size, rank)`: the rank-th smallest value in the box.

        Pillow takes the rank as the SECOND argument -- `RankFilter(3, 4)` is the
        median of a 3x3 window -- so a one-argument constructor rejects the call
        every Pillow example makes.
        """

        name = "RankFilter"

        def __init__(self, size=3, rank=0):
            self.size = size
            self.rank = rank

        def filter(self, image):
            return _wrap(
                _H("__vis_pil_rank__", image._handle, int(self.size), int(self.rank))
            )

    class MedianFilter(RankFilter):
        name = "MedianFilter"

        def __init__(self, size=3):
            RankFilter.__init__(self, size, (size * size) // 2)

    class MinFilter(RankFilter):
        name = "MinFilter"

        def __init__(self, size=3):
            RankFilter.__init__(self, size, 0)

    class MaxFilter(RankFilter):
        name = "MaxFilter"

        def __init__(self, size=3):
            RankFilter.__init__(self, size, size * size - 1)

    class ModeFilter:
        """Pillow's `ModeFilter`: the value that occurs MOST OFTEN in the box.

        A rank is not a mode. The median of a window agrees with its mode only by
        accident, so filtering a label map, a screenshot or a scanned form with
        the median silently answers a different picture and never says so.

        The host raster has no mode kernel, so the window is counted here over
        the image's own bytes: O(w*h*size^2) in Python -- cheap for the small,
        flat images a mode filter is for, slow for a photograph. Like Pillow, a
        value seen only once or twice is not a mode and that pixel keeps what it
        had; unlike Pillow, an edge window counts the pixels that exist instead
        of the black border Pillow pads in, which paints a dark rim.
        """

        name = "ModeFilter"

        def __init__(self, size=3):
            self.size = size

        def filter(self, image):
            size = int(self.size)
            width, height = image.size
            if size < 2 or width < 1 or height < 1:
                return image.copy()
            data = image.tobytes()
            if len(data) % (width * height) != 0:
                raise ValueError("image has wrong mode")
            bands = len(data) // (width * height)
            if bands < 1 or image.mode == "1":
                raise ValueError("image has wrong mode")
            out = bytearray(data)
            half = size // 2
            stride = width * bands
            for y in range(height):
                top = max(0, y - half)
                bottom = min(height - 1, y + half)
                for x in range(width):
                    left = max(0, x - half)
                    right = min(width - 1, x + half)
                    here = y * stride + x * bands
                    for band in range(bands):
                        counts = {}
                        best = -1
                        seen = 0
                        for wy in range(top, bottom + 1):
                            row = wy * stride + band
                            for wx in range(left, right + 1):
                                value = data[row + wx * bands]
                                count = counts.get(value, 0) + 1
                                counts[value] = count
                                if count > seen or (count == seen and value < best):
                                    seen = count
                                    best = value
                        if seen > 2:
                            out[here + band] = best
            return Image_mod.frombytes(image.mode, (width, height), bytes(out))

    class UnsharpMask:
        name = "UnsharpMask"

        def __init__(self, radius=2, percent=150, threshold=3):
            self.radius = radius
            self.percent = percent

        def filter(self, image):
            blurred = image.filter(GaussianBlur(self.radius))
            f = self.percent / 100.0
            # sharpened = image + f*(image - blurred) = blend(blurred, image, 1+f)
            return Image_mod.blend(blurred, image, 1.0 + f)

    ImageFilter.GaussianBlur = GaussianBlur
    ImageFilter.BoxBlur = BoxBlur
    ImageFilter.MedianFilter = MedianFilter
    ImageFilter.MinFilter = MinFilter
    ImageFilter.MaxFilter = MaxFilter
    ImageFilter.ModeFilter = ModeFilter
    ImageFilter.RankFilter = RankFilter
    ImageFilter.UnsharpMask = UnsharpMask

    class MultibandFilter(_Kernel):
        pass

    class Color3DLUT:
        name = "Color3DLUT"

        def __init__(self, size, table, channels=3, target_mode=None, **k):
            self.size = size if isinstance(size, (tuple, list)) else (size, size, size)
            self.table = list(table)
            self.channels = channels
            self.mode = target_mode

        @classmethod
        def generate(cls, size, callback, channels=3, target_mode=None):
            sz = size if isinstance(size, (tuple, list)) else (size, size, size)
            sr, sg, sb = sz
            table = []
            for b in range(sb):
                for g in range(sg):
                    for r in range(sr):
                        table.extend(
                            callback(
                                r / (sr - 1 or 1), g / (sg - 1 or 1), b / (sb - 1 or 1)
                            )
                        )
            return cls(sz, table, channels, target_mode)

        def filter(self, image):
            sr, sg, sb = self.size
            data = image.convert("RGB").getdata()
            out = bytearray()
            tbl = self.table
            for px in data:
                ir = int(px[0] / 255.0 * (sr - 1) + 0.5)
                ig = int(px[1] / 255.0 * (sg - 1) + 0.5)
                ib = int(px[2] / 255.0 * (sb - 1) + 0.5)
                idx = (ib * sg * sr + ig * sr + ir) * self.channels
                for c in range(3):
                    out.append(max(0, min(255, int(round(tbl[idx + c] * 255.0)))))
            b64 = base64.b64encode(bytes(out)).decode("ascii")
            return _wrap(
                _H("__vis_pil_frombytes__", "RGB", image.size[0], image.size[1], b64)
            )

    ImageFilter.MultibandFilter = MultibandFilter
    ImageFilter.Color3DLUT = Color3DLUT

    # -- ImageChops ----------------------------------------------------------
    ImageChops = types.ModuleType("PIL.ImageChops")

    def _chop(op):
        return lambda a, b: _wrap(_H("__vis_pil_chop__", op, a._handle, b._handle))

    ImageChops.difference = _chop("difference")

    def _chops_arith(op, a, b, scale, offset):
        """Pillow's add/subtract: ``(a op b) / scale + offset``, clipped. The host op
        knows only the unscaled form, so a scale or an offset is applied here --
        they used to be accepted and dropped, answering a plain sum or difference."""
        if float(scale) == 1.0 and not offset:
            return _wrap(_H("__vis_pil_chop__", op, a._handle, b._handle))
        sc = float(scale)
        off = float(offset)
        plus = op == "add"

        def one(x, y):
            return _clip255((x + y if plus else x - y) / sc + off)

        da = list(a.getdata())
        db = list(b.getdata())
        if da and isinstance(da[0], tuple):
            out = [tuple(one(p, q) for p, q in zip(pa, pb)) for pa, pb in zip(da, db)]
        else:
            out = [one(pa, pb) for pa, pb in zip(da, db)]
        res = a.copy()
        res.putdata(out)
        return res

    ImageChops.add = lambda a, b, scale=1.0, offset=0: _chops_arith(
        "add", a, b, scale, offset
    )
    ImageChops.subtract = lambda a, b, scale=1.0, offset=0: _chops_arith(
        "subtract", a, b, scale, offset
    )
    ImageChops.multiply = _chop("multiply")
    ImageChops.screen = _chop("screen")
    ImageChops.lighter = _chop("lighter")
    ImageChops.darker = _chop("darker")
    ImageChops.add_modulo = _chop("add_modulo")
    ImageChops.subtract_modulo = _chop("subtract_modulo")
    ImageChops.logical_and = _chop("logical_and")
    ImageChops.logical_or = _chop("logical_or")
    ImageChops.logical_xor = _chop("logical_xor")
    ImageChops.lighter = _chop("lighter")
    ImageChops.invert = lambda image: image.point(
        [255 - i for i in range(256)] * len(image.getbands())
    )
    ImageChops.duplicate = lambda image: image.copy()
    ImageChops.constant = lambda image, value: new("L", image.size, value)
    ImageChops.overlay = _chop("overlay")
    ImageChops.soft_light = _chop("soft_light")
    ImageChops.hard_light = _chop("hard_light")
    ImageChops.blend = lambda im1, im2, alpha: blend(im1, im2, alpha)
    ImageChops.composite = lambda im1, im2, mask: composite(im1, im2, mask)

    def _chops_offset(image, xoffset, yoffset=None):
        if yoffset is None:
            yoffset = xoffset
        return _wrap(
            _H("__vis_pil_offset__", image._handle, int(xoffset), int(yoffset))
        )

    ImageChops.offset = _chops_offset

    # -- ImageOps ------------------------------------------------------------
    ImageOps = types.ModuleType("PIL.ImageOps")

    def _ops_grayscale(image):
        return image.convert("L")

    def _lut(image, lut):
        """PIL's own band expansion for a 256-entry table: 'P' is read as 'L', 'RGB'
        gets the table once per band, and anything else -- 'RGBA' included -- is
        refused exactly as Pillow refuses it. Point-mapping an 'RGBA' image with one
        table inverted its ALPHA too and quietly answered a transparent picture."""
        if image.mode == "P":
            image = image.convert("L")
        if image.mode == "L":
            return image.point(lut)
        if image.mode == "RGB":
            return image.point(lut + lut + lut)
        raise OSError("not supported for this image mode")

    def _ops_invert(image):
        return _lut(image, [255 - i for i in range(256)])

    def _ops_mirror(image):
        return image.transpose(FLIP_LEFT_RIGHT)

    def _ops_flip(image):
        return image.transpose(FLIP_TOP_BOTTOM)

    def _ops_posterize(image, bits):
        mask = (~(2 ** (8 - bits) - 1)) & 255
        return _lut(image, [i & mask for i in range(256)])

    def _ops_solarize(image, threshold=128):
        return _lut(image, [(255 - i if i >= threshold else i) for i in range(256)])

    def _ops_autocontrast(image, cutoff=0, ignore=None, mask=None, preserve_tone=False):
        """PIL's `autocontrast`, per BAND: each band is stretched off its OWN
        histogram (the whole-image `mask`, the `ignore` levels and the `cutoff`
        percentages included). Stretching one luminance table over every band tinted
        anything whose channels had different ranges."""
        histogram = (
            image.convert("L").histogram(mask)
            if preserve_tone
            else image.histogram(mask)
        )
        lut = []
        for layer in range(0, len(histogram), 256):
            h = list(histogram[layer : layer + 256])
            if ignore is not None:
                for ix in [ignore] if isinstance(ignore, int) else ignore:
                    h[int(ix)] = 0
            if cutoff:
                lo_cut, hi_cut = (
                    cutoff if isinstance(cutoff, tuple) else (cutoff, cutoff)
                )
                n = sum(h)
                cut = int(n * lo_cut // 100)
                for lo in range(256):
                    if cut > h[lo]:
                        cut = cut - h[lo]
                        h[lo] = 0
                    else:
                        h[lo] -= cut
                        cut = 0
                    if cut <= 0:
                        break
                cut = int(n * hi_cut // 100)
                for hi in range(255, -1, -1):
                    if cut > h[hi]:
                        cut = cut - h[hi]
                        h[hi] = 0
                    else:
                        h[hi] -= cut
                        cut = 0
                    if cut <= 0:
                        break
            lo = next((i for i in range(256) if h[i] > 0), 0)
            hi = next((i for i in range(255, -1, -1) if h[i] > 0), 0)
            if hi <= lo:
                lut.extend(list(range(256)))
            else:
                scale = 255.0 / (hi - lo)
                offset = -lo * scale
                lut.extend(
                    max(0, min(255, int(i * scale + offset))) for i in range(256)
                )
        if len(lut) == 256:
            return _lut(image, lut)
        return image.point(lut)

    def _ops_expand(image, border=0, fill=0):
        if isinstance(border, int):
            l = t = r = b = border
        elif len(border) == 2:
            l = r = border[0]
            t = b = border[1]
        else:
            l, t, r, b = border
        w, h = image.size
        out = new(image.mode, (w + l + r, h + t + b), fill)
        out.paste(image, (l, t))
        return out

    def _ops_fit(image, size, method=BICUBIC, bleed=0.0, centering=(0.5, 0.5)):
        w, h = size
        iw, ih = image.size
        scale = max(w / float(iw), h / float(ih))
        nw, nh = int(round(iw * scale)), int(round(ih * scale))
        tmp = image.resize((nw, nh), method)
        left = int((nw - w) * centering[0])
        top = int((nh - h) * centering[1])
        return tmp.crop((left, top, left + w, top + h))

    def _ops_pad(image, size, method=BICUBIC, color=None, centering=(0.5, 0.5)):
        w, h = size
        iw, ih = image.size
        scale = min(w / float(iw), h / float(ih))
        nw, nh = int(round(iw * scale)), int(round(ih * scale))
        tmp = image.resize((nw, nh), method)
        out = new(image.mode, (w, h), color if color is not None else 0)
        left = int((w - nw) * centering[0])
        top = int((h - nh) * centering[1])
        out.paste(tmp, (left, top))
        return out

    def _ops_contain(image, size, method=BICUBIC):
        w, h = size
        iw, ih = image.size
        scale = min(w / float(iw), h / float(ih), 1.0)
        return image.resize((max(1, int(iw * scale)), max(1, int(ih * scale))), method)

    def _ops_cover(image, size, method=BICUBIC):
        w, h = size
        iw, ih = image.size
        scale = max(w / float(iw), h / float(ih))
        return image.resize((max(1, int(iw * scale)), max(1, int(ih * scale))), method)

    def _ops_scale(image, factor, resample=BICUBIC):
        w, h = image.size
        return image.resize(
            (max(1, int(round(w * factor))), max(1, int(round(h * factor)))), resample
        )

    def _ops_colorize(
        image, black, white, mid=None, blackpoint=0, whitepoint=255, midpoint=127
    ):
        black = _getrgb(black) if isinstance(black, str) else black
        white = _getrgb(white) if isinstance(white, str) else white
        l = image if image.mode == "L" else image.convert("L")
        rl = [int(black[0] + (white[0] - black[0]) * i / 255.0) for i in range(256)]
        gl = [int(black[1] + (white[1] - black[1]) * i / 255.0) for i in range(256)]
        bl = [int(black[2] + (white[2] - black[2]) * i / 255.0) for i in range(256)]
        return merge("RGB", (l.point(rl), l.point(gl), l.point(bl)))

    def _ops_equalize(image, mask=None):
        return _ops_autocontrast(image)

    ImageOps.grayscale = _ops_grayscale
    ImageOps.invert = _ops_invert
    ImageOps.mirror = _ops_mirror
    ImageOps.flip = _ops_flip
    ImageOps.posterize = _ops_posterize
    ImageOps.solarize = _ops_solarize
    ImageOps.autocontrast = _ops_autocontrast
    ImageOps.equalize = _ops_equalize
    ImageOps.expand = _ops_expand
    ImageOps.fit = _ops_fit
    ImageOps.pad = _ops_pad
    ImageOps.contain = _ops_contain
    ImageOps.cover = _ops_cover
    ImageOps.scale = _ops_scale
    ImageOps.colorize = _ops_colorize

    def _ops_crop(image, border=0):
        if isinstance(border, int):
            l = t = r = b = border
        elif len(border) == 2:
            l = r = border[0]
            t = b = border[1]
        else:
            l, t, r, b = border
        w, h = image.size
        return image.crop((l, t, w - r, h - b))

    _EXIF_ORIENTATION_OPS = {
        2: (FLIP_LEFT_RIGHT,),
        3: (ROTATE_180,),
        4: (FLIP_TOP_BOTTOM,),
        5: (FLIP_LEFT_RIGHT, ROTATE_90),
        6: (ROTATE_270,),
        7: (FLIP_LEFT_RIGHT, ROTATE_270),
        8: (ROTATE_90,),
    }

    def _ops_exif_transpose(image, in_place=False):
        # Orientation (0x0112) is the one EXIF tag that changes the PIXELS: a
        # phone photo is stored landscape with "rotate me" attached, so a shim
        # that ignored it handed the model a sideways picture.
        ops = _EXIF_ORIENTATION_OPS.get(image.getexif().get(0x0112))
        out = image
        for op in ops or ():
            out = out.transpose(op)
        if out is image:
            out = image.copy()
        # the rotation is now baked in: keep the rest of the EXIF, drop the tag
        # (and the raw block it would be re-read from) so it cannot apply twice.
        rest = Exif(
            {k: v for k, v in image.getexif().items() if k != 0x0112},
            image.getexif()._ifds,
        )
        out._exif = rest
        out.info = dict(image.info)
        out.info.pop("exif", None)
        if in_place:
            image._set([out._handle, out._w, out._h, out.mode])
            image._exif = rest
            image.info.pop("exif", None)
            return None
        return out

    def _ops_deform(image, deformer, resample=BICUBIC):
        # Apply a deformer via its getmesh(image) -> [(box, quad), ...] using MESH.
        mesh = deformer.getmesh(image)
        return image.transform(image.size, MESH, mesh, resample)

    ImageOps.crop = _ops_crop
    ImageOps.exif_transpose = _ops_exif_transpose
    ImageOps.deform = _ops_deform

    # -- ImageEnhance --------------------------------------------------------
    ImageEnhance = types.ModuleType("PIL.ImageEnhance")

    class _Enhance:
        def enhance(self, factor):
            return Image_mod.blend(self.degenerate, self.image, factor)

    def _keep_alpha(degenerate, image):
        """PIL keeps the ORIGINAL alpha in every enhancer's degenerate image, so a
        factor never fades an 'RGBA' picture out."""
        if "A" in image.getbands():
            degenerate.putalpha(image.getchannel("A"))
        return degenerate

    class Color(_Enhance):
        def __init__(self, image):
            self.image = image
            self.intermediate_mode = "LA" if "A" in image.getbands() else "L"
            self.degenerate = image.convert(self.intermediate_mode).convert(image.mode)

    class Contrast(_Enhance):
        def __init__(self, image):
            self.image = image
            # PIL's flat mean picture is built in 'L' and CONVERTED; building it in
            # the image's own mode read the mean as a packed int, so an RGB contrast
            # blended towards pure red instead of towards gray.
            mean = int(_Stat(image.convert("L")).mean[0] + 0.5)
            self.degenerate = _keep_alpha(
                new("L", image.size, mean).convert(image.mode), image
            )

    class Brightness(_Enhance):
        def __init__(self, image):
            self.image = image
            self.degenerate = _keep_alpha(new(image.mode, image.size, 0), image)

    class Sharpness(_Enhance):
        def __init__(self, image):
            self.image = image
            self.degenerate = _keep_alpha(image.filter(ImageFilter.SMOOTH), image)

    ImageEnhance.Color = Color
    ImageEnhance.Contrast = Contrast
    ImageEnhance.Brightness = Brightness
    ImageEnhance.Sharpness = Sharpness

    # -- ImageStat -----------------------------------------------------------
    ImageStat = types.ModuleType("PIL.ImageStat")

    class _Stat:
        def __init__(self, image_or_list, mask=None):
            if isinstance(image_or_list, list):
                self.h = image_or_list
            else:
                self.h = image_or_list.histogram(mask)
            self.bands = list(range(max(1, len(self.h) // 256)))

        def _band(self, b):
            return self.h[b * 256 : (b + 1) * 256]

        @property
        def count(self):
            return [sum(self._band(b)) for b in self.bands]

        @property
        def sum(self):
            return [
                float(sum(i * self._band(b)[i] for i in range(256))) for b in self.bands
            ]

        @property
        def sum2(self):
            return [
                float(sum(i * i * self._band(b)[i] for i in range(256)))
                for b in self.bands
            ]

        @property
        def mean(self):
            c = self.count
            s = self.sum
            return [s[b] / (c[b] or 1) for b in self.bands]

        @property
        def median(self):
            out = []
            for b in self.bands:
                hb = self._band(b)
                half = sum(hb) // 2
                acc = 0
                med = 0
                for i in range(256):
                    acc += hb[i]
                    if acc > half:
                        med = i
                        break
                out.append(med)
            return out

        @property
        def rms(self):
            c = self.count
            s2 = self.sum2
            return [math.sqrt(s2[b] / (c[b] or 1)) for b in self.bands]

        @property
        def var(self):
            c = self.count
            s2 = self.sum2
            mn = self.mean
            return [(s2[b] / (c[b] or 1)) - mn[b] ** 2 for b in self.bands]

        @property
        def stddev(self):
            return [math.sqrt(max(0.0, v)) for v in self.var]

        @property
        def extrema(self):
            out = []
            for b in self.bands:
                hb = self._band(b)
                lo = next((i for i in range(256) if hb[i] > 0), 0)
                hi = next((i for i in range(255, -1, -1) if hb[i] > 0), 0)
                out.append((lo, hi))
            return out

    ImageStat.Stat = _Stat

    # -- ImageMath -----------------------------------------------------------
    ImageMath = types.ModuleType("PIL.ImageMath")

    class _Operand:
        def __init__(self, im):
            self.im = im

        def _bin(self, other, fn_img, fn_scalar):
            o = other.im if isinstance(other, _Operand) else other
            if hasattr(o, "_handle"):
                return _Operand(fn_img(self.im, o))
            return _Operand(self.im.point(lambda i: fn_scalar(i, o)))

        def __add__(self, o):
            return self._bin(o, lambda a, b: ImageChops.add(a, b), lambda i, v: i + v)

        def __radd__(self, o):
            return self.__add__(o)

        def __sub__(self, o):
            return self._bin(
                o, lambda a, b: ImageChops.subtract(a, b), lambda i, v: i - v
            )

        def __mul__(self, o):
            return self._bin(
                o, lambda a, b: ImageChops.multiply(a, b), lambda i, v: i * v
            )

        def __rmul__(self, o):
            return self.__mul__(o)

        def __and__(self, o):
            return self._bin(
                o, lambda a, b: ImageChops.logical_and(a, b), lambda i, v: i & int(v)
            )

        def __or__(self, o):
            return self._bin(
                o, lambda a, b: ImageChops.logical_or(a, b), lambda i, v: i | int(v)
            )

    def _imagemath_env(options, kw):
        env = {}
        if options:
            env.update(options)
        env.update(kw)
        operands = {}
        for k, v in env.items():
            operands[k] = _Operand(v) if hasattr(v, "_handle") else v

        def _convert(op, mode):
            im = op.im if isinstance(op, _Operand) else op
            return _Operand(im.convert(mode))

        operands["convert"] = _convert
        operands["float"] = lambda op: op
        operands["int"] = lambda op: op
        operands["abs"] = abs
        operands["min"] = min
        operands["max"] = max
        return operands

    def _imagemath_eval(expression, _dict=None, **kw):
        operands = _imagemath_env(_dict, kw)
        result = eval(expression, {"__builtins__": {}}, operands)
        return result.im if isinstance(result, _Operand) else result

    def _imagemath_lambda_eval(expression, options=None, **kw):
        """PIL's `lambda_eval`: the expression is a CALLABLE taking the environment
        dict, not source text. Routing it into `eval` raised a TypeError out of
        `compile`, so every `lambda_eval` call failed."""
        result = expression(_imagemath_env(options, kw))
        return result.im if isinstance(result, _Operand) else result

    ImageMath.eval = _imagemath_eval
    ImageMath.lambda_eval = _imagemath_lambda_eval
    ImageMath.unsafe_eval = _imagemath_eval

    # -- ImageSequence -------------------------------------------------------
    ImageSequence = types.ModuleType("PIL.ImageSequence")

    class _SeqIterator:
        def __init__(self, im):
            self.im = im
            self.pos = 0

        def __iter__(self):
            return self

        def __next__(self):
            try:
                self.im.seek(self.pos)
            except EOFError:
                raise StopIteration
            self.pos += 1
            return self.im

        def __getitem__(self, ix):
            try:
                self.im.seek(int(ix))
            except EOFError:
                raise IndexError("no such frame")
            return self.im

    def _all_frames(im, func=None):
        ims = list(im) if isinstance(im, (list, tuple)) else [im]
        out = []
        for one in ims:
            start = one.tell()
            for frame in _SeqIterator(one):
                snap = frame.copy()
                out.append(func(snap) if func else snap)
            try:
                one.seek(start)
            except EOFError:
                pass
        return out

    ImageSequence.Iterator = _SeqIterator
    ImageSequence.all_frames = _all_frames
    Image_mod.Exif = Exif

    # -- ImagePalette --------------------------------------------------------
    ImagePalette = types.ModuleType("PIL.ImagePalette")

    class _Palette:
        def __init__(self, mode="RGB", palette=None, size=0):
            self.mode = mode
            self.palette = (
                list(palette)
                if palette is not None
                else [i for i in range(256) for _ in range(3)]
            )

        def getdata(self):
            return (self.mode, bytes(bytearray(int(x) & 255 for x in self.palette)))

        def tobytes(self):
            return bytes(bytearray(int(x) & 255 for x in self.palette))

        def getcolor(self, color, image=None):
            return _getrgb(color)[0]

    ImagePalette.ImagePalette = _Palette

    # -- ImageTransform ------------------------------------------------------
    ImageTransform = types.ModuleType("PIL.ImageTransform")

    class _Transform:
        def __init__(self, data):
            self.data = data

        def getdata(self):
            return (self.method, self.data)

    class AffineTransform(_Transform):
        method = AFFINE

    class ExtentTransform(_Transform):
        method = EXTENT

    class PerspectiveTransform(_Transform):
        method = PERSPECTIVE

    class QuadTransform(_Transform):
        method = QUAD

    class MeshTransform(_Transform):
        method = MESH

    ImageTransform.Transform = _Transform
    ImageTransform.AffineTransform = AffineTransform
    ImageTransform.ExtentTransform = ExtentTransform
    ImageTransform.PerspectiveTransform = PerspectiveTransform
    ImageTransform.QuadTransform = QuadTransform
    ImageTransform.MeshTransform = MeshTransform

    # -- PIL.features --------------------------------------------------------
    features = types.ModuleType("PIL.features")
    _FEATURES = {
        "jpg": True,
        "zlib": True,
        "libjpeg_turbo": False,
        "freetype2": True,
        "raqm": False,
        "webp": False,
        "transp_webp": False,
        "jpg_2000": False,
    }
    features.check = lambda feature: bool(_FEATURES.get(feature, False))
    features.check_feature = features.check
    features.check_codec = lambda feature: True
    features.check_module = lambda module: True
    features.version = lambda feature: None
    features.version_feature = lambda feature: None
    features.version_codec = lambda feature: None
    features.version_module = lambda module: None
    features.get_supported = lambda: ["jpg", "zlib", "freetype2"]
    features.get_supported_modules = lambda: ["freetype2"]
    features.get_supported_codecs = lambda: ["jpg", "zlib"]
    features.get_supported_features = lambda: []
    features.pilinfo = lambda out=None, supported_formats=True: None

    # -- ExifTags / TiffTags -------------------------------------------------
    ExifTags = types.ModuleType("PIL.ExifTags")
    ExifTags.TAGS = {
        256: "ImageWidth",
        257: "ImageLength",
        258: "BitsPerSample",
        259: "Compression",
        262: "PhotometricInterpretation",
        271: "Make",
        272: "Model",
        274: "Orientation",
        277: "SamplesPerPixel",
        282: "XResolution",
        283: "YResolution",
        296: "ResolutionUnit",
        305: "Software",
        306: "DateTime",
        315: "Artist",
        316: "HostComputer",
        33432: "Copyright",
        34665: "ExifOffset",
        36867: "DateTimeOriginal",
        37377: "ShutterSpeedValue",
        37378: "ApertureValue",
        37386: "FocalLength",
        40962: "PixelXDimension",
        40963: "PixelYDimension",
    }
    ExifTags.GPSTAGS = {
        0: "GPSVersionID",
        1: "GPSLatitudeRef",
        2: "GPSLatitude",
        3: "GPSLongitudeRef",
        4: "GPSLongitude",
        5: "GPSAltitudeRef",
        6: "GPSAltitude",
        7: "GPSTimeStamp",
    }

    class _TagEnum:
        pass

    ExifTags.Base = _TagEnum
    ExifTags.GPS = _TagEnum
    ExifTags.Interop = _TagEnum
    ExifTags.IFD = _TagEnum
    ExifTags.LightSource = _TagEnum

    TiffTags = types.ModuleType("PIL.TiffTags")
    TiffTags.TAGS = dict(ExifTags.TAGS)
    TiffTags.TAGS_V2 = {}
    TiffTags.lookup = lambda tag, group=None: None

    # -- import-compatible modules for GUI/file-plugin entry points ----------
    # They are intentionally explicit about unavailable host integration instead
    # of making ``from PIL import ImageGrab`` fail as though Pillow were broken.
    def _host_unavailable(name):
        def unavailable(*args, **kwargs):
            raise NotImplementedError(name + " is unavailable in the vis sandbox")

        return unavailable

    class _Parser:
        """Pillow's incremental `ImageFile.Parser`. The host decoder is one-shot, so
        the fed chunks are buffered and the complete file is opened at `close()`;
        `feed` used to refuse outright."""

        def __init__(self):
            self.data = b""
            self.image = None
            self.offset = 0
            self.finished = 0

        def reset(self):
            self.data = b""
            self.image = None
            self.finished = 0

        def feed(self, data):
            if self.finished:
                raise EOFError("cannot feed a closed parser")
            self.data += bytes(data)

        def close(self):
            self.finished = 1
            if self.image is None:
                self.image = _open(self.data)
            return self.image

    def _raise_oserror(error):
        raise OSError("decoder error %s" % (error,))

    ImageFile = types.ModuleType("PIL.ImageFile")
    ImageFile.ImageFile = Image
    ImageFile.StubImageFile = Image
    ImageFile.Parser = _Parser
    ImageFile.raise_oserror = _raise_oserror
    ImageFile.LOAD_TRUNCATED_IMAGES = False
    ImageGrab = types.ModuleType("PIL.ImageGrab")
    ImageGrab.grab = _host_unavailable("PIL.ImageGrab.grab")
    ImageGrab.grabclipboard = _host_unavailable("PIL.ImageGrab.grabclipboard")
    ImageTk = types.ModuleType("PIL.ImageTk")
    ImageTk.PhotoImage = _host_unavailable("PIL.ImageTk.PhotoImage")
    ImageTk.BitmapImage = _host_unavailable("PIL.ImageTk.BitmapImage")
    ImageTk.getimage = _host_unavailable("PIL.ImageTk.getimage")
    ImageWin = types.ModuleType("PIL.ImageWin")
    ImageWin.Dib = _host_unavailable("PIL.ImageWin.Dib")
    ImageWin.HDC = _host_unavailable("PIL.ImageWin.HDC")
    ImageWin.Window = _host_unavailable("PIL.ImageWin.Window")
    ImageQt = types.ModuleType("PIL.ImageQt")
    ImageQt.ImageQt = _host_unavailable("PIL.ImageQt.ImageQt")
    ImageQt.toqimage = _host_unavailable("PIL.ImageQt.toqimage")
    ImageQt.toqpixmap = _host_unavailable("PIL.ImageQt.toqpixmap")
    PSDraw = types.ModuleType("PIL.PSDraw")
    PSDraw.PSDraw = _host_unavailable("PIL.PSDraw.PSDraw")
    ImageShow = types.ModuleType("PIL.ImageShow")
    ImageShow.Viewer = _host_unavailable("PIL.ImageShow.Viewer")
    ImageShow.register = _host_unavailable("PIL.ImageShow.register")
    ImageShow.show = _host_unavailable("PIL.ImageShow.show")

    # -- ImageMorph ------------------------------------------------------------
    ImageMorph = types.ModuleType("PIL.ImageMorph")

    _MORPH_LUT_SIZE = 1 << 9
    _MORPH_ROTATION = [6, 3, 0, 7, 4, 1, 8, 5, 2]
    _MORPH_MIRROR = [2, 1, 0, 5, 4, 3, 8, 7, 6]

    class _LutBuilder:
        """PIL's `ImageMorph.LutBuilder`: a 512-entry morphological lookup table, one
        entry per 3x3 binary neighbourhood, built from pattern strings
        (`4:(... .0. .1.)->1`) or one of the named operations. The module used to be
        an empty placeholder, so every morphology call failed on attribute access."""

        PATTERNS = {
            "corner": ["1:(... ... ...)->0", "4:(00. 01. ...)->1"],
            "dilation4": ["4:(... .0. .1.)->1"],
            "dilation8": ["4:(... .0. .1.)->1", "4:(... .0. ..1)->1"],
            "erosion4": ["4:(... .1. .0.)->0"],
            "erosion8": ["4:(... .1. .0.)->0", "4:(... .1. ..0)->0"],
            "edge": [
                "1:(... ... ...)->0",
                "4:(.0. .1. ...)->1",
                "4:(01. .1. ...)->1",
            ],
        }

        def __init__(self, patterns=None, op_name=None):
            self.patterns = list(patterns) if patterns else []
            self.lut = None
            if op_name is not None:
                if op_name not in self.PATTERNS:
                    raise Exception("Unknown pattern " + str(op_name) + "!")
                self.patterns = list(self.PATTERNS[op_name])

        def add_patterns(self, patterns):
            self.patterns += list(patterns)

        def build_default_lut(self):
            # bit 4 is the CENTRE pixel: the identity operation.
            self.lut = bytearray(
                1 if (i & (1 << 4)) else 0 for i in range(_MORPH_LUT_SIZE)
            )

        def get_lut(self):
            return self.lut

        def _string_permute(self, pattern, permutation):
            return "".join(pattern[p] for p in permutation)

        def _pattern_permute(self, basic_pattern, options, basic_result):
            patterns = [(basic_pattern, basic_result)]
            if "4" in options:
                res = patterns[-1][1]
                for _ in range(4):
                    patterns.append(
                        (self._string_permute(patterns[-1][0], _MORPH_ROTATION), res)
                    )
            if "M" in options:
                for pattern, res in list(patterns):
                    patterns.append((self._string_permute(pattern, _MORPH_MIRROR), res))
            if "N" in options:
                for pattern, res in list(patterns):
                    flipped = (
                        pattern.replace("0", "Z").replace("1", "0").replace("Z", "1")
                    )
                    patterns.append((flipped, 1 - int(res)))
            return patterns

        def build_lut(self):
            self.build_default_lut()
            patterns = []
            for p in self.patterns:
                m = re.search(r"(\w*):?\s*\((.+?)\)\s*->\s*(\d)", p.replace("\n", ""))
                if not m:
                    raise Exception('Syntax error in pattern "' + p + '"')
                options, pattern, result = m.group(1), m.group(2), int(m.group(3))
                patterns += self._pattern_permute(
                    pattern.replace(" ", ""), options, result
                )
            compiled = [
                (re.compile(pattern.replace(".", "[01]")), res)
                for pattern, res in patterns
            ]
            for i in range(_MORPH_LUT_SIZE):
                bits = bin(i)[2:]
                bits = ("0" * (9 - len(bits)) + bits)[::-1]
                for rx, res in compiled:
                    if rx.match(bits):
                        self.lut[i] = 1 if res else 0
            return self.lut

    class _MorphOp:
        """PIL's `ImageMorph.MorphOp`: applies a 512-entry LUT to an 'L' image whose
        pixels are 0 or 255. Border pixels stay 0, exactly as the C operator leaves
        them, and `apply` answers `(changed-pixel count, image)`."""

        def __init__(self, lut=None, op_name=None, patterns=None):
            self.lut = lut
            if op_name is not None:
                self.lut = _LutBuilder(op_name=op_name).build_lut()
            elif patterns is not None:
                self.lut = _LutBuilder(patterns=patterns).build_lut()

        def _scan(self, image):
            if self.lut is None:
                raise Exception("No operator loaded")
            if image.mode != "L":
                raise ValueError("Image mode must be L")
            w, h = image.size
            px = list(image.getdata())
            bits = [1 if v else 0 for v in px]
            out = [0] * (w * h)
            hits = []
            for y in range(1, h - 1):
                for x in range(1, w - 1):
                    i = y * w + x
                    index = (
                        bits[i - w - 1]
                        | (bits[i - w] << 1)
                        | (bits[i - w + 1] << 2)
                        | (bits[i - 1] << 3)
                        | (bits[i] << 4)
                        | (bits[i + 1] << 5)
                        | (bits[i + w - 1] << 6)
                        | (bits[i + w] << 7)
                        | (bits[i + w + 1] << 8)
                    )
                    if self.lut[index]:
                        out[i] = 255
                        hits.append((x, y))
            return w, h, px, out, hits

        def apply(self, image):
            w, h, px, out, _hits = self._scan(image)
            outimage = new("L", (w, h), 0)
            outimage.putdata(out)
            changed = sum(1 for i in range(w * h) if (255 if px[i] else 0) != out[i])
            return changed, outimage

        def match(self, image):
            return self._scan(image)[4]

        def get_on_pixels(self, image):
            if image.mode != "L":
                raise ValueError("Image mode must be L")
            w, _h = image.size
            return [(i % w, i // w) for i, v in enumerate(image.getdata()) if v]

        def load_lut(self, filename):
            with open(filename, "rb") as f:
                self.lut = bytearray(f.read())
            if len(self.lut) != _MORPH_LUT_SIZE:
                self.lut = None
                raise Exception("Wrong size operator file!")
            return self.lut

        def save_lut(self, filename):
            if self.lut is None:
                raise Exception("No operator loaded")
            with open(filename, "wb") as f:
                f.write(bytes(self.lut))

        def set_lut(self, lut):
            self.lut = lut

    ImageMorph.LutBuilder = _LutBuilder
    ImageMorph.MorphOp = _MorphOp
    ImageMorph.LUT_SIZE = _MORPH_LUT_SIZE
    ImageMorph.ROTATION_MATRIX = _MORPH_ROTATION
    ImageMorph.MIRROR_MATRIX = _MORPH_MIRROR

    # -- ImagePath -------------------------------------------------------------
    ImagePath = types.ModuleType("PIL.ImagePath")

    class _Path:
        """PIL's `ImagePath.Path`: the point sequence `ImageDraw` takes for polygons
        and lines. The module was missing altogether, so `from PIL import ImagePath`
        raised ImportError."""

        def __init__(self, xy):
            if isinstance(xy, _Path):
                self.xy = list(xy.xy)
                return
            pts = list(xy)
            if pts and isinstance(pts[0], (int, float)):
                if len(pts) % 2:
                    raise ValueError("wrong number of coordinates")
                self.xy = [
                    (float(pts[i]), float(pts[i + 1])) for i in range(0, len(pts), 2)
                ]
            else:
                self.xy = [(float(p[0]), float(p[1])) for p in pts]

        def __len__(self):
            return len(self.xy)

        def __getitem__(self, i):
            if isinstance(i, slice):
                return [p for p in self.xy[i]]
            return self.xy[i]

        def __setitem__(self, i, value):
            self.xy[i] = (float(value[0]), float(value[1]))

        def __iter__(self):
            return iter(self.xy)

        def __repr__(self):
            return "<Path " + repr(self.xy) + ">"

        def tolist(self, flat=0):
            if flat:
                return [c for p in self.xy for c in p]
            return list(self.xy)

        def getbbox(self):
            if not self.xy:
                return (0.0, 0.0, 0.0, 0.0)
            xs = [p[0] for p in self.xy]
            ys = [p[1] for p in self.xy]
            return (min(xs), min(ys), max(xs), max(ys))

        def compact(self, distance=2):
            """PIL drops points closer than `distance` (Manhattan) to the one kept
            before them and answers how many went."""
            if not self.xy:
                return 0
            kept = [self.xy[0]]
            dropped = 0
            for p in self.xy[1:]:
                if abs(p[0] - kept[-1][0]) + abs(p[1] - kept[-1][1]) < distance:
                    dropped += 1
                else:
                    kept.append(p)
            self.xy = kept
            return dropped

        def map(self, function):
            self.xy = [tuple(function(x, y)) for x, y in self.xy]

        def transform(self, matrix):
            a, b, c, d, e, f = matrix[:6]
            self.xy = [(a * x + b * y + c, d * x + e * y + f) for x, y in self.xy]

    ImagePath.Path = _Path

    # -- assemble the PIL package -------------------------------------------
    PIL = types.ModuleType("PIL")
    PIL.__doc__ = (
        "vis Pillow-compatible shim backed by the host com.blockether/imaging renderer."
    )
    PIL.__version__ = "10.0-vis-imaging"
    PIL.__path__ = []
    PIL.UnidentifiedImageError = UnidentifiedImageError
    PIL.Image = Image_mod
    PIL.ImageDraw = ImageDraw
    PIL.ImageFilter = ImageFilter
    PIL.ImageOps = ImageOps
    PIL.ImageColor = ImageColor
    PIL.ImageEnhance = ImageEnhance
    PIL.ImageChops = ImageChops
    PIL.ImageFont = ImageFont
    PIL.ImageStat = ImageStat
    PIL.ImageMath = ImageMath
    PIL.ImageSequence = ImageSequence
    PIL.ImagePalette = ImagePalette
    PIL.ImageTransform = ImageTransform
    PIL.ImagePath = ImagePath
    PIL.features = features
    PIL.ExifTags = ExifTags
    PIL.TiffTags = TiffTags
    PIL.ImageMorph = ImageMorph
    PIL.ImageFile = ImageFile
    PIL.ImageGrab = ImageGrab
    PIL.ImageTk = ImageTk
    PIL.ImageWin = ImageWin
    PIL.ImageQt = ImageQt
    PIL.PSDraw = PSDraw
    PIL.ImageShow = ImageShow
    PIL.__all__ = [
        "Image",
        "ImageDraw",
        "ImageFilter",
        "ImageOps",
        "ImageColor",
        "ImageEnhance",
        "ImageChops",
        "ImageFont",
        "ImageStat",
        "ImageMath",
        "ImageSequence",
        "ImagePalette",
        "ImageTransform",
        "ImagePath",
        "features",
        "ExifTags",
        "TiffTags",
        "ImageMorph",
    ]

    sys.modules["PIL"] = PIL
    sys.modules["PIL.Image"] = Image_mod
    sys.modules["PIL.ImageDraw"] = ImageDraw
    sys.modules["PIL.ImageFilter"] = ImageFilter
    sys.modules["PIL.ImageOps"] = ImageOps
    sys.modules["PIL.ImageColor"] = ImageColor
    sys.modules["PIL.ImageEnhance"] = ImageEnhance
    sys.modules["PIL.ImageChops"] = ImageChops
    sys.modules["PIL.ImageFont"] = ImageFont
    sys.modules["PIL.ImageStat"] = ImageStat
    sys.modules["PIL.ImageMath"] = ImageMath
    sys.modules["PIL.ImageSequence"] = ImageSequence
    sys.modules["PIL.ImagePalette"] = ImagePalette
    sys.modules["PIL.ImageTransform"] = ImageTransform
    sys.modules["PIL.ImagePath"] = ImagePath
    sys.modules["PIL.features"] = features
    sys.modules["PIL.ExifTags"] = ExifTags
    sys.modules["PIL.TiffTags"] = TiffTags
    sys.modules["PIL.ImageMorph"] = ImageMorph
    sys.modules["PIL.ImageFile"] = ImageFile
    sys.modules["PIL.ImageGrab"] = ImageGrab
    sys.modules["PIL.ImageTk"] = ImageTk
    sys.modules["PIL.ImageWin"] = ImageWin
    sys.modules["PIL.ImageQt"] = ImageQt
    sys.modules["PIL.PSDraw"] = PSDraw
    sys.modules["PIL.ImageShow"] = ImageShow

    # Autoload: staple onto builtins so PIL.Image / Image.new work in every
    # python_execution block WITHOUT an explicit import (mirrors json/yaml/matplotlib).
    try:
        import builtins as _b

        _b.PIL = PIL
        _b.Image = Image_mod
        _b.ImageDraw = ImageDraw
        _b.ImageFilter = ImageFilter
        _b.ImageOps = ImageOps
        _b.ImageColor = ImageColor
        _b.ImageEnhance = ImageEnhance
        _b.ImageChops = ImageChops
        _b.ImageFont = ImageFont
        _b.ImageStat = ImageStat
        _b.ImageMath = ImageMath
        _b.ImageSequence = ImageSequence
        _b.ImagePalette = ImagePalette
        _b.ImageTransform = ImageTransform
        _b.ImagePath = ImagePath
        _b.ImageMorph = ImageMorph
    except Exception:
        pass


__vis_install_pil__()
del __vis_install_pil__
