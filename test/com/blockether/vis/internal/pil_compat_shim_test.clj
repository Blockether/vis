(ns com.blockether.vis.internal.pil-compat-shim-test
  "The Pillow (PIL)-compat shim installed into every sandbox context via the
   generic sandbox-shim mechanism (`extension/sandbox-shims`): a `PIL` package
   published into `sys.modules` (so `from PIL import Image` works) and backed by
   the host com.blockether/imaging renderer. All image ops delegate across the
   boundary to the host `__vis_pil_*` callables, keeping the pixels on the JVM."
  (:require [clojure.repl :as repl]
            [clojure.string :as str]
            [com.blockether.imaging :as im]
            [com.blockether.vis.internal.env-python :as ep]
            [com.blockether.vis.internal.foundation.mpl-capture :as mpl-capture]
            [com.blockether.vis.internal.foundation.shim-pil :as shim-pil]
            [com.blockether.vis.test-python-context :as tpc]
            [lazytest.core :refer [defdescribe expect it]])
  (:import [java.util Arrays Base64]
           [org.graalvm.polyglot Context]))

(defn- ev [^Context c code] (ep/->clj (.eval c "python" code)))

(defn- live-raster?
  "Does the host registry still hold this handle's raster? The registry is what
   the Java heap actually pays for; the Python `Image` is a handle wrapper."
  [h]
  (contains? (deref @#'shim-pil/registry) (long h)))

(defn- live-rasters
  "How many rasters the host registry is holding right now."
  ^long []
  (count (deref @#'shim-pil/registry)))

(defmacro with-python-context
  [& body]
  `(let [~(with-meta 'python-context {:tag `Context}) (tpc/shared)]
     ~@body))

(def ^:private gray-png-b64
  "4x4 8-bit grayscale PNG (colour type 0); pixel (x,y) is 40 + 10*(4y+x)."
  (str "iVBORw0KGgoAAAANSUhEUgAAAAQAAAAECAAAAACMmsGiAAAAHElEQVR4nGPQMLJxYwiISs"
       "ljqGjqmcawYNWWfQA3jAcxBo1JrAAAAABJRU5ErkJggg=="))

(def ^:private gray-alpha-png-b64
  "3x3 8-bit gray+alpha PNG (colour type 4), every pixel gray 90 at alpha 128."
  (str "iVBORw0KGgoAAAANSUhEUgAAAAMAAAADCAQAAAD8IX00AAAAD0lEQVR4nGOIagBBBhQKAE"
       "/jB6sEnE9fAAAAAElFTkSuQmCC"))

(defdescribe
  pil-module-test
  (it "publishes PIL + PIL.Image under sys.modules"
      (with-python-context
        (expect (true? (ev python-context
                           "import PIL.Image\n__import__('sys').modules.get('PIL') is not None")))
        (expect (true? (ev python-context
                           "__import__('sys').modules.get('PIL.Image') is not None")))))
  (it "autoloads PIL onto builtins (no import needed)"
      (with-python-context (expect (true? (ev python-context "PIL.Image is not None")))))
  (it "supports `from PIL import Image, ImageDraw`"
      (with-python-context
        (expect (true?
                  (ev python-context
                      "from PIL import Image, ImageDraw\nImage.new('RGB',(2,2)).size == (2,2)")))))
  (it "exposes a version string"
      (with-python-context (expect (= "10.0-vis-imaging"
                                      (ev python-context "__import__('PIL').__version__"))))))

(defdescribe
  pil-image-test
  (it "new + getpixel + putpixel round-trip"
      (with-python-context
        (expect
          (true? (ev python-context
                     (str "from PIL import Image\n" "im = Image.new('RGB',(4,4),(10,20,30))\n"
                          "im.putpixel((1,1),(9,8,7))\n"
                          "im.getpixel((0,0)) == (10,20,30) and im.getpixel((1,1)) == (9,8,7)"))))))
  (it "saves a real PNG (magic bytes) to a BytesIO buffer"
      (with-python-context
        (expect (true? (ev python-context
                           (str "from PIL import Image\nimport io\n"
                                "im = Image.new('RGB',(8,8),(1,2,3))\n"
                                "b = io.BytesIO(); im.save(b,'PNG')\n"
                                "list(b.getvalue()[:8]) == [137,80,78,71,13,10,26,10]"))))))
  (it "open round-trips a saved PNG"
      (with-python-context
        (expect (true? (ev python-context
                           (str "from PIL import Image\nimport io\n"
                                "src = Image.new('RGB',(6,6),(200,100,50))\n"
                                "b = io.BytesIO(); src.save(b,'PNG'); b.seek(0)\n"
                                "op = Image.open(b)\n"
                                "op.size == (6,6) and op.getpixel((2,2)) == (200,100,50)"))))))
  (it "open reports a grayscale source as mode L, not RGB"
      ;; A raster is always RGBA once decoded, so the mode can only come from the
      ;; SOURCE colour type `imaging/probe` reports. This 4x4 8-bit gray PNG
      ;; (colour type 0) is the smallest thing that proves it.
      (with-python-context
        (expect (true?
                  (ev python-context
                      (str "from PIL import Image\nimport io, base64\n"
                           "raw = base64.b64decode('"
                           gray-png-b64
                           "')\n"
                           "im = Image.open(io.BytesIO(raw))\n"
                           "im.mode == 'L' and im.size == (4,4) and im.getpixel((1,1)) == 90"))))))
  (it "open reports a gray+alpha source as mode LA"
      (with-python-context (expect (true? (ev python-context
                                              (str "from PIL import Image\nimport io, base64\n"
                                                   "raw = base64.b64decode('"
                                                   gray-alpha-png-b64
                                                   "')\n"
                                                   "im = Image.open(io.BytesIO(raw))\n"
                                                   "im.mode == 'LA' and im.size == (3,3)"))))))
  (it "Image.open detects the format; a fresh Image has none"
      (with-python-context
        (expect (true? (ev python-context
                           (str
                             "from PIL import Image\nimport io\n"
                             "im = Image.new('RGB',(4,4),(1,2,3))\n" "outs = []\n"
                             "for f in ('PNG','JPEG','GIF','BMP'):\n"
                             "    b = io.BytesIO(); im.save(b,f)\n"
                             "    outs.append(Image.open(io.BytesIO(b.getvalue())).format == f)\n"
                             "all(outs) and Image.new('L',(2,2)).format is None"))))))
  (it "convert('L') uses sRGB-space luminance (Pillow ITU-R 601-2)"
      (with-python-context
        ;; round(0.299*10 + 0.587*20 + 0.114*30) == 18
        (expect (= 18
                   (ev python-context
                       (str "from PIL import Image\n"
                            "Image.new('RGB',(2,2),(10,20,30)).convert('L').getpixel((0,0))"))))))
  (it "an 'L' image round-trips its byte value (no gamma)"
      (with-python-context (expect (= 200
                                      (ev python-context
                                          (str
                                            "from PIL import Image\n"
                                            "l = Image.new('L',(2,2),128); l.putpixel((0,0),200)\n"
                                            "l.getpixel((0,0))"))))))
  (it "resize / crop / rotate(expand) / transpose change dimensions"
      (with-python-context
        (expect (true? (ev python-context
                           (str "from PIL import Image\n" "im = Image.new('RGB',(40,30),(0,0,0))\n"
                                "im.resize((20,15)).size == (20,15) "
                                "and im.crop((0,0,10,8)).size == (10,8) "
                                "and im.rotate(90, expand=True).size == (30,40) "
                                "and im.transpose(Image.ROTATE_90).size == (30,40)")))))))

(defdescribe
  pil-draw-and-modules-test
  (it "ImageDraw paints shapes onto the image"
      (with-python-context (expect (= [0 0 255]
                                      (ev python-context
                                          (str "from PIL import Image, ImageDraw\n"
                                               "im = Image.new('RGB',(20,20),(0,0,0))\n"
                                               "d = ImageDraw.Draw(im)\n"
                                               "d.rectangle([0,0,19,19], fill=(0,0,255))\n"
                                               "list(im.getpixel((10,10)))"))))))
  (it "ImageColor parses hex and named colours"
      (with-python-context
        (expect (true? (ev python-context
                           (str "from PIL import ImageColor\n"
                                "tuple(ImageColor.getrgb('#ff8800')) == (255,136,0) "
                                "and tuple(ImageColor.getrgb('navy')) == (0,0,128)"))))))
  (it "ImageFilter GaussianBlur and SHARPEN keep the size"
      (with-python-context
        (expect (true? (ev python-context
                           (str "from PIL import Image, ImageFilter\n"
                                "im = Image.new('RGB',(16,16),(120,120,120))\n"
                                "im.filter(ImageFilter.GaussianBlur(2)).size == (16,16) "
                                "and im.filter(ImageFilter.SHARPEN).size == (16,16)"))))))
  (it "ImageOps.invert inverts channels"
      (with-python-context (expect (= [245 235 225]
                                      (ev python-context
                                          (str "from PIL import Image, ImageOps\n"
                                               "im = Image.new('RGB',(2,2),(10,20,30))\n"
                                               "list(ImageOps.invert(im).getpixel((0,0)))"))))))
  (it "split then merge is a round-trip"
      (with-python-context
        (expect
          (true?
            (ev python-context
                (str
                  "from PIL import Image\n" "im = Image.new('RGB',(4,4),(7,9,11))\n"
                  "bands = im.split()\n"
                  "len(bands) == 3 and Image.merge('RGB', bands).getpixel((0,0)) == (7,9,11)"))))))
  (it "ImageChops.difference of an image with itself is zero"
      (with-python-context
        (expect (= [0 0 0]
                   (ev python-context
                       (str "from PIL import Image, ImageChops\n"
                            "im = Image.new('RGB',(3,3),(50,60,70))\n"
                            "list(ImageChops.difference(im, im.copy()).getpixel((0,0)))"))))))
  (it "ImageEnhance.Brightness enhances toward the original"
      (with-python-context
        (expect
          (true?
            (ev python-context
                (str "from PIL import Image, ImageEnhance\n"
                     "im = Image.new('RGB',(2,2),(100,100,100))\n"
                     "ImageEnhance.Brightness(im).enhance(0.5).getpixel((0,0)) == (50,50,50)")))))))

(defdescribe pil-show-attachment-test
             (it "Image.show() records an inline PNG attachment on the active sink"
                 (with-python-context (let [sink (atom [])]
                                        (binding [mpl-capture/*attachment-sink* sink]
                                          (ev python-context
                                              (str "from PIL import Image\n"
                                                   "Image.new('RGB',(12,8),(200,50,25)).show()")))
                                        (expect (= 1 (count @sink)))
                                        (expect (= "image" (:kind (first @sink))))
                                        (expect (= "image/png" (:media-type (first @sink))))
                                        (expect (= "12x8" (:dims (first @sink))))))))

(defdescribe
  pil-extended-api-test
  (it "publishes the extended submodules"
      (with-python-context
        (expect
          (true? (ev python-context
                     (str "import PIL\n" "all(hasattr(PIL, m) for m in"
                          " ['ImageStat','ImageMath','ImageSequence','ImagePalette',"
                          "  'ImageTransform','features','ExifTags','TiffTags','ImageMorph'])"))))))
  (it "Image.transform AFFINE identity preserves pixels"
      (with-python-context
        (expect (true? (ev python-context
                           (str "from PIL import Image\n"
                                "im = Image.new('RGB',(16,16),(10,20,30))\n"
                                "t = im.transform((16,16), Image.AFFINE, (1,0,0,0,1,0))\n"
                                "t.getpixel((5,5)) == (10,20,30)"))))))
  (it "Image.transform QUAD maps the corners"
      (with-python-context
        (expect (true? (ev python-context
                           (str "from PIL import Image\n" "im = Image.new('RGB',(16,16),(7,8,9))\n"
                                "q = im.transform((16,16), Image.QUAD, (0,0,0,16,16,16,16,0))\n"
                                "q.getpixel((8,8)) == (7,8,9)"))))))
  (it "reduce / getcolors / entropy behave"
      (with-python-context
        (expect (true? (ev python-context
                           (str "from PIL import Image\n" "im = Image.new('L',(10,10),50)\n"
                                "im.reduce(2).size == (5,5) and"
                                " im.getcolors()[0] == (100,50) and abs(im.entropy()) < 1e-9"))))))
  (it "ImageStat computes mean / stddev / extrema"
      (with-python-context
        (expect (true? (ev python-context
                           (str "from PIL import Image, ImageStat\n"
                                "s = ImageStat.Stat(Image.new('L',(10,10),50))\n"
                                "abs(s.mean[0]-50) < 1e-6 and abs(s.stddev[0]) < 1e-6"
                                " and s.extrema[0] == (50,50) and s.count[0] == 100"))))))
  (it "ImageMath.eval does pixel arithmetic"
      (with-python-context
        (expect (true? (ev python-context
                           (str "from PIL import Image, ImageMath\n"
                                "a = Image.new('L',(4,4),200); b = Image.new('L',(4,4),100)\n"
                                "ImageMath.eval('a + b', a=a, b=b).getpixel((0,0)) == 255"))))))
  (it "ImageChops blend-mode + offset ops work"
      (with-python-context
        (expect (true? (ev python-context
                           (str "from PIL import Image, ImageChops\n"
                                "a = Image.new('L',(4,4),200); b = Image.new('L',(4,4),100)\n"
                                "ImageChops.blend(a,b,0.5).getpixel((0,0)) == 150 and"
                                " ImageChops.offset(a,1,1).size == (4,4) and"
                                " ImageChops.overlay(a,b).getpixel((0,0)) is not None"))))))
  (it "Image.alpha_composite blends RGBA over RGBA"
      (with-python-context
        (expect (true? (ev python-context
                           (str "from PIL import Image\n"
                                "base = Image.new('RGBA',(4,4),(255,0,0,255))\n"
                                "top = Image.new('RGBA',(4,4),(0,0,255,128))\n"
                                "p = Image.alpha_composite(base, top).getpixel((0,0))\n"
                                "p[3] == 255 and p[2] > 100 and p[0] > 100"))))))
  (it "module-level gradients + effects produce images"
      (with-python-context
        (expect (true?
                  (ev python-context
                      (str
                        "from PIL import Image\n"
                        "Image.linear_gradient('L').getpixel((0,255)) == 255 and"
                        " Image.radial_gradient('L').size == (256,256) and"
                        " Image.effect_mandelbrot((16,16),(-2,-1.5,1,1.5),50).size == (16,16)"))))))
  (it "ImageDraw gains rounded_rectangle / regular_polygon / circle + floodfill"
      (with-python-context
        (expect
          (true?
            (ev
              python-context
              (str
                "from PIL import Image, ImageDraw\n" "c = Image.new('RGB',(40,40),'white')\n"
                "d = ImageDraw.Draw(c)\n"
                "d.rounded_rectangle((2,2,38,38), radius=8, fill='red', outline='black', width=2)\n"
                "d.regular_polygon(((20,20),10), 6, fill='blue')\n"
                "d.circle((20,20), 5, fill='green')\n"
                "ImageDraw.floodfill(c, (0,0), (0,255,0))\n" "c.getpixel((0,0)) == (0,255,0)"))))))
  (it "ImageOps gains crop / exif_transpose"
      (with-python-context (expect (true?
                                     (ev python-context
                                         (str "from PIL import Image, ImageOps\n"
                                              "im = Image.new('RGB',(10,10),(5,5,5))\n"
                                              "ImageOps.crop(im, 2).size == (6,6) and"
                                              " ImageOps.exif_transpose(im).size == (10,10)")))))))

(defdescribe
  pil-multi-frame-test
  (it "save_all + append_images writes a real multi-frame GIF"
      (with-python-context
        (expect (true?
                  (ev python-context
                      (str "from PIL import Image\nimport io\n"
                           "a = Image.new('RGB',(8,8),(255,0,0))\n"
                           "b = Image.new('RGB',(8,8),(0,0,255))\n"
                           "buf = io.BytesIO()\n"
                           "a.save(buf,'GIF',save_all=True,append_images=[b],duration=120,loop=0)\n"
                           "g = buf.getvalue()\n" "g[:6] == b'GIF89a' and len(g) > 20"))))))
  (it "an animated GIF reopens with n_frames / is_animated / seek"
      (with-python-context
        (expect (true? (ev python-context
                           (str "from PIL import Image\nimport io\n"
                                "a = Image.new('RGB',(8,8),(255,0,0))\n"
                                "b = Image.new('RGB',(8,8),(0,0,255))\n"
                                "buf = io.BytesIO()\n"
                                "a.save(buf,'GIF',save_all=True,append_images=[b],duration=120)\n"
                                "im = Image.open(io.BytesIO(buf.getvalue()))\n"
                                "im.seek(1); second = im.getpixel((4,4))\n"
                                "im.seek(0); first = im.getpixel((4,4))\n"
                                "im.n_frames == 2 and im.is_animated and im.format == 'GIF' "
                                "and first == (255,0,0) and second == (0,0,255) "
                                "and im.info.get('duration') == 120"))))))
  (it "a single-frame save stays a still image"
      (with-python-context (expect (true? (ev python-context
                                              (str
                                                "from PIL import Image\nimport io\n"
                                                "buf = io.BytesIO()\n"
                                                "Image.new('RGB',(4,4),(1,2,3)).save(buf,'GIF')\n"
                                                "im = Image.open(io.BytesIO(buf.getvalue()))\n"
                                                "im.n_frames == 1 and not im.is_animated")))))))

(defdescribe
  pil-palette-and-quality-test
  (it "quantize produces mode P with a readable palette"
      (with-python-context
        (expect (true? (ev python-context
                           (str "from PIL import Image\n"
                                "q = Image.new('RGB',(8,8),(10,200,30)).quantize(colors=4)\n"
                                "pal = q.getpalette()\n"
                                "q.mode == 'P' and pal and len(pal) % 3 == 0 "
                                "and pal[:3] == [10,200,30] "
                                "and q.convert('RGB').getpixel((0,0)) == (10,200,30)"))))))
  (it "putpalette round-trips through getpalette"
      (with-python-context
        (expect (= [1 2 3 4 5 6]
                   (ev python-context
                       (str "from PIL import Image\n"
                            "p = Image.new('RGB',(4,4),(0,0,0)).quantize(colors=2)\n"
                            "p.putpalette([1,2,3,4,5,6])\n" "list(p.getpalette()[:6])"))))))
  (it "JPEG quality actually changes the encoded size"
      (with-python-context
        (expect (true? (ev python-context
                           (str "from PIL import Image\nimport io\n"
                                "src = Image.new('RGB',(64,64),(0,0,0))\n"
                                "for x in range(64):\n" "    for y in range(64):\n"
                                "        src.putpixel((x,y), ((x*7)%256,(y*13)%256,(x*y)%256))\n"
                                "lo = io.BytesIO(); src.save(lo,'JPEG',quality=5)\n"
                                "hi = io.BytesIO(); src.save(hi,'JPEG',quality=95)\n"
                                "len(lo.getvalue()) * 2 < len(hi.getvalue())"))))))
  (it "P-mode quantisation IS com.blockether/imaging's, not an algorithm of ours"
      (with-python-context
        (let
          [[png-b64 palette]
           (ev python-context
               (str "from PIL import Image\nimport io, base64\n"
                    "src = Image.new('RGB',(32,24),(0,0,0))\n"
                    "for x in range(32):\n" "    for y in range(24):\n"
                    "        src.putpixel((x,y), ((x*7)%256,(y*11)%256,(x*y)%256))\n"
                    "buf = io.BytesIO(); src.save(buf,'PNG')\n"
                    "q = src.quantize(colors=8)\n"
                    "[base64.b64encode(buf.getvalue()).decode(), list(q.getpalette()[:24])]"))

           png
           (.decode (Base64/getDecoder) ^String png-b64)]

          (with-open [img (im/decode png)]
            ;; The shim only packs what the Rust median-cut returns: same bytes in,
            ;; same palette out. If this drifts, someone re-implemented a quantiser.
            (expect (= (mapcat (fn [c]
                                 [(bit-and (bit-shift-right ^long c 16) 0xff)
                                  (bit-and (bit-shift-right ^long c 8) 0xff)
                                  (bit-and ^long c 0xff)])
                               (:palette (im/quantize img {:colors 8})))
                       (map long palette)))))))
  (it
    "ImageFilter IS com.blockether/imaging's convolve and rank filter"
    (with-python-context
      (let
        [[src-b64 blur-b64 med-b64]
         (ev python-context
             (str "from PIL import Image, ImageFilter\nimport io, base64\n"
                  "src = Image.new('RGB',(24,18),(0,0,0))\n"
                  "for x in range(24):\n" "    for y in range(18):\n"
                  "        src.putpixel((x,y), ((x*9)%256,(y*13)%256,(x*y)%256))\n" "def enc(im):\n"
                  "    b = io.BytesIO(); im.save(b,'PNG')\n"
                  "    return base64.b64encode(b.getvalue()).decode()\n"
                  "[enc(src), enc(src.filter(ImageFilter.BoxBlur(1))),\n"
                  " enc(src.filter(ImageFilter.MedianFilter(3)))]"))

         ->img
         #(im/decode (.decode (Base64/getDecoder) ^String %))]

        (with-open
          [src
           (->img src-b64)

           blur
           (->img blur-b64)

           med
           (->img med-b64)

           ours-blur
           (im/convolve src 3 (repeat 9 1.0) {:scale 9.0})

           ours-med
           (im/rank-filter src 3 :median)]

          ;; Pixel-for-pixel: the shim hands the kernel over and packs the
          ;; answer back — the neighbourhood maths happens in Rust.
          (expect (= (seq (im/pixels ours-blur)) (seq (im/pixels blur))))
          (expect (= (seq (im/pixels ours-med)) (seq (im/pixels med))))))))
  (it "save(optimize=True) IS com.blockether/imaging's optimizer, losslessly"
      (with-python-context
        (let
          [[plain-b64 opt-b64]
           (ev python-context
               (str "from PIL import Image\nimport io, base64\n"
                    "src = Image.new('RGB',(64,64),(0,0,0))\n" "for x in range(64):\n"
                    "    for y in range(64):\n"
                    "        src.putpixel((x,y), ((x*7)%256,(y*5)%256,((x+y)*3)%256))\n"
                    "def enc(**kw):\n" "    b = io.BytesIO(); src.save(b,'PNG',**kw)\n"
                    "    return base64.b64encode(b.getvalue()).decode()\n"
                    "[enc(), enc(optimize=True)]"))

           ^bytes plain
           (.decode (Base64/getDecoder) ^String plain-b64)

           ^bytes opt
           (.decode (Base64/getDecoder) ^String opt-b64)]

          ;; Pillow's flag is honoured, not ignored: smaller bytes, the SAME bytes
          ;; the library's optimizer produces, and the very same picture.
          (expect (< (alength opt) (alength plain)))
          (expect (= (seq opt) (seq (im/optimize plain))))
          (with-open
            [a
             (im/decode plain)

             b
             (im/decode opt)]

            (expect (= (seq (im/pixels a)) (seq (im/pixels b)))))))))

(defdescribe
  pil-transpose-test
  (it "Image.transpose is imaging's flip/rotate, with Pillow's own orientations"
      (with-python-context
        ;; 3x2 of six distinguishable pixels; every method is asserted against the
        ;; layout Pillow produces, so a library rotation flipping direction fails here.
        (expect (= [[[3 2] [70 40 10 160 130 100]] [[3 2] [100 130 160 10 40 70]]
                    [[2 3] [70 160 40 130 10 100]] [[3 2] [160 130 100 70 40 10]]
                    [[2 3] [100 10 130 40 160 70]] [[2 3] [10 100 40 130 70 160]]
                    [[2 3] [160 70 130 40 100 10]]]
                   (ev python-context
                       (str
                         "from PIL import Image\n"
                         "im = Image.new('RGB',(3,2))\n" "vals = [10,40,70,100,130,160]\n"
                         "for y in range(2):\n" "    for x in range(3):\n"
                         "        im.putpixel((x,y), (vals[y*3+x],0,0))\n" "def rows(t):\n"
                         "    w, h = t.size\n" "    return [list(t.size),\n"
                         "            [t.getpixel((x,y))[0] for y in range(h) for x in range(w)]]\n"
                         "[rows(im.transpose(m)) for m in range(7)]")))))))

(defdescribe
  pil-exif-and-limits-test
  (it "getexif is a mutable, cached Exif mapping"
      (with-python-context
        (expect (true?
                  (ev python-context
                      (str "from PIL import Image\n" "im = Image.new('RGB',(4,4),(1,2,3))\n"
                           "im.getexif()[271] = 'vis'\n"
                           "im.getexif().get(271) == 'vis' and im.getexif().get(9999) is None"))))))
  (it "exif_transpose applies orientation 6 and clears the tag"
      (with-python-context
        (expect (true? (ev python-context
                           (str "from PIL import Image, ImageOps\n"
                                "im = Image.new('RGB',(4,2),(10,20,30))\n"
                                "im.info['exif'] = b'\\x00'\n"
                                "im.getexif()[274] = 6\n" "out = ImageOps.exif_transpose(im)\n"
                                "out.size == (2,4) and 274 not in out.getexif() "
                                "and 'exif' not in out.info and im.size == (4,2)"))))))
  (it "exif_transpose(in_place=True) returns None and rotates the original"
      (with-python-context
        (expect (true? (ev python-context
                           (str "from PIL import Image, ImageOps\n"
                                "im = Image.new('RGB',(4,2),(1,2,3))\n" "im.getexif()[274] = 8\n"
                                "r = ImageOps.exif_transpose(im, in_place=True)\n"
                                "r is None and im.size == (2,4) and 274 not in im.getexif()"))))))
  (it "an image with no orientation is copied unchanged"
      (with-python-context (expect (true? (ev python-context
                                              (str "from PIL import Image, ImageOps\n"
                                                   "im = Image.new('RGB',(4,2),(1,2,3))\n"
                                                   "out = ImageOps.exif_transpose(im)\n"
                                                   "out is not im and out.size == (4,2) "
                                                   "and out.getpixel((0,0)) == (1,2,3)"))))))
  ;; The decoder allocates RGBA8, so 512 MiB is ~134 MP. Guarding on the HEADER
  ;; keeps the error truthful instead of the image crate's "cannot identify".
  (it "a header-huge PNG fails with an explicit too-large message"
      (with-python-context
        (expect
          (true?
            (ev python-context
                (str
                  "from PIL import Image\n"
                  "import io, struct, zlib, binascii\n" "def chunk(t, d):\n"
                  "    return (struct.pack('>I', len(d)) + t + d\n"
                  "            + struct.pack('>I', binascii.crc32(t + d) & 0xffffffff))\n"
                  "ihdr = struct.pack('>IIBBBBB', 20000, 20000, 8, 6, 0, 0, 0)\n"
                  "png = (b'\\x89PNG\\r\\n\\x1a\\n' + chunk(b'IHDR', ihdr)\n"
                  "       + chunk(b'IDAT', zlib.compress(b'\\x00' * 10)) + chunk(b'IEND', b''))\n"
                  "try:\n"
                  "    Image.open(io.BytesIO(png)); ok = False\n" "except OSError as e:\n"
                  "    ok = 'too large' in str(e) and '20000x20000' in str(e)\n" "ok")))))))

(defdescribe
  pil-package-submodule-test
  (it
    "imports file and desktop integration modules without pretending support"
    (with-python-context
      (expect
        (true?
          (ev
            python-context
            (str
              "from PIL import ImageFile, ImageGrab, ImageTk, ImageQt, PSDraw, ImageShow, UnidentifiedImageError\n"
              "ok = ImageFile.ImageFile is not None and issubclass(UnidentifiedImageError, OSError)\n"
              "try:\n    ImageGrab.grab()\nexcept NotImplementedError:\n    unavailable = True\n"
              "ok and unavailable")))))))

(defdescribe
  pil-desktop-import-regression-test
  (it
    "exposes conventional desktop symbols with a clear sandbox failure"
    (with-python-context
      (expect
        (true?
          (ev
            python-context
            (str
              "from PIL.ImageTk import BitmapImage, getimage\n"
              "from PIL.ImageWin import Dib, HDC, Window\n"
              "from PIL.ImageQt import toqimage, toqpixmap\n"
              "from PIL.ImageShow import Viewer, register\n"
              "symbols = [BitmapImage, getimage, Dib, HDC, Window, toqimage, toqpixmap, Viewer, register]\n"
              "try:\n    Dib()\nexcept NotImplementedError as error:\n    unavailable = 'PIL.ImageWin.Dib' in str(error)\n"
              "all(callable(symbol) for symbol in symbols) and unavailable")))))))

(defdescribe
  pil-draw-batching-test
  "A run of consecutive ImageDraw ops shares ONE live cdylib image; the pixels are
   flushed back into the host raster before any other op reads them."
  (it "consecutive draws accumulate and interleave with pixel reads"
      (with-python-context
        (expect (= [[255 0 0] [0 255 0] [0 0 255] [0 0 0]]
                   (ev python-context
                       (str "from PIL import Image, ImageDraw\n"
                            "im = Image.new('RGB',(20,20),(0,0,0))\n" "d = ImageDraw.Draw(im)\n"
                            "d.point((1,1), fill=(255,0,0))\n" "d.point((2,2), fill=(0,255,0))\n"
                            "d.rectangle([5,5,6,6], fill=(0,0,255))\n"
                            "[list(im.getpixel(p)) for p in [(1,1),(2,2),(5,5),(0,0)]]"))))))
  (it "pending draws are flushed for copy / tobytes / save"
      (with-python-context
        (expect (true? (ev python-context
                           (str "from PIL import Image, ImageDraw\n"
                                "im = Image.new('RGB',(4,4),(0,0,0))\n" "d = ImageDraw.Draw(im)\n"
                                "d.rectangle([0,0,3,3], fill=(1,2,3))\n" "c = im.copy()\n"
                                "list(c.getpixel((2,2))) == [1,2,3] "
                                "and im.tobytes()[:3] == bytes([1,2,3])"))))))
  (it "a draw run past the flush ceiling keeps every op"
      (with-python-context
        (expect (= [[255 0 0] [255 0 0] [255 0 0]]
                   (ev python-context
                       ;; 5000 ops > `max-pending-draws`, so the queue is force-flushed
                       ;; mid-run: the ops on both sides of the boundary must survive.
                       (str "from PIL import Image, ImageDraw\n"
                            "im = Image.new('RGB',(100,100),(0,0,0))\n"
                            "d = ImageDraw.Draw(im)\n" "for i in range(5000):\n"
                            "    d.point((i % 100, i // 100), fill=(255,0,0))\n"
                            "[list(im.getpixel(p)) for p in [(0,0),(50,20),(99,49)]]"))))))
  (it "a run of draws pays neither a canvas conversion nor a cdylib call per op"
      ;; COUNTED, never timed -- the same lesson as the three stopwatches below: this
      ;; run's wall-clock budget went red at 1056 ms on a loaded runner with nothing
      ;; wrong. Converting the 480k-pixel canvas in and out per op cost ~80 ms each --
      ;; about 160 s for this loop. Even sharing ONE live image, a cdylib `draw` call
      ;; per op still cost ~1.4 ms (~3 s here) because the canvas round-trips through
      ;; the renderer per call. Queued as ONE batch, the whole run crosses the boundary
      ;; exactly once in each direction, and THAT is a property of the code.
      (with-python-context
        (let
          [draw!
           im/draw!

           from-pixels
           im/from-pixels

           pixels
           im/pixels

           crossings
           (atom {:draw 0 :in 0 :out 0})

           painted
           (with-redefs
             [im/draw!
              (fn [& args]
                (swap! crossings update :draw inc)
                (apply draw! args))

              im/from-pixels
              (fn [& args]
                (swap! crossings update :in inc)
                (apply from-pixels args))

              im/pixels
              (fn [& args]
                (swap! crossings update :out inc)
                (apply pixels args))]

             (ev python-context
                 (str "from PIL import Image, ImageDraw\n"
                      "im = Image.new('RGB',(800,600),(0,0,0))\n"
                      "d = ImageDraw.Draw(im)\n" "for i in range(2000):\n"
                      "    d.point((i % 800, i % 600), fill=(255,255,255))\n"
                      "list(im.getpixel((200,400)))")))]

          (expect (= [255 255 255] painted))
          (expect (= {:draw 1 :in 1 :out 1} @crossings)
                  (str "2000 draws crossed the cdylib as " (pr-str @crossings))))))
  ;; Regression: the flush converted the canvas with `aset-byte`, i.e.
  ;; `java.lang.reflect.Array/setByte` -- four reflective stores per pixel, 65 ms per
  ;; 800x600 flush against 1.8 ms for the inlined store. No stopwatch pins that
  ;; portably, and three have now failed: a wall-clock budget went red on a loaded
  ;; runner at 2350 ms with nothing wrong; a canvas-size ratio went red because a
  ;; 480k-pixel canvas legitimately costs ~120x a 1.2k-pixel one; and timing the
  ;; reflective store against the shipped conversion in the same JVM -- ten warm-ups,
  ;; best of five each -- still read 32 ms against 37 ms on CI, where a runner
  ;; grinding through 6129 test cases never lets either loop leave the interpreter
  ;; (the same measurement is 1 ms against 46 ms on an idle machine). The margin is a
  ;; property of the CODE, not of the host, so assert the code: the conversion yields
  ;; exactly the reference bytes, and it names `aset` on a `^bytes` local -- never a
  ;; reflective or per-pixel store.
  (it
    "converting a drawn canvas does not pay a reflective store per pixel"
    (let
      [w
       800

       h
       600

       px
       (int-array (* w h) (unchecked-int 0xff204060))

       raster
       (shim-pil/->Raster px w h 0xff000000)

       reflective
       (let
         [n
          (alength ^ints px)

          b
          (byte-array (* 4 n))]

         (dotimes [i n]
           (let
             [p (bit-or 0xff000000 (bit-and 0xffffffff (aget ^ints px i)))
              o (* 4 i)]

             (aset-byte b o (unchecked-byte (bit-and (bit-shift-right p 16) 0xff)))
             (aset-byte b (+ o 1) (unchecked-byte (bit-and (bit-shift-right p 8) 0xff)))
             (aset-byte b (+ o 2) (unchecked-byte (bit-and p 0xff)))
             (aset-byte b (+ o 3) (unchecked-byte (bit-and (bit-shift-right p 24) 0xff)))))
         b)

       names
       (into #{}
             (comp (filter symbol?) (map str))
             (tree-seq coll?
                       seq
                       (read-string
                         (repl/source-fn
                           'com.blockether.vis.internal.foundation.shim-pil/raster->rgba))))

       offenders
       (filterv #(re-find #"^aset-|reflect\.|etRGB" %) names)]

      (expect (Arrays/equals ^bytes (#'shim-pil/raster->rgba raster) ^bytes reflective))
      (expect (contains? names "aset")
              "raster->rgba no longer stores through `aset` on a ^bytes local")
      (expect (empty? offenders)
              (str "raster->rgba reaches for "
                   (pr-str offenders)
                   " -- a reflective or per-pixel store")))))

(defdescribe
  pil-font-family-test
  "The font a caller asks for reaches the host renderer. It used to be dropped at
   the bridge -- every `draw.text` was painted in the fallback sans face at
   whatever size was requested -- which is what made monospaced grid renders come
   out letter-spaced and mangled."
  (it "a monospace family measures every glyph at the same advance"
      (with-python-context
        (expect
          (true?
            (ev python-context
                (str "from PIL import Image, ImageDraw, ImageFont\n"
                     "d = ImageDraw.Draw(Image.new('RGB',(10,10),(0,0,0)))\n"
                     "mono = ImageFont.truetype('Noto Sans Mono', 20)\n"
                     "sans = ImageFont.truetype('Noto Sans', 20)\n"
                     "(d.textlength('iiii', font=mono) == d.textlength('WWWW', font=mono)"
                     " and d.textlength('iiii', font=sans) < d.textlength('WWWW', font=sans))"))))))
  (it
    "the requested family reaches the PIXELS, not just the measurement"
    (with-python-context
      (expect
        (true?
          (ev
            python-context
            (str
              "from PIL import Image, ImageDraw, ImageFont\n" "def ink(family):\n"
              "    im = Image.new('RGB',(240,48),(0,0,0))\n" "    d = ImageDraw.Draw(im)\n"
              "    d.text((4,4), 'iiiiii', font=ImageFont.truetype(family, 24), fill=(255,255,255))\n"
              "    b = im.getbbox()\n"
              "    return b[2] - b[0]\n" "ink('Noto Sans Mono') > ink('Noto Sans') + 4"))))))
  (it "a -Bold face is rendered bold, weight read off the font name"
      (with-python-context
        (expect
          (true? (ev python-context
                     (str "from PIL import Image, ImageDraw, ImageFont\n"
                          "d = ImageDraw.Draw(Image.new('RGB',(10,10),(0,0,0)))\n"
                          "reg = ImageFont.truetype('NotoSans-Regular.ttf', 20)\n"
                          "bold = ImageFont.truetype('NotoSans-Bold.ttf', 20)\n"
                          "d.textlength('Hello', font=bold) > d.textlength('Hello', font=reg)"))))))
  (it "an unknown font falls back instead of failing"
      (with-python-context
        (expect (true? (ev python-context
                           (str "from PIL import Image, ImageDraw, ImageFont\n"
                                "im = Image.new('RGB',(120,40),(0,0,0))\n"
                                "d = ImageDraw.Draw(im)\n"
                                "f = ImageFont.truetype('/no/such/dir/Totally-Unknown.ttf', 20)\n"
                                "d.text((2,2), 'vis', font=f, fill=(255,255,255))\n"
                                "d.textlength('vis', font=f) > 0 and im.getbbox() is not None"))))))
  (it "no font at all still measures and paints, exactly as before"
      (with-python-context
        (expect (true? (ev python-context
                           (str "from PIL import Image, ImageDraw\n"
                                "im = Image.new('RGB',(120,40),(0,0,0))\n"
                                "d = ImageDraw.Draw(im)\n"
                                "d.text((2,2), 'vis', fill=(255,255,255))\n"
                                "d.textlength('vis') > 0 and im.getbbox() is not None")))))))

;; Regression, report 49f413b1 (the dark-theme logo session): quantizing the
;; logo with Pillow's own spelling `im.quantize(colors=8, method=Image.MEDIANCUT)`
;; died with "module 'PIL.Image' has no attribute 'MEDIANCUT'" -- the shim never
;; published the Quantize/Dither/Palette constants Pillow staples onto the module.
(defdescribe pil-quantize-constant-regression-test
             (it "publishes the Quantize/Dither/Palette constants on the module and the enums"
                 (with-python-context
                   (expect (= [0 1 2 3 0 3 0 1 true]
                              (ev python-context
                                  (str "from PIL import Image\n"
                                       "[Image.MEDIANCUT, Image.MAXCOVERAGE, Image.FASTOCTREE, "
                                       "Image.LIBIMAGEQUANT, Image.NONE, Image.FLOYDSTEINBERG, "
                                       "Image.WEB, Image.ADAPTIVE, "
                                       "Image.Quantize.MEDIANCUT == Image.MEDIANCUT "
                                       "and Image.Dither.FLOYDSTEINBERG == Image.FLOYDSTEINBERG "
                                       "and Image.Palette.ADAPTIVE == Image.ADAPTIVE]"))))))
             (it "quantize accepts method=Image.MEDIANCUT and returns a palette image"
                 (with-python-context
                   (expect (true? (ev python-context
                                      (str "from PIL import Image\n"
                                           "im = Image.new('RGB',(8,8),(200,30,40))\n"
                                           "q = im.quantize(colors=8, method=Image.MEDIANCUT, "
                                           "dither=Image.Dither.NONE)\n"
                                           "q.mode == 'P' and len(q.getpalette()) >= 3")))))))

;; Regression, report 5075808e (the iOS screenshot session): every dropped `Image`
;; left its host raster -- a packed int[], 4 bytes per pixel, ~12 MB for one phone
;; screenshot -- in the per-JVM registry FOREVER. GraalPy does not refcount, so
;; only an explicit `close()` ever freed one, and an in-place op such as
;; `thumbnail` replaced a raster while still pinning the one it replaced. A loop
;; over a directory of screenshots therefore ended in `OSError: Java heap space`,
;; and `del im` + `gc.collect()` changed nothing.
(defdescribe
  pil-raster-lifetime-test
  (it "frees the raster of a dropped image once nothing can reach it"
      (with-python-context (let
                             [handles (ev python-context
                                          (str "from PIL import Image\n" "import gc\n"
                                               "hs = []\n" "for _ in range(8):\n"
                                               "    im = Image.new('RGB',(64,64))\n"
                                               "    hs.append(im._handle)\n"
                                               "    del im\n" "gc.collect()\n"
                                               "__vis_run_reapers__()\n" "hs"))]
                             (expect (= 8 (count handles)))
                             (expect (= [] (filterv #(live-raster? %) handles))))))
  (it "sweeps INSIDE a block, so a loop over big images cannot fill the heap"
      ;; The reported failure exhausted the heap inside ONE block, where the
      ;; runtime's boundary reaper never gets a turn -- so allocating an
      ;; image is itself what sweeps. 80 dropped 4 MiB rasters are 320 MiB
      ;; the block can never reach again.
      (with-python-context (let
                             [before
                              (live-rasters)

                              done
                              (ev python-context
                                  (str "from PIL import Image\n"
                                       "for _ in range(80):\n"
                                       "    im = Image.new('RGB',(1024,1024))\n"
                                       "    del im\n" "'done'"))

                              grown
                              (- (live-rasters) before)]

                             (expect (= "done" done))
                             (expect (> 32 grown)))))
  (it "frees the raster an in-place op replaced, without waiting for a collection"
      (with-python-context (let
                             [[before after]
                              (ev python-context
                                  (str "from PIL import Image\n"
                                       "im = Image.new('RGB',(64,64))\n" "before = im._handle\n"
                                       "im.thumbnail((8,8))\n" "[before, im._handle]"))]
                             (expect (not= before after))
                             (expect (not (live-raster? before)))
                             (expect (live-raster? after)))))
  (it "keeps a raster two images share until the last of them lets it go"
      ;; `ImageOps.exif_transpose(im, in_place=True)` hands one handle to two
      ;; Images: freeing on the first drop would leave the other reading a
      ;; raster that is gone.
      (with-python-context (let
                             [shared (ev python-context
                                         (str "from PIL import Image\n"
                                              "a = Image.new('RGB',(16,16))\n"
                                              "b = Image.new('RGB',(16,16))\n"
                                              "b._set([a._handle, a._w, a._h, a.mode])\n"
                                              "a.close()\n" "b._handle"))]
                             (expect (live-raster? shared))
                             (expect (= [0 0 0] (ev python-context "list(b.getpixel((0,0)))")))
                             (ev python-context "b.close()")
                             (expect (not (live-raster? shared)))
                             ;; closing twice is a no-op, never a second free
                             (expect (= "ok" (ev python-context "b.close()\n'ok'"))))))
  (it "names a closed image instead of leaking a Java null-pointer message"
      (with-python-context (let
                             [msg (ev python-context
                                      (str "from PIL import Image\n"
                                           "im = Image.new('RGB',(4,4))\n" "im.close()\n"
                                           "try:\n" "    im.getpixel((0,0))\n"
                                           "    msg = 'NO ERROR'\n" "except OSError as e:\n"
                                           "    msg = str(e)\n" "msg"))]
                             (expect (str/includes? msg "is not live"))
                             (expect (not (str/includes? msg "Cannot invoke")))))))

;; Regression, report 55ed67f6 (the Android launcher-icon session): `paste` and
;; `composite` read a mask's BLUE byte instead of its alpha band, so
;; `im.paste(im, box, im)` -- the idiom for dropping a transparent PNG onto a
;; canvas -- blended every pixel by its own blueness. Gold (253,198,80) landed
;; cream (254,237,200) on white and brown on charcoal, which is how a set of
;; side-by-side app-icon proofs came out lying about the icons they compared.
(defdescribe
  pil-mask-band-regression-test
  (it "pastes an opaque RGBA source through its alpha band, unblended"
      (with-python-context (expect (= [253 198 80]
                                      (ev python-context
                                          (str "from PIL import Image\n"
                                               "src = Image.new('RGBA',(4,4),(253,198,80,255))\n"
                                               "dst = Image.new('RGB',(4,4),(255,255,255))\n"
                                               "dst.paste(src,(0,0),src)\n"
                                               "list(dst.getpixel((1,1)))"))))))
  (it "leaves the destination alone where the mask's alpha is zero"
      (with-python-context (expect (= [255 255 255]
                                      (ev python-context
                                          (str "from PIL import Image\n"
                                               "src = Image.new('RGBA',(4,4),(253,198,80,0))\n"
                                               "dst = Image.new('RGB',(4,4),(255,255,255))\n"
                                               "dst.paste(src,(0,0),src)\n"
                                               "list(dst.getpixel((1,1)))"))))))
  (it "blends an 'L' mask by its gray value"
      (with-python-context (expect (= [254 226 167]
                                      (ev python-context
                                          (str "from PIL import Image\n"
                                               "src = Image.new('RGBA',(4,4),(253,198,80,255))\n"
                                               "dst = Image.new('RGB',(4,4),(255,255,255))\n"
                                               "dst.paste(src,(0,0),Image.new('L',(4,4),128))\n"
                                               "list(dst.getpixel((1,1)))"))))))
  (it "takes a '1' mask as a boolean bitmap, and fills one with white"
      (with-python-context
        (expect
          (= [253 198 80 255]
             (ev python-context
                 (str "from PIL import Image\n"
                      "src = Image.new('RGBA',(4,4),(253,198,80,255))\n"
                      "dst = Image.new('RGB',(4,4),(255,255,255))\n"
                      "dst.paste(src,(0,0),Image.new('1',(4,4),1))\n"
                      "list(dst.getpixel((1,1))) + [Image.new('1',(2,2),1).getpixel((0,0))]"))))))
  (it "composites through the mask's alpha band"
      (with-python-context
        (expect (= [253 198 80]
                   (ev python-context
                       (str "from PIL import Image\n"
                            "gold = Image.new('RGB',(4,4),(253,198,80))\n"
                            "white = Image.new('RGB',(4,4),(255,255,255))\n"
                            "m = Image.new('RGBA',(4,4),(0,0,0,255))\n"
                            "list(Image.composite(gold, white, m).getpixel((1,1)))"))))))
  (it "refuses a mask mode PIL cannot read instead of blending some band of it"
      (with-python-context
        (expect (= "bad transparency mask"
                   (ev python-context
                       (str "from PIL import Image\n"
                            "src = Image.new('RGBA',(4,4),(253,198,80,255))\n"
                            "dst = Image.new('RGB',(4,4),(255,255,255))\n"
                            "try:\n" "    dst.paste(src,(0,0),Image.new('RGB',(4,4),(0,0,255)))\n"
                            "    msg = 'NO ERROR'\n" "except ValueError as e:\n"
                            "    msg = str(e)\n" "msg"))))))
  ;; `Image.new('LA', size, (gray, alpha))` used to die inside the host with a raw
  ;; Java NPE, because a two-element colour fell through to the RGB(A) arm and
  ;; unpacked nil. An 'LA' raster keeps the gray replicated across R/G/B here, so
  ;; the pair is read off the first and last components.
  (it "fills an 'LA' image from a (gray, alpha) pair"
      (with-python-context
        (expect (= [200 128]
                   (ev python-context
                       (str "from PIL import Image\n"
                            "p = Image.new('LA',(2,2),(200,128)).getpixel((0,0))\n"
                            "[p[0], p[-1]]")))))))
