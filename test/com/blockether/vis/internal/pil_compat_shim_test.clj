(ns com.blockether.vis.internal.pil-compat-shim-test
  "The Pillow (PIL)-compat shim installed into every sandbox context via the
   generic sandbox-shim mechanism (`extension/sandbox-shims`): a `PIL` package
   published into `sys.modules` (so `from PIL import Image` works) and backed by
   the host com.blockether/imaging renderer. All image ops delegate across the
   boundary to the host `__vis_pil_*` callables, keeping the pixels on the JVM."
  (:require [com.blockether.imaging :as im]
            [com.blockether.vis.internal.env-python :as ep]
            [com.blockether.vis.internal.foundation.mpl-capture :as mpl-capture]
            [lazytest.core :refer [defdescribe expect it]])
  (:import [java.util Base64]
           [org.graalvm.polyglot Context]))

(defn- ev [^Context c code] (ep/->clj (.eval c "python" code)))

;; A namespace-local context avoids paying GraalPy + shim bootstrap per assertion.
(defonce ^:private python-context* (delay (ep/create-python-context {})))

(defmacro with-python-context
  [& body]
  `(let [~(with-meta 'python-context {:tag `Context}) (:python-context @python-context*)]
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
      (with-python-context
        (let
          [t0
           (System/nanoTime)

           painted
           (ev python-context
               (str "from PIL import Image, ImageDraw\n" "im = Image.new('RGB',(800,600),(0,0,0))\n"
                    "d = ImageDraw.Draw(im)\n" "for i in range(2000):\n"
                    "    d.point((i % 800, i % 600), fill=(255,255,255))\n"
                    "list(im.getpixel((200,400)))"))

           ms
           (/ (- (System/nanoTime) t0) 1e6)]

          (expect (= [255 255 255] painted))
          ;; Converting the 480k-pixel canvas in and out per op cost ~80 ms each -- about
          ;; 160 s for this loop. Even sharing ONE live image, a cdylib `draw` call per op
          ;; still cost ~1.4 ms (~3 s here) because the canvas round-trips through the
          ;; renderer per call. Queued as ONE batch, the whole run is a few milliseconds.
          (expect (< ms 1000) (str "2000 draws took " (long ms) " ms")))))
  (it "reading a drawn canvas does not pay a reflective store per pixel"
      (with-python-context
        (let
          [t0
           (System/nanoTime)

           painted
           (ev python-context
               (str "from PIL import Image, ImageDraw\n"
                    "im = Image.new('RGB',(800,600),(0,0,0))\n" "d = ImageDraw.Draw(im)\n"
                    "for i in range(20):\n" "    d.rectangle([i,i,i+4,i+4], fill=(255,0,0))\n"
                    "    im.getpixel((i+1,i+1))\n" "list(im.getpixel((1,1)))"))

           ms
           (/ (- (System/nanoTime) t0) 1e6)]

          (expect (= [255 0 0] painted))
          ;; Every read flushes the queue, so the 480k-pixel canvas is converted both
          ;; ways per cycle. `aset-byte` is `java.lang.reflect.Array/setByte`: four
          ;; reflective stores per pixel cost ~65 ms a flush, ~1.4 s for these cycles.
          ;; The inlined array store does the same conversion in ~1.2 ms.
          (expect (< ms 800) (str "20 draw/read cycles took " (long ms) " ms"))))))

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

;; The TUI rasterizer needs the repo (for the bundled font asset) and /tmp (for
;; the unpacked TTF); a bare context denies both.
(defonce ^:private fs-python-context*
  (delay (ep/create-python-context {}
                                   (constantly ["/tmp" "/private/tmp"
                                                (System/getProperty "user.dir")]))))

(def ^:private tui-png-tool "extensions/channels/vis-channel-tui/tools/tui_png.py")

(defn- ev-tui-png
  "Evaluate `code` with the TUI rasterizer's module globals already loaded."
  [code]
  (ev (:python-context @fs-python-context*)
      (str "import os\n"
           "os.chdir("
           (pr-str (System/getProperty "user.dir"))
           ")\n"
           "exec(open(" (pr-str tui-png-tool)
           ").read(), globals())\n" code)))

(defdescribe
  tui-png-bundled-font-test
  "`tools/tui_png.py` rasterizes captured TUI frames in the JetBrains Mono this
   repo bundles for its docs, so a render does not depend on what happens to be
   installed on the machine that produced it."
  (it "unpacks the bundled woff2 and prefers it over every host font"
      (expect (true? (ev-tui-png (str "reg, bold = _bundled()\n"
                                      "(reg.endswith('JetBrainsMono-Regular.ttf')\n"
                                      " and bold.endswith('JetBrainsMono-Bold.ttf')\n"
                                      " and os.path.getsize(reg) > 50000\n"
                                      " and os.path.getsize(bold) > 50000\n"
                                      " and _families()[0] == (reg, bold))")))))
  (it "paints the default face as a real monospace at the fitted cell size"
      (expect (true? (ev-tui-png
                       (str "f, dx, dy = font()\n"
                            "d = ImageDraw.Draw(Image.new('RGB',(10,10),(0,0,0)))\n"
                            "d.textlength('iiii', font=f) == d.textlength('WWWW', font=f) > 0")))))
  (it "draws the bold weight heavier, off the very same variable file"
      ;; Monospace advances are identical per weight, so only the INK can tell a
      ;; bold face from a regular one -- and it must, or `bold` cells are a lie.
      (expect (true? (ev-tui-png
                       (str "def ink(path):\n"
                            "    im = Image.new('RGB',(200,40),(0,0,0))\n"
                            "    f = ImageFont.truetype(path, 28)\n"
                            "    ImageDraw.Draw(im).text((4,4),'MMMM',font=f,fill=(255,255,255))\n"
                            "    return sum(i * n for i, n in enumerate(im.histogram()[:256]))\n"
                            "reg, bold = _bundled()\n" "ink(bold) > ink(reg) * 1.1")))))
  (it "keeps the embedded mono family as the last resort, but never a missing file"
      (expect (true? (ev-tui-png
                       (str
                         "(_load('Noto Sans Mono', 20) is not None\n"
                         " and _load('/no/such/dir/Nope-Regular.ttf', 20) is None\n"
                         " and _families()[-1] == ('Noto Sans Mono', 'Noto Sans Mono Bold'))"))))))
