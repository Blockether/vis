(ns com.blockether.vis.internal.pil-compat-shim-test
  "The Pillow (PIL)-compat shim installed into every sandbox context via the
   generic sandbox-shim mechanism (`extension/sandbox-shims`): a `PIL` package
   published into `sys.modules` (so `from PIL import Image` works) and backed by
   a pure-JVM Java2D / ImageIO renderer. All image ops delegate across the
   boundary to the host `__vis_pil_*` callables, keeping the pixels on the JVM."
  (:require [com.blockether.vis.internal.env-python :as ep]
            [com.blockether.vis.internal.foundation.mpl-capture :as mpl-capture]
            [lazytest.core :refer [defdescribe expect it]])
  (:import [org.graalvm.polyglot Context]))

(defn- ev [^Context c code] (ep/->clj (.eval c "python" code)))

(defmacro with-python-context
  [& body]
  `(let
     [~(with-meta 'python-context {:tag `Context}) (:python-context (ep/create-python-context {}))]
     (try ~@body (finally (.close ~'python-context)))))

(defdescribe
  pil-module-test
  (it "publishes PIL + PIL.Image under sys.modules"
      (with-python-context
        (expect (true? (ev python-context "import PIL.Image\n__import__('sys').modules.get('PIL') is not None")))
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
      (with-python-context (expect (= "10.0-vis-java2d"
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
