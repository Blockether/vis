def __vis_install_pptx__():
    import sys, types, base64

    _new = __vis_pptx_new__
    _ssize = __vis_pptx_slide_size__
    _set_ssize = __vis_pptx_set_slide_size__
    _add_slide = __vis_pptx_add_slide__
    _add_textbox = __vis_pptx_add_textbox__
    _add_shape = __vis_pptx_add_shape__
    _add_picture = __vis_pptx_add_picture__
    _set_geom = __vis_pptx_set_geom__
    _set_fill = __vis_pptx_set_fill__
    _set_text = __vis_pptx_set_shape_text__
    _get_text = __vis_pptx_get_shape_text__
    _add_para = __vis_pptx_add_para__
    _add_run = __vis_pptx_add_run__
    _run_text = __vis_pptx_set_run_text__
    _para_text = __vis_pptx_set_para_text__
    _run_font = __vis_pptx_set_run_font__
    _para_font = __vis_pptx_set_para_font__
    _para_align = __vis_pptx_set_para_align__
    _para_level = __vis_pptx_set_para_level__
    _save = __vis_pptx_save__

    EMU_PER_INCH = 914400
    EMU_PER_PT = 12700

    class PptxException(Exception):
        pass

    def _raise(ok, msg):
        if not ok:
            raise PptxException(str(msg))
        return msg

    class Length(int):
        @property
        def inches(self):
            return self / 914400.0

        @property
        def pt(self):
            return self / 12700.0

        @property
        def cm(self):
            return self / 360000.0

        @property
        def mm(self):
            return self / 36000.0

        @property
        def emu(self):
            return int(self)

    def Emu(v):
        return Length(int(v))

    def Pt(v):
        return Length(int(round(v * 12700)))

    def Inches(v):
        return Length(int(round(v * 914400)))

    def Cm(v):
        return Length(int(round(v * 360000)))

    def Mm(v):
        return Length(int(round(v * 36000)))

    def Centipoints(v):
        return Length(int(round(v * 127)))

    class RGBColor(tuple):
        def __new__(cls, r, g, b):
            return super().__new__(cls, (r, g, b))

        def __str__(self):
            return ''.join(format(x, '02X') for x in self)

        @classmethod
        def from_string(cls, s):
            return cls(int(s[0:2], 16), int(s[2:4], 16), int(s[4:6], 16))

    class _Enum(object):
        def __init__(self, *names):
            for n in names:
                setattr(self, n, n)

    PP_ALIGN = _Enum('LEFT', 'CENTER', 'RIGHT', 'JUSTIFY', 'JUSTIFY_LOW', 'DISTRIBUTE', 'THAI_DISTRIBUTE')
    PP_PARAGRAPH_ALIGNMENT = PP_ALIGN
    MSO_ANCHOR = _Enum('TOP', 'MIDDLE', 'BOTTOM')
    MSO_VERTICAL_ANCHOR = MSO_ANCHOR
    MSO_SHAPE = _Enum('RECTANGLE', 'ROUNDED_RECTANGLE', 'OVAL', 'ISOCELES_TRIANGLE', 'RIGHT_TRIANGLE',
                      'DIAMOND', 'PENTAGON', 'HEXAGON', 'CHEVRON', 'STAR_5_POINT', 'RIGHT_ARROW',
                      'LEFT_ARROW', 'UP_ARROW', 'DOWN_ARROW', 'HEART', 'CLOUD', 'SUN', 'MOON',
                      'LIGHTNING_BOLT', 'PLAQUE')
    MSO_SHAPE_TYPE = _Enum('AUTO_SHAPE', 'PICTURE', 'TEXT_BOX', 'PLACEHOLDER')

    def _emu(v):
        return int(v) if v is not None else None

    class _ColorFormat(object):
        def __init__(self, font):
            self._font = font

        @property
        def rgb(self):
            return self._font._props.get('color')

        @rgb.setter
        def rgb(self, value):
            self._font._props['color'] = str(value)
            self._font._flush()

        @property
        def type(self):
            return 1

    class _Font(object):
        def __init__(self, flush):
            self._props = {}
            self._flush_cb = flush
            self.color = _ColorFormat(self)

        def _flush(self):
            self._flush_cb(dict(self._props))

        def _get(self, k):
            return self._props.get(k)

        def _set(self, k, v):
            self._props[k] = v
            self._flush()

        @property
        def bold(self):
            return self._get('bold')

        @bold.setter
        def bold(self, v):
            self._set('bold', v)

        @property
        def italic(self):
            return self._get('italic')

        @italic.setter
        def italic(self, v):
            self._set('italic', v)

        @property
        def underline(self):
            return self._get('underline')

        @underline.setter
        def underline(self, v):
            self._set('underline', v)

        @property
        def size(self):
            s = self._get('size')
            return Length(s) if s is not None else None

        @size.setter
        def size(self, v):
            self._set('size', int(v))

        @property
        def name(self):
            return self._get('name')

        @name.setter
        def name(self, v):
            self._set('name', v)

    class _Run(object):
        def __init__(self, sh, pidx, ridx):
            self._sh = sh
            self._pidx = pidx
            self._ridx = ridx
            self._text = ''
            self.font = _Font(lambda props: _raise(*_run_font(sh._pres._h, sh._sid, pidx, ridx, props)))

        @property
        def text(self):
            return self._text

        @text.setter
        def text(self, value):
            self._text = str(value)
            _raise(*_run_text(self._sh._pres._h, self._sh._sid, self._pidx, self._ridx, self._text))

    class _Paragraph(object):
        def __init__(self, sh, pidx):
            self._sh = sh
            self._pidx = pidx
            self._runs = []
            self.font = _Font(lambda props: _raise(*_para_font(sh._pres._h, sh._sid, pidx, props)))

        @property
        def text(self):
            return ''.join(r._text for r in self._runs)

        @text.setter
        def text(self, value):
            _raise(*_para_text(self._sh._pres._h, self._sh._sid, self._pidx, str(value)))
            self._runs = [_Run(self._sh, self._pidx, 0)]
            self._runs[0]._text = str(value)

        @property
        def runs(self):
            return list(self._runs)

        def add_run(self):
            ok, ridx = _add_run(self._sh._pres._h, self._sh._sid, self._pidx)
            _raise(ok, ridx)
            r = _Run(self._sh, self._pidx, ridx)
            self._runs.append(r)
            return r

        @property
        def alignment(self):
            return getattr(self, '_align', None)

        @alignment.setter
        def alignment(self, value):
            self._align = value
            _raise(*_para_align(self._sh._pres._h, self._sh._sid, self._pidx, value))

        @property
        def level(self):
            return getattr(self, '_level', 0)

        @level.setter
        def level(self, value):
            self._level = value
            _raise(*_para_level(self._sh._pres._h, self._sh._sid, self._pidx, int(value)))

    class _TextFrame(object):
        def __init__(self, sh):
            self._sh = sh
            self._paras = [_Paragraph(sh, 0)]

        @property
        def text(self):
            return _raise(*_get_text(self._sh._pres._h, self._sh._sid))

        @text.setter
        def text(self, value):
            _raise(*_set_text(self._sh._pres._h, self._sh._sid, str(value)))
            p = _Paragraph(self._sh, 0)
            p._runs = [_Run(self._sh, 0, 0)]
            p._runs[0]._text = str(value)
            self._paras = [p]

        @property
        def paragraphs(self):
            return list(self._paras)

        def add_paragraph(self):
            ok, pidx = _add_para(self._sh._pres._h, self._sh._sid)
            _raise(ok, pidx)
            p = _Paragraph(self._sh, pidx)
            self._paras.append(p)
            return p

        @property
        def word_wrap(self):
            return getattr(self, '_ww', True)

        @word_wrap.setter
        def word_wrap(self, v):
            self._ww = v

        def clear(self):
            self.text = ''
            return self

    class _FillColor(object):
        def __init__(self, sh):
            self._sh = sh

        @property
        def rgb(self):
            return getattr(self, '_rgb', None)

        @rgb.setter
        def rgb(self, value):
            self._rgb = value
            _raise(*_set_fill(self._sh._pres._h, self._sh._sid, str(value)))

        @property
        def type(self):
            return 1

    class _Fill(object):
        def __init__(self, sh, kind='none'):
            self._sh = sh
            self._kind = kind
            self._fore = _FillColor(sh)

        @property
        def type(self):
            return {'solid': 1, 'background': 5, 'none': None}[self._kind]

        def solid(self):
            self._kind = 'solid'

        def background(self):
            self._kind = 'background'

        @property
        def fore_color(self):
            if self._kind != 'solid':
                cls = '_NoFill' if self._kind == 'background' else '_NoneFill'
                raise TypeError('fill type %s has no foreground color, call .solid() or .patterned() first' % cls)
            return self._fore

    class Shape(object):
        def __init__(self, pres, sid, ph_type=None, name=None, fill_kind='none'):
            self._pres = pres
            self._sid = sid
            self._ph_type = ph_type
            self.name = name or ''
            self._tf = _TextFrame(self)
            self.fill = _Fill(self, fill_kind)

        @property
        def text_frame(self):
            return self._tf

        @property
        def has_text_frame(self):
            return True

        @property
        def text(self):
            return self._tf.text

        @text.setter
        def text(self, value):
            self._tf.text = value

        @property
        def is_placeholder(self):
            return self._ph_type is not None

        def _set_geom(self, key, value):
            geom = {'left': None, 'top': None, 'width': None, 'height': None}
            geom[key] = int(value)
            _raise(*_set_geom(self._pres._h, self._sid, geom['left'], geom['top'], geom['width'], geom['height']))
            setattr(self, '_' + key, int(value))

        @property
        def left(self):
            return getattr(self, '_left', None)

        @left.setter
        def left(self, v):
            self._set_geom('left', v)

        @property
        def top(self):
            return getattr(self, '_top', None)

        @top.setter
        def top(self, v):
            self._set_geom('top', v)

        @property
        def width(self):
            return getattr(self, '_width', None)

        @width.setter
        def width(self, v):
            self._set_geom('width', v)

        @property
        def height(self):
            return getattr(self, '_height', None)

        @height.setter
        def height(self, v):
            self._set_geom('height', v)

    class _Placeholders(object):
        def __init__(self, items):
            self._items = items

        def __getitem__(self, idx):
            for i, sh in self._items:
                if i == idx:
                    return sh
            raise KeyError(idx)

        def __iter__(self):
            return iter(sh for _, sh in self._items)

        def __len__(self):
            return len(self._items)

    class _Shapes(object):
        def __init__(self, slide):
            self._slide = slide
            self._items = []

        def _append(self, sh):
            self._items.append(sh)
            return sh

        def add_textbox(self, left, top, width, height):
            ok, sid = _add_textbox(self._slide._pres._h, self._slide._index,
                                   _emu(left), _emu(top), _emu(width), _emu(height))
            _raise(ok, sid)
            sh = Shape(self._slide._pres, sid, name='TextBox', fill_kind='background')
            sh._left, sh._top, sh._width, sh._height = int(left), int(top), int(width), int(height)
            return self._append(sh)

        def add_shape(self, shape_type, left, top, width, height):
            ok, sid = _add_shape(self._slide._pres._h, self._slide._index, str(shape_type),
                                 _emu(left), _emu(top), _emu(width), _emu(height))
            _raise(ok, sid)
            sh = Shape(self._slide._pres, sid, name='AutoShape')
            sh._left, sh._top, sh._width, sh._height = int(left), int(top), int(width), int(height)
            return self._append(sh)

        def add_picture(self, image_file, left, top, width=None, height=None):
            if hasattr(image_file, 'read'):
                data = image_file.read()
                ext = 'png'
            else:
                with open(image_file, 'rb') as f:
                    data = f.read()
                ext = str(image_file).rsplit('.', 1)[-1].lower()
            b64 = base64.b64encode(data).decode('ascii')
            ok, res = _add_picture(self._slide._pres._h, self._slide._index,
                                   _emu(left), _emu(top), _emu(width), _emu(height), b64, ext)
            _raise(ok, res)
            pic = Picture(self._slide._pres, left, top, res['width'], res['height'])
            self._items.append(pic)
            return pic

        @property
        def title(self):
            for sh in self._items:
                if sh._ph_type and 'TITLE' in str(sh._ph_type):
                    return sh
            return None

        @property
        def placeholders(self):
            return _Placeholders([(sh._ph_idx, sh) for sh in self._items if sh._ph_type is not None])

        def __iter__(self):
            return iter(self._items)

        def __len__(self):
            return len(self._items)

        def __getitem__(self, idx):
            return self._items[idx]

    class Picture(object):
        def __init__(self, pres, left, top, width, height):
            self._pres = pres
            self.left = int(left) if left is not None else None
            self.top = int(top) if top is not None else None
            self.width = Length(width)
            self.height = Length(height)
            self.name = 'Picture'
            self._ph_type = None

    class Slide(object):
        def __init__(self, pres, index, placeholders):
            self._pres = pres
            self._index = index
            self.shapes = _Shapes(self)
            for ph in placeholders:
                sh = Shape(pres, ph['shape_id'], ph_type=ph['ph_type'], name=ph.get('name'))
                sh._ph_idx = ph['idx']
                self.shapes._append(sh)

        @property
        def placeholders(self):
            return self.shapes.placeholders

        @property
        def slide_layout(self):
            return getattr(self, '_layout', None)

    class _SlideLayout(object):
        def __init__(self, index):
            self.index = index
            self.name = 'Layout ' + str(index)

    class _SlideLayouts(object):
        def __init__(self):
            self._n = 11

        def __getitem__(self, idx):
            if idx < 0:
                idx += self._n
            if idx < 0 or idx >= self._n:
                raise IndexError('slide layout index out of range')
            return _SlideLayout(idx)

        def __len__(self):
            return self._n

        def __iter__(self):
            return iter(_SlideLayout(i) for i in range(self._n))

    class _Slides(object):
        def __init__(self, pres):
            self._pres = pres
            self._items = []

        def add_slide(self, slide_layout=None):
            li = slide_layout.index if isinstance(slide_layout, _SlideLayout) else (slide_layout if isinstance(slide_layout, int) else 6)
            ok, res = _add_slide(self._pres._h, li)
            _raise(ok, res)
            sl = Slide(self._pres, res['index'], res['placeholders'])
            sl._layout = slide_layout
            self._items.append(sl)
            return sl

        def __iter__(self):
            return iter(self._items)

        def __len__(self):
            return len(self._items)

        def __getitem__(self, idx):
            return self._items[idx]

    class Presentation(object):
        def __init__(self, pptx=None):
            ok, res = _new(None, None)
            _raise(ok, res)
            self._h = res['handle']
            self._width = Length(res['width'])
            self._height = Length(res['height'])
            self.slides = _Slides(self)
            self.slide_layouts = _SlideLayouts()

        @property
        def slide_width(self):
            return self._width

        @slide_width.setter
        def slide_width(self, value):
            self._width = Length(int(value))
            _raise(*_set_ssize(self._h, int(value), int(self._height)))

        @property
        def slide_height(self):
            return self._height

        @slide_height.setter
        def slide_height(self, value):
            self._height = Length(int(value))
            _raise(*_set_ssize(self._h, int(self._width), int(value)))

        @property
        def slide_masters(self):
            return []

        def save(self, path):
            b64 = _raise(*_save(self._h))
            data = base64.b64decode(b64)
            if hasattr(path, 'write'):
                path.write(data)
            else:
                with open(path, 'wb') as f:
                    f.write(data)

    mod = types.ModuleType('pptx')
    mod.Presentation = Presentation
    mod.__version__ = '1.0.2'

    util = types.ModuleType('pptx.util')
    util.Length = Length
    util.Emu = Emu
    util.Pt = Pt
    util.Inches = Inches
    util.Cm = Cm
    util.Mm = Mm
    util.Centipoints = Centipoints
    mod.util = util

    dml = types.ModuleType('pptx.dml')
    color_mod = types.ModuleType('pptx.dml.color')
    color_mod.RGBColor = RGBColor
    dml.color = color_mod
    mod.dml = dml

    enum = types.ModuleType('pptx.enum')
    enum_text = types.ModuleType('pptx.enum.text')
    enum_text.PP_ALIGN = PP_ALIGN
    enum_text.PP_PARAGRAPH_ALIGNMENT = PP_PARAGRAPH_ALIGNMENT
    enum_text.MSO_ANCHOR = MSO_ANCHOR
    enum_text.MSO_VERTICAL_ANCHOR = MSO_VERTICAL_ANCHOR
    enum_shapes = types.ModuleType('pptx.enum.shapes')
    enum_shapes.MSO_SHAPE = MSO_SHAPE
    enum_shapes.MSO_AUTO_SHAPE_TYPE = MSO_SHAPE
    enum_shapes.MSO_SHAPE_TYPE = MSO_SHAPE_TYPE
    enum.text = enum_text
    enum.shapes = enum_shapes
    mod.enum = enum

    exc = types.ModuleType('pptx.exc')
    exc.PythonPptxError = PptxException
    mod.exc = exc

    sys.modules['pptx'] = mod
    sys.modules['pptx.util'] = util
    sys.modules['pptx.dml'] = dml
    sys.modules['pptx.dml.color'] = color_mod
    sys.modules['pptx.enum'] = enum
    sys.modules['pptx.enum.text'] = enum_text
    sys.modules['pptx.enum.shapes'] = enum_shapes
    sys.modules['pptx.exc'] = exc

    try:
        sys.modules['builtins'].pptx = mod
    except Exception:
        pass

__vis_install_pptx__()
del __vis_install_pptx__
