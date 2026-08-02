def __vis_install_pptx__():
    import sys, types, base64

    _bi = sys.modules["builtins"]
    _build = __vis_pptx_build__

    EMU_PER_INCH = 914400
    EMU_PER_CM = 360000
    EMU_PER_MM = 36000
    EMU_PER_PT = 12700
    EMU_PER_CENTIPOINT = 127

    class PptxException(Exception):
        pass

    def _raise(ok, val):
        if not ok:
            raise PptxException(str(val))
        return val

    # -- units ---------------------------------------------------------------

    class Length(int):
        @property
        def inches(self):
            return self / EMU_PER_INCH

        @property
        def cm(self):
            return self / EMU_PER_CM

        @property
        def mm(self):
            return self / EMU_PER_MM

        @property
        def pt(self):
            return self / EMU_PER_PT

        @property
        def centipoints(self):
            return int(self // EMU_PER_CENTIPOINT)

        @property
        def emu(self):
            return int(self)

    def Emu(v):
        return Length(int(v))

    def Pt(v):
        return Length(int(round(float(v) * EMU_PER_PT)))

    def Inches(v):
        return Length(int(round(float(v) * EMU_PER_INCH)))

    def Cm(v):
        return Length(int(round(float(v) * EMU_PER_CM)))

    def Mm(v):
        return Length(int(round(float(v) * EMU_PER_MM)))

    def Centipoints(v):
        return Length(int(round(float(v) * EMU_PER_CENTIPOINT)))

    def _emu(v):
        return None if v is None else int(v)

    def _cpt(v):
        """A font/spacing size in centipoints, from Length, Pt() or a number of points."""
        if v is None:
            return None
        if isinstance(v, Length):
            return int(v) // EMU_PER_CENTIPOINT
        if isinstance(v, int) and v >= 100 * EMU_PER_CENTIPOINT:
            return int(v) // EMU_PER_CENTIPOINT
        return int(round(float(v) * 100))

    # -- colour --------------------------------------------------------------

    class RGBColor(tuple):
        def __new__(cls, r, g, b):
            return tuple.__new__(cls, (int(r) & 255, int(g) & 255, int(b) & 255))

        def __str__(self):
            return "%02X%02X%02X" % self

        def __repr__(self):
            return "RGBColor(0x%02x, 0x%02x, 0x%02x)" % self

        @classmethod
        def from_string(cls, s):
            s = str(s).lstrip("#")
            return cls(int(s[0:2], 16), int(s[2:4], 16), int(s[4:6], 16))

    def _hex(color):
        if color is None:
            return None
        if isinstance(color, RGBColor):
            return str(color)
        if isinstance(color, (tuple, list)) and len(color) >= 3:
            return "%02X%02X%02X" % (
                int(color[0]) & 255,
                int(color[1]) & 255,
                int(color[2]) & 255,
            )
        if isinstance(color, int):
            return "%06X" % (color & 0xFFFFFF)
        s = str(color).lstrip("#").strip()
        return s.upper() if len(s) == 6 else s

    # -- enums ---------------------------------------------------------------

    class _EnumMember(str):
        pass

    class _Enum(object):
        def __init__(self, *names):
            for n in names:
                setattr(self, n, _EnumMember(n))

    PP_ALIGN = _Enum(
        "LEFT",
        "CENTER",
        "RIGHT",
        "JUSTIFY",
        "JUSTIFY_LOW",
        "DISTRIBUTE",
        "THAI_DISTRIBUTE",
    )
    PP_PARAGRAPH_ALIGNMENT = PP_ALIGN
    MSO_ANCHOR = _Enum("TOP", "MIDDLE", "BOTTOM")
    MSO_VERTICAL_ANCHOR = MSO_ANCHOR
    MSO_AUTO_SIZE = _Enum("NONE", "SHAPE_TO_FIT_TEXT", "TEXT_TO_FIT_SHAPE")
    MSO_THEME_COLOR = _Enum(
        "NOT_THEME_COLOR",
        "ACCENT_1",
        "ACCENT_2",
        "ACCENT_3",
        "ACCENT_4",
        "ACCENT_5",
        "ACCENT_6",
        "BACKGROUND_1",
        "BACKGROUND_2",
        "DARK_1",
        "DARK_2",
        "LIGHT_1",
        "LIGHT_2",
        "TEXT_1",
        "TEXT_2",
        "HYPERLINK",
        "FOLLOWED_HYPERLINK",
    )
    MSO_FILL = _Enum("SOLID", "BACKGROUND", "GRADIENT", "PATTERNED", "PICTURE")
    MSO_FILL_TYPE = MSO_FILL
    MSO_LINE_DASH_STYLE = _Enum(
        "SOLID",
        "DASH",
        "DASH_DOT",
        "DASH_DOT_DOT",
        "LONG_DASH",
        "LONG_DASH_DOT",
        "ROUND_DOT",
        "SQUARE_DOT",
    )
    MSO_CONNECTOR = _Enum("STRAIGHT", "ELBOW", "CURVE")
    MSO_CONNECTOR_TYPE = MSO_CONNECTOR
    PP_PLACEHOLDER = _Enum(
        "TITLE",
        "CENTER_TITLE",
        "SUBTITLE",
        "BODY",
        "OBJECT",
        "PICTURE",
        "TABLE",
        "CHART",
        "SLIDE_NUMBER",
        "FOOTER",
        "DATE",
    )
    PP_PLACEHOLDER_TYPE = PP_PLACEHOLDER

    _ALIGN = {
        "LEFT": "l",
        "CENTER": "ctr",
        "RIGHT": "r",
        "JUSTIFY": "just",
        "JUSTIFY_LOW": "justLow",
        "DISTRIBUTE": "dist",
        "THAI_DISTRIBUTE": "thaiDist",
    }
    _ALIGN_BACK = dict((v, k) for k, v in _ALIGN.items())
    _ANCHOR = {"TOP": "t", "MIDDLE": "ctr", "BOTTOM": "b"}
    _ANCHOR_BACK = dict((v, k) for k, v in _ANCHOR.items())
    _DASH = {
        "SOLID": "solid",
        "DASH": "dash",
        "DASH_DOT": "dashDot",
        "DASH_DOT_DOT": "lgDashDotDot",
        "LONG_DASH": "lgDash",
        "LONG_DASH_DOT": "lgDashDot",
        "ROUND_DOT": "sysDot",
        "SQUARE_DOT": "sysDash",
    }

    MSO_SHAPE = _Enum(
        "RECTANGLE",
        "ROUNDED_RECTANGLE",
        "SNIP_ROUNDED_RECTANGLE",
        "OVAL",
        "ISOSCELES_TRIANGLE",
        "ISOCELES_TRIANGLE",
        "RIGHT_TRIANGLE",
        "DIAMOND",
        "PARALLELOGRAM",
        "TRAPEZOID",
        "PENTAGON",
        "REGULAR_PENTAGON",
        "HEXAGON",
        "OCTAGON",
        "CHEVRON",
        "ARROW",
        "RIGHT_ARROW",
        "LEFT_ARROW",
        "UP_ARROW",
        "DOWN_ARROW",
        "LEFT_RIGHT_ARROW",
        "BENT_ARROW",
        "CIRCULAR_ARROW",
        "STAR_4_POINT",
        "STAR_5_POINT",
        "STAR_6_POINT",
        "STAR_8_POINT",
        "HEART",
        "CLOUD",
        "SUN",
        "MOON",
        "LIGHTNING_BOLT",
        "PLAQUE",
        "DONUT",
        "SMILEY_FACE",
        "BLOCK_ARC",
        "CAN",
        "CUBE",
        "LINE_CALLOUT_1",
        "ROUNDED_RECTANGULAR_CALLOUT",
        "OVAL_CALLOUT",
        "RECTANGULAR_CALLOUT",
        "FLOWCHART_PROCESS",
        "FLOWCHART_DECISION",
        "FLOWCHART_TERMINATOR",
        "FLOWCHART_DATA",
        "FLOWCHART_DOCUMENT",
        "CHEVRON_RIBBON",
        "PIE",
        "ARC",
        "TEAR",
    )
    MSO_AUTO_SHAPE_TYPE = MSO_SHAPE
    MSO_SHAPE_TYPE = _Enum(
        "AUTO_SHAPE", "PICTURE", "TEXT_BOX", "PLACEHOLDER", "TABLE", "LINE", "GROUP"
    )

    _PRESET = {
        "RECTANGLE": "rect",
        "ROUNDED_RECTANGLE": "roundRect",
        "SNIP_ROUNDED_RECTANGLE": "snipRoundRect",
        "OVAL": "ellipse",
        "ISOSCELES_TRIANGLE": "triangle",
        "ISOCELES_TRIANGLE": "triangle",
        "RIGHT_TRIANGLE": "rtTriangle",
        "DIAMOND": "diamond",
        "PARALLELOGRAM": "parallelogram",
        "TRAPEZOID": "trapezoid",
        "PENTAGON": "homePlate",
        "REGULAR_PENTAGON": "pentagon",
        "HEXAGON": "hexagon",
        "OCTAGON": "octagon",
        "CHEVRON": "chevron",
        "ARROW": "rightArrow",
        "RIGHT_ARROW": "rightArrow",
        "LEFT_ARROW": "leftArrow",
        "UP_ARROW": "upArrow",
        "DOWN_ARROW": "downArrow",
        "LEFT_RIGHT_ARROW": "leftRightArrow",
        "BENT_ARROW": "bentArrow",
        "CIRCULAR_ARROW": "circularArrow",
        "STAR_4_POINT": "star4",
        "STAR_5_POINT": "star5",
        "STAR_6_POINT": "star6",
        "STAR_8_POINT": "star8",
        "HEART": "heart",
        "CLOUD": "cloud",
        "SUN": "sun",
        "MOON": "moon",
        "LIGHTNING_BOLT": "lightningBolt",
        "PLAQUE": "plaque",
        "DONUT": "donut",
        "SMILEY_FACE": "smileyFace",
        "BLOCK_ARC": "blockArc",
        "CAN": "can",
        "CUBE": "cube",
        "LINE_CALLOUT_1": "borderCallout1",
        "ROUNDED_RECTANGULAR_CALLOUT": "wedgeRoundRectCallout",
        "OVAL_CALLOUT": "wedgeEllipseCallout",
        "RECTANGULAR_CALLOUT": "wedgeRectCallout",
        "FLOWCHART_PROCESS": "flowChartProcess",
        "FLOWCHART_DECISION": "flowChartDecision",
        "FLOWCHART_TERMINATOR": "flowChartTerminator",
        "FLOWCHART_DATA": "flowChartInputOutput",
        "FLOWCHART_DOCUMENT": "flowChartDocument",
        "CHEVRON_RIBBON": "ribbon",
        "PIE": "pie",
        "ARC": "arc",
        "TEAR": "teardrop",
    }
    _CONNECTOR = {
        "STRAIGHT": "line",
        "ELBOW": "bentConnector3",
        "CURVE": "curvedConnector3",
    }
    _PH_TYPE = {
        "TITLE": "title",
        "CENTER_TITLE": "ctrTitle",
        "SUBTITLE": "subTitle",
        "BODY": "body",
        "OBJECT": "body",
        "PICTURE": "pic",
        "TABLE": "tbl",
        "CHART": "chart",
        "SLIDE_NUMBER": "sldNum",
        "FOOTER": "ftr",
        "DATE": "dt",
    }
    _PH_BACK = {
        "title": "TITLE",
        "ctrTitle": "CENTER_TITLE",
        "subTitle": "SUBTITLE",
        "body": "BODY",
        "pic": "PICTURE",
        "tbl": "TABLE",
        "chart": "CHART",
        "sldNum": "SLIDE_NUMBER",
        "ftr": "FOOTER",
        "dt": "DATE",
    }

    def _preset_of(shape_type):
        if shape_type is None:
            return "rect"
        key = str(shape_type)
        return _PRESET.get(key, key if key.islower() or key[0].islower() else "rect")

    def _shape_basename(shape_type):
        """python-pptx names an autoshape after its MSO_SHAPE member, title-cased."""
        key = str(shape_type) if shape_type is not None else "RECTANGLE"
        return " ".join(w.capitalize() for w in key.split("_") if w) or "Shape"

    def _clean(d):
        return dict((k, v) for k, v in d.items() if v is not None)

    # -- image bytes / intrinsic size ---------------------------------------

    def _image_bytes(image_file):
        if isinstance(image_file, (bytes, bytearray)):
            return bytes(image_file)
        if hasattr(image_file, "read"):
            data = image_file.read()
            return data if isinstance(data, bytes) else bytes(data)
        with open(str(image_file), "rb") as f:
            return f.read()

    def _px_size(data):
        """(width_px, height_px, dpi) for PNG / JPEG / GIF / BMP, else None."""
        try:
            if data[:8] == b"\x89PNG\r\n\x1a\n":
                w = int.from_bytes(data[16:20], "big")
                h = int.from_bytes(data[20:24], "big")
                dpi = 72.0
                i = 8
                while i + 8 <= len(data):
                    ln = int.from_bytes(data[i : i + 4], "big")
                    typ = data[i + 4 : i + 8]
                    if typ == b"pHYs" and data[i + 16] == 1:
                        ppm = int.from_bytes(data[i + 8 : i + 12], "big")
                        if ppm:
                            dpi = ppm * 0.0254
                        break
                    if typ == b"IDAT":
                        break
                    i += 12 + ln
                return (w, h, dpi)
            if data[:2] == b"\xff\xd8":
                dpi = 72.0
                i = 2
                while i + 4 < len(data):
                    if data[i] != 0xFF:
                        i += 1
                        continue
                    marker = data[i + 1]
                    ln = int.from_bytes(data[i + 2 : i + 4], "big")
                    if (
                        marker == 0xE0
                        and data[i + 4 : i + 8] == b"JFIF"
                        and data[i + 11] == 1
                    ):
                        x = int.from_bytes(data[i + 12 : i + 14], "big")
                        if x:
                            dpi = float(x)
                    if marker in (
                        0xC0,
                        0xC1,
                        0xC2,
                        0xC3,
                        0xC5,
                        0xC6,
                        0xC7,
                        0xC9,
                        0xCA,
                        0xCB,
                        0xCD,
                        0xCE,
                        0xCF,
                    ):
                        h = int.from_bytes(data[i + 5 : i + 7], "big")
                        w = int.from_bytes(data[i + 7 : i + 9], "big")
                        return (w, h, dpi)
                    i += 2 + ln
                return None
            if data[:6] in (b"GIF87a", b"GIF89a"):
                return (
                    int.from_bytes(data[6:8], "little"),
                    int.from_bytes(data[8:10], "little"),
                    72.0,
                )
            if data[:2] == b"BM":
                return (
                    int.from_bytes(data[18:22], "little", signed=True),
                    abs(int.from_bytes(data[22:26], "little", signed=True)),
                    96.0,
                )
        except Exception:
            return None
        return None

    def _native_emu(data):
        size = _px_size(data)
        if not size:
            return (Inches(1.0), Inches(1.0))
        w, h, dpi = size
        dpi = dpi if dpi and dpi > 1 else 72.0
        return (
            Length(int(round(w / dpi * EMU_PER_INCH))),
            Length(int(round(h / dpi * EMU_PER_INCH))),
        )

    # -- colour / fill / line facades ---------------------------------------

    class _ColorFormat(object):
        """`.rgb` over one string key of an owning spec dict."""

        def __init__(self, owner, key="color"):
            self._owner = owner
            self._key = key

        @property
        def rgb(self):
            v = self._owner.get(self._key)
            return RGBColor.from_string(v) if v else None

        @rgb.setter
        def rgb(self, value):
            self._owner[self._key] = _hex(value)

        @property
        def type(self):
            return "RGB" if self._owner.get(self._key) else None

        @property
        def theme_color(self):
            return MSO_THEME_COLOR.NOT_THEME_COLOR

        @theme_color.setter
        def theme_color(self, value):
            pass

        @property
        def brightness(self):
            return 0.0

        @brightness.setter
        def brightness(self, value):
            pass

    class _Fill(object):
        """python-pptx FillFormat over `owner[key]` (a fill spec)."""

        def __init__(self, owner, key="fill"):
            self._owner = owner
            self._key = key

        def _d(self, kind=None):
            cur = self._owner.get(self._key)
            if not isinstance(cur, dict):
                cur = {"type": kind or "solid"}
                self._owner[self._key] = cur
            elif kind:
                cur["type"] = kind
            return cur

        @property
        def type(self):
            cur = self._owner.get(self._key)
            if isinstance(cur, dict):
                return cur.get("type")
            return "solid" if cur else None

        def solid(self):
            self._d("solid")

        def background(self):
            self._owner[self._key] = {"type": "none"}

        def patterned(self):
            self._d("solid")

        def gradient(self):
            d = self._d("gradient")
            d.setdefault(
                "stops",
                [
                    {"position": 0.0, "color": "FFFFFF"},
                    {"position": 1.0, "color": "000000"},
                ],
            )

        @property
        def gradient_stops(self):
            return self._d("gradient").setdefault("stops", [])

        @property
        def gradient_angle(self):
            return self._d("gradient").get("angle", 0.0)

        @gradient_angle.setter
        def gradient_angle(self, value):
            self._d("gradient")["angle"] = float(value)

        def _solid_or_raise(self, what):
            cur = self._owner.get(self._key)
            kind = (
                cur.get("type") if isinstance(cur, dict) else ("solid" if cur else None)
            )
            if kind not in ("solid", "patterned"):
                # python-pptx: FillFormat delegates to a _NoneFill/_NoFill/_GradFill
                # object that simply has no fore/back colour.
                raise TypeError(
                    "fill type %s has no %s color"
                    % (
                        "_NoneFill"
                        if kind is None
                        else "_%sFill" % str(kind).capitalize(),
                        what,
                    )
                )
            return self._d("solid")

        @property
        def fore_color(self):
            return _ColorFormat(self._solid_or_raise("foreground"), "color")

        @property
        def back_color(self):
            return _ColorFormat(self._solid_or_raise("background"), "back_color")

        @property
        def transparency(self):
            return 1.0 - float(self._d().get("alpha", 1.0))

        @transparency.setter
        def transparency(self, value):
            self._d()["alpha"] = 1.0 - float(value)

    class _LineFill(object):
        """`line.fill` — writes straight into the line spec, which is flat."""

        def __init__(self, line):
            self._line = line

        def solid(self):
            self._line.pop("type", None)

        def background(self):
            self._line["type"] = "none"

        @property
        def type(self):
            return "none" if self._line.get("type") == "none" else "solid"

        @property
        def fore_color(self):
            self._line.pop("type", None)
            return _ColorFormat(self._line, "color")

    class _LineFormat(object):
        def __init__(self, owner, key="line"):
            self._owner = owner
            self._key = key

        def _d(self):
            cur = self._owner.get(self._key)
            if not isinstance(cur, dict):
                cur = {}
                self._owner[self._key] = cur
            return cur

        @property
        def color(self):
            return _ColorFormat(self._d(), "color")

        @property
        def fill(self):
            return _LineFill(self._d())

        @property
        def width(self):
            w = self._d().get("width")
            return Length(w) if w is not None else Length(0)

        @width.setter
        def width(self, value):
            self._d()["width"] = _emu(value)

        @property
        def dash_style(self):
            return self._d().get("dash")

        @dash_style.setter
        def dash_style(self, value):
            self._d()["dash"] = _DASH.get(str(value), str(value))

    # -- text ----------------------------------------------------------------

    class _Font(object):
        """Run properties over a spec dict (a run, or a paragraph `font` map)."""

        def __init__(self, owner):
            self._d = owner

        @property
        def bold(self):
            return self._d.get("bold")

        @bold.setter
        def bold(self, value):
            self._d["bold"] = None if value is None else bool(value)

        @property
        def italic(self):
            return self._d.get("italic")

        @italic.setter
        def italic(self, value):
            self._d["italic"] = None if value is None else bool(value)

        @property
        def underline(self):
            return self._d.get("underline")

        @underline.setter
        def underline(self, value):
            if value is None or isinstance(value, bool):
                self._d["underline"] = value
            else:
                self._d["underline"] = str(value)

        @property
        def strike(self):
            return self._d.get("strike")

        @strike.setter
        def strike(self, value):
            self._d["strike"] = None if value is None else bool(value)

        @property
        def size(self):
            cpt = self._d.get("size")
            return Length(cpt * EMU_PER_CENTIPOINT) if cpt else None

        @size.setter
        def size(self, value):
            self._d["size"] = _cpt(value)

        @property
        def name(self):
            return self._d.get("font")

        @name.setter
        def name(self, value):
            self._d["font"] = None if value is None else str(value)

        @property
        def color(self):
            return _ColorFormat(self._d, "color")

        @property
        def fill(self):
            return _Fill(self._d, "_font_fill")

        @property
        def language_id(self):
            return None

        @language_id.setter
        def language_id(self, value):
            pass

    class _Hyperlink(object):
        def __init__(self, owner):
            self._d = owner

        @property
        def address(self):
            return self._d.get("hyperlink")

        @address.setter
        def address(self, value):
            self._d["hyperlink"] = None if value is None else str(value)

    class _Run(object):
        def __init__(self, d=None):
            self._d = d if d is not None else {"text": ""}

        @property
        def text(self):
            return self._d.get("text", "")

        @text.setter
        def text(self, value):
            self._d["text"] = "" if value is None else str(value)

        @property
        def font(self):
            return _Font(self._d)

        @property
        def hyperlink(self):
            return _Hyperlink(self._d)

        def _spec(self):
            return _clean(
                dict((k, v) for k, v in self._d.items() if not k.startswith("_"))
            )

    class _Paragraph(object):
        def __init__(self, d=None):
            self._d = d if d is not None else {}
            self._d.setdefault("_runs", [])

        @property
        def runs(self):
            return tuple(self._d["_runs"])

        def add_run(self):
            r = _Run()
            self._d["_runs"].append(r)
            return r

        def add_line_break(self):
            self._d["_runs"].append(_Run({"break": True}))

        def clear(self):
            self._d["_runs"] = []
            self._d.pop("text", None)
            return self

        @property
        def text(self):
            runs = self._d["_runs"]
            if runs:
                return "".join("\n" if r._d.get("break") else r.text for r in runs)
            return self._d.get("text", "")

        @text.setter
        def text(self, value):
            self._d["_runs"] = []
            self._d.pop("text", None)
            text = "" if value is None else str(value)
            parts = text.split("\v")
            for n, part in enumerate(parts):
                if n:
                    self._d["_runs"].append(_Run({"break": True}))
                self._d["_runs"].append(_Run({"text": part}))

        @property
        def font(self):
            return _Font(self._d.setdefault("font", {}))

        @property
        def alignment(self):
            a = self._d.get("align")
            return getattr(PP_ALIGN, _ALIGN_BACK[a]) if a in _ALIGN_BACK else None

        @alignment.setter
        def alignment(self, value):
            if value is None:
                self._d.pop("align", None)
            else:
                self._d["align"] = _ALIGN.get(str(value), str(value))

        @property
        def level(self):
            return int(self._d.get("level", 0))

        @level.setter
        def level(self, value):
            self._d["level"] = int(value or 0)

        @property
        def line_spacing(self):
            if "line_spacing_pct" in self._d:
                return self._d["line_spacing_pct"]
            cpt = self._d.get("line_spacing_pts")
            return Length(cpt * EMU_PER_CENTIPOINT) if cpt else None

        @line_spacing.setter
        def line_spacing(self, value):
            self._d.pop("line_spacing_pct", None)
            self._d.pop("line_spacing_pts", None)
            if value is None:
                return
            if isinstance(value, Length):
                self._d["line_spacing_pts"] = _cpt(value)
            elif isinstance(value, float) and value < 10:
                self._d["line_spacing_pct"] = float(value)
            else:
                self._d["line_spacing_pts"] = _cpt(value)

        @property
        def space_before(self):
            cpt = self._d.get("space_before")
            return Length(cpt * EMU_PER_CENTIPOINT) if cpt else None

        @space_before.setter
        def space_before(self, value):
            self._d["space_before"] = _cpt(value)

        @property
        def space_after(self):
            cpt = self._d.get("space_after")
            return Length(cpt * EMU_PER_CENTIPOINT) if cpt else None

        @space_after.setter
        def space_after(self, value):
            self._d["space_after"] = _cpt(value)

        @property
        def bullet(self):
            return self._d.get("bullet")

        @bullet.setter
        def bullet(self, value):
            self._d["bullet"] = value

        def _spec(self):
            out = dict((k, v) for k, v in self._d.items() if not k.startswith("_"))
            runs = [r._spec() for r in self._d["_runs"]]
            if runs:
                out["runs"] = runs
            if isinstance(out.get("font"), dict):
                font = _clean(out["font"])
                if font:
                    out["font"] = font
                else:
                    out.pop("font")
            return _clean(out)

    class _TextFrame(object):
        def __init__(self, owner):
            self._d = owner.setdefault("text_frame", {})
            self._d.setdefault("_paragraphs", [_Paragraph()])

        @property
        def paragraphs(self):
            return tuple(self._d["_paragraphs"])

        def add_paragraph(self):
            p = _Paragraph()
            self._d["_paragraphs"].append(p)
            return p

        def clear(self):
            self._d["_paragraphs"] = [_Paragraph()]
            return self._d["_paragraphs"][0]

        @property
        def text(self):
            return "\n".join(p.text for p in self._d["_paragraphs"])

        @text.setter
        def text(self, value):
            text = "" if value is None else str(value)
            paras = []
            for chunk in text.split("\n"):
                p = _Paragraph()
                p.text = chunk
                paras.append(p)
            self._d["_paragraphs"] = paras or [_Paragraph()]

        @property
        def word_wrap(self):
            return self._d.get("word_wrap")

        @word_wrap.setter
        def word_wrap(self, value):
            self._d["word_wrap"] = None if value is None else bool(value)

        @property
        def vertical_anchor(self):
            a = self._d.get("anchor")
            return getattr(MSO_ANCHOR, _ANCHOR_BACK[a]) if a in _ANCHOR_BACK else None

        @vertical_anchor.setter
        def vertical_anchor(self, value):
            if value is None:
                self._d.pop("anchor", None)
            else:
                self._d["anchor"] = _ANCHOR.get(str(value), str(value))

        @property
        def auto_size(self):
            return self._d.get("auto_size")

        @auto_size.setter
        def auto_size(self, value):
            self._d["auto_size"] = None if value is None else str(value)

        def fit_text(self, *args, **kwargs):
            return None

        @property
        def font(self):
            return _Font(self._d.setdefault("font", {}))

        def _spec(self):
            out = dict((k, v) for k, v in self._d.items() if not k.startswith("_"))
            out["paragraphs"] = [p._spec() for p in self._d["_paragraphs"]]
            return _clean(out)

    def _margin_prop(key):
        def getter(self):
            v = self._d.get(key)
            return Length(v) if v is not None else None

        def setter(self, value):
            self._d[key] = _emu(value)

        return property(getter, setter)

    _TextFrame.margin_left = _margin_prop("margin_left")
    _TextFrame.margin_right = _margin_prop("margin_right")
    _TextFrame.margin_top = _margin_prop("margin_top")
    _TextFrame.margin_bottom = _margin_prop("margin_bottom")

    # -- shapes --------------------------------------------------------------

    def _geom_prop(key):
        def getter(self):
            v = self._d.get(key)
            return Length(v) if v is not None else None

        def setter(self, value):
            self._d[key] = _emu(value)

        return property(getter, setter)

    class Shape(object):
        def __init__(self, d, shape_id=1, shape_type=None):
            self._d = d
            self._id = shape_id
            self._shape_type = shape_type

        # geometry
        left = _geom_prop("left")
        top = _geom_prop("top")
        width = _geom_prop("width")
        height = _geom_prop("height")

        @property
        def rotation(self):
            return float(self._d.get("rotation", 0.0))

        @rotation.setter
        def rotation(self, value):
            self._d["rotation"] = float(value)

        @property
        def name(self):
            return self._d.get("name", "")

        @name.setter
        def name(self, value):
            self._d["name"] = str(value)

        @property
        def shape_id(self):
            return self._id

        @property
        def shape_type(self):
            return self._shape_type

        @property
        def has_text_frame(self):
            return self._d.get("kind") in ("textbox", "auto", "connector")

        @property
        def text_frame(self):
            if not self.has_text_frame:
                raise PptxException("shape has no text frame")
            return _TextFrame(self._d)

        @property
        def text(self):
            return self.text_frame.text

        @text.setter
        def text(self, value):
            self.text_frame.text = value

        @property
        def has_table(self):
            return False

        @property
        def has_chart(self):
            return False

        @property
        def fill(self):
            return _Fill(self._d, "fill")

        @property
        def line(self):
            return _LineFormat(self._d, "line")

        @property
        def shadow(self):
            return _Shadow(self._d)

        @property
        def is_placeholder(self):
            return "ph" in self._d

        @property
        def placeholder_format(self):
            return _PlaceholderFormat(self._d.get("ph", {}))

        @property
        def adjustments(self):
            return self._d.setdefault("adjustments", [])

        @property
        def element(self):
            return self._d

        def _spec(self):
            out = dict((k, v) for k, v in self._d.items() if not k.startswith("_"))
            tf = out.get("text_frame")
            if isinstance(tf, dict):
                out["text_frame"] = _TextFrame({"text_frame": tf})._spec()
            return _clean(out)

    class _Shadow(object):
        def __init__(self, owner):
            self._d = owner

        @property
        def inherit(self):
            return self._d.get("shadow") is None

        @inherit.setter
        def inherit(self, value):
            self._d["shadow"] = None if value else False

    class _PlaceholderFormat(object):
        def __init__(self, ph):
            self._ph = ph

        @property
        def idx(self):
            return int(self._ph.get("idx", 0))

        @property
        def type(self):
            t = self._ph.get("type")
            return getattr(PP_PLACEHOLDER, _PH_BACK.get(t, "BODY")) if t else None

    class Picture(Shape):
        def __init__(self, d, shape_id=1):
            Shape.__init__(self, d, shape_id, MSO_SHAPE_TYPE.PICTURE)

        @property
        def has_text_frame(self):
            return False

        @property
        def image(self):
            return _Image(self._d.get("image", {}))

        @property
        def crop_left(self):
            return self._d.get("image", {}).get("crop", {}).get("left", 0.0)

        def _crop(self, side, value):
            self._d.setdefault("image", {}).setdefault("crop", {})[side] = float(value)

    for _side in ("left", "right", "top", "bottom"):

        def _mk(side):
            def getter(self):
                return self._d.get("image", {}).get("crop", {}).get(side, 0.0)

            def setter(self, value):
                self._crop(side, value)

            return property(getter, setter)

        setattr(Picture, "crop_" + _side, _mk(_side))

    class _Image(object):
        def __init__(self, d):
            self._d = d

        @property
        def blob(self):
            data = self._d.get("data")
            return base64.b64decode(data) if data else b""

        @property
        def size(self):
            s = _px_size(self.blob)
            return (s[0], s[1]) if s else (0, 0)

    class GraphicFrame(Shape):
        def __init__(self, d, shape_id=1, table=None):
            Shape.__init__(self, d, shape_id, MSO_SHAPE_TYPE.TABLE)
            self._table = table

        @property
        def has_table(self):
            return self._table is not None

        @property
        def has_text_frame(self):
            return False

        @property
        def table(self):
            if self._table is None:
                raise PptxException("shape has no table")
            return self._table

        def _spec(self):
            out = dict((k, v) for k, v in self._d.items() if not k.startswith("_"))
            if self._table is not None:
                out["table"] = self._table._spec()
            return _clean(out)

    # -- table ---------------------------------------------------------------

    class _Cell(object):
        def __init__(self, d):
            self._d = d

        @property
        def text_frame(self):
            return _TextFrame(self._d)

        @property
        def text(self):
            tf = self._d.get("text_frame")
            if tf is not None:
                return _TextFrame(self._d).text
            return self._d.get("text", "")

        @text.setter
        def text(self, value):
            self._d.pop("text", None)
            self.text_frame.text = value

        @property
        def fill(self):
            return _Fill(self._d, "fill")

        @property
        def vertical_anchor(self):
            a = self._d.get("anchor")
            return getattr(MSO_ANCHOR, _ANCHOR_BACK[a]) if a in _ANCHOR_BACK else None

        @vertical_anchor.setter
        def vertical_anchor(self, value):
            self._d["anchor"] = _ANCHOR.get(str(value), str(value))

        @property
        def span_height(self):
            return int(self._d.get("row_span", 1))

        @property
        def span_width(self):
            return int(self._d.get("grid_span", 1))

        @property
        def is_merge_origin(self):
            return self.span_height > 1 or self.span_width > 1

        def merge(self, other):
            r1, c1 = self._d["_rc"]
            r2, c2 = other._d["_rc"]
            self._d["grid_span"] = abs(c2 - c1) + 1
            self._d["row_span"] = abs(r2 - r1) + 1

        def _spec(self):
            out = dict((k, v) for k, v in self._d.items() if not k.startswith("_"))
            tf = out.get("text_frame")
            if isinstance(tf, dict):
                out["text_frame"] = _TextFrame({"text_frame": tf})._spec()
            return _clean(out)

    _Cell.margin_left = _margin_prop("margin_left")
    _Cell.margin_right = _margin_prop("margin_right")
    _Cell.margin_top = _margin_prop("margin_top")
    _Cell.margin_bottom = _margin_prop("margin_bottom")

    class _Row(object):
        def __init__(self, d, cells):
            self._d = d
            self._cells = cells

        @property
        def cells(self):
            return tuple(self._cells)

        @property
        def height(self):
            v = self._d.get("height")
            return Length(v) if v is not None else None

        @height.setter
        def height(self, value):
            self._d["height"] = _emu(value)

        def __iter__(self):
            return iter(self._cells)

        def __len__(self):
            return len(self._cells)

    class _Column(object):
        def __init__(self, table, index):
            self._t = table
            self._i = index

        @property
        def width(self):
            w = self._t._d["col_widths"][self._i]
            return Length(w) if w is not None else None

        @width.setter
        def width(self, value):
            self._t._d["col_widths"][self._i] = _emu(value)

    class _RowCollection(tuple):
        pass

    class _Table(object):
        def __init__(self, rows, cols, width, height):
            self._d = {
                "col_widths": [int(width // cols)] * cols,
                "first_row": True,
                "band_row": True,
            }
            self._rows = []
            row_h = int(height // rows) if rows else 0
            for r in range(rows):
                cells = []
                cellspecs = []
                for c in range(cols):
                    cd = {"_rc": (r, c)}
                    cellspecs.append(cd)
                    cells.append(_Cell(cd))
                rd = {"height": row_h, "_cells": cellspecs}
                self._rows.append(_Row(rd, cells))
            self._nrows = rows
            self._ncols = cols

        @property
        def rows(self):
            return _RowCollection(self._rows)

        @property
        def columns(self):
            return tuple(_Column(self, i) for i in range(self._ncols))

        def cell(self, row_idx, col_idx):
            return self._rows[row_idx]._cells[col_idx]

        def _flag(key):
            def getter(self):
                return bool(self._d.get(key, False))

            def setter(self, value):
                self._d[key] = bool(value)

            return property(getter, setter)

        first_row = _flag("first_row")
        first_col = _flag("first_col")
        last_row = _flag("last_row")
        last_col = _flag("last_col")
        horz_banding = _flag("band_row")
        vert_banding = _flag("band_col")
        del _flag

        def _spec(self):
            out = dict((k, v) for k, v in self._d.items() if not k.startswith("_"))
            out["rows"] = [
                _clean(
                    {
                        "height": row._d.get("height"),
                        "cells": [c._spec() for c in row._cells],
                    }
                )
                for row in self._rows
            ]
            return out

    # -- shape trees ---------------------------------------------------------

    class _Placeholders(object):
        def __init__(self, shapes):
            self._shapes = shapes

        def _all(self):
            return [s for s in self._shapes if s.is_placeholder]

        def __getitem__(self, idx):
            for s in self._all():
                if s.placeholder_format.idx == idx:
                    return s
            raise KeyError("no placeholder with idx %r" % (idx,))

        def __iter__(self):
            return iter(self._all())

        def __len__(self):
            return len(self._all())

    class _Shapes(object):
        def __init__(self, slide):
            self._slide = slide
            self._shapes = []

        def _next_id(self):
            return len(self._shapes) + 2

        def _add(self, shape):
            self._shapes.append(shape)
            return shape

        def add_textbox(self, left, top, width, height):
            d = _clean(
                {
                    "kind": "textbox",
                    "left": _emu(left),
                    "top": _emu(top),
                    "width": _emu(width),
                    "height": _emu(height),
                    "name": "TextBox %d" % (len(self._shapes) + 1),
                }
            )
            sh = Shape(d, self._next_id(), MSO_SHAPE_TYPE.TEXT_BOX)
            _ = sh.text_frame  # materialise an empty text frame, like python-pptx
            return self._add(sh)

        def add_shape(self, autoshape_type_id, left, top, width, height):
            d = _clean(
                {
                    "kind": "auto",
                    "preset": _preset_of(autoshape_type_id),
                    "left": _emu(left),
                    "top": _emu(top),
                    "width": _emu(width),
                    "height": _emu(height),
                    "name": "%s %d"
                    % (_shape_basename(autoshape_type_id), len(self._shapes) + 1),
                }
            )
            sh = Shape(d, self._next_id(), MSO_SHAPE_TYPE.AUTO_SHAPE)
            _ = sh.text_frame
            return self._add(sh)

        def add_picture(self, image_file, left, top, width=None, height=None):
            data = _image_bytes(image_file)
            nw, nh = _native_emu(data)
            if width is None and height is None:
                width, height = nw, nh
            elif width is None:
                width = int(round(int(height) * (nw / nh))) if nh else nw
            elif height is None:
                height = int(round(int(width) * (nh / nw))) if nw else nh
            d = _clean(
                {
                    "kind": "picture",
                    "left": _emu(left),
                    "top": _emu(top),
                    "width": _emu(width),
                    "height": _emu(height),
                    "name": "Picture %d" % (len(self._shapes) + 1),
                    "image": {"data": base64.b64encode(data).decode("ascii")},
                }
            )
            return self._add(Picture(d, self._next_id()))

        def add_table(self, rows, cols, left, top, width, height):
            table = _Table(rows, cols, int(width), int(height))
            d = _clean(
                {
                    "kind": "table",
                    "left": _emu(left),
                    "top": _emu(top),
                    "width": _emu(width),
                    "height": _emu(height),
                    "name": "Table %d" % (len(self._shapes) + 1),
                }
            )
            return self._add(GraphicFrame(d, self._next_id(), table))

        def add_connector(self, connector_type, begin_x, begin_y, end_x, end_y):
            x1, y1, x2, y2 = int(begin_x), int(begin_y), int(end_x), int(end_y)
            d = _clean(
                {
                    "kind": "connector",
                    "preset": _CONNECTOR.get(str(connector_type), "line"),
                    "left": min(x1, x2),
                    "top": min(y1, y2),
                    "width": abs(x2 - x1),
                    "height": abs(y2 - y1),
                    "flip_h": x2 < x1 or None,
                    "flip_v": y2 < y1 or None,
                    "name": "Connector %d" % (len(self._shapes) + 1),
                }
            )
            return self._add(Shape(d, self._next_id(), MSO_SHAPE_TYPE.LINE))

        @property
        def title(self):
            for s in self._shapes:
                ph = s._d.get("ph") or {}
                if ph.get("type") in ("title", "ctrTitle"):
                    return s
            return None

        @property
        def placeholders(self):
            return _Placeholders(self._shapes)

        def index(self, shape):
            return self._shapes.index(shape)

        def __iter__(self):
            return iter(self._shapes)

        def __len__(self):
            return len(self._shapes)

        def __getitem__(self, i):
            return self._shapes[i]

    # -- notes ---------------------------------------------------------------

    class _NotesSlide(object):
        def __init__(self, slide):
            self._slide = slide
            self._d = {}

        @property
        def notes_text_frame(self):
            return _NotesTextFrame(self._slide)

        @property
        def placeholders(self):
            return (self.notes_text_frame,)

        @property
        def shapes(self):
            return (self.notes_text_frame,)

    class _NotesTextFrame(object):
        def __init__(self, slide):
            self._slide = slide

        @property
        def text(self):
            return self._slide._d.get("notes", "")

        @text.setter
        def text(self, value):
            self._slide._d["notes"] = "" if value is None else str(value)

        @property
        def text_frame(self):
            return self

        @property
        def paragraphs(self):
            p = _Paragraph()
            p.text = self.text
            return (p,)

        def add_paragraph(self):
            p = _Paragraph()
            return p

    class _Background(object):
        def __init__(self, slide_d):
            self._d = slide_d

        @property
        def fill(self):
            return _Fill(self._d, "background")

    # -- layouts / slides ----------------------------------------------------

    _TITLE = (838200, 365125, 10515600, 1325563)
    _BODY = (838200, 1825625, 10515600, 4351338)

    def _ph(type_, idx, box, size=None, align=None, orient=None, name=None):
        left, top, width, height = box
        return _clean(
            {
                "type": type_,
                "idx": idx,
                "left": left,
                "top": top,
                "width": width,
                "height": height,
                "size": size,
                "align": align,
                "orient": orient,
                "name": name,
            }
        )

    def _standard_layouts():
        return [
            {
                "name": "Title Slide",
                "type": "title",
                "placeholders": [
                    _ph(
                        "ctrTitle", 0, (1524000, 1122363, 9144000, 2387600), 4400, "ctr"
                    ),
                    _ph(
                        "subTitle", 1, (1524000, 3602038, 9144000, 1655762), 2400, "ctr"
                    ),
                ],
            },
            {
                "name": "Title and Content",
                "type": "obj",
                "placeholders": [
                    _ph("title", 0, _TITLE, 4400),
                    _ph("body", 1, _BODY, 2800),
                ],
            },
            {
                "name": "Section Header",
                "type": "secHead",
                "placeholders": [
                    _ph("title", 0, (831850, 1709738, 10515600, 2852737), 4000),
                    _ph("body", 1, (831850, 4589463, 10515600, 1500187), 2000),
                ],
            },
            {
                "name": "Two Content",
                "type": "twoObj",
                "placeholders": [
                    _ph("title", 0, _TITLE, 4400),
                    _ph("body", 1, (838200, 1825625, 5181600, 4351338), 2400),
                    _ph("body", 2, (6172200, 1825625, 5181600, 4351338), 2400),
                ],
            },
            {
                "name": "Comparison",
                "type": "twoTxTwoObj",
                "placeholders": [
                    _ph("title", 0, _TITLE, 4000),
                    _ph("body", 1, (838200, 1681163, 5181600, 823912), 2400),
                    _ph("body", 2, (838200, 2505075, 5181600, 3684588), 2000),
                    _ph("body", 3, (6172200, 1681163, 5183188, 823912), 2400),
                    _ph("body", 4, (6172200, 2505075, 5183188, 3684588), 2000),
                ],
            },
            {
                "name": "Title Only",
                "type": "titleOnly",
                "placeholders": [_ph("title", 0, _TITLE, 4400)],
            },
            {"name": "Blank", "type": "blank", "placeholders": []},
            {
                "name": "Content with Caption",
                "type": "objTx",
                "placeholders": [
                    _ph("title", 0, (839788, 457200, 3932237, 1600200), 3200),
                    _ph("body", 1, (5183188, 987425, 6172200, 4873625), 2800),
                    _ph("body", 2, (839788, 2057400, 3932237, 3811588), 1400),
                ],
            },
            {
                "name": "Picture with Caption",
                "type": "picTx",
                "placeholders": [
                    _ph("title", 0, (839788, 457200, 3932237, 1600200), 3200),
                    _ph("pic", 1, (5183188, 987425, 6172200, 4873625)),
                    _ph("body", 2, (839788, 2057400, 3932237, 3811588), 1400),
                ],
            },
            {
                "name": "Title and Vertical Text",
                "type": "vertTx",
                "placeholders": [
                    _ph("title", 0, _TITLE, 4400),
                    _ph("body", 1, _BODY, 2800, orient="vert"),
                ],
            },
            {
                "name": "Vertical Title and Text",
                "type": "vertTitleAndTx",
                "placeholders": [
                    _ph(
                        "title",
                        0,
                        (8724900, 365125, 2628900, 5811838),
                        4400,
                        orient="vert",
                    ),
                    _ph(
                        "body",
                        1,
                        (838200, 365125, 7734300, 5811838),
                        2800,
                        orient="vert",
                    ),
                ],
            },
        ]

    # The layout geometry above is authored for a 16:9 canvas; python-pptx's own
    # default template is 4:3, so scale horizontally to whatever canvas is in use.
    _DESIGN_WIDTH = 12192000

    def _layouts_for(width):
        specs = _standard_layouts()
        if int(width) == _DESIGN_WIDTH:
            return specs
        k = float(width) / _DESIGN_WIDTH
        for spec in specs:
            for ph in spec["placeholders"]:
                ph["left"] = int(round(ph["left"] * k))
                ph["width"] = int(round(ph["width"] * k))
        return specs

    class _SlideLayout(object):
        def __init__(self, spec, index, master):
            self._spec = spec
            self._index = index
            self._master = master

        @property
        def name(self):
            return self._spec["name"]

        @property
        def slide_master(self):
            return self._master

        @property
        def placeholders(self):
            return tuple(self._spec["placeholders"])

        @property
        def shapes(self):
            return ()

    class _SlideLayouts(object):
        def __init__(self, specs, master):
            self._specs = specs
            self._master = master

        def __getitem__(self, i):
            if isinstance(i, str):
                return self.get_by_name(i)
            if i < 0:
                i += len(self._specs)
            if not 0 <= i < len(self._specs):
                raise IndexError("slide layout index out of range")
            return _SlideLayout(self._specs[i], i, self._master)

        def get_by_name(self, name, default=None):
            for n, spec in enumerate(self._specs):
                if spec["name"] == name:
                    return _SlideLayout(spec, n, self._master)
            return default

        def index(self, layout):
            return layout._index

        def __len__(self):
            return len(self._specs)

        def __iter__(self):
            return (self[i] for i in range(len(self._specs)))

    class _SlideMaster(object):
        def __init__(self, prs):
            self._prs = prs

        @property
        def slide_layouts(self):
            return self._prs.slide_layouts

        @property
        def placeholders(self):
            return ()

        @property
        def shapes(self):
            return ()

    class Slide(object):
        def __init__(self, prs, layout, slide_id):
            self._prs = prs
            self._layout = layout
            self._id = slide_id
            self._d = {"layout": layout._index}
            self._shapes = _Shapes(self)
            for ph in layout._spec["placeholders"]:
                d = {
                    "kind": "textbox",
                    "ph": {"type": ph["type"], "idx": ph["idx"]},
                    "name": "%s Placeholder %d" % (ph["type"], ph["idx"] + 1),
                }
                shape = Shape(
                    d, len(self._shapes._shapes) + 2, MSO_SHAPE_TYPE.PLACEHOLDER
                )
                _ = shape.text_frame
                self._shapes._add(shape)

        @property
        def shapes(self):
            return self._shapes

        @property
        def placeholders(self):
            return self._shapes.placeholders

        @property
        def slide_layout(self):
            return self._layout

        @property
        def slide_id(self):
            return self._id

        @property
        def has_notes_slide(self):
            return "notes" in self._d

        @property
        def notes_slide(self):
            self._d.setdefault("notes", "")
            return _NotesSlide(self)

        @property
        def background(self):
            return _Background(self._d)

        @property
        def follow_master_background(self):
            return "background" not in self._d

        def _spec(self):
            out = dict(self._d)
            shapes = []
            for s in self._shapes:
                spec = s._spec()
                # an untouched placeholder with no text is still worth emitting:
                # PowerPoint shows the layout prompt, exactly like python-pptx.
                shapes.append(spec)
            out["shapes"] = shapes
            return _clean(out)

    class _Slides(object):
        def __init__(self, prs):
            self._prs = prs
            self._slides = []

        def add_slide(self, slide_layout):
            s = Slide(self._prs, slide_layout, 256 + len(self._slides))
            self._slides.append(s)
            return s

        def index(self, slide):
            return self._slides.index(slide)

        def get(self, slide_id, default=None):
            for s in self._slides:
                if s.slide_id == slide_id:
                    return s
            return default

        def __iter__(self):
            return iter(self._slides)

        def __len__(self):
            return len(self._slides)

        def __getitem__(self, i):
            return self._slides[i]

    class _CoreProperties(object):
        _KEYS = (
            "title",
            "subject",
            "author",
            "keywords",
            "comments",
            "category",
            "last_modified_by",
        )

        def __init__(self, d):
            self._d = d

        def __getattr__(self, name):
            if name.startswith("_"):
                raise AttributeError(name)
            if name in _CoreProperties._KEYS:
                return self._d.get(name, "")
            raise AttributeError(name)

        def __setattr__(self, name, value):
            if name.startswith("_"):
                object.__setattr__(self, name, value)
            elif name in _CoreProperties._KEYS:
                self._d[name] = "" if value is None else str(value)
            else:
                object.__setattr__(self, name, value)

    class Presentation(object):
        def __init__(self, pptx=None):
            if pptx is not None:
                raise PptxException(
                    "the vis pptx shim builds new presentations only; "
                    "opening or editing an existing .pptx is not supported"
                )
            # python-pptx's default template is 10in x 7.5in (4:3).
            self._d = {"width": 9144000, "height": 6858000, "properties": {}}
            self._layout_specs = _layouts_for(self._d["width"])
            self._master = _SlideMaster(self)
            self._layouts = _SlideLayouts(self._layout_specs, self._master)
            self._slides = _Slides(self)

        @property
        def slides(self):
            return self._slides

        @property
        def slide_layouts(self):
            return self._layouts

        @property
        def slide_master(self):
            return self._master

        @property
        def slide_masters(self):
            return (self._master,)

        @property
        def core_properties(self):
            return _CoreProperties(self._d["properties"])

        @property
        def slide_width(self):
            return Length(self._d["width"])

        @slide_width.setter
        def slide_width(self, value):
            self._d["width"] = int(value)

        @property
        def slide_height(self):
            return Length(self._d["height"])

        @slide_height.setter
        def slide_height(self, value):
            self._d["height"] = int(value)

        def _spec(self):
            spec = dict(self._d)
            if not spec.get("properties"):
                spec.pop("properties", None)
            spec["layouts"] = self._layout_specs
            spec["slides"] = [s._spec() for s in self._slides]
            return spec

        def save(self, path):
            b64 = _raise(*_build(self._spec()))
            data = base64.b64decode(b64)
            if hasattr(path, "write"):
                path.write(data)
            else:
                with open(str(path), "wb") as f:
                    f.write(data)

    # -- module wiring -------------------------------------------------------

    mod = types.ModuleType("pptx")
    mod.Presentation = Presentation
    mod.__version__ = "1.0.2"
    mod.__path__ = []

    api = types.ModuleType("pptx.api")
    api.Presentation = Presentation
    mod.api = api

    presentation_mod = types.ModuleType("pptx.presentation")
    presentation_mod.Presentation = Presentation
    mod.presentation = presentation_mod

    util = types.ModuleType("pptx.util")
    util.Length = Length
    util.Emu = Emu
    util.Pt = Pt
    util.Inches = Inches
    util.Cm = Cm
    util.Mm = Mm
    util.Centipoints = Centipoints
    mod.util = util

    dml = types.ModuleType("pptx.dml")
    color_mod = types.ModuleType("pptx.dml.color")
    color_mod.RGBColor = RGBColor
    dml.color = color_mod
    mod.dml = dml

    enum = types.ModuleType("pptx.enum")
    enum_text = types.ModuleType("pptx.enum.text")
    enum_text.PP_ALIGN = PP_ALIGN
    enum_text.PP_PARAGRAPH_ALIGNMENT = PP_PARAGRAPH_ALIGNMENT
    enum_text.MSO_ANCHOR = MSO_ANCHOR
    enum_text.MSO_VERTICAL_ANCHOR = MSO_VERTICAL_ANCHOR
    enum_text.MSO_AUTO_SIZE = MSO_AUTO_SIZE
    enum_shapes = types.ModuleType("pptx.enum.shapes")
    enum_shapes.MSO_SHAPE = MSO_SHAPE
    enum_shapes.MSO_AUTO_SHAPE_TYPE = MSO_AUTO_SHAPE_TYPE
    enum_shapes.MSO_SHAPE_TYPE = MSO_SHAPE_TYPE
    enum_shapes.MSO_CONNECTOR = MSO_CONNECTOR
    enum_shapes.MSO_CONNECTOR_TYPE = MSO_CONNECTOR_TYPE
    enum_shapes.PP_PLACEHOLDER = PP_PLACEHOLDER
    enum_shapes.PP_PLACEHOLDER_TYPE = PP_PLACEHOLDER_TYPE
    enum_dml = types.ModuleType("pptx.enum.dml")
    enum_dml.MSO_THEME_COLOR = MSO_THEME_COLOR
    enum_dml.MSO_FILL = MSO_FILL
    enum_dml.MSO_FILL_TYPE = MSO_FILL_TYPE
    enum_dml.MSO_LINE_DASH_STYLE = MSO_LINE_DASH_STYLE
    enum.text = enum_text
    enum.shapes = enum_shapes
    enum.dml = enum_dml
    mod.enum = enum

    shapes_mod = types.ModuleType("pptx.shapes")
    base_mod = types.ModuleType("pptx.shapes.base")
    base_mod.BaseShape = Shape
    autoshape_mod = types.ModuleType("pptx.shapes.autoshape")
    autoshape_mod.Shape = Shape
    picture_mod = types.ModuleType("pptx.shapes.picture")
    picture_mod.Picture = Picture
    graphfrm_mod = types.ModuleType("pptx.shapes.graphfrm")
    graphfrm_mod.GraphicFrame = GraphicFrame
    shapes_mod.base = base_mod
    shapes_mod.autoshape = autoshape_mod
    shapes_mod.picture = picture_mod
    shapes_mod.graphfrm = graphfrm_mod
    mod.shapes = shapes_mod

    table_mod = types.ModuleType("pptx.table")
    table_mod.Table = _Table
    table_mod._Cell = _Cell
    table_mod._Row = _Row
    table_mod._Column = _Column
    mod.table = table_mod

    text_mod = types.ModuleType("pptx.text")
    text_text_mod = types.ModuleType("pptx.text.text")
    text_text_mod.TextFrame = _TextFrame
    text_text_mod._Paragraph = _Paragraph
    text_text_mod._Run = _Run
    text_text_mod.Font = _Font
    text_mod.text = text_text_mod
    mod.text = text_mod

    chart_mod = types.ModuleType("pptx.chart")
    chart_data_mod = types.ModuleType("pptx.chart.data")

    class CategoryChartData:
        def __init__(self):
            self.categories = []
            self.series = []

        def add_category(self, label):
            self.categories.append(label)
            return label

        def add_series(self, name, values=()):
            series = (name, tuple(values))
            self.series.append(series)
            return series

    chart_data_mod.CategoryChartData = CategoryChartData
    chart_data_mod.ChartData = CategoryChartData
    chart_mod.data = chart_data_mod
    mod.chart = chart_mod

    # Chart OOXML output is not supported by the compact Rust deck writer.
    # The data object above is still useful for code that assembles decks
    # conditionally, and imports retain the normal python-pptx package shape.

    exc = types.ModuleType("pptx.exc")
    exc.PythonPptxError = PptxException
    exc.PackageNotFoundError = PptxException
    exc.InvalidXmlError = PptxException
    mod.exc = exc

    for name, m in [
        ("pptx", mod),
        ("pptx.api", api),
        ("pptx.presentation", presentation_mod),
        ("pptx.util", util),
        ("pptx.dml", dml),
        ("pptx.dml.color", color_mod),
        ("pptx.chart", chart_mod),
        ("pptx.chart.data", chart_data_mod),
        ("pptx.enum", enum),
        ("pptx.enum.text", enum_text),
        ("pptx.enum.shapes", enum_shapes),
        ("pptx.enum.dml", enum_dml),
        ("pptx.shapes", shapes_mod),
        ("pptx.shapes.base", base_mod),
        ("pptx.shapes.autoshape", autoshape_mod),
        ("pptx.shapes.picture", picture_mod),
        ("pptx.shapes.graphfrm", graphfrm_mod),
        ("pptx.table", table_mod),
        ("pptx.text", text_mod),
        ("pptx.text.text", text_text_mod),
        ("pptx.exc", exc),
    ]:
        sys.modules[name] = m

    try:
        _bi.pptx = mod
    except Exception:
        pass


__vis_install_pptx__()
del __vis_install_pptx__
