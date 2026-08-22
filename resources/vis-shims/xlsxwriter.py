def __vis_install_xlsxwriter__():
    import sys, types, base64, datetime, io

    _bi = sys.modules["builtins"]
    _build = __vis_xlsx_build__

    class XlsxWriterException(Exception):
        """Base error raised by this xlsxwriter subset."""

        pass

    def _raise(ok, msg):
        if not ok:
            raise XlsxWriterException(str(msg))

    def _cell_to_rowcol(cell):
        cell = cell.upper().replace("$", "")
        i = 0
        col = 0
        while i < len(cell) and cell[i].isalpha():
            col = col * 26 + (ord(cell[i]) - ord("A") + 1)
            i += 1
        row = int(cell[i:]) if i < len(cell) else 1
        return (row - 1, col - 1)

    def _looks_cell(s):
        if not s or not s[0].isalpha():
            return False
        seen = False
        for ch in s.replace("$", ""):
            if ch.isalpha():
                if seen:
                    return False
            elif ch.isdigit():
                seen = True
            else:
                return False
        return seen

    def _detect(data):
        if data is None:
            return ("blank", "")
        if isinstance(data, bool):
            return ("boolean", data)
        if isinstance(data, (int, float)):
            return ("number", float(data))
        if isinstance(data, (datetime.datetime, datetime.date, datetime.time)):
            return ("datetime", data.isoformat())
        s = str(data)
        if s.startswith("="):
            return ("formula", s)
        return ("string", s)

    def _blob(source, options=None):
        """Image bytes from `image_data` (a file-like buffer) or a filename."""
        data = (options or {}).get("image_data")
        if data is not None:
            raw = data.getvalue() if hasattr(data, "getvalue") else data.read()
        elif hasattr(source, "read"):
            raw = source.read()
        elif isinstance(source, (bytes, bytearray)):
            raw = bytes(source)
        else:
            with open(source, "rb") as fh:
                raw = fh.read()
        return base64.b64encode(bytes(raw)).decode("ascii")

    class Format:
        """Cell format returned by Workbook.add_format: bold, italic, number format, alignment."""

        def __init__(self, props=None):
            self._props = dict(props or {})

        def set_properties(self, props):
            self._props.update(props or {})

        def __getattr__(self, name):
            if name.startswith("set_"):
                key = name[4:]

                def setter(value=True):
                    self._props[key] = value

                return setter
            raise AttributeError(name)

    def _fmt(fmt):
        if fmt is None:
            return None
        if isinstance(fmt, dict):
            return dict(fmt) or None
        props = dict(getattr(fmt, "_props", {}) or {})
        return props or None

    class Worksheet:
        """Worksheet: write, write_row, write_column, set_column and freeze_panes into one sheet of a workbook."""

        def __init__(self, wb, index, name):
            self._wb = wb
            self.index = index
            self.name = name
            self._cells = []
            self._columns = []
            self._rows = []
            self._merges = []
            self._images = []
            self._spec = {}

        # -- addressing ----------------------------------------------------
        def _rc(self, args):
            args = list(args)
            if args and isinstance(args[0], str) and _looks_cell(args[0]):
                r, c = _cell_to_rowcol(args[0])
                return r, c, args[1:]
            return args[0], args[1], args[2:]

        def _put(self, r, c, kind, val, fmt, **extra):
            cell = {"row": int(r), "col": int(c), "type": kind, "value": val}
            props = _fmt(fmt)
            if props:
                cell["format"] = props
            cell.update(extra)
            self._cells.append(cell)
            return 0

        # -- writing -------------------------------------------------------
        def write(self, *args):
            r, c, rest = self._rc(args)
            data = rest[0] if rest else None
            fmt = rest[1] if len(rest) > 1 else None
            kind, val = _detect(data)
            if kind == "string" and (
                val.startswith("http://")
                or val.startswith("https://")
                or val.startswith("mailto:")
            ):
                return self.write_url(r, c, val, fmt)
            return self._put(r, c, kind, val, fmt)

        def write_string(self, *args):
            r, c, rest = self._rc(args)
            return self._put(
                r,
                c,
                "string",
                str(rest[0]) if rest else "",
                rest[1] if len(rest) > 1 else None,
            )

        def write_number(self, *args):
            r, c, rest = self._rc(args)
            return self._put(
                r, c, "number", float(rest[0]), rest[1] if len(rest) > 1 else None
            )

        def write_boolean(self, *args):
            r, c, rest = self._rc(args)
            return self._put(
                r, c, "boolean", bool(rest[0]), rest[1] if len(rest) > 1 else None
            )

        def write_formula(self, *args):
            r, c, rest = self._rc(args)
            fmt = rest[1] if len(rest) > 1 else None
            result = rest[2] if len(rest) > 2 else None
            extra = {} if result is None else {"result": result}
            return self._put(r, c, "formula", str(rest[0]), fmt, **extra)

        write_array_formula = write_formula
        write_dynamic_array_formula = write_formula

        def write_datetime(self, *args):
            r, c, rest = self._rc(args)
            v = rest[0]
            iso = v.isoformat() if hasattr(v, "isoformat") else str(v)
            return self._put(r, c, "datetime", iso, rest[1] if len(rest) > 1 else None)

        def write_blank(self, *args):
            r, c, rest = self._rc(args)
            return self._put(r, c, "blank", "", rest[1] if len(rest) > 1 else None)

        def write_url(self, *args):
            r, c, rest = self._rc(args)
            url = str(rest[0])
            fmt = rest[1] if len(rest) > 1 else None
            string = rest[2] if len(rest) > 2 else None
            tip = rest[3] if len(rest) > 3 else None
            extra = {}
            if string is not None:
                extra["text"] = str(string)
            if tip is not None:
                extra["tip"] = str(tip)
            return self._put(r, c, "url", url, fmt, **extra)

        def write_rich_string(self, *args):
            """`write_rich_string(row, col, *fragments[, cell_format])`."""
            r, c, rest = self._rc(args)
            rest = list(rest)
            cell_fmt = None
            if len(rest) > 1 and not isinstance(rest[-1], str):
                cell_fmt = rest.pop()
            runs = []
            pending = None
            for part in rest:
                if isinstance(part, str):
                    run = {"text": part}
                    props = _fmt(pending)
                    if props:
                        run["format"] = props
                    runs.append(run)
                    pending = None
                else:
                    pending = part
            return self._put(r, c, "rich", "", cell_fmt, runs=runs)

        def write_row(self, *args):
            r, c, rest = self._rc(args)
            data = rest[0] if rest else []
            fmt = rest[1] if len(rest) > 1 else None
            for i, v in enumerate(data):
                self.write(r, c + i, v, fmt)
            return 0

        def write_column(self, *args):
            r, c, rest = self._rc(args)
            data = rest[0] if rest else []
            fmt = rest[1] if len(rest) > 1 else None
            for i, v in enumerate(data):
                self.write(r + i, c, v, fmt)
            return 0

        # -- layout --------------------------------------------------------
        def merge_range(self, *args):
            args = list(args)
            if args and isinstance(args[0], str):
                a, b = args[0].split(":")
                r1, c1 = _cell_to_rowcol(a)
                r2, c2 = _cell_to_rowcol(b)
                rest = args[1:]
            else:
                r1, c1, r2, c2 = args[0], args[1], args[2], args[3]
                rest = args[4:]
            data = rest[0] if rest else None
            fmt = rest[1] if len(rest) > 1 else None
            kind, val = _detect(data)
            merge = {
                "first_row": int(r1),
                "first_col": int(c1),
                "last_row": int(r2),
                "last_col": int(c2),
                "text": "" if data is None else str(data),
            }
            props = _fmt(fmt)
            if props:
                merge["format"] = props
            self._merges.append(merge)
            # A merge writes its own top-left text; a number/date keeps its type.
            if kind in ("number", "boolean", "datetime", "formula"):
                self._put(r1, c1, kind, val, fmt)
            return 0

        def set_column(self, *args):
            args = list(args)
            if args and isinstance(args[0], str):
                a, b = (args[0].split(":") + [args[0]])[:2]
                first_col = _cell_to_rowcol(a + "1")[1]
                last_col = _cell_to_rowcol(b + "1")[1]
                rest = args[1:]
            else:
                first_col, last_col = args[0], args[1]
                rest = args[2:]
            width = rest[0] if rest else None
            cell_format = rest[1] if len(rest) > 1 else None
            options = rest[2] if len(rest) > 2 else None
            col = {"first": int(first_col), "last": int(last_col)}
            if width is not None:
                col["width"] = float(width)
            props = _fmt(cell_format)
            if props:
                col["format"] = props
            if options and options.get("hidden"):
                col["hidden"] = True
            self._columns.append(col)
            return 0

        def set_column_pixels(self, *args):
            args = list(args)
            if len(args) > 2 and args[2] is not None:
                args[2] = float(args[2]) / 7.0
            return self.set_column(*args)

        def set_row(self, row, height=None, cell_format=None, options=None):
            entry = {"index": int(row)}
            if height is not None:
                entry["height"] = float(height)
            props = _fmt(cell_format)
            if props:
                entry["format"] = props
            if options and options.get("hidden"):
                entry["hidden"] = True
            self._rows.append(entry)
            return 0

        def set_row_pixels(self, row, height=None, cell_format=None, options=None):
            h = None if height is None else float(height) * 0.75
            return self.set_row(row, h, cell_format, options)

        def set_default_row(self, *a, **k):
            return 0

        def freeze_panes(self, *args):
            if args and isinstance(args[0], str):
                row, col = _cell_to_rowcol(args[0])
            else:
                row = args[0] if args else 0
                col = args[1] if len(args) > 1 else 0
            self._spec["freeze"] = [int(row), int(col)]
            return 0

        split_panes = freeze_panes

        def autofit(self, *a, **k):
            self._spec["autofit"] = True
            return 0

        def autofilter(self, *args):
            if args and isinstance(args[0], str):
                a, b = args[0].split(":")
                r1, c1 = _cell_to_rowcol(a)
                r2, c2 = _cell_to_rowcol(b)
            else:
                r1, c1, r2, c2 = args[0], args[1], args[2], args[3]
            self._spec["autofilter"] = {
                "first_row": int(r1),
                "first_col": int(c1),
                "last_row": int(r2),
                "last_col": int(c2),
            }
            return 0

        def insert_image(self, *args):
            r, c, rest = self._rc(args)
            source = rest[0] if rest else None
            options = rest[1] if len(rest) > 1 else None
            options = options or {}
            image = {"row": int(r), "col": int(c), "data": _blob(source, options)}
            if options.get("x_scale") is not None:
                image["scale_x"] = float(options["x_scale"])
            if options.get("y_scale") is not None:
                image["scale_y"] = float(options["y_scale"])
            if options.get("x_offset"):
                image["x_offset"] = int(options["x_offset"])
            if options.get("y_offset"):
                image["y_offset"] = int(options["y_offset"])
            if options.get("description"):
                image["alt_text"] = str(options["description"])
            self._images.append(image)
            return 0

        embed_image = insert_image

        def activate(self):
            self._spec["active"] = True
            return 0

        def select(self):
            self._spec["selected"] = True
            return 0

        def hide(self):
            self._spec["hidden"] = True
            return 0

        def set_tab_color(self, color):
            self._spec["tab_color"] = color
            return 0

        def hide_gridlines(self, option=1):
            self._spec["hide_gridlines"] = int(option)
            return 0

        def protect(self, *a, **k):
            self._spec["protect"] = True
            return 0

        def set_zoom(self, zoom=100):
            self._spec["zoom"] = int(zoom)
            return 0

        def set_landscape(self):
            self._spec["landscape"] = True
            return 0

        def set_portrait(self):
            self._spec["landscape"] = False
            return 0

        def __getattr__(self, name):
            # Page-setup and print options vis does not model: accept and ignore.
            if name.startswith(("set_", "print_", "fit_", "repeat_", "center_")):

                def noop(*a, **k):
                    return 0

                return noop
            raise AttributeError(name)

        def _to_spec(self):
            spec = dict(self._spec)
            spec["name"] = self.name
            if self._columns:
                spec["columns"] = self._columns
            if self._rows:
                spec["rows"] = self._rows
            if self._cells:
                spec["cells"] = self._cells
            if self._merges:
                spec["merges"] = self._merges
            if self._images:
                spec["images"] = self._images
            return spec

    class Workbook:
        """Create an .xlsx file at `path`: add_worksheet() sheets, add_format() formats, then close() to write it."""

        def __init__(self, filename=None, options=None):
            self.filename = filename
            self.options = dict(options or {})
            self._closed = False
            self.worksheets_objs = []
            self.data = None
            self._properties = {}

        def add_worksheet(self, name=None):
            index = len(self.worksheets_objs)
            if name is None:
                name = "Sheet%d" % (index + 1)
            name = str(name)
            if any(ws.name == name for ws in self.worksheets_objs):
                raise XlsxWriterException("Duplicate worksheet name: %r" % name)
            ws = Worksheet(self, index, name)
            self.worksheets_objs.append(ws)
            return ws

        add_chartsheet = add_worksheet

        def add_format(self, properties=None):
            return Format(properties)

        def worksheets(self):
            return list(self.worksheets_objs)

        def get_worksheet_by_name(self, name):
            for ws in self.worksheets_objs:
                if ws.name == name:
                    return ws
            return None

        def set_properties(self, properties=None, **kwargs):
            self._properties.update(properties or {})
            self._properties.update(kwargs)
            return 0

        def define_name(self, *a, **k):
            return 0

        def set_size(self, *a, **k):
            return 0

        def close(self):
            if self._closed:
                return
            spec = {"sheets": [ws._to_spec() for ws in self.worksheets_objs]}
            if not spec["sheets"]:
                spec["sheets"] = [{"name": "Sheet1"}]
            if self._properties:
                spec["properties"] = {
                    k: v for k, v in self._properties.items() if v is not None
                }
            ok, b64 = _build(spec)
            _raise(ok, b64)
            self._closed = True
            data = base64.b64decode(b64)
            self.data = data
            if self.filename is not None:
                if hasattr(self.filename, "write"):
                    self.filename.write(data)
                else:
                    with open(self.filename, "wb") as f:
                        f.write(data)

        def __enter__(self):
            return self

        def __exit__(self, *a):
            self.close()
            return False

    mod = types.ModuleType("xlsxwriter")
    mod.__doc__ = (
        "Rust-backed `xlsxwriter` .xlsx writer: model the workbook in Python, built on close. "
        "Not supported: `constant_memory` streaming, VBA, charts, data validation."
    )
    mod.Workbook = Workbook
    mod.Worksheet = Worksheet
    mod.Format = Format
    mod.XlsxWriterException = XlsxWriterException
    mod.__version__ = "3.2.9"

    _wbmod = types.ModuleType("xlsxwriter.workbook")
    _wbmod.__doc__ = "xlsxwriter.workbook: the Workbook object that creates worksheets and saves the .xlsx file."
    _wbmod.Workbook = Workbook
    mod.workbook = _wbmod
    _wsmod = types.ModuleType("xlsxwriter.worksheet")
    _wsmod.__doc__ = "xlsxwriter.worksheet: the Worksheet object that writes cells, rows and columns."
    _wsmod.Worksheet = Worksheet
    mod.worksheet = _wsmod
    _fmtmod = types.ModuleType("xlsxwriter.format")
    _fmtmod.__doc__ = (
        "xlsxwriter.format: the Format cell-format object a workbook hands out."
    )
    _fmtmod.Format = Format
    mod.format = _fmtmod
    _exc = types.ModuleType("xlsxwriter.exceptions")
    _exc.__doc__ = (
        "xlsxwriter.exceptions: XlsxWriterException, the base error this shim raises."
    )
    _exc.XlsxWriterException = XlsxWriterException
    mod.exceptions = _exc

    def _col_to_name(col):
        name = ""
        col += 1
        while col > 0:
            col, rem = divmod(col - 1, 26)
            name = chr(65 + rem) + name
        return name

    def xl_rowcol_to_cell(row, col, row_abs=False, col_abs=False):
        return (
            ("$" if col_abs else "")
            + _col_to_name(col)
            + ("$" if row_abs else "")
            + str(row + 1)
        )

    _util = types.ModuleType("xlsxwriter.utility")
    _util.__doc__ = "xlsxwriter.utility: xl_rowcol_to_cell and the other A1/rowcol cell-reference helpers."
    _util.xl_cell_to_rowcol = _cell_to_rowcol
    _util.xl_rowcol_to_cell = xl_rowcol_to_cell
    _util.xl_col_to_name = _col_to_name
    mod.utility = _util

    sys.modules["xlsxwriter"] = mod
    for _sub in ("workbook", "worksheet", "format", "exceptions", "utility"):
        sys.modules["xlsxwriter." + _sub] = getattr(mod, _sub)
    try:
        _bi.xlsxwriter = mod
    except Exception:
        pass

    _ = io


__vis_install_xlsxwriter__()
del __vis_install_xlsxwriter__
