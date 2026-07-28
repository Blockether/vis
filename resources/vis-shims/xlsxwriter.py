def __vis_install_xlsxwriter__():
    import sys, types, base64, datetime
    _bi = sys.modules['builtins']
    _new = __vis_xlsx_new__
    _add_sheet = __vis_xlsx_add_sheet__
    _add_format = __vis_xlsx_add_format__
    _write = __vis_xlsx_write__
    _url = __vis_xlsx_url__
    _merge = __vis_xlsx_merge__
    _set_col = __vis_xlsx_set_column__
    _set_row = __vis_xlsx_set_row__
    _close = __vis_xlsx_close__

    class XlsxWriterException(Exception):
        pass

    def _raise(ok, msg):
        if not ok:
            raise XlsxWriterException(str(msg))

    def _cell_to_rowcol(cell):
        cell = cell.upper().replace('$', '')
        i = 0
        col = 0
        while i < len(cell) and cell[i].isalpha():
            col = col * 26 + (ord(cell[i]) - ord('A') + 1)
            i += 1
        row = int(cell[i:]) if i < len(cell) else 1
        return (row - 1, col - 1)

    def _looks_cell(s):
        if not s or not s[0].isalpha():
            return False
        seen = False
        for ch in s.replace('$', ''):
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
            return ('blank', '')
        if isinstance(data, bool):
            return ('boolean', data)
        if isinstance(data, (int, float)):
            return ('number', float(data))
        if isinstance(data, (datetime.datetime, datetime.date, datetime.time)):
            return ('datetime', data.isoformat())
        s = str(data)
        if s.startswith('='):
            return ('formula', s)
        return ('string', s)

    class Format:
        def __init__(self, props=None):
            self._props = dict(props or {})
            self._id = None

        def set_properties(self, props):
            self._props.update(props or {})
            self._id = None

        def __getattr__(self, name):
            if name.startswith('set_'):
                key = name[4:]

                def setter(value=True):
                    self._props[key] = value
                    self._id = None
                return setter
            raise AttributeError(name)

    class Worksheet:
        def __init__(self, wb, index, name):
            self._wb = wb
            self.index = index
            self.name = name

        def _rc(self, args):
            args = list(args)
            if args and isinstance(args[0], str) and _looks_cell(args[0]):
                r, c = _cell_to_rowcol(args[0])
                return r, c, args[1:]
            return args[0], args[1], args[2:]

        def _put(self, r, c, kind, val, fmt):
            ok, res = _write(self._wb._h, self.index, r, c, kind, val, self._wb._fmt_id(fmt))
            _raise(ok, res)
            return 0

        def write(self, *args):
            r, c, rest = self._rc(args)
            data = rest[0] if rest else None
            fmt = rest[1] if len(rest) > 1 else None
            kind, val = _detect(data)
            if kind == 'string' and (val.startswith('http://') or val.startswith('https://') or val.startswith('mailto:')):
                return self.write_url(r, c, val, fmt)
            return self._put(r, c, kind, val, fmt)

        def write_string(self, *args):
            r, c, rest = self._rc(args)
            return self._put(r, c, 'string', str(rest[0]) if rest else '', rest[1] if len(rest) > 1 else None)

        def write_number(self, *args):
            r, c, rest = self._rc(args)
            return self._put(r, c, 'number', float(rest[0]), rest[1] if len(rest) > 1 else None)

        def write_boolean(self, *args):
            r, c, rest = self._rc(args)
            return self._put(r, c, 'boolean', bool(rest[0]), rest[1] if len(rest) > 1 else None)

        def write_formula(self, *args):
            r, c, rest = self._rc(args)
            return self._put(r, c, 'formula', str(rest[0]), rest[1] if len(rest) > 1 else None)

        def write_datetime(self, *args):
            r, c, rest = self._rc(args)
            v = rest[0]
            iso = v.isoformat() if hasattr(v, 'isoformat') else str(v)
            return self._put(r, c, 'datetime', iso, rest[1] if len(rest) > 1 else None)

        def write_blank(self, *args):
            r, c, rest = self._rc(args)
            return self._put(r, c, 'blank', '', rest[1] if len(rest) > 1 else None)

        def write_url(self, *args):
            r, c, rest = self._rc(args)
            url = str(rest[0])
            fmt = rest[1] if len(rest) > 1 else None
            string = rest[2] if len(rest) > 2 else None
            tip = rest[3] if len(rest) > 3 else None
            ok, res = _url(self._wb._h, self.index, r, c, url, string, tip, self._wb._fmt_id(fmt))
            _raise(ok, res)
            return 0

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

        def merge_range(self, *args):
            args = list(args)
            if args and isinstance(args[0], str):
                a, b = args[0].split(':')
                r1, c1 = _cell_to_rowcol(a)
                r2, c2 = _cell_to_rowcol(b)
                rest = args[1:]
            else:
                r1, c1, r2, c2 = args[0], args[1], args[2], args[3]
                rest = args[4:]
            data = rest[0] if rest else None
            fmt = rest[1] if len(rest) > 1 else None
            kind, val = _detect(data)
            ok, res = _merge(self._wb._h, self.index, r1, c1, r2, c2, kind, val, self._wb._fmt_id(fmt))
            _raise(ok, res)
            return 0

        def set_column(self, *args):
            args = list(args)
            if args and isinstance(args[0], str):
                a, b = (args[0].split(':') + [args[0]])[:2]
                first_col = _cell_to_rowcol(a + '1')[1]
                last_col = _cell_to_rowcol(b + '1')[1]
                rest = args[1:]
            else:
                first_col, last_col = args[0], args[1]
                rest = args[2:]
            width = rest[0] if rest else None
            cell_format = rest[1] if len(rest) > 1 else None
            options = rest[2] if len(rest) > 2 else None
            hidden = bool(options.get('hidden')) if options else False
            ok, res = _set_col(self._wb._h, self.index, first_col, last_col, width, self._wb._fmt_id(cell_format), hidden)
            _raise(ok, res)
            return 0

        def set_row(self, row, height=None, cell_format=None, options=None):
            hidden = bool(options.get('hidden')) if options else False
            ok, res = _set_row(self._wb._h, self.index, row, height, self._wb._fmt_id(cell_format), hidden)
            _raise(ok, res)
            return 0

        def set_default_row(self, *a, **k):
            return 0

        def freeze_panes(self, *a, **k):
            return 0

        def autofit(self, *a, **k):
            return 0

        def activate(self):
            return 0

    class Workbook:
        def __init__(self, filename=None, options=None):
            ok, h = _new()
            _raise(ok, h)
            self._h = h
            self.filename = filename
            self._closed = False
            self.worksheets_objs = []
            self.data = None

        def add_worksheet(self, name=None):
            ok, res = _add_sheet(self._h, name)
            _raise(ok, res)
            ws = Worksheet(self, res['index'], res['name'])
            self.worksheets_objs.append(ws)
            return ws

        def add_format(self, properties=None):
            return Format(properties)

        def _fmt_id(self, fmt):
            if fmt is None:
                return -1
            if fmt._id is None:
                ok, fid = _add_format(self._h, fmt._props)
                _raise(ok, fid)
                fmt._id = fid
            return fmt._id

        def worksheets(self):
            return list(self.worksheets_objs)

        def get_worksheet_by_name(self, name):
            for ws in self.worksheets_objs:
                if ws.name == name:
                    return ws
            return None

        def define_name(self, *a, **k):
            return 0

        def set_properties(self, *a, **k):
            return 0

        def close(self):
            if self._closed:
                return
            ok, b64 = _close(self._h)
            _raise(ok, b64)
            self._closed = True
            data = base64.b64decode(b64)
            self.data = data
            if self.filename is not None:
                if hasattr(self.filename, 'write'):
                    self.filename.write(data)
                else:
                    with open(self.filename, 'wb') as f:
                        f.write(data)

        def __enter__(self):
            return self

        def __exit__(self, *a):
            self.close()
            return False

    mod = types.ModuleType('xlsxwriter')
    mod.Workbook = Workbook
    mod.Worksheet = Worksheet
    mod.Format = Format
    mod.XlsxWriterException = XlsxWriterException
    mod.__version__ = '3.2.9'

    _wbmod = types.ModuleType('xlsxwriter.workbook')
    _wbmod.Workbook = Workbook
    mod.workbook = _wbmod
    _wsmod = types.ModuleType('xlsxwriter.worksheet')
    _wsmod.Worksheet = Worksheet
    mod.worksheet = _wsmod
    _fmtmod = types.ModuleType('xlsxwriter.format')
    _fmtmod.Format = Format
    mod.format = _fmtmod
    _exc = types.ModuleType('xlsxwriter.exceptions')
    _exc.XlsxWriterException = XlsxWriterException
    mod.exceptions = _exc

    def _col_to_name(col):
        name = ''
        col += 1
        while col > 0:
            col, rem = divmod(col - 1, 26)
            name = chr(65 + rem) + name
        return name

    def xl_rowcol_to_cell(row, col, row_abs=False, col_abs=False):
        return ('$' if col_abs else '') + _col_to_name(col) + ('$' if row_abs else '') + str(row + 1)

    _util = types.ModuleType('xlsxwriter.utility')
    _util.xl_cell_to_rowcol = _cell_to_rowcol
    _util.xl_rowcol_to_cell = xl_rowcol_to_cell
    _util.xl_col_to_name = _col_to_name
    mod.utility = _util

    sys.modules['xlsxwriter'] = mod
    for _sub in ('workbook', 'worksheet', 'format', 'exceptions', 'utility'):
        sys.modules['xlsxwriter.' + _sub] = getattr(mod, _sub)
    try:
        _bi.xlsxwriter = mod
    except Exception:
        pass

__vis_install_xlsxwriter__()
del __vis_install_xlsxwriter__
