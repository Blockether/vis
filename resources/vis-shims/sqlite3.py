def __vis_install_sqlite3__():
    import sys, types
    _bi = sys.modules['builtins']
    _connect = __vis_sqlite_connect__
    _execute = __vis_sqlite_execute__
    _executemany = __vis_sqlite_executemany__
    _executescript = __vis_sqlite_executescript__
    _commit = __vis_sqlite_commit__
    _rollback = __vis_sqlite_rollback__
    _close = __vis_sqlite_close__
    _total_changes = __vis_sqlite_total_changes__
    BLOB_TAG = '__vis_blob__'
    mod = types.ModuleType('sqlite3')
    class Warning(Exception): pass
    class Error(Exception): pass
    class InterfaceError(Error): pass
    class DatabaseError(Error): pass
    class DataError(DatabaseError): pass
    class OperationalError(DatabaseError): pass
    class IntegrityError(DatabaseError): pass
    class InternalError(DatabaseError): pass
    class ProgrammingError(DatabaseError): pass
    class NotSupportedError(DatabaseError): pass
    def _raise(msg):
        low = (msg or '').lower()
        if 'unique' in low or 'constraint' in low or 'not null' in low or 'foreign key' in low:
            raise IntegrityError(msg)
        raise OperationalError(msg)
    def _call(fn, *args):
        res = fn(*args)
        ok = res[0]
        payload = res[1]
        if not ok:
            _raise(payload)
        return payload
    def _decode_cell(v):
        if v is None:
            return None
        if isinstance(v, list) and len(v) == 2 and v[0] == BLOB_TAG:
            import base64
            return base64.b64decode(v[1])
        return v
    def _encode_param(v):
        if isinstance(v, (bytes, bytearray)):
            import base64
            return [BLOB_TAG, base64.b64encode(bytes(v)).decode('ascii')]
        if isinstance(v, bool):
            return 1 if v else 0
        if isinstance(v, int) and (v > 9223372036854775807 or v < -9223372036854775808):
            raise OverflowError('Python int too large to convert to SQLite INTEGER')
        if v is not None and not isinstance(v, (int, float, str)):
            raise InterfaceError('Error binding parameter - probably unsupported type.')
        return v
    def _encode_params(params):
        if params is None:
            return None
        if isinstance(params, dict):
            return dict((k, _encode_param(x)) for k, x in params.items())
        return [_encode_param(x) for x in params]
    class Row:
        def __init__(self, cursor, values):
            self._keys = [d[0] for d in (cursor.description or [])]
            self._vals = list(values)
        def keys(self):
            return list(self._keys)
        def __getitem__(self, k):
            if isinstance(k, (int, slice)):
                return self._vals[k]
            for i, kk in enumerate(self._keys):
                if kk == k or kk.lower() == str(k).lower():
                    return self._vals[i]
            raise IndexError(k)
        def __iter__(self):
            return iter(self._vals)
        def __len__(self):
            return len(self._vals)
        def __eq__(self, other):
            if isinstance(other, Row):
                return self._vals == other._vals
            return list(self._vals) == list(other)
        def __repr__(self):
            return 'Row' + repr(tuple(self._vals))
    class Cursor:
        def __init__(self, connection):
            self.connection = connection
            self.description = None
            self.rowcount = -1
            self.lastrowid = None
            self.arraysize = 1
            self._rows = []
            self._idx = 0
        def _apply(self, payload):
            desc = payload.get('description')
            if desc is None:
                self.description = None
            else:
                self.description = [(name, None, None, None, None, None, None) for name in desc]
            self.rowcount = payload.get('rowcount', -1)
            self.lastrowid = payload.get('lastrowid')
            raw = payload.get('rows') or []
            self._rows = [tuple(_decode_cell(c) for c in r) for r in raw]
            self._idx = 0
        def execute(self, sql, params=None):
            self._apply(_call(_execute, self.connection._h, sql, _encode_params(params)))
            return self
        def executemany(self, sql, seq_of_params):
            enc = [_encode_params(p) for p in seq_of_params]
            self._apply(_call(_executemany, self.connection._h, sql, enc))
            return self
        def executescript(self, sql):
            self._apply(_call(_executescript, self.connection._h, sql))
            return self
        def _row(self, values):
            rf = self.connection.row_factory
            if rf is Row:
                return Row(self, values)
            if rf is not None:
                return rf(self, values)
            return values
        def fetchone(self):
            if self._idx >= len(self._rows):
                return None
            r = self._rows[self._idx]
            self._idx += 1
            return self._row(r)
        def fetchmany(self, size=None):
            n = size if size is not None else self.arraysize
            out = []
            while n > 0 and self._idx < len(self._rows):
                out.append(self._row(self._rows[self._idx]))
                self._idx += 1
                n -= 1
            return out
        def fetchall(self):
            out = [self._row(r) for r in self._rows[self._idx:]]
            self._idx = len(self._rows)
            return out
        def __iter__(self):
            return self
        def __next__(self):
            r = self.fetchone()
            if r is None:
                raise StopIteration
            return r
        def close(self):
            self._rows = []
        def __enter__(self):
            return self
        def __exit__(self, *a):
            self.close()
            return False
    class Connection:
        def __init__(self, database):
            self._h = _call(_connect, database)
            self.row_factory = None
            self.text_factory = str
            self.isolation_level = ''
        def cursor(self):
            return Cursor(self)
        def execute(self, sql, params=None):
            return self.cursor().execute(sql, params)
        def executemany(self, sql, seq):
            return self.cursor().executemany(sql, seq)
        def executescript(self, sql):
            return self.cursor().executescript(sql)
        def commit(self):
            _call(_commit, self._h)
        def rollback(self):
            _call(_rollback, self._h)
        def close(self):
            _call(_close, self._h)
        @property
        def total_changes(self):
            return _call(_total_changes, self._h)
        def create_function(self, name, narg, func, **kw):
            raise NotSupportedError('create_function requires a host callback bridge, unavailable in the vis sqlite3 shim')
        def create_aggregate(self, name, narg, cls):
            raise NotSupportedError('create_aggregate is unavailable in the vis sqlite3 shim')
        def set_trace_callback(self, cb):
            return None
        def interrupt(self):
            return None
        def iterdump(self):
            _dq = chr(34)
            _sq = chr(39)
            def _q(s):
                return _sq + str(s).replace(_sq, _sq + _sq) + _sq
            def _lit(v):
                if v is None:
                    return 'NULL'
                if isinstance(v, bool):
                    return '1' if v else '0'
                if isinstance(v, (int, float)):
                    return str(v)
                if isinstance(v, (bytes, bytearray)):
                    return 'X' + _q(bytes(v).hex())
                return _q(v)
            yield 'BEGIN TRANSACTION;'
            cur = self.cursor()
            cur.execute('SELECT name, type, sql FROM sqlite_master WHERE sql IS NOT NULL ORDER BY (type != ' + _q('table') + '), name')
            schema = cur.fetchall()
            tables = []
            for nm, typ, sql in schema:
                yield sql + ';'
                if typ == 'table':
                    tables.append(nm)
            for nm in tables:
                dcur = self.cursor()
                dcur.execute('SELECT * FROM ' + _dq + nm + _dq)
                for row in dcur.fetchall():
                    vals = ', '.join(_lit(v) for v in row)
                    yield 'INSERT INTO ' + _dq + nm + _dq + ' VALUES(' + vals + ');'
            yield 'COMMIT;'
        def __enter__(self):
            return self
        def __exit__(self, exc_type, exc, tb):
            if exc_type is None:
                self.commit()
            else:
                self.rollback()
            return False
    def connect(database=':memory:', timeout=5.0, detect_types=0, isolation_level='', check_same_thread=True, factory=None, cached_statements=128, uri=False, **kw):
        return Connection(database if isinstance(database, str) else str(database))
    def register_adapter(*a, **k):
        return None
    def register_converter(*a, **k):
        return None
    def complete_statement(sql):
        return sql.strip().endswith(';')
    def enable_callback_tracebacks(*a, **k):
        return None
    mod.connect = connect
    mod.Connection = Connection
    mod.Cursor = Cursor
    mod.Row = Row
    mod.Error = Error
    mod.Warning = Warning
    mod.InterfaceError = InterfaceError
    mod.DatabaseError = DatabaseError
    mod.DataError = DataError
    mod.OperationalError = OperationalError
    mod.IntegrityError = IntegrityError
    mod.InternalError = InternalError
    mod.ProgrammingError = ProgrammingError
    mod.NotSupportedError = NotSupportedError
    mod.register_adapter = register_adapter
    mod.register_converter = register_converter
    mod.complete_statement = complete_statement
    mod.enable_callback_tracebacks = enable_callback_tracebacks
    mod.PARSE_DECLTYPES = 1
    mod.PARSE_COLNAMES = 2
    mod.version = '2.6.0'
    mod.version_info = (2, 6, 0)
    mod.sqlite_version = '3.53.2'
    mod.sqlite_version_info = (3, 53, 2)
    mod.paramstyle = 'qmark'
    mod.apilevel = '2.0'
    mod.threadsafety = 1
    mod.Binary = bytes
    mod.LEGACY_TRANSACTION_CONTROL = -1
    sys.modules['sqlite3'] = mod
    sys.modules['sqlite3.dbapi2'] = mod
    _bi.sqlite3 = mod
__vis_install_sqlite3__()
del __vis_install_sqlite3__
