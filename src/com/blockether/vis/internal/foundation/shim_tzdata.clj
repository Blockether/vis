(ns com.blockether.vis.internal.foundation.shim-tzdata
  "Built-in sandbox SHIM: `zoneinfo`, `pytz` and `dateutil` for the model's Python
   sandbox, backed by the JVM's `java.time` IANA time-zone database.

   GraalPy ships no writable filesystem, so the real `zoneinfo` / `pytz` /
   `tzdata` crash at import (`_tzpath` calls `getcwd`, which the denied FS refuses
   with an UN-catchable Java `SecurityException` that aborts the whole eval). This
   extension contributes a `:ext/sandbox-shims` entry that
   `env-python/build-agent-context` installs into every sandbox Context (main +
   every `sub_loop` fork): host callables resolve zone offsets / DST / names via
   `java.time.ZoneId` (604+ zones, no data files), then a Python preamble publishes
   `zoneinfo`, `pytz`, `tzdata` and the `dateutil` package (`dateutil.tz`,
   `dateutil.parser`, `dateutil.relativedelta`) into `sys.modules` and staples them
   onto builtins. All tz math happens on the JVM; only small metadata vectors cross
   the strings-only boundary. Kills the whole class of tz-aware-`datetime` failures."
  (:require [com.blockether.vis.core :as vis])
  (:import [java.time LocalDateTime ZoneId ZoneOffset]
           [java.util Locale TimeZone]))

;; ---------------------------------------------------------------------------
;; Host bridge: java.time IANA zone rules. The Python side holds only zone-id
;; strings + wall-clock [y m d H M S] vectors; every offset/DST/name lookup
;; happens here on the JVM, so no tz data files (and no getcwd) are ever needed.
;; ---------------------------------------------------------------------------

(defn- zone-exists?
  "True when `key` names a resolvable java.time zone."
  [key]
  (try (ZoneId/of (str key)) true (catch Throwable _ false)))

(defn- i ^long [x] (long x))

(defn- tz-info
  "For wall-clock `[y m d H M S]` interpreted in zone `key`, return
   `[offset-seconds dst-seconds abbrev]`."
  [key ymdhms]
  (let [[y mo d H M S]
        (map i ymdhms)

        z
        (ZoneId/of (str key))

        rules
        (.getRules z)

        ldt
        (LocalDateTime/of (int y) (int mo) (int d) (int H) (int M) (int S))

        off
        (.getOffset rules ldt)

        inst
        (.toInstant (.atZone ldt z))

        dst
        (.getDaylightSavings rules inst)

        in-dst
        (pos? (.getSeconds dst))

        tzname
        (.getDisplayName (TimeZone/getTimeZone (str key)) in-dst TimeZone/SHORT Locale/US)]

    [(long (.getTotalSeconds off)) (long (.getSeconds dst)) (str tzname)]))

(defn- tz-fromutc
  "For wall-clock `[y m d H M S]` interpreted as UTC in zone `key`, return
   `[offset-seconds]` to add to reach that zone's local wall time."
  [key ymdhms]
  (let [[y mo d H M S]
        (map i ymdhms)

        z
        (ZoneId/of (str key))

        rules
        (.getRules z)

        ldt
        (LocalDateTime/of (int y) (int mo) (int d) (int H) (int M) (int S))

        inst
        (.toInstant ldt ZoneOffset/UTC)

        off
        (.getOffset rules inst)]

    [(long (.getTotalSeconds off))]))

(defn- tz-available
  "Sorted vector of every available IANA zone id."
  []
  (vec (sort (ZoneId/getAvailableZoneIds))))

(defn- tz-local
  "The JVM's default zone id (best-effort local zone)."
  []
  (str (.getId (ZoneId/systemDefault))))

(defn- tz-envelope [f] (try [true (f)] (catch Throwable t [false (str (or (.getMessage t) t))])))

(defn- tzdata-bridge-bindings
  "Host callables the tz shim delegates to (java.time-backed)."
  []
  {"__vis_tz_exists__" (fn [k]
                         (zone-exists? k))
   "__vis_tz_info__" (fn [k ymd]
                       (tz-envelope #(tz-info k ymd)))
   "__vis_tz_fromutc__" (fn [k ymd]
                          (tz-envelope #(tz-fromutc k ymd)))
   "__vis_tz_available__" (fn []
                            (tz-available))
   "__vis_tz_local__" (fn []
                        (tz-local))})

(def ^:private tzdata-shim-src
  "
def __vis_install_tzdata__():
    import datetime as _dt
    import types as _types
    import sys as _sys
    import builtins as _bi
    import email.utils as _eut

    _ZERO = _dt.timedelta(0)

    # ============================ zoneinfo ============================
    class ZoneInfoNotFoundError(KeyError):
        pass

    class ZoneInfo(_dt.tzinfo):
        _cache = {}

        def __new__(cls, key):
            key = str(key)
            got = cls._cache.get(key)
            if got is not None:
                return got
            if not __vis_tz_exists__(key):
                raise ZoneInfoNotFoundError(key)
            self = _dt.tzinfo.__new__(cls)
            self.key = key
            cls._cache[key] = self
            return self

        @classmethod
        def no_cache(cls, key):
            key = str(key)
            if not __vis_tz_exists__(key):
                raise ZoneInfoNotFoundError(key)
            self = _dt.tzinfo.__new__(cls)
            self.key = key
            return self

        @classmethod
        def from_file(cls, fobj, key=None):
            raise ValueError('ZoneInfo.from_file is unavailable in this sandbox')

        @classmethod
        def clear_cache(cls, only_keys=None):
            if only_keys is None:
                cls._cache.clear()
            else:
                for k in list(only_keys):
                    cls._cache.pop(str(k), None)

        def _info(self, dt):
            ok, payload = __vis_tz_info__(self.key, [dt.year, dt.month, dt.day, dt.hour, dt.minute, dt.second])
            if not ok:
                raise ValueError(str(payload))
            return payload

        def utcoffset(self, dt):
            if dt is None:
                return None
            return _dt.timedelta(seconds=self._info(dt)[0])

        def dst(self, dt):
            if dt is None:
                return None
            return _dt.timedelta(seconds=self._info(dt)[1])

        def tzname(self, dt):
            if dt is None:
                return None
            return self._info(dt)[2]

        def fromutc(self, dt):
            if dt.tzinfo is not self:
                raise ValueError('fromutc: dt.tzinfo is not self')
            ok, payload = __vis_tz_fromutc__(self.key, [dt.year, dt.month, dt.day, dt.hour, dt.minute, dt.second])
            if not ok:
                raise ValueError(str(payload))
            return (dt + _dt.timedelta(seconds=payload[0]))

        def __str__(self):
            return self.key

        def __repr__(self):
            return 'zoneinfo.ZoneInfo(key=' + repr(self.key) + ')'

        def __reduce__(self):
            return (self.__class__, (self.key,))

    def available_timezones():
        return set(__vis_tz_available__())

    _zi_mod = _types.ModuleType('zoneinfo')
    _zi_mod.ZoneInfo = ZoneInfo
    _zi_mod.ZoneInfoNotFoundError = ZoneInfoNotFoundError
    _zi_mod.available_timezones = available_timezones
    _zi_mod.TZPATH = ()
    _zi_mod.reset_tzpath = lambda to=None: None
    _zi_mod.__all__ = ['ZoneInfo', 'ZoneInfoNotFoundError', 'available_timezones', 'reset_tzpath', 'TZPATH']

    # a bare tzdata package so `import tzdata` succeeds (libs probe for it)
    _tzdata_mod = _types.ModuleType('tzdata')
    _tzdata_mod.IANA_VERSION = 'system'
    _tzdata_mod.__version__ = 'system'

    # ============================ pytz ============================
    class UnknownTimeZoneError(KeyError):
        pass

    class Error(Exception):
        pass

    class _BaseTz(_dt.tzinfo):
        def localize(self, dt, is_dst=False):
            if dt.tzinfo is not None:
                raise ValueError('Not naive datetime (tzinfo is already set)')
            return dt.replace(tzinfo=self)

        def normalize(self, dt):
            if dt.tzinfo is None:
                raise ValueError('Naive time - no tzinfo set')
            return dt.astimezone(self)

    class _PytzZone(_BaseTz):
        def __init__(self, key):
            self._z = ZoneInfo(key)
            self.zone = key

        def utcoffset(self, dt):
            return self._z.utcoffset(dt)

        def dst(self, dt):
            return self._z.dst(dt)

        def tzname(self, dt):
            return self._z.tzname(dt)

        def fromutc(self, dt):
            return self._z.fromutc(dt.replace(tzinfo=self._z)).replace(tzinfo=self)

        def __str__(self):
            return self.zone

        def __repr__(self):
            return '<DstTzInfo ' + repr(self.zone) + '>'

    class _UTC(_BaseTz):
        zone = 'UTC'

        def utcoffset(self, dt):
            return _ZERO

        def dst(self, dt):
            return _ZERO

        def tzname(self, dt):
            return 'UTC'

        def fromutc(self, dt):
            return dt + _ZERO

        def __str__(self):
            return 'UTC'

        def __repr__(self):
            return '<UTC>'

    _utc = _UTC()

    def _pytz_timezone(name):
        name = str(name)
        if name in ('UTC', 'GMT', 'Etc/UTC', 'Etc/GMT'):
            return _utc
        try:
            return _PytzZone(name)
        except ZoneInfoNotFoundError:
            raise UnknownTimeZoneError(name)

    def _FixedOffset(minutes):
        return _dt.timezone(_dt.timedelta(minutes=minutes))

    _pytz_mod = _types.ModuleType('pytz')
    _pytz_mod.timezone = _pytz_timezone
    _pytz_mod.utc = _utc
    _pytz_mod.UTC = _utc
    _pytz_mod.FixedOffset = _FixedOffset
    _pytz_mod.UnknownTimeZoneError = UnknownTimeZoneError
    _pytz_mod.Error = Error
    _pytz_mod.AmbiguousTimeError = type('AmbiguousTimeError', (Error,), {})
    _pytz_mod.NonExistentTimeError = type('NonExistentTimeError', (Error,), {})
    _pytz_mod.InvalidTimeError = type('InvalidTimeError', (Error,), {})
    _allzones = sorted(__vis_tz_available__())
    _pytz_mod.all_timezones = list(_allzones)
    _pytz_mod.all_timezones_set = set(_allzones)
    _pytz_mod.common_timezones = list(_allzones)
    _pytz_mod.common_timezones_set = set(_allzones)
    _pytz_mod.country_timezones = {}
    _pytz_mod.country_names = {}
    _pytz_mod.__version__ = 'system'

    # ============================ dateutil ============================
    def _gettz(name=None):
        if name is None or name == '':
            try:
                return ZoneInfo(__vis_tz_local__())
            except Exception:
                return _dt.timezone.utc
        try:
            return ZoneInfo(str(name))
        except ZoneInfoNotFoundError:
            return None

    def _tzoffset(name, offset):
        if isinstance(offset, _dt.timedelta):
            td = offset
        else:
            td = _dt.timedelta(seconds=int(offset))
        return _dt.timezone(td, name if name else None)

    _tz_mod = _types.ModuleType('dateutil.tz')
    _tz_mod.gettz = _gettz
    _tz_mod.tzutc = lambda: _dt.timezone.utc
    _tz_mod.tzlocal = lambda: _gettz(None)
    _tz_mod.tzoffset = _tzoffset
    _tz_mod.UTC = _dt.timezone.utc

    # ----- relativedelta -----
    def _days_in_month(y, m):
        if m == 2:
            leap = (y % 4 == 0 and (y % 100 != 0 or y % 400 == 0))
            return 29 if leap else 28
        return (31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)[m - 1]

    class weekday(object):
        def __init__(self, wkday, n=None):
            self.weekday = wkday
            self.n = n

        def __call__(self, n):
            if n == self.n:
                return self
            return weekday(self.weekday, n)

        def __eq__(self, other):
            try:
                return self.weekday == other.weekday and self.n == other.n
            except Exception:
                return False

        def __hash__(self):
            return hash((self.weekday, self.n))

        def __repr__(self):
            names = ('MO', 'TU', 'WE', 'TH', 'FR', 'SA', 'SU')
            s = names[self.weekday]
            if self.n is None:
                return s
            return s + '(' + repr(self.n) + ')'

    MO, TU, WE, TH, FR, SA, SU = (weekday(i) for i in range(7))

    class relativedelta(object):
        def __init__(self, dt1=None, dt2=None, years=0, months=0, days=0, leapdays=0,
                     weeks=0, hours=0, minutes=0, seconds=0, microseconds=0,
                     year=None, month=None, day=None, weekday=None, yearday=None,
                     nlyearday=None, hour=None, minute=None, second=None, microsecond=None):
            if dt1 is not None and dt2 is not None:
                # difference mode (relative-only, approximate like dateutil)
                delta = dt1 - dt2 if hasattr(dt1, 'toordinal') else None
                months_ = (dt1.year - dt2.year) * 12 + (dt1.month - dt2.month)
                self.years = 0
                self.months = 0
                # build by adding months then residual
                base = self.__class__(months=months_) + dt2
                if base > dt1:
                    months_ -= 1
                    base = self.__class__(months=months_) + dt2
                r = dt1 - base
                self.years = months_ // 12
                self.months = months_ % 12
                self.days = r.days
                self.hours = r.seconds // 3600
                self.minutes = (r.seconds % 3600) // 60
                self.seconds = r.seconds % 60
                self.microseconds = r.microseconds
                self.leapdays = 0
                self.year = self.month = self.day = None
                self.hour = self.minute = self.second = self.microsecond = None
                self.weekday = None
            else:
                self.years = years
                self.months = months
                self.days = days + weeks * 7
                self.leapdays = leapdays
                self.hours = hours
                self.minutes = minutes
                self.seconds = seconds
                self.microseconds = microseconds
                self.year = year
                self.month = month
                self.day = day
                self.hour = hour
                self.minute = minute
                self.second = second
                self.microsecond = microsecond
                self.weekday = weekday
            self._fix()

        def _fix(self):
            if abs(self.months) > 11:
                s = 1 if self.months >= 0 else -1
                div, mod = divmod(self.months * s, 12)
                self.months = mod * s
                self.years += div * s

        @property
        def weeks(self):
            return int(self.days / 7)

        def normalized(self):
            return self

        def __add__(self, other):
            if isinstance(other, relativedelta):
                return relativedelta(
                    years=self.years + other.years,
                    months=self.months + other.months,
                    days=self.days + other.days,
                    hours=self.hours + other.hours,
                    minutes=self.minutes + other.minutes,
                    seconds=self.seconds + other.seconds,
                    microseconds=self.microseconds + other.microseconds,
                    year=other.year if other.year is not None else self.year,
                    month=other.month if other.month is not None else self.month,
                    day=other.day if other.day is not None else self.day,
                    weekday=other.weekday if other.weekday is not None else self.weekday,
                    hour=other.hour if other.hour is not None else self.hour,
                    minute=other.minute if other.minute is not None else self.minute,
                    second=other.second if other.second is not None else self.second,
                    microsecond=other.microsecond if other.microsecond is not None else self.microsecond,
                )
            if not hasattr(other, 'year'):
                return NotImplemented
            year = (self.year if self.year is not None else other.year) + self.years
            month = self.month if self.month is not None else other.month
            month += self.months
            while month > 12:
                month -= 12
                year += 1
            while month < 1:
                month += 12
                year -= 1
            day = min(self.day if self.day is not None else other.day, _days_in_month(year, month))
            repl = {'year': year, 'month': month, 'day': day}
            if self.hour is not None:
                repl['hour'] = self.hour
            if self.minute is not None:
                repl['minute'] = self.minute
            if self.second is not None:
                repl['second'] = self.second
            if self.microsecond is not None:
                repl['microsecond'] = self.microsecond
            ret = other.replace(**repl)
            ret = ret + _dt.timedelta(days=self.days, hours=self.hours,
                                      minutes=self.minutes, seconds=self.seconds,
                                      microseconds=self.microseconds)
            if self.weekday is not None:
                wd = self.weekday.weekday
                n = self.weekday.n if self.weekday.n is not None else 1
                if n > 0:
                    jump = (wd - ret.weekday()) % 7
                    ret = ret + _dt.timedelta(days=jump + (n - 1) * 7)
                else:
                    jump = (ret.weekday() - wd) % 7
                    ret = ret - _dt.timedelta(days=jump + (-n - 1) * 7)
            return ret

        def __radd__(self, other):
            return self.__add__(other)

        def __rsub__(self, other):
            return self.__neg__().__add__(other)

        def __sub__(self, other):
            if not isinstance(other, relativedelta):
                return NotImplemented
            return self.__add__(-other)

        def __neg__(self):
            return relativedelta(
                years=-self.years, months=-self.months, days=-self.days,
                hours=-self.hours, minutes=-self.minutes, seconds=-self.seconds,
                microseconds=-self.microseconds, year=self.year, month=self.month,
                day=self.day, weekday=self.weekday, hour=self.hour, minute=self.minute,
                second=self.second, microsecond=self.microsecond)

        def __mul__(self, k):
            k = int(k)
            return relativedelta(
                years=self.years * k, months=self.months * k, days=self.days * k,
                hours=self.hours * k, minutes=self.minutes * k, seconds=self.seconds * k,
                microseconds=self.microseconds * k, year=self.year, month=self.month,
                day=self.day, weekday=self.weekday, hour=self.hour, minute=self.minute,
                second=self.second, microsecond=self.microsecond)

        __rmul__ = __mul__

        def __repr__(self):
            parts = []
            for a in ('years', 'months', 'days', 'hours', 'minutes', 'seconds', 'microseconds'):
                v = getattr(self, a)
                if v:
                    parts.append(a + '=' + repr(v))
            for a in ('year', 'month', 'day', 'weekday', 'hour', 'minute', 'second', 'microsecond'):
                v = getattr(self, a)
                if v is not None:
                    parts.append(a + '=' + repr(v))
            return 'relativedelta(' + ', '.join(parts) + ')'

    _rd_mod = _types.ModuleType('dateutil.relativedelta')
    _rd_mod.relativedelta = relativedelta
    _rd_mod.weekday = weekday
    _rd_mod.MO, _rd_mod.TU, _rd_mod.WE, _rd_mod.TH, _rd_mod.FR, _rd_mod.SA, _rd_mod.SU = MO, TU, WE, TH, FR, SA, SU
    _rd_mod.weekdays = (MO, TU, WE, TH, FR, SA, SU)

    # ----- parser -----
    class ParserError(ValueError):
        pass

    _MONTHS = {}
    _mfull = ('january', 'february', 'march', 'april', 'may', 'june', 'july',
              'august', 'september', 'october', 'november', 'december')
    for _i, _mn in enumerate(_mfull):
        _MONTHS[_mn] = _i + 1
        _MONTHS[_mn[:3]] = _i + 1

    _ISO_FMTS = (
        '%Y-%m-%dT%H:%M:%S', '%Y-%m-%d %H:%M:%S', '%Y-%m-%dT%H:%M',
        '%Y-%m-%d %H:%M', '%Y-%m-%d', '%Y/%m/%d %H:%M:%S', '%Y/%m/%d',
        '%B %d %Y', '%B %d, %Y', '%d %B %Y', '%b %d %Y', '%b %d, %Y',
        '%d %b %Y', '%Y%m%d', '%H:%M:%S', '%H:%M',
    )
    _MDY_FMTS = ('%m/%d/%Y %H:%M:%S', '%m/%d/%Y %H:%M', '%m/%d/%Y', '%m/%d/%y')
    _DMY_FMTS = ('%d/%m/%Y %H:%M:%S', '%d/%m/%Y %H:%M', '%d/%m/%Y', '%d/%m/%y',
                 '%d.%m.%Y %H:%M:%S', '%d.%m.%Y')

    def _try_iso(s):
        t = s
        if t.endswith('Z') or t.endswith('z'):
            t = t[:-1] + '+00:00'
        try:
            return _dt.datetime.fromisoformat(t)
        except Exception:
            pass
        try:
            return _dt.datetime.combine(_dt.date.fromisoformat(s), _dt.time())
        except Exception:
            return None

    def _try_rfc(s):
        low = s.lower()
        _wk = ('mon', 'tue', 'wed', 'thu', 'fri', 'sat', 'sun')
        if (',' not in s) or (not any(low.startswith(w) for w in _wk)):
            return None
        try:
            return _eut.parsedate_to_datetime(s)
        except Exception:
            return None

    def _try_formats(s, dayfirst=False):
        if dayfirst:
            order = _ISO_FMTS + _DMY_FMTS + _MDY_FMTS
        else:
            order = _ISO_FMTS + _MDY_FMTS + _DMY_FMTS
        for fmt in order:
            try:
                d = _dt.datetime.strptime(s, fmt)
                if '%Y' not in fmt and '%y' not in fmt:
                    d = d.replace(year=1900)
                return d
            except Exception:
                continue
        return None

    def _clean_token(tok):
        return tok.strip().strip(',').strip()

    def _try_tokens(s, default, dayfirst):
        raw = s.replace(',', ' ').split()
        toks = [t for t in (x.strip() for x in raw) if t]
        year = month = day = None
        hour = minute = second = 0
        ampm = None
        tzoff = None
        nums = []
        for t in toks:
            tl = t.lower()
            if tl in _MONTHS:
                month = _MONTHS[tl]
                continue
            if tl in ('am', 'pm'):
                ampm = tl
                continue
            if tl in ('utc', 'gmt', 'z'):
                tzoff = _dt.timezone.utc
                continue
            if ':' in t:
                bits = t.split(':')
                try:
                    hour = int(bits[0])
                    minute = int(bits[1]) if len(bits) > 1 else 0
                    second = int(bits[2]) if len(bits) > 2 else 0
                    continue
                except Exception:
                    pass
            core = t
            if (t.startswith('+') or t.startswith('-')) and ':' in t:
                try:
                    sign = 1 if t[0] == '+' else -1
                    hh, mm = t[1:].split(':')
                    tzoff = _dt.timezone(sign * _dt.timedelta(hours=int(hh), minutes=int(mm)))
                    continue
                except Exception:
                    pass
            digits = core.rstrip('stndrh')
            try:
                nums.append(int(digits))
            except Exception:
                continue
        # assign remaining numbers
        for n in nums:
            if n > 31 and year is None:
                year = n
        rem = [n for n in nums if not (n > 31)]
        if month is None:
            # need month from numbers
            if len(rem) >= 2:
                if dayfirst:
                    day, month = rem[0], rem[1]
                else:
                    month, day = rem[0], rem[1]
                rem = rem[2:]
            elif len(rem) == 1 and day is None:
                day = rem[0]
                rem = []
        else:
            for n in rem:
                if day is None and 1 <= n <= 31:
                    day = n
                elif year is None and n > 31:
                    year = n
            rem2 = []
            for n in rem:
                if n == day:
                    continue
                rem2.append(n)
            rem = rem2
        if year is None:
            for n in rem:
                if n >= 1000:
                    year = n
                elif year is None and n <= 99:
                    year = 2000 + n
        if year is None:
            year = default.year if default is not None else _dt.date.today().year
        if month is None or day is None:
            return None
        if ampm == 'pm' and hour < 12:
            hour += 12
        if ampm == 'am' and hour == 12:
            hour = 0
        try:
            d = _dt.datetime(year, month, day, hour, minute, second)
        except Exception:
            return None
        if tzoff is not None:
            d = d.replace(tzinfo=tzoff)
        return d

    _TZ_ABBR = {'utc': 0, 'gmt': 0, 'z': 0, 'ut': 0, 'est': -18000, 'edt': -14400,
                'cst': -21600, 'cdt': -18000, 'mst': -25200, 'mdt': -21600,
                'pst': -28800, 'pdt': -25200, 'bst': 3600, 'cet': 3600, 'cest': 7200,
                'eet': 7200, 'eest': 10800, 'ist': 19800, 'jst': 32400, 'kst': 32400,
                'aest': 36000, 'aedt': 39600, 'nzst': 43200, 'hst': -36000, 'akst': -32400}

    def parse(timestr, parserinfo=None, default=None, ignoretz=False, tzinfos=None,
              dayfirst=False, yearfirst=False, fuzzy=False, fuzzy_with_tokens=False, **kw):
        if not isinstance(timestr, str):
            timestr = str(timestr)
        s = timestr.strip()
        _tzab = None
        _pp = s.split()
        if _pp:
            _ab = _pp[-1]
            if tzinfos and _ab in tzinfos:
                _tzv = tzinfos[_ab]
                _tzab = _tzv
                s = ' '.join(_pp[:-1])
            elif _ab.lower() in _TZ_ABBR:
                _tzab = _TZ_ABBR[_ab.lower()]
                s = ' '.join(_pp[:-1])
        got = _try_iso(s)
        if got is None:
            got = _try_rfc(s)
        if got is None:
            got = _try_formats(s, dayfirst)
        if got is None:
            got = _try_tokens(s, default, dayfirst)
        if got is None:
            raise ParserError('Unknown string format: ' + timestr)
        if _tzab is not None and got.tzinfo is None and not ignoretz:
            if isinstance(_tzab, int):
                got = got.replace(tzinfo=_dt.timezone(_dt.timedelta(seconds=_tzab)))
            elif _tzab is not None:
                got = got.replace(tzinfo=_tzab)
        if default is not None and got.tzinfo is None:
            pass
        if ignoretz and got.tzinfo is not None:
            got = got.replace(tzinfo=None)
        if fuzzy_with_tokens:
            return (got, ())
        return got

    def isoparse(s):
        r = _try_iso(str(s).strip())
        if r is None:
            raise ParserError('Unknown ISO format: ' + str(s))
        return r

    _parser_mod = _types.ModuleType('dateutil.parser')
    _parser_mod.parse = parse
    _parser_mod.isoparse = isoparse
    _parser_mod.ParserError = ParserError

    # dateutil package
    _du_mod = _types.ModuleType('dateutil')
    _du_mod.tz = _tz_mod
    _du_mod.parser = _parser_mod
    _du_mod.relativedelta = _rd_mod
    _du_mod.__version__ = 'system'
    _du_mod.__path__ = []

    # ---- register in sys.modules ----
    _sys.modules['zoneinfo'] = _zi_mod
    _sys.modules['tzdata'] = _tzdata_mod
    _sys.modules['pytz'] = _pytz_mod
    _sys.modules['dateutil'] = _du_mod
    _sys.modules['dateutil.tz'] = _tz_mod
    _sys.modules['dateutil.parser'] = _parser_mod
    _sys.modules['dateutil.relativedelta'] = _rd_mod

    # staple onto builtins so they resolve without import
    try:
        _bi.zoneinfo = _zi_mod
        _bi.pytz = _pytz_mod
        _bi.dateutil = _du_mod
    except Exception:
        pass


__vis_install_tzdata__()
del __vis_install_tzdata__
")

(def vis-extension
  (vis/extension
    {:ext/name "foundation-shim-tzdata"
     :ext/description
     "Sandbox shim: `zoneinfo`, `pytz`, `tzdata` and `dateutil` (tz/parser/relativedelta) backed by the JVM java.time IANA zone database. Fixes the un-catchable getcwd SecurityException the real modules hit under the sandbox's denied filesystem. No pip, no native wheel, no data files."
     :ext/version "0.1.0"
     :ext/author "Blockether"
     :ext/owner "vis"
     :ext/license "Apache-2.0"
     :ext/kind "foundation"
     :ext/sandbox-shims
     [{:shim/name "tzdata"
       :shim/description
       "zoneinfo/pytz/tzdata + dateutil.{tz,parser,relativedelta} backed by JVM java.time (604+ IANA zones, DST-correct, no data files)."
       :shim/bindings tzdata-bridge-bindings
       :shim/preamble tzdata-shim-src}]}))

(vis/register-extension! vis-extension)
