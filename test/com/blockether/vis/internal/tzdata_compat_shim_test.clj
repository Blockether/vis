(ns com.blockether.vis.internal.tzdata-compat-shim-test
  "The tzdata-compat shim installed into every sandbox context via the generic
   sandbox-shim mechanism: `zoneinfo`, `pytz`, `tzdata` and the `dateutil` package
   (tz/parser/relativedelta) published into `sys.modules`, backed by the JVM
   java.time IANA zone database. Fixes the un-catchable getcwd SecurityException
   the real modules hit under the sandbox's denied filesystem."
  (:require [com.blockether.vis.internal.env-python :as ep]
            [lazytest.core :refer [defdescribe expect it]])
  (:import [org.graalvm.polyglot Context]))

(defn- ev [^Context c code] (ep/->clj (.eval c "python" code)))

(defmacro with-python-context
  [& body]
  `(let
     [~(with-meta 'python-context {:tag `Context}) (:python-context (ep/create-python-context {}))]
     (try ~@body (finally (.close ~'python-context)))))

(defdescribe
  zoneinfo-module-test
  (it "publishes zoneinfo/pytz/dateutil under sys.modules"
      (with-python-context
        (expect (true? (ev python-context
                           (str "import sys\n"
                                "all(sys.modules.get(m) is not None for m in "
                                "['zoneinfo','pytz','dateutil','dateutil.parser'])"))))))
  (it "imports zoneinfo without the getcwd SecurityException the real module hits"
      (with-python-context (expect (true? (ev python-context
                                              (str "from zoneinfo import ZoneInfo\n"
                                                   "isinstance(str(ZoneInfo('UTC')), str)"))))))
  (it "exposes 300+ IANA zones from java.time"
      (with-python-context
        (expect (true? (ev python-context
                           "import zoneinfo\nlen(zoneinfo.available_timezones()) > 300")))))
  (it "makes `import tzdata` succeed"
      (with-python-context (expect (true? (ev python-context
                                              "import tzdata\nhasattr(tzdata, 'IANA_VERSION')"))))))

(defdescribe
  zoneinfo-dst-test
  (it "computes DST-correct offsets and names (EDT summer, EST winter)"
      (with-python-context
        (expect (true? (ev python-context
                           (str
                             "import datetime as dt\nfrom zoneinfo import ZoneInfo\n"
                             "ny = ZoneInfo('America/New_York')\n"
                             "s = dt.datetime(2021,7,1,12,tzinfo=ny)\n"
                             "w = dt.datetime(2021,1,1,12,tzinfo=ny)\n"
                             "s.utcoffset()==dt.timedelta(hours=-4) and s.tzname()=='EDT' "
                             "and w.utcoffset()==dt.timedelta(hours=-5) and w.tzname()=='EST'"))))))
  (it "converts UTC across a spring-forward boundary via fromutc/astimezone"
      (with-python-context
        (expect (true? (ev python-context
                           (str "import datetime as dt\nfrom zoneinfo import ZoneInfo\n"
                                "ny = ZoneInfo('America/New_York')\n"
                                "u = dt.datetime(2021,3,14,7,0,tzinfo=dt.timezone.utc)\n"
                                "loc = u.astimezone(ny)\n"
                                "loc.hour==3 and loc.utcoffset()==dt.timedelta(hours=-4)"))))))
  (it
    "caches ZoneInfo instances by key (identity) and raises on unknown zones"
    (with-python-context
      (expect
        (true?
          (ev
            python-context
            (str
              "import zoneinfo\nfrom zoneinfo import ZoneInfo\n"
              "ok = ZoneInfo('UTC') is ZoneInfo('UTC')\n" "bad = False\n"
              "try:\n    ZoneInfo('Not/AZone')\nexcept zoneinfo.ZoneInfoNotFoundError:\n    bad = True\n"
              "ok and bad")))))))

(defdescribe
  pytz-test
  (it "localizes a naive datetime to a DST-correct aware one"
      (with-python-context
        (expect
          (true? (ev python-context
                     (str "import datetime as dt, pytz\n" "tz = pytz.timezone('Europe/London')\n"
                          "aw = tz.localize(dt.datetime(2021,7,1,12))\n"
                          "aw.utcoffset()==dt.timedelta(hours=1) and tz.zone=='Europe/London'"))))))
  (it
    "exposes utc plus all_timezones and raises UnknownTimeZoneError"
    (with-python-context
      (expect
        (true?
          (ev
            python-context
            (str
              "import pytz\n"
              "ok = pytz.utc.tzname(None)=='UTC' and len(pytz.all_timezones)>300\n" "bad = False\n"
              "try:\n    pytz.timezone('Bogus/Zone')\nexcept pytz.UnknownTimeZoneError:\n    bad = True\n"
              "ok and bad")))))))

(defdescribe
  dateutil-test
  (it "parses ISO-8601 with a trailing Z into a UTC-aware datetime"
      (with-python-context
        (expect (true? (ev python-context
                           (str "import datetime as dt\nfrom dateutil import parser\n"
                                "parser.parse('2021-07-01T15:30:45Z')"
                                "==dt.datetime(2021,7,1,15,30,45,tzinfo=dt.timezone.utc)"))))))
  (it "parses natural month-name dates with AM/PM and RFC-2822"
      (with-python-context
        (expect (true?
                  (ev python-context
                      (str "import datetime as dt\nfrom dateutil import parser\n"
                           "a = parser.parse('July 1 2021 3:04 PM')==dt.datetime(2021,7,1,15,4)\n"
                           "b = parser.parse('Mon, 02 Jan 2021 15:04:05 +0000')"
                           "==dt.datetime(2021,1,2,15,4,5,tzinfo=dt.timezone.utc)\n" "a and b"))))))
  (it "honours dayfirst for ambiguous slash dates"
      (with-python-context
        (expect (true?
                  (ev python-context
                      (str "import datetime as dt\nfrom dateutil import parser\n"
                           "parser.parse('01/07/2021', dayfirst=True)==dt.datetime(2021,7,1)"))))))
  (it "applies relativedelta month clamping and nth-weekday selection"
      (with-python-context
        (expect
          (true?
            (ev
              python-context
              (str
                "import datetime as dt\n"
                "from dateutil.relativedelta import relativedelta, FR\n"
                "a = dt.datetime(2021,1,31,12)+relativedelta(months=1)==dt.datetime(2021,2,28,12)\n"
                "b = dt.date(2021,7,1)+relativedelta(day=31,weekday=FR(-1))==dt.date(2021,7,30)\n"
                "a and b")))))))
