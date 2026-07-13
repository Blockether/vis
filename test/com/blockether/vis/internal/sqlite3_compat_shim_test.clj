(ns com.blockether.vis.internal.sqlite3-compat-shim-test
  "The sqlite3-compat shim installed into every sandbox context via the generic
   sandbox-shim mechanism: a DB-API 2.0 `sqlite3` module published into
   `sys.modules`, backed by the JVM xerial sqlite-jdbc driver (GraalPy ships no
   `_sqlite3` native extension). Connections live host-side by integer handle."
  (:require [com.blockether.vis.internal.env-python :as ep]
            [lazytest.core :refer [defdescribe expect it]])
  (:import [org.graalvm.polyglot Context]))

(defn- ev [^Context c code] (ep/->clj (.eval c "python" code)))

(defmacro with-python-context
  [& body]
  `(let [~'python-context (:python-context (ep/create-python-context {}))]
     (try ~@body (finally (.close ^Context ~'python-context)))))

(defdescribe
  sqlite3-module-test
  (it "publishes sqlite3 under sys.modules and reports DB-API metadata"
      (with-python-context
        (expect (true? (ev python-context
                           (str "import sys, sqlite3\n" "sqlite3 is sys.modules['sqlite3'] "
                                "and sqlite3.apilevel=='2.0' and sqlite3.paramstyle=='qmark' "
                                "and sqlite3.sqlite_version_info[0]==3"))))))
  (it "works with no import (stapled onto builtins)"
      (with-python-context (expect (= 1
                                      (ev python-context
                                          (str "c=sqlite3.connect(':memory:')\n"
                                               "c.execute('create table t(x)').connection is c\n"
                                               "cur=c.execute('insert into t values (1)')\n"
                                               "c.execute('select x from t').fetchone()[0]")))))))

(defdescribe
  sqlite3-crud-test
  (it
    "round-trips DDL/DML and orders a SELECT with qmark params"
    (with-python-context
      (expect
        (=
          [[2 "bob" 25] [1 "alice" 30]]
          (ev
            python-context
            (str
              "import sqlite3\nc=sqlite3.connect(':memory:')\n"
              "c.execute('create table u(id integer primary key, name text, age integer)')\n"
              "c.execute('insert into u(name,age) values (?,?)', ('alice',30))\n"
              "c.execute('insert into u(name,age) values (?,?)', ('bob',25))\n"
              "[list(r) for r in c.execute('select id,name,age from u order by age').fetchall()]"))))))
  (it
    "supports executemany with named params and aggregates"
    (with-python-context
      (expect
        (=
          [["old" 2] ["young" 2]]
          (ev
            python-context
            (str
              "import sqlite3\nc=sqlite3.connect(':memory:')\n"
              "c.execute('create table u(name text, age integer)')\n"
              "c.executemany('insert into u values (:n,:a)', "
              "[{'n':'a','a':40},{'n':'b','a':22},{'n':'c','a':30},{'n':'d','a':25}])\n"
              "q='select case when age>=30 then :hi else :lo end g, count(*) c from u group by g order by g'\n"
              "[list(r) for r in c.execute(q, {'hi':'old','lo':'young'}).fetchall()]"))))))
  (it "reports lastrowid after INSERT and total_changes"
      (with-python-context
        (expect
          (= [3 3]
             (ev python-context
                 (str "import sqlite3\nc=sqlite3.connect(':memory:')\n"
                      "c.execute('create table u(id integer primary key, n text)')\n"
                      "cur=c.cursor()\n"
                      "for x in ['a','b','c']: cur.execute('insert into u(n) values (?)', (x,))\n"
                      "[cur.lastrowid, c.total_changes]")))))))

(defdescribe
  sqlite3-row-and-errors-test
  (it "exposes sqlite3.Row with by-name and by-index access"
      (with-python-context (expect
                             (= ["alice" 30 ["name" "age"]]
                                (ev python-context
                                    (str "import sqlite3\nc=sqlite3.connect(':memory:')\n"
                                         "c.row_factory=sqlite3.Row\n"
                                         "c.execute('create table u(name text, age integer)')\n"
                                         "c.execute('insert into u values (?,?)', ('alice',30))\n"
                                         "r=c.execute('select name,age from u').fetchone()\n"
                                         "[r['name'], r[1], list(r.keys())]"))))))
  (it "raises IntegrityError on a constraint violation"
      (with-python-context
        (expect (true? (ev python-context
                           (str "import sqlite3\nc=sqlite3.connect(':memory:')\n"
                                "c.execute('create table u(id integer primary key)')\n"
                                "c.execute('insert into u values (1)')\n"
                                "ok=False\n" "try:\n c.execute('insert into u values (1)')\n"
                                "except sqlite3.IntegrityError: ok=True\n" "ok"))))))
  (it "raises OperationalError on a bad statement"
      (with-python-context
        (expect (true? (ev python-context
                           (str "import sqlite3\nc=sqlite3.connect(':memory:')\n"
                                "ok=False\n" "try:\n c.execute('select * from missing')\n"
                                "except sqlite3.OperationalError: ok=True\n" "ok"))))))
  (it "round-trips a BLOB as Python bytes"
      (with-python-context (expect (true? (ev python-context
                                              (str
                                                "import sqlite3\nc=sqlite3.connect(':memory:')\n"
                                                "c.execute('create table b(d blob)')\n"
                                                "raw=bytes([0,1,2,255,7])\n"
                                                "c.execute('insert into b values (?)', (raw,))\n"
                                                "got=c.execute('select d from b').fetchone()[0]\n"
                                                "isinstance(got, bytes) and got==raw")))))))

(defdescribe
  sqlite3-context-and-script-test
  (it "commits on a clean `with` block and iterates a cursor"
      (with-python-context
        (expect (= 10
                   (ev python-context
                       (str "import sqlite3\n" "with sqlite3.connect(':memory:') as c:\n"
                            " c.execute('create table t(x integer)')\n"
                            " c.executemany('insert into t values (?)', [(i,) for i in range(5)])\n"
                            " total=sum(row[0] for row in c.execute('select x from t'))\n"
                            "total"))))))
  (it
    "runs executescript with multiple statements"
    (with-python-context
      (expect
        (=
          3
          (ev
            python-context
            (str
              "import sqlite3\nc=sqlite3.connect(':memory:')\n"
              "c.executescript('create table a(x); insert into a values (1); insert into a values (2);')\n"
              "c.execute('select sum(x) from a').fetchone()[0]")))))))
