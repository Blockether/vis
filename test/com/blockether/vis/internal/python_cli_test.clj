(ns com.blockether.vis.internal.python-cli-test
  "End-to-end cover for the `vis python` standalone interpreter helpers
   (`python-cli-context` / `run-python-source!`). Drives the SAME
   `env/*` machinery the native binary runs, so these assertions hold on
   both the JVM and the native image. Boots ONE no-network sandbox for the
   ns (context creation is expensive) and captures the real-terminal
   output by rebinding `config/original-stdout`."
  (:require [com.blockether.vis.internal.config :as config]
            [com.blockether.vis.internal.main]
            [lazytest.core :refer [defdescribe expect it]]))

(def ^:private python-cli-context #'com.blockether.vis.internal.main/python-cli-context)

(def ^:private run-python-source! #'com.blockether.vis.internal.main/run-python-source!)

(defn- run-src
  "Run one Python block through the CLI helper, capturing the terminal
   output. Returns {:exit code :out captured-stdout}."
  [ctx code]
  (let [baos
        (java.io.ByteArrayOutputStream.)

        ps
        (java.io.PrintStream. baos true "UTF-8")]

    (with-redefs [config/original-stdout ps]
      (let [exit (run-python-source! ctx code)]
        {:exit exit :out (.toString baos "UTF-8")}))))

(defdescribe
  python-cli-test
  (let [ctx (python-cli-context {:network? false})]
    (it "runs a basic print block: exit 0, output surfaces"
        (let [{:keys [exit out]} (run-src ctx "print('hi', 1 + 1)")]
          (expect (= 0 exit))
          (expect (re-find #"hi 2" out))))
    (it "a bare trailing expression does NOT echo (agent-sandbox semantics)"
        (let [{:keys [exit out]} (run-src ctx "40 + 2")]
          (expect (= 0 exit))
          (expect (not (re-find #"42" out)))))
    (it "a raised exception renders the error and exits 1"
        (let [{:keys [exit out]} (run-src ctx "raise ValueError('boom')")]
          (expect (= 1 exit))
          (expect (re-find #"boom" out))))
    (it "state persists across blocks in the same context"
        (run-src ctx "carry = 7")
        (let [{:keys [exit out]} (run-src ctx "print('carry', carry + 1)")]
          (expect (= 0 exit))
          (expect (re-find #"carry 8" out))))
    (it "numpy shim computes"
        (let [{:keys [exit out]}
              (run-src ctx "import numpy as np\nprint('np', int(np.arange(5).sum()))")]
          (expect (= 0 exit))
          (expect (re-find #"np 10" out))))
    (it "pandas shim computes"
        (let [{:keys [exit out]}
              (run-src ctx
                       (str "import pandas as pd\n"
                            "print('pd', int(pd.DataFrame({'a': [1, 2, 3]})['a'].sum()))"))]
          (expect (= 0 exit))
          (expect (re-find #"pd 6" out))))
    (it "sqlite3 shim roundtrips"
        (let [{:keys [exit out]}
              (run-src ctx
                       (str "import sqlite3\n"
                            "c = sqlite3.connect(':memory:')\n"
                            "c.execute('create table t(n int)')\n"
                            "c.executemany('insert into t values (?)', [(3,), (4,)])\n"
                            "print('sql', c.execute('select sum(n) from t').fetchone()[0])"))]
          (expect (= 0 exit))
          (expect (re-find #"sql 7" out))))
    (it "yaml shim parses"
        (let [{:keys [exit out]} (run-src ctx
                                          (str "import yaml\n"
                                               "d = yaml.safe_load('a: 1\\nb: [2, 3]')\n"
                                               "print('yaml', d['a'], d['b'][1])"))]
          (expect (= 0 exit))
          (expect (re-find #"yaml 1 3" out))))
    (it "http-client shims import clean"
        (let [{:keys [exit out]} (run-src ctx
                                          (str "import requests, httpx, bs4, toml, tabulate\n"
                                               "print('imports ok')"))]
          (expect (= 0 exit))
          (expect (re-find #"imports ok" out))))
    (it "a no-network context blocks socket name resolution"
        (let [{:keys [exit out]} (run-src ctx
                                          (str "import socket\n" "try:\n"
                                               "    socket.gethostbyname('example.com')\n"
                                               "    print('resolved')\n"
                                               "except Exception:\n" "    print('blocked')"))]
          (expect (= 0 exit))
          (expect (re-find #"blocked" out))))
    (it "a network-enabled context builds without error"
        (expect (some? (python-cli-context {:network? true}))))))
