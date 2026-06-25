(ns com.blockether.vis.ext.language-python.repl-manager
  "A MANAGED Python REPL: a persistent interpreter subprocess running a tiny
   line-framed eval server — one JSON request per line in, one JSON response per
   line out. Globals persist across evals (real REPL state). One process per dir;
   the `Process` handle is cached so teardown is clean."
  (:require [charred.api :as json]
            [clojure.java.io :as io]
            [com.blockether.vis.ext.language-python.interpreter :as interp])
  (:import [java.io BufferedReader BufferedWriter]))

;; The eval server. Reads a JSON object per stdin line: {"code": "..."} (or
;; {"op": "ping"}). Runs it in a persistent namespace with REPL semantics — the
;; LAST top-level expression's value is captured (repr'd) — capturing stdout /
;; stderr, and replies with one JSON line: {ok, out, err, value, exc}.
(def ^:private server-script
  "import sys, json, io, ast, contextlib, traceback
_G = {'__name__': '__vis_repl__'}
def _run(code):
    out = io.StringIO(); err = io.StringIO()
    value = None; ok = True; exc = None
    try:
        with contextlib.redirect_stdout(out), contextlib.redirect_stderr(err):
            block = ast.parse(code, mode='exec')
            body = block.body
            if body and isinstance(body[-1], ast.Expr):
                pre = ast.Module(body[:-1], [])
                last = ast.Expression(body[-1].value)
                exec(compile(pre, '<repl>', 'exec'), _G)
                value = eval(compile(last, '<repl>', 'eval'), _G)
            else:
                exec(compile(block, '<repl>', 'exec'), _G)
    except BaseException:
        ok = False; exc = traceback.format_exc()
    return {'ok': ok, 'out': out.getvalue(), 'err': err.getvalue(),
            'value': (repr(value) if value is not None else None), 'exc': exc}
def _main():
    for line in sys.stdin:
        line = line.strip()
        if not line:
            continue
        try:
            req = json.loads(line)
        except Exception:
            sys.stdout.write(json.dumps({'ok': False, 'exc': 'bad request'}) + '\\n'); sys.stdout.flush(); continue
        res = {'ok': True, 'pong': True} if req.get('op') == 'ping' else _run(req.get('code', ''))
        sys.stdout.write(json.dumps(res) + '\\n'); sys.stdout.flush()
_main()
")

;; dir -> {:process ^Process :writer :reader :cmd :pid :started-at}
(defonce ^:private processes (atom {}))

(defn- alive? [info] (boolean (some-> ^Process (:process info) .isAlive)))

(defn start!
  "Spawn (or replace) the managed Python REPL for `dir`. Returns a status map."
  [dir _opts]
  (when-let [old (get @processes dir)]
    (try (.destroy ^Process (:process old)) (catch Throwable _ nil)))
  (let [cmd (vec (concat (interp/resolve-command dir) ["-u" "-c" server-script]))
        pb  (doto (ProcessBuilder. ^java.util.List cmd)
              (.directory (io/file dir))
              (.redirectErrorStream false))
        p   (.start pb)
        info {:process p
              :writer  (io/writer (.getOutputStream p))
              :reader  (io/reader (.getInputStream p))
              :cmd     cmd
              :pid     (.pid p)
              :started-at (System/currentTimeMillis)}]
    (swap! processes assoc dir info)
    {:status :up :pid (.pid p) :cmd cmd :dir dir}))

(defn- request!
  [dir req timeout-ms]
  (let [info (get @processes dir)]
    (when-not (alive? info)
      (throw (ex-info "Python REPL is not running for this dir — repl_start(\"python\") first."
               {:type :py/no-repl :dir dir})))
    (locking info
      (let [^BufferedWriter w (:writer info)
            ^BufferedReader r (:reader info)]
        (.write w (str (json/write-json-str req) "\n"))
        (.flush w)
        (let [fut  (future (.readLine r))
              line (deref fut timeout-ms ::timeout)]
          (if (= line ::timeout)
            (do (future-cancel fut)
              (throw (ex-info "Python eval timed out" {:type :py/timeout :dir dir})))
            (if (nil? line)
              (throw (ex-info "Python REPL closed the connection (process died)"
                       {:type :py/closed :dir dir}))
              (json/read-json line))))))))

(defn eval!
  "Evaluate `code` in the REPL for `dir`. Returns
   {\"ok\" \"out\" \"err\" \"value\" \"exc\"}."
  [dir code timeout-ms]
  (request! dir {"code" (str code)} (or timeout-ms 30000)))

(defn stop! [dir]
  (when-let [info (get @processes dir)]
    (try (.destroy ^Process (:process info)) (catch Throwable _ nil))
    (try (when (.isAlive ^Process (:process info)) (.destroyForcibly ^Process (:process info)))
      (catch Throwable _ nil)))
  (swap! processes dissoc dir)
  {:status :stopped :dir dir})

(defn status [dir]
  (let [info (get @processes dir)]
    {:dir dir
     :status (if (alive? info) :up :down)
     :pid (some-> ^Process (:process info) .pid)
     :cmd (:cmd info)}))
