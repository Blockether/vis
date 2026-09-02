(ns com.blockether.vis.internal.python-host
  "THE door from the sandbox back into Vis, and the only one.

   The embedded interpreter calls out through ONE function it was handed, so
   everything a block can ask the host for arrives here as two strings: the name
   of a tool and a JSON envelope of its arguments. What crosses is DATA - a tool
   takes plain values and answers plain values, because the boundary carries
   text and nothing else. A live object (an interpreter handle, a stream, a file)
   crossed it and never will; a handle the model holds is a PYTHON object built
   over calls that come back through here.

   One interpreter serves every session in this process, so the envelope names
   the session that called (`vis-python-runtime`'s `install`) and this registry
   answers per session: `shell` bound for two workspaces is two different
   functions under one name, and a session that never bound a name gets a
   refusal that says so instead of a neighbour's tool.

   The host is bound ONCE per process. Binding is idempotent because the
   interpreter is: a second `bind!` would replace a live upcall stub while a
   block sits inside it."
  (:require [charred.api :as json]
            [clojure.string :as str]
            [com.blockether.vis.internal.persistance :as persistance]
            [com.blockether.vis-python-runtime :as runtime]
            [taoensso.telemere :as tel]))

(defonce
  ^:private
  ^{:doc
    "session id -> `{python-name fn}`: what THAT session may call back into.

   A process-wide table because the interpreter is process-wide; the session
   key is what keeps two sandboxes from reaching each other's bindings."}
  registry
  (atom {}))

(def ^:private door-session
  "The registry key every session shares, for the SHIM DOORS.

   No session is named this, so nothing but [[install-doors!]] writes it and
   `forget-session!` never takes it away."
  "__vis_doors__")

(defn- host-null-fault
  "The error text for a host tool that hit a Java `NullPointerException`.

   Left raw it reads like the model's OWN Python - a bare `Cannot invoke ...
   because ... is null` naming some private local - and the model retries the
   identical call forever. Say whose bug it is at the boundary, where the two
   sides are still distinguishable."
  [tool ^NullPointerException e]
  (str "internal tool fault: the vis tool `"
       tool
       "` hit a Java "
       "NullPointerException ("
       (.getMessage e)
       ") - a vis engine/tool bug, NOT "
       "your Python. Retrying the same call fails identically; take a different "
       "approach or report it to the user."))

(defn- envelope
  "JSON text of one reply: `{\"value\" ...}` or `{\"error\" ...}`.

   A value the encoder refuses is a FAILURE of that call and nothing else - the
   guest gets a catchable exception naming the tool, while a throw left to
   escape would take the block, the turn and the interpreter's opinion of this
   session with it."
  [tool reply]
  (try (persistance/->json reply)
       (catch Throwable _
         (json/write-json-str {"error" (str "the vis tool `" tool
                                            "` answered a value this boundary "
                                            "cannot carry: only JSON data crosses it")}))))

(defn- failure-data
  "The `ex-info` data of `t` as EDN TEXT the guest carries back untouched, or nil.

   A tool's failure is typed — `:type :vis/tool-failure`, the symbol that failed
   — and that type is what the trailer reads; a message alone would force the
   engine to parse its own prose. Only scalars travel: a value the reader could
   not answer identically is dropped rather than stringified into a lie."
  [t]
  (when (instance? clojure.lang.IExceptionInfo t)
    (let [scalar?
          (fn [v]
            (or (keyword? v) (string? v) (number? v) (boolean? v) (symbol? v)))

          data
          (into {}
                (filter (fn [[k v]]
                          (and (keyword? k) (scalar? v))))
                (ex-data t))]

      (when (seq data) (pr-str data)))))

(defn- answer
  "Run `f` on `args` and answer the reply map the guest reads."
  [tool f args]
  (try {"value" (apply f args)}
       (catch NullPointerException e {"error" (host-null-fault tool e)})
       (catch Throwable t
         (cond-> {"error" (or (ex-message t) (str t))}
           (failure-data t)
           (assoc "error_data" (failure-data t))))))

(defonce
  ^:private
  ^{:doc
    "session id -> the dynamic binding frame of whoever is driving that session.

   A block's driver binds Vars around the call it makes - the attachment sink is
   the loud one - and then the interpreter serves the block on ONE pinned thread
   of its own, while a `par` worker upcalls from a CPython thread that was never
   in Clojure at all. Neither sees the driver's `binding`, so a tool reading a
   dynamic Var would find it unbound and refuse work it was asked for.

   The driver publishes its frame here for the length of the call and
   [[dispatch]] installs it around the tool it serves: the bindings that reach a
   door are the ones of the block that asked, whichever thread the door runs on."}
  frames
  (atom {}))

(defn conveying*
  "Call `f`, conveying THIS thread's dynamic bindings to `session`'s host calls."
  [session f]
  (swap! frames assoc session (clojure.lang.Var/getThreadBindingFrame))
  (try (f) (finally (swap! frames dissoc session))))

(defmacro conveying
  "Evaluate `body`, conveying the current dynamic bindings to `session`'s host calls."
  [session & body]
  `(conveying* ~session
               (fn []
                 ~@body)))

(defn- answer-in-frame
  "[[answer]], under the binding frame the driver of `session` published."
  [session tool f args]
  (if-let [frame (get @frames session)]
    (let [held (clojure.lang.Var/getThreadBindingFrame)]
      (clojure.lang.Var/resetThreadBindingFrame frame)
      (try (answer tool f args) (finally (clojure.lang.Var/resetThreadBindingFrame held))))
    (answer tool f args)))
(defn dispatch
  "Serve one call from the sandbox: WHO called, `payload` in, reply JSON out.

   `caller` is the INTERPRETER's answer — the namespace the call was made from —
   and it is the only thing this authorizes against. The payload also carries a
   session, because the guest's envelope always did, but that field is written
   by the guest: a block that named a neighbour's session used to be served the
   neighbour's tools, with the neighbour's roots (measured: a confined block read
   a file its own policy had refused it one statement earlier). A mismatch is a
   forgery attempt and is recorded as one.

   THE host function itself, kept public so a test can measure the boundary
   without an interpreter. It never throws: a failure is a reply the guest
   raises as `RuntimeError`, because a host that throws here unwinds through the
   interpreter's upcall stub."
  [caller tool payload]
  (let [request
        (json/read-json payload)

        claimed
        (get request "session")

        session
        (if (str/blank? (str caller)) nil caller)

        args
        (vec (get request "args"))

        f
        (or (get-in @registry [session tool]) (get-in @registry [door-session tool]))]

    (when (and claimed session (not= claimed session))
      (tel/log! {:level :warn
                 :id ::session-mismatch
                 :data {:tool tool :caller session :claimed claimed}
                 :msg "a sandbox call named a session other than its own"}))
    (envelope tool
              (if f
                (answer-in-frame session tool f args)
                {"error" (str "no vis tool named `" tool "` in this session")}))))

(defonce ^:private
         ^{:doc "Whether THIS process has already handed the interpreter its host function."} bound
  (atom false))

(defn bind!
  "Make [[dispatch]] the host THIS process's interpreter calls back into, if this
   process has an interpreter to bind.

   Idempotent: the interpreter holds one upcall stub, and rebinding it while a
   block is inside a call would swap the target under a live frame. Answers
   whether a host is bound now.

   Two things this used to get wrong, both measured while loading a Python
   extension in a process that never started a sandbox. It marked itself done
   BEFORE the bind, so a bind that threw left the flag set and every later caller
   skipped it — the first extension in such a process failed to load and the rest
   only appeared to work. And it insisted on the interpreter existing at all:
   binding needs the cdylib resolved, which a process that only talks to the
   extension host has no reason to have fetched. A library that will not resolve
   is not an error here, it is a process with nothing to bind — the extension
   host binds its OWN interpreter in its own process, and the parent binds when
   it builds a sandbox."
  []
  (when-not @bound
    (locking bound
      (when-not @bound
        (try (runtime/bind-host! dispatch)
             (reset! bound true)
             (catch Throwable t
               (tel/log! {:level :debug
                          :id ::no-interpreter-to-bind
                          :data {:error (ex-message t)}}))))))
  @bound)

(defn- install!
  "Register `tools` for `session` and bind each name in the guest with `bind-one`."
  [session tools bind-one]
  (bind!)
  (swap! registry update session merge tools)
  (mapv (fn [[nm _]]
          (bind-one session nm))
        tools))

(defn install-tools!
  "Bind `tools` - `{python-name fn}` - as `session`'s host tools and answer the
   names bound.

   Registration and installation are ONE step on purpose: a name the guest can
   call but the registry does not know is a refusal the model cannot act on."
  [session tools]
  (install! session tools runtime/install-tool!))

(defn install-sync-tools!
  "Bind `tools` as `session`'s host tools that answer DIRECTLY, and answer the
   names bound.

   For Python the host runs, not the model: a sandbox tool hands back a thunk
   the block runner settles, and trusted code - an extension calling
   `vis.shell(...)` in the middle of a line - has no runner and no `await`.
   The installer is a PARAMETER because trusted Python does not run where the
   sandbox does: an extension session binds its names in the unconfined child
   process, a sandbox session in this one, and the registry that answers them is
   the same either way."
  ([session tools] (install-sync-tools! session tools runtime/install-sync-tool!))
  ([session tools install-one] (install! session tools install-one)))

(defn install-doors!
  "Install `tools` into `session` AND register them for every session.

   A shim door is ONE host capability, not a per-session closure, and Python is
   PROCESS state: `nippy` and `ruff` staple themselves onto
   `builtins` and `sys.modules`, so a second session's `import nippy` finds the
   module the FIRST session built - holding proxies that still name it. The
   shared key is what keeps those doors answering after that session is gone."
  [session tools]
  (swap! registry update door-session merge tools)
  (install-sync-tools! session tools))
(defn forget-tools!
  "Drop `names` from `session`'s registry, answering nothing.

   For a tool that exists only for the length of ONE call — a host callback an
   adapter hands INTO Python — the registry is what makes the name callable, so
   forgetting it is what takes the capability back."
  [session names]
  (swap! registry update session #(apply dissoc % names))
  nil)

(defn forget-session!
  "Drop `session`'s bindings, answering how many names went.

   The interpreter's namespace is closed by whoever owns the session; this is
   the host half, and leaving it behind would keep every closure a finished
   session captured."
  [session]
  (let [gone (count (get @registry session))]
    (swap! registry dissoc session)
    gone))
