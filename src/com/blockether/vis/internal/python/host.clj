(ns com.blockether.vis.internal.python.host
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
            [com.blockether.vis.internal.config.runtime-settings :as rt]
            [com.blockether.vis.internal.persistance.codec :as codec]
            [com.blockether.vis-python-runtime :as runtime]
            [com.blockether.vis.internal.task :as task]
            [taoensso.telemere :as tel]))

(def ^:dynamic *guest-thread?*
  "True while the host serves a call from a thread that the guest created.
   Such a call has no active extension call, so its doors can limit what it does."
  false)

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
  (try (codec/->json reply)
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
           (:vis/task-cancelled (ex-data t))
           (assoc "cancelled" true)

           (failure-data t)
           (assoc "error_data" (failure-data t))))))

(defonce ^:private frames (atom {}))

(def ^:dynamic *driver-call* nil)
(def ^:dynamic *caller* nil)
(def ^:dynamic *callback-call* nil)
(def ^:dynamic *task-origin* nil)
(def ^:dynamic *hold-task-activity* nil)

(defn conveying*
  "Publish this invocation's bindings for callbacks on another thread."
  [session call f]
  (let [key [session call]]
    (binding [*driver-call* key]
      (swap! frames assoc key (clojure.lang.Var/getThreadBindingFrame))
      (try (f) (finally (swap! frames dissoc key))))))

(defn active-call? [session call] (contains? @frames [session call]))

(defmacro conveying
  [session & body]
  `(conveying* ~session (task/call-id) (fn [] ~@body)))

(def ^:dynamic *door*
  "`[session thread]` while `thread` serves a host call that the driver of `session`
   waits for, else nil. The thread is part of the value because a `future` copies the
   bindings, and its later work is not that host call."
  nil)

(defn serving?
  "True when this thread serves a host call that the driver of `session` waits for.
   A driver call into `session` from here would wait for itself."
  [session]
  (let [[served thread] *door*]
    (and (some? served) (= served session) (identical? thread (Thread/currentThread)))))

(defn- answer-in-frame
  "[[answer]], under the binding frame the driver of `session` published.
   [[*guest-thread?*]] stays from the serving thread, because the driver's frame
   belongs to another call."
  [session call tool f args]
  (if-let [frame (get @frames [session call])]
    (let [held (clojure.lang.Var/getThreadBindingFrame)
          guest-thread? *guest-thread?*]

      (clojure.lang.Var/resetThreadBindingFrame frame)
      (try (binding [*guest-thread?* guest-thread?
                     ;; A thread that the guest created is not the call that the driver waits for.
                     *door* (when-not guest-thread? [session (Thread/currentThread)])
                     *caller* [session call]]

             (answer tool f args))
           (finally (clojure.lang.Var/resetThreadBindingFrame held))))
    (binding [*caller* [session call]] (answer tool f args))))

(defn- guest-records
  "`x` with each record that the guest sent read as a plain map of its fields.

   The guest sends a record as `{\"__vis_object__\" name \"__vis_attrs__\" fields}`.
   A Clojure tool reads only the fields, as before. The class name stays as
   `::record` metadata, so an extension worker gets it back (#427)."
  [x]
  (cond (map? x) (let [record-name
                       (get x "__vis_object__")

                       fields
                       (get x "__vis_attrs__")]

                   (if (and (= 2 (count x)) (string? record-name) (map? fields))
                     (with-meta (update-vals fields guest-records) {::record record-name})
                     (update-vals x guest-records)))
        (sequential? x) (mapv guest-records x)
        :else x))

(defn call-args
  "The argument vector a tool fn takes for Python `args` and `kwargs`.

   Clojure tools consume options maps; Python callables recover the marked
   kwargs before invocation. User positional maps never carry this metadata."
  [args kwargs]
  (cond-> (vec args)
    (seq kwargs)
    (conj (with-meta kwargs {::keyword-arguments true}))))

(defonce ^:private task-sessions (atom {}))

(defn own-tasks! [context session]
  (when session (swap! task-sessions assoc context (str session)))
  nil)

(defn task-session [context] (get @task-sessions context context))

(defonce ^:private task-tools (atom {}))

(defn mark-task-tools!
  "Mark the sandbox bindings owned by a Python extension."
  [session entries]
  (swap! task-tools update session
         (fn [held]
           (reduce (fn [m {:keys [name task? detachable?]}]
                     (if task? (assoc m name {:detachable? (not= false detachable?)}) (dissoc m name)))
                   held entries))))

(defn- task-outcome [reply]
  (cond (get reply "cancelled") {:state :cancelled}
        (contains? reply "error") {:state :failed :error (get reply "error")
                                   :error-data (get reply "error_data")}
        :else {:state :done :value (get reply "value")}))

(defn task-reply
  "Encode the outcome shared by guest waits and the SDK shell door."
  [result]
  (cond-> {"state" (name (:state result))}
    (contains? result :value) (assoc "value" (:value result))
    (:message result) (assoc "message" (:message result))
    (:cancelling result) (assoc "cancelling" (:cancelling result))
    (:error result) (assoc "error" (:error result))
    (:error-data result) (assoc "error_data" (:error-data result))))

(defn- task-call [session call tool f request]
  (let [[arg x] (get request "args")
        sid (task-session session)]
    (cond
      (= tool "_task_start")
      (if-let [tool-spec (and f (get-in @task-tools [session arg]))]
        (let [args (call-args (guest-records (get request "tool_args"))
                              (guest-records (get request "kwargs")))
              row (task/start! sid (assoc tool-spec :kind :tool
                                           :name (str arg (get request "call_display" "(...)"))
                                           :origin *task-origin* :hold-activity *hold-task-activity*
                                            :cancel-on-request? (= "shell" arg))
                               #(binding [rt/*blocking-wall-park* nil
                                          rt/*blocking-wall-hold* nil]
                                  (task-outcome (answer arg f args))))]
          {"id" (:id row) "name" (:name row)})
        (throw (ex-info (str "no Task tool named `" arg "` in this session") {})))

      (= tool "_task_list")
      (into [] (keep #(task/guest-task sid (:id %))) (task/tasks sid))

      :else
      (let [owned? (task/guest-task sid arg)]
        (case tool
          "_task_await"
          (let [token (when-let [token (get x "token")] [call token])
                opts {:block call :token token :timeout-ms (min 250 (long (get x "timeout_ms" 0)))
                      :leave? (true? (get x "leave"))
                      :observation? (true? (get x "observation"))
                      :hold rt/*blocking-wall-hold*}]
            (if-let [result (when owned? (task/await! sid arg opts))]
              (task-reply result)
              {"state" "missing"}))
          "_task_ack" (when owned? (task/acknowledge! sid arg))
          "_task_cancel" (boolean (and owned? (task/cancel! sid arg :agent :cancel x)))
          "_task_state" (if-let [row (when owned? (task/state sid arg))] (name (:state row)) "missing")
          "_task_cancelling" (if owned? (task/cancelling sid arg) 0)
          "_task_uncancel" (when owned? (task/uncancel! sid arg))
          "_task_rename" (when owned? (task/rename! sid arg (str x))))))))

(def ^:private task-ops
  #{"_task_start" "_task_await" "_task_ack" "_task_cancel" "_task_state" "_task_cancelling"
    "_task_uncancel" "_task_rename" "_task_list"})

(defn- dispatch-call
  [caller call tool payload]
  (let [request
        (json/read-json payload)

        claimed
        (get request "session")

        session
        (if (str/blank? (str caller)) nil caller)

        args
        (call-args (guest-records (get request "args")) (guest-records (get request "kwargs")))

        task-op? (and session (not *guest-thread?*) (contains? task-ops tool))
        start-tool (first (get request "args"))
        f (if task-op?
            (fn [] (task-call session call tool (get-in @registry [session start-tool]) request))
            (or (get-in @registry [session tool]) (get-in @registry [door-session tool])))]

    (when (and claimed session (not= claimed session))
      ;; A stale envelope never selects another session's closure or permissions.
      ;; Only the connection-bound caller decides which registered tool can run.
      (tel/log! {:level (if f :debug :warn)
                 :id ::session-mismatch
                 :data {:tool tool :caller session :claimed claimed :served? (some? f)}
                 :msg "a sandbox call named a session other than its own"}))
    (envelope tool
              (if f
                (answer-in-frame session call tool f (when-not task-op? args))
                {"error" (str "no vis tool named `" tool "` in this session")}))))

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
  ([caller tool payload]
   (let [[driver call] *driver-call*]
     (dispatch-call caller (or *callback-call* (when (= caller driver) call)) tool payload)))
  ([caller call tool payload]
   (dispatch-call caller call tool payload)))

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
        (try (runtime/bind-host-with-call! dispatch)
             (reset! bound true)
             (catch Throwable t
               (tel/log!
                 {:level :debug :id ::no-interpreter-to-bind :data {:error (ex-message t)}}))))))
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
   call but the registry does not know is a refusal the model cannot act on.

   `bind-one` says WHERE the guest half is bound: this process's interpreter by
   default, or a session's own worker when the caller hands one in. The registry
   half is the parent's either way — a worker calls back here."
  ([session tools] (install-tools! session tools runtime/install-tool!))
  ([session tools bind-one] (install! session tools bind-one)))

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
   PROCESS state: `attach` and `ls` staple their entry points onto `builtins`, so
   a second session can find names the FIRST session built — holding proxies that
   still name it. The shared key is what keeps those doors answering after that
   session is gone."
  ([session tools] (install-doors! session tools runtime/install-sync-tool!))
  ([session tools install-one]
   (swap! registry update door-session merge tools)
   (install-sync-tools! session tools install-one)))

(defn forget-tools!
  "Drop `names` from `session`'s registry, answering nothing.

   For a tool that exists only for the length of ONE call — a host callback an
   adapter hands INTO Python — the registry is what makes the name callable, so
   forgetting it is what takes the capability back."
  [session names]
  (swap! registry update session #(apply dissoc % names))
  (swap! task-tools update session #(apply dissoc % names))
  nil)

(defn forget-session!
  "Drop `session`'s bindings, answering how many names went.

   The interpreter's namespace is closed by whoever owns the session; this is
   the host half, and leaving it behind would keep every closure a finished
   session captured."
  [session]
  (let [gone (count (get @registry session))]
    (swap! registry dissoc session)
    (swap! task-tools dissoc session)
    (swap! task-sessions dissoc session)
    gone))
