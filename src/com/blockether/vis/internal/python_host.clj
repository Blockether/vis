(ns com.blockether.vis.internal.python-host
  "THE door from the sandbox back into Vis, and the only one.

   The embedded interpreter calls out through ONE function it was handed, so
   everything a block can ask the host for arrives here as two strings: the name
   of a tool and a JSON envelope of its arguments. What crosses is DATA - a tool
   takes plain values and answers plain values, because the boundary carries
   text and nothing else. A live object (a Context, a stream, a handle) never
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
            [com.blockether.vis.internal.persistance :as persistance]
            [com.blockether.vis-python-runtime :as runtime]))

(defonce
  ^:private
  ^{:doc
    "session id -> `{python-name fn}`: what THAT session may call back into.

   A process-wide table because the interpreter is process-wide; the session
   key is what keeps two sandboxes from reaching each other's bindings."}
  registry
  (atom {}))

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

(defn- answer
  "Run `f` on `args` and answer the reply map the guest reads."
  [tool f args]
  (try {"value" (apply f args)}
       (catch NullPointerException e {"error" (host-null-fault tool e)})
       (catch Throwable t {"error" (or (ex-message t) (str t))})))

(defn dispatch
  "Serve one call from the sandbox: `payload` in, reply JSON out.

   THE host function itself, kept public so a test can measure the boundary
   without an interpreter. It never throws: a failure is a reply the guest
   raises as `RuntimeError`, because a host that throws here unwinds through the
   interpreter's upcall stub."
  [tool payload]
  (let [request
        (json/read-json payload)

        session
        (get request "session")

        args
        (vec (get request "args"))

        f
        (get-in @registry [session tool])]

    (envelope
      tool
      (if f (answer tool f args) {"error" (str "no vis tool named `" tool "` in this session")}))))

(defonce ^:private
         ^{:doc "Whether THIS process has already handed the interpreter its host function."} bound
  (atom false))

(defn bind!
  "Make [[dispatch]] the host this process's interpreter calls back into.

   Idempotent: the interpreter holds one upcall stub, and rebinding it while a
   block is inside a call would swap the target under a live frame."
  []
  (when (compare-and-set! bound false true) (runtime/bind-host! dispatch))
  nil)

(defn install-tools!
  "Bind `tools` - `{python-name fn}` - as `session`'s host tools and answer the
   names bound.

   Registration and installation are ONE step on purpose: a name the guest can
   call but the registry does not know is a refusal the model cannot act on."
  [session tools]
  (bind!)
  (swap! registry update session merge tools)
  (mapv (fn [[nm _]]
          (runtime/install-tool! session nm))
        tools))

(defn forget-session!
  "Drop `session`'s bindings, answering how many names went.

   The interpreter's namespace is closed by whoever owns the session; this is
   the host half, and leaving it behind would keep every closure a finished
   session captured."
  [session]
  (let [gone (count (get @registry session))]
    (swap! registry dissoc session)
    gone))
