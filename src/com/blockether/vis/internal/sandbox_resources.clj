(ns com.blockether.vis.internal.sandbox-resources
  "Host objects a sandbox shim lends to the guest, and the lifetime they die on.

   A shim cannot hand the guest the real thing. The sandbox runs with
   `allowAllAccess false` and no host-access grant, so guest Python may never
   hold or call a JVM object — the model would reach straight through it. What
   the guest gets is an INTEGER HANDLE: a name for a thing it can ask about but
   never touch.

   That leaves the thing itself on the host, and something has to own it. Every
   shim used to answer that question for itself, with its own `(atom {})`, its
   own counter and its own close path, so \"who frees this, and when?\" was
   re-decided per shim and got a different answer each time — usually \"the guest
   calls close(), and if it doesn't, nobody\". A model that hit an error, or a
   session that was cancelled, then left a decoded image (an `int[]` of w*h*4:
   48 MB for one 4000x3000 frame), a JDBC connection or an SSH socket alive until
   the PROCESS died.

   Nor can the guest be trusted to be the owner even in principle. On CPython the
   object would die with the scope that held it, because CPython refcounts.
   GraalPy does not: a file dropped without `with open(...)` is closed whenever
   the JVM GC gets there (see `bin/vis-agent`), so guest-side ownership trades a
   bounded memory leak for an unbounded descriptor leak — and this codebase has
   already been wedged by EMFILE once.

   So ownership lives here, and a shim DECLARES it rather than implementing it:

       :shim/resources
       {::conns {:resource/label   \"sqlite connection\"
                 :resource/release (fn [_h ^Connection c] (.close c))
                 :resource/max     64}}

   `install-sandbox-shims!` reads that declaration and wires it. The shim then
   uses [[open!]] / [[value]] / [[close!]] and never keeps a registry of its own,
   which is the point: there is no per-shim bookkeeping left to get wrong, and
   nothing to remember at teardown.

   A handle is owned by a SCOPE — the Context whose guest asked for it. The scope
   id is `System/identityHashCode` of that Context: an INT, never the Context.
   That is load-bearing, not tidiness. A strong reference to a Context in a
   process-global map would pin it, and a pinned Context is exactly the leak
   `env-python/new-engine!` exists to prevent, since an Engine retains every
   Context ever built on it."
  (:require [taoensso.telemere :as tel])
  (:import (org.graalvm.polyglot Context Engine)))

(defonce ^:private kinds
  ;; kind -> {:resource/release :resource/label :resource/max}. Declared by the
  ;; engine from `:shim/resources`, never by a shim calling in.
  (atom {}))

(defonce ^:private tables
  ;; kind -> {handle value}
  (atom {}))

(defonce ^:private counters
  ;; kind -> last handle. Monotonic, so the smallest key IS the oldest entry.
  (atom {}))

(defonce ^:private owned
  ;; scope -> {kind #{handle}}. Nothing in here is a Context.
  (atom {}))

(defonce ^:private owners
  ;; kind -> {handle scope}. The reverse of `owned`, so freeing a handle never
  ;; needs the caller to remember whose it was: a shim's `close()` op takes the
  ;; handle the guest gave it and nothing else.
  (atom {}))

(defn declare-kinds!
  "Register `resources` (a shim's `:shim/resources` map) so its kinds can be
   opened and released. Idempotent: shims are installed once per Context, and a
   re-declaration of the same kind is the same declaration."
  [resources]
  (when (seq resources) (swap! kinds merge resources))
  nil)

(defn declared?
  "Is `kind` declared? False means a shim called [[open!]] for something it never
   declared — a leak, since nothing would know how to free it."
  [kind]
  (contains? @kinds kind))

(defn scope-of
  "Scope id of `ctx` — its identity hash, never the Context itself (see the ns
   docstring). nil ctx → nil, which owns nothing."
  [ctx]
  (when ctx (System/identityHashCode ctx)))

(defn value
  "The live value behind `handle`, or nil once it is gone."
  [kind handle]
  (get (get @tables kind) (long handle)))

(defn update!
  "Apply `f` to the value behind `handle` in place. No-op once it is gone, so a
   late write from a half-finished op cannot resurrect a freed entry."
  [kind handle f & args]
  (swap! tables update
    kind
    (fn [t]
      (if (contains? t (long handle)) (apply update t (long handle) f args) t)))
  nil)

(defn- release!
  "Run `kind`'s declared release for one entry. Best-effort by construction: one
   bad release must never abort a teardown, or everything after it leaks."
  [kind handle v]
  (when-let [f (:resource/release (get @kinds kind))]
    (try (f handle v)
         (catch Throwable t
           (tel/log! {:level :warn :id ::release-failed :data {:kind kind :handle handle} :error t}
                     (str "sandbox resource " kind " failed to release"))))))

(defn- forget!
  "Drop `handle` from its table and from its owner's set, returning the value it
   held. The owner is looked up, never passed in."
  [kind handle]
  (let [h
        (long handle)

        v
        (value kind h)

        scope
        (get-in @owners [kind h])]

    (swap! tables update kind dissoc h)
    (swap! owners update kind dissoc h)
    (when scope (swap! owned update-in [scope kind] disj h))
    v))

(defn close!
  "Release ONE handle and forget it. Idempotent and silent for a handle that is
   already gone, because it is reached from both the guest's own `close()` and
   from teardown."
  [kind handle]
  (when-let [v (forget! kind handle)]
    (release! kind handle v))
  nil)

(defn- evict-oldest!
  "Enforce `:resource/max` before minting a new handle: release the oldest entry
   so a runaway guest inside ONE session cannot grow a kind without limit. The
   cap is a backstop; the SCOPE is what ties a lifetime to a lifetime."
  [kind]
  (when-let [cap (:resource/max (get @kinds kind))]
    (let [t (get @tables kind)]
      (when (>= (count t) (long cap))
        (let [oldest (first (sort (keys t)))]
          (when-let [v (forget! kind oldest)]
            (release! kind oldest v))
          (tel/log! {:level :debug :id ::evicted-oldest :data {:kind kind :handle oldest :cap cap}}
                    (str "sandbox resource " kind " hit its cap; released the oldest")))))))

(defn open!
  "Take `v` under `kind`, own it for `scope`, and return the guest's handle.

   A nil scope owns nothing — that is the shim-trigger probe, which is
   process-wide and closes its own throwaway Context immediately."
  [scope kind v]
  (when-not (declared? kind)
    (throw (ex-info (str "sandbox resource " kind
                         " was opened but never declared -"
                         " add it to the shim's :shim/resources so teardown knows how to free it")
                    {:type ::undeclared-resource :kind kind})))
  (evict-oldest! kind)
  (let [h (long (get (swap! counters update kind (fnil inc 0)) kind))]
    (swap! tables update kind assoc h v)
    (when scope
      (swap! owners update kind assoc h scope)
      (swap! owned update-in [scope kind] (fnil conj #{}) h))
    h))

(defn release-scope!
  "Release every handle `ctx`'s guest still holds, then drop the scope.

   Called from `dispose-environment!` (a session) and `close-quietly!` (an
   extension context), BEFORE the Context closes so a release still has a live
   handle to work with."
  [ctx]
  (when-let [scope (scope-of ctx)]
    (let [held (get @owned scope)]
      (swap! owned dissoc scope)
      (doseq [[kind handles] held
              h handles]

        (when-let [v (forget! kind h)]
          (release! kind h v)))))
  nil)

(defn dispose!
  "Tear ONE sandbox down completely, in the only order that works:

     1. hand back the host objects its guest still holds ([[release-scope!]]),
        while the handles are still live
     2. close the Context — which is what reaps GraalPy's per-context action
        threads. Leave them and the sandbox becomes IMMORTAL: a running thread
        is a GC root, so it pins the Context, its Engine and the whole Python
        heap, and no amount of collecting gets them back
     3. close the Engine — which is what returns that heap. An Engine retains
        everything ever built on it, so closing only the Context frees nothing

  Miss any step and you lose a different thing, which is why this exists once
  rather than being spelled out at each teardown. Accepts either the map a
  `create-python-context` returns or a bare Context (its Engine is derived).
  Best-effort throughout: teardown must never throw."
  [sandbox]
  (when sandbox
    (let [ctx
          (if (map? sandbox) (:python-context sandbox) sandbox)

          engine
          (if (map? sandbox)
            (:python-engine sandbox)
            (try (.getEngine ^Context sandbox) (catch Throwable _ nil)))]

      (when ctx
        (try (release-scope! ctx) (catch Throwable _ nil))
        (try (.close ^Context ctx true) (catch Throwable _ nil)))
      (when engine (try (.close ^Engine engine true) (catch Throwable _ nil)))))
  nil)

(defmacro with-sandbox
  "Bind `sym` to what `create-expr` returns and [[dispose!]] it when the body
   leaves, however it leaves. For a sandbox whose life IS this block: a test
   runner, a one-shot probe, a project-metadata read."
  [[sym create-expr] & body]
  `(let [~sym ~create-expr]
     (try ~@body (finally (dispose! ~sym)))))

(defmacro keeping-sandbox
  "Bind `sym` to what `create-expr` returns, and [[dispose!]] it ONLY if the body
   throws. On success the body's value owns it.

   For the other half of the problem: a sandbox meant to OUTLIVE the block that
   builds it — a session env — where everything between creating it and handing
   it over can still fail. Without this, a throw in that stretch abandons the
   sandbox, and an abandoned sandbox is never collected (see [[dispose!]]), so
   the failure path leaks worse than the success path ever could."
  [[sym create-expr] & body]
  `(let [~sym ~create-expr]
     (try ~@body (catch Throwable t# (dispose! ~sym) (throw t#)))))

(defn live-count
  "How many handles of `kind` are live. For tests and diagnostics."
  [kind]
  (count (get @tables kind)))
