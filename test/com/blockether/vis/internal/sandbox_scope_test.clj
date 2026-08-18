(ns com.blockether.vis.internal.sandbox-scope-test
  "Host objects a guest opened must not outlive the session that opened them.

   A shim hands the guest an integer handle and keeps the real thing — a decoded
   image (`int[]` of w*h*4: 48 MB for one 4000x3000 frame), a JDBC connection, an
   SSH socket — in a process-global registry. The guest is supposed to hand it
   back with `close()`; a model that hit an error, or a session that was
   cancelled, routinely does not. Those objects used to live until the PROCESS
   died: long after the session's Context, its Engine and its env were gone, and
   entirely out of reach of the engine-per-session fix, because they are host
   state rather than anything inside GraalPy.

   `env-python`'s scope ties them to the Context instead. These tests pin the
   contract itself rather than any one shim, so a shim added later is covered by
   the same guarantee the moment it registers a releaser."
  (:require [com.blockether.vis.internal.env-python :as ep]
            [lazytest.core :refer [defdescribe expect it]]))

(defdescribe scope-releases-on-teardown-test
  (it "releases every handle a scope still owns, and forgets the scope"
    (let
      [released
       (atom [])

       ;; A stand-in shim: the contract is the registry-key + releaser pair, not
       ;; anything about images or sockets.
       _
       (ep/register-scope-releaser! ::probe-handles (fn [h] (swap! released conj h)))

       ;; Two Contexts standing in for two sessions. Any object identity works —
       ;; the scope is `System/identityHashCode`, never the object itself.
       ctx-a
       (Object.)

       ctx-b
       (Object.)]

      (ep/own-in-scope! (ep/scope-of ctx-a) ::probe-handles 1)
      (ep/own-in-scope! (ep/scope-of ctx-a) ::probe-handles 2)
      (ep/own-in-scope! (ep/scope-of ctx-b) ::probe-handles 99)

      ;; Tearing down A must not touch B's handle.
      (ep/release-scope! ctx-a)
      (expect (= #{1 2} (set @released)) "A's handles were not all released")

      (reset! released [])
      (ep/release-scope! ctx-b)
      (expect (= [99] @released) "B's handle was not released with B")

      ;; Idempotent: a second teardown of the same scope releases nothing again,
      ;; which matters because dispose paths can run twice on a cancelled turn.
      (reset! released [])
      (ep/release-scope! ctx-a)
      (ep/release-scope! ctx-b)
      (expect (= [] @released) "a released scope released its handles twice"))))

(defdescribe scope-disown-test
  (it "a handle the guest closed itself is not closed again at teardown"
    (let
      [released
       (atom [])

       _
       (ep/register-scope-releaser! ::disown-probe (fn [h] (swap! released conj h)))

       ctx
       (Object.)]

      (ep/own-in-scope! (ep/scope-of ctx) ::disown-probe 7)
      (ep/own-in-scope! (ep/scope-of ctx) ::disown-probe 8)
      ;; The guest's own `close()` path.
      (ep/disown-in-scope! (ep/scope-of ctx) ::disown-probe 7)
      (ep/release-scope! ctx)
      (expect (= [8] @released)
              "teardown released a handle the guest had already closed"))))

(defdescribe scope-survives-a-bad-releaser-test
  ;; One shim's broken release must never abort a teardown: everything after it
  ;; in the same scope would leak, which is the very failure being fixed.
  (it "keeps releasing after a releaser throws"
    (let
      [released
       (atom [])

       _
       (ep/register-scope-releaser! ::boom-probe (fn [_] (throw (RuntimeException. "boom"))))

       _
       (ep/register-scope-releaser! ::after-probe (fn [h] (swap! released conj h)))

       ctx
       (Object.)]

      (ep/own-in-scope! (ep/scope-of ctx) ::boom-probe 1)
      (ep/own-in-scope! (ep/scope-of ctx) ::after-probe 2)
      (ep/release-scope! ctx)
      (expect (= [2] @released)
              "a throwing releaser stopped the rest of the teardown"))))

(defdescribe scope-holds-no-context-reference-test
  ;; Load-bearing, not cosmetic: a strong reference to a Context in a
  ;; process-global map would PIN it, and a pinned Context is exactly the leak
  ;; `new-engine!` exists to prevent. The scope is an identity hash — an int.
  (it "scopes a Context by identity hash, never by reference"
    (let [ctx (Object.)]
      (expect (= (System/identityHashCode ctx) (ep/scope-of ctx)))
      (expect (int? (ep/scope-of ctx))
              "the scope key must be an int, or it would pin the Context"))))

(defdescribe scope-ignores-scopeless-owners-test
  ;; The shim-trigger probe wires bindings with a nil scope: it is process-wide,
  ;; owns nothing, and closes its own context immediately.
  (it "owning under a nil scope is a no-op rather than a crash"
    (let
      [released
       (atom [])

       _
       (ep/register-scope-releaser! ::nil-probe (fn [h] (swap! released conj h)))]

      (expect (= 5 (ep/own-in-scope! nil ::nil-probe 5)) "own! must return the handle")
      (ep/disown-in-scope! nil ::nil-probe 5)
      (ep/release-scope! nil)
      (expect (= [] @released)))))
