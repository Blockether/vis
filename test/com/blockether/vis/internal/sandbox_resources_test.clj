(ns com.blockether.vis.internal.sandbox-resources-test
  "The lifetime a shim's host objects die on, and the rule that keeps shims honest.

   Two halves, and the second is the one that matters over time. The first pins
   the MECHANISM: a handle is owned by the Context that opened it, and teardown
   frees it. The second pins PARTICIPATION: a shim that lends the guest a host
   object must say so in `:shim/resources`, and must not hand-roll a registry of
   its own. Without that half the mechanism is a convention, and a convention is
   a thing the next author forgets — silently, because a leaked image or socket
   fails nothing until a gateway has been up for a day."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [com.blockether.vis.internal.foundation.shim-paramiko]
            [com.blockether.vis.internal.foundation.shim-pil]
            [com.blockether.vis.internal.foundation.shim-sqlite3]
            [com.blockether.vis.internal.sandbox-resources :as res]
            [lazytest.core :refer [defdescribe expect it]]))

;; ── the mechanism ─────────────────────────────────────────────────────────

(defn- fresh-kind!
  "A declared kind whose releases land in `sink`."
  [k sink]
  (res/declare-kinds! {k {:resource/label (name k)
                          :resource/release (fn [h _v]
                                              (swap! sink conj h))}})
  k)

(defdescribe
  scope-teardown-test
  (it
    "frees what a scope owns, leaves other scopes alone, and is idempotent"
    (let [freed
          (atom [])

          k
          (fresh-kind! ::teardown freed)

          ;; Any object stands in for a Context: the scope is its identity hash.
          ctx-a
          (Object.)

          ctx-b
          (Object.)

          h1
          (res/open! (res/scope-of ctx-a) k :a1)

          h2
          (res/open! (res/scope-of ctx-a) k :a2)

          h3
          (res/open! (res/scope-of ctx-b) k :b1)]

      (expect (= :a1 (res/value k h1)) "a live handle must resolve to its value")
      (res/release-scope! ctx-a)
      (expect (= #{h1 h2} (set @freed)) "A's handles were not all freed")
      (expect (nil? (res/value k h1)) "a freed handle still resolves")
      (expect (= :b1 (res/value k h3)) "tearing down A touched B")
      (reset! freed [])
      (res/release-scope! ctx-a)
      (expect (= [] @freed) "a released scope freed its handles twice")
      (res/release-scope! ctx-b)
      (expect (= [h3] @freed)))))

(defdescribe guest-close-test
             ;; The guest's close takes the handle and nothing else: ownership is recorded
             ;; at open, so no caller has to remember whose a handle was.
             (it "a handle the guest closed itself is not freed again at teardown"
                 (let [freed
                       (atom [])

                       k
                       (fresh-kind! ::guest-close freed)

                       ctx
                       (Object.)

                       h1
                       (res/open! (res/scope-of ctx) k :one)

                       h2
                       (res/open! (res/scope-of ctx) k :two)]

                   (res/close! k h1)
                   (expect (= [h1] @freed))
                   (res/release-scope! ctx)
                   (expect (= [h1 h2] @freed) "teardown re-freed a handle the guest had closed"))))

(defdescribe bad-releaser-test
             ;; One shim's broken release must not abort a teardown, or everything after it
             ;; in the same scope leaks — the very failure being fixed.
             (it "keeps going after a release throws"
                 (let [freed
                       (atom [])

                       _
                       (res/declare-kinds! {::boom {:resource/label "boom"
                                                    :resource/release (fn [_ _]
                                                                        (throw (RuntimeException.
                                                                                 "boom")))}})

                       k
                       (fresh-kind! ::after freed)

                       ctx
                       (Object.)]

                   (res/open! (res/scope-of ctx) ::boom :x)
                   (let [h (res/open! (res/scope-of ctx) k :y)]
                     (res/release-scope! ctx)
                     (expect (= [h] @freed) "a throwing release stopped the rest of teardown")))))

(defdescribe undeclared-kind-test
             ;; Opening something nothing knows how to free is a leak by construction, so it
             ;; is refused loudly at the call rather than discovered on a heap dump.
             (it "refuses to open a kind that was never declared"
                 (let [thrown (try (res/open! nil ::never-declared :x)
                                   nil
                                   (catch clojure.lang.ExceptionInfo e e))]
                   (expect (some? thrown) "opening an undeclared kind must throw")
                   (expect (= ::res/undeclared-resource (:type (ex-data thrown))))
                   (expect (str/includes? (ex-message thrown) ":shim/resources")
                           "the message must say how to fix it"))))

(defdescribe cap-test
             (it "releases the oldest entry once a kind is at its cap"
                 (let [freed
                       (atom [])

                       _
                       (res/declare-kinds! {::capped {:resource/label "capped"
                                                      :resource/release (fn [h _]
                                                                          (swap! freed conj h))
                                                      :resource/max 3}})

                       ctx
                       (Object.)

                       hs
                       (doall (for [i (range 5)]
                                (res/open! (res/scope-of ctx) ::capped i)))]

                   (expect (= 3 (res/live-count ::capped))
                           (str "cap not enforced: " (res/live-count ::capped) " live"))
                   (expect (= (take 2 hs) @freed) "the cap freed something other than the oldest")
                   (res/release-scope! ctx))))

(defdescribe scope-is-not-a-reference-test
             ;; Load-bearing, not tidiness: a strong reference to a Context in a
             ;; process-global map would PIN it, and a pinned Context is exactly the leak
             ;; `env-python/new-engine!` exists to prevent.
             (it "scopes by identity hash, never by reference"
                 (let [ctx (Object.)]
                   (expect (= (System/identityHashCode ctx) (res/scope-of ctx)))
                   (expect (int? (res/scope-of ctx)))
                   (expect (nil? (res/scope-of nil)) "a nil Context must own nothing"))))

;; ── participation: the rule that keeps the mechanism from rotting ─────────

(def ^:private shim-sources
  "Every sandbox-shim source file, read from disk. Reading the SOURCE is the
   point: a shim that hand-rolls a registry is invisible to any runtime check,
   because nothing it does is wrong — it simply never asks to be owned."
  (delay (->> (file-seq (io/file "src/com/blockether/vis/internal/foundation"))
              (filter #(.isFile ^java.io.File %))
              (filter #(re-matches #"shim_.*\.clj" (.getName ^java.io.File %)))
              (mapv (fn [f]
                      [(.getName ^java.io.File f) (slurp f)])))))

(defdescribe
  shim-declares-what-it-lends-test
  (it
    "every kind a shim opens is declared in that shim's :shim/resources"
    (let [offenders
          (for [[nm src] @shim-sources
                :let [opened (set (map second (re-seq #"res/open!\s+[^\s]+\s+(::[a-z0-9-]+)" src)))
                      declared (set (map second (re-seq #"(::[a-z0-9-]+)\s+\{:resource/label" src)))
                      missing (remove declared opened)]
                :when (seq missing)]

            (str nm " opens " (str/join ", " missing) " without declaring it"))]
      (expect (empty? offenders) (str/join "; " offenders))))
  (it "no shim keeps a handle table of its own"
      ;; The registry + counter pair IS the leak shape: a process-global map of
      ;; host objects keyed by a number the guest holds, with nothing tying an
      ;; entry to a lifetime. `sandbox-resources` owns that table now, so a shim
      ;; growing one back means the lifetime rule has been quietly opted out of.
      (let [offenders (for [[nm src] @shim-sources
                            :when (and (re-find #"\(defonce[^\n]*counter\s+\(atom 0\)" src)
                                       (re-find #"\(defonce[^\n]*registry\s+\(atom \{\}\)" src))]

                        nm)]
        (expect (empty? offenders)
                (str "these shims hand-roll a handle registry instead of declaring"
                     " :shim/resources: "
                     (str/join ", " offenders))))))

(defdescribe shipped-shims-declare-releases-test
             ;; The three shims that actually lend host objects, checked as VALUES rather
             ;; than as source text, so a rename cannot quietly drop one.
             (it "the shipped resource kinds are declared with a release"
                 (doseq [k [:com.blockether.vis.internal.foundation.shim-pil/images
                            :com.blockether.vis.internal.foundation.shim-sqlite3/conns
                            :com.blockether.vis.internal.foundation.shim-paramiko/sessions
                            :com.blockether.vis.internal.foundation.shim-paramiko/sftp
                            :com.blockether.vis.internal.foundation.shim-paramiko/servers]]
                   (expect (res/declared? k) (str k " is not declared — nothing would free it")))))
