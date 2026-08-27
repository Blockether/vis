(ns com.blockether.vis.internal.manifest-test
  "The distribution has one closed manifest and no ambient discovery format."
  (:require [clojure.java.io :as io]
            [clojure.spec.alpha :as s]
            [com.blockether.vis.internal.manifest :as manifest]
            [lazytest.core :refer [defdescribe expect it throws?]]))

(defn- parsed
  "Parse manifest EDN written for a test, through the ONE parser."
  [text]
  (manifest/parse "test-manifest.edn" text))

(defn- refused? [text] (throws? clojure.lang.ExceptionInfo #(parsed text)))

(defdescribe
  manifest-shape-test
  (it "reads one initialization vector and nothing else"
      (let [m (manifest/read-manifest)]
        (expect (s/valid? ::manifest/manifest m) (pr-str (s/explain-data ::manifest/manifest m)))
        (expect (= #{:initialization} (set (keys m))))
        (expect (seq (:initialization m)))))
  (it "answers exactly one manifest on the classpath"
      ;; The whole point of naming resources explicitly: two manifests would
      ;; mean one silently shadows the other, which is what ambient discovery did.
      (expect (= 1
                 (count (enumeration-seq (.getResources (clojure.lang.RT/baseLoader)
                                                        manifest/manifest-resource))))))
  (it "names an initializer that really resolves to something callable"
      ;; Not `qualified-symbol?` - the spec already guarantees that shape. This
      ;; asks reality: a renamed or deleted Var makes the distribution incomplete.
      (doseq [initializer (manifest/initializers)]
        (expect (ifn? (requiring-resolve initializer)) initializer)))
  (it "declares one existing static resource per pack that owns documents"
      (let [paths (manifest/apropos-resource-paths)]
        (expect (seq paths))
        (expect (= (count paths) (count (set paths))))
        (doseq [path paths]
          (expect (some? (io/resource path)) path))))
  (it "answers every entry as a map carrying at least its initializer"
      (doseq [entry (manifest/entries)]
        (expect (qualified-symbol? (:register entry)) entry)
        (expect (every? #{:register :apropos :is-optional :because} (keys entry)) entry))))

(defdescribe
  entry-shape-test
  (it "accepts a bare symbol and a declared entry, and normalizes both to maps"
      (expect (= [{:register 'example.alpha/register!}
                  {:register 'example.beta/register! :apropos "META-INF/vis/apropos/docs.edn"}
                  {:register 'example.gamma/register!
                   :is-optional true
                   :because "the native library may be absent"}]
                 (parsed (str "{:initialization "
                              "[example.alpha/register! " "{:register example.beta/register! "
                              ":apropos \"META-INF/vis/apropos/docs.edn\"} "
                              "{:register example.gamma/register! "
                              ":is-optional true "
                              ":because \"the native library may be absent\"}]}")))))
  (it "refuses a manifest that carries anything but the initialization vector"
      ;; Regression: the resource list used to be a second top-level key, so every
      ;; pack's documents were declared far from the pack that registers them, and
      ;; a `:version` number nobody ever read used to be required beside it.
      (expect (refused? "{:initialization [a.b/c!] :apropos [\"x.edn\"]}"))
      (expect (refused? "{}"))
      (expect (refused? "{:version 1 :initialization [a.b/c!]}")))
  (it "refuses an empty, duplicated or unqualified initialization"
      (expect (refused? "{:initialization []}"))
      (expect (refused? "{:initialization [a.b/c! a.b/c!]}"))
      (expect (refused? "{:initialization [register!]}"))
      (expect (refused? "{:initialization #{a.b/c!}}")))
  (it "refuses an entry with an unknown key or a resource that is not a path"
      (expect (refused? "{:initialization [{:register a.b/c! :extra 1}]}"))
      (expect (refused? "{:initialization [{:apropos \"x.edn\"}]}"))
      (expect (refused? "{:initialization [{:register a.b/c! :apropos \"/x.edn\"}]}"))
      (expect (refused? "{:initialization [{:register a.b/c! :apropos \"\"}]}")))
  (it "refuses a weakness nobody explained, and an explanation of no weakness"
      (expect (refused? "{:initialization [{:register a.b/c! :is-optional true}]}"))
      (expect (refused? "{:initialization [{:register a.b/c! :because \"why\"}]}"))
      (expect (refused? (str "{:initialization "
                             "[{:register a.b/c! :is-optional false :because \"why\"}]}"))))
  (it "refuses a tagged literal anywhere in the manifest"
      (expect (refused? "{:initialization [#inst \"2020-01-01\"]}"))))

(defdescribe
  initialization-test
  (it "invokes each initializer once, in manifest order"
      (let [calls
            (atom [])

            fns
            {'example.alpha/start! #(swap! calls conj :alpha)
             'example.beta/start! #(swap! calls conj :beta)}

            state
            (atom {:initialized #{} :failed {}})]

        (with-redefs [clojure.core/requiring-resolve #(get fns %)]
          (let [result (manifest/initialize-entries! state
                                                     [{:register 'example.alpha/start!}
                                                      {:register 'example.beta/start!}])]
            (expect (= [:alpha :beta] @calls))
            (expect (= 2 (:initialized result)))
            (expect (= [] (:failed result)))
            ;; Idempotent: what stands never stands twice.
            (manifest/initialize-entries! state
                                          [{:register 'example.alpha/start!}
                                           {:register 'example.beta/start!}])
            (expect (= [:alpha :beta] @calls))))))
  (it "steps over an optional pack that fails and keeps the rest of the engine"
      (let [calls
            (atom [])

            fns
            {'example.alpha/start! #(swap! calls conj :alpha)
             'example.voice/start! #(throw (ex-info "no native library" {}))
             'example.beta/start! #(swap! calls conj :beta)}

            state
            (atom {:initialized #{} :failed {}})

            entries
            [{:register 'example.alpha/start!}
             {:register 'example.voice/start!
              :is-optional true
              :because "the native library may be absent"} {:register 'example.beta/start!}]]

        (with-redefs [clojure.core/requiring-resolve #(get fns %)]
          (let [result (manifest/initialize-entries! state entries)]
            (expect (= [:alpha :beta] @calls))
            (expect (= 2 (:initialized result)))
            (expect (= [{:initializer 'example.voice/start!
                         :phase :invoke
                         :error "no native library"
                         :because "the native library may be absent"}]
                       (:failed result)))
            ;; NON-retrying: nine call sites reach this, and a namespace that
            ;; cannot load would pay its full load every single time.
            (manifest/initialize-entries! state entries)
            (expect (= [:alpha :beta] @calls))))))
  (it "reports the phase a failure happened in"
      (let [state
            (atom {:initialized #{} :failed {}})

            entries
            [{:register 'example.missing/start! :is-optional true :because "absent"}]]

        (with-redefs [clojure.core/requiring-resolve (constantly nil)]
          (manifest/initialize-entries! state entries))
        (expect (= [:resolve]
                   (mapv :phase (:failed (manifest/initialize-entries! state entries)))))))
  (it "THROWS when a required initializer fails, and stops right there"
      ;; A distribution that cannot build itself is a build defect, not a fact
      ;; about this machine - and a half-registered engine that looks alive is
      ;; worse than a loud death.
      (let [calls
            (atom [])

            fns
            {'example.alpha/start! #(swap! calls conj :alpha)
             'example.broken/start! #(throw (ex-info "boom" {}))
             'example.beta/start! #(swap! calls conj :beta)}

            state
            (atom {:initialized #{} :failed {}})

            entries
            [{:register 'example.alpha/start!} {:register 'example.broken/start!}
             {:register 'example.beta/start!}]

            thrown
            (with-redefs [clojure.core/requiring-resolve #(get fns %)]
              (try (manifest/initialize-entries! state entries)
                   nil
                   (catch clojure.lang.ExceptionInfo e (ex-data e))))]

        (expect (= :manifest/initializer-failed (:type thrown)))
        (expect (= 'example.broken/start! (:initializer thrown)))
        (expect (= :invoke (:phase thrown)))
        (expect (= [:alpha] @calls))
        (expect (= {} (:failed @state))))))
