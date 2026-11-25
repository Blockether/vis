(ns com.blockether.vis.internal.python.host-test
  "The door from the sandbox back into Vis, measured from both sides.

   The envelope cases call the door directly, because the reply shape is a
   contract of its own: a tool that throws, a tool nobody bound and a host bug
   have to arrive as something the guest can catch and the model can read. The
   rest run a real block through the embedded CPython, which is the only way to
   prove that the session naming and the deferral survive the crossing."
  (:require [charred.api :as json]
            [clojure.string :as str]
            [com.blockether.vis.internal.extension.core :as extension]
            [com.blockether.vis-python-runtime :as runtime]
            [com.blockether.vis.internal.python.host :as python-host]
            [com.blockether.vis.internal.python.runtime :as python-runtime]
            [lazytest.core :refer [defdescribe expect it]]))

(defn- reply
  "The reply map for one call to `tool` with `args` in `session`."
  [session tool args]
  (json/read-json
    (python-host/dispatch session tool (json/write-json-str {"session" session "args" args}))))

(defn- block-session!
  "An interpreter session equipped with the sandbox runtime and `tools`."
  [tools]
  (python-runtime/ensure-library!)
  (runtime/initialize!)
  (let [session (str "vis-host-" (System/nanoTime))]
    (runtime/install-runtime! session)
    (python-host/install-tools! session tools)
    session))

(defn- printed
  "What a block PRINTED, trimmed - a block's one success channel."
  [session code]
  (let [answer (json/read-json (runtime/run-block session code))]
    (expect (nil? (get answer "error")) (str (get answer "error")))
    (some-> (get answer "stdout")
            str
            clojure.string/trim)))

(defn- start-call
  "Start a driver call and expose its thread so overlap can be measured without sleeps."
  [f]
  (let [thread
        (promise)

        result
        (future (deliver thread (Thread/currentThread)) (f))]

    {:thread thread :result result}))

(defn- publish-from-callback
  "Dispatch without driver bindings, as on an extension worker's host-callback thread."
  [session headline]
  (binding [extension/*activity-content-sink* nil]
    (reply session "publish" [{"headline" headline}])))

(defdescribe dispatch-envelope-test
             (it "dispatch envelope"
                 (let [session (block-session! {"echo" (fn [x]
                                                         {"said" x})
                                                "boom" (fn []
                                                         (throw (ex-info "tool refused" {})))
                                                "faulty" (fn []
                                                           (let [absent nil]
                                                             (.length ^String absent)))})]
                   (try
                     ;; a value comes back as data under `value`
                     (expect (= {"value" {"said" "hi"}} (reply session "echo" ["hi"])))
                     ;; a tool that throws is an error the guest can catch, with the tool's own message
                     (expect (= {"error" "tool refused"} (reply session "boom" [])))
                     ;; a name this session never bound is refused by name
                     (expect (= {"error" "no vis tool named `nope` in this session"}
                                (reply session "nope" [])))
                     ;; a host NullPointerException says whose bug it is
                     (expect (re-find #"internal tool fault"
                                      (get (reply session "faulty" []) "error")))
                     ;; a session is only served its own bindings
                     (expect (= {"error" "no vis tool named `echo` in this session"}
                                (reply "somebody-else" "echo" ["hi"])))
                     (finally (expect (= 3 (python-host/forget-session! session)))
                              (runtime/close-session! session))))))

(defdescribe
  host-tool-through-the-interpreter-test
  (it "host tool through the interpreter"
      (let [session (block-session! {"greet" (fn [who]
                                               (str "hello " who))})]
        (try
          ;; a block awaits a Clojure function and reads its value as Python data
          (expect (= "hello world" (printed session "print(await greet('world'))")))
          ;; a failing tool arrives as a catchable exception carrying the host's message
          (python-host/install-tools! session
                                      {"refuse" (fn []
                                                  (throw (ex-info "not today" {})))})
          (expect (= "not today"
                     (printed session
                              (str "try:\n" "    await refuse()\n"
                                   "except Exception as failure:\n" "    print(failure)"))))
          (finally (python-host/forget-session! session) (runtime/close-session! session))))))

(defdescribe two-sessions-one-name-test
             (it "two sessions one name"
                 ;; One interpreter holds every session in the process, so the same tool name
                 ;; bound in two of them has to reach two different functions.
                 (let [one
                       (block-session! {"whose" (fn []
                                                  "first")})

                       two
                       (block-session! {"whose" (fn []
                                                  "second")})]

                   (try (expect (= "first" (printed one "print(await whose())")))
                        (expect (= "second" (printed two "print(await whose())")))
                        (finally (doseq [session [one two]]
                                   (python-host/forget-session! session)
                                   (runtime/close-session! session)))))))

(defdescribe a-forged-session-buys-nothing-test
             (it "a forged session buys nothing"
                 ;; Measured before this was closed: `vis_runtime.host_call` is an ordinary
                 ;; module function and the session in its envelope is JSON the guest writes, so
                 ;; a block that named a neighbour's session was served the neighbour's tools —
                 ;; with the neighbour's roots. The interpreter now says who called, and that is
                 ;; the only thing this authorizes against.
                 (let [served
                       (atom [])

                       victim
                       (block-session! {"secret" (fn []
                                                   (swap! served conj :ran)
                                                   "SECRET")})

                       attacker
                       (block-session! {})]

                   ;; the session that owns the tool is served
                   (expect (= "SECRET" (get (reply victim "secret" []) "value")))
                   ;; a caller that is not that session is refused, whatever the payload claims
                   (let [answer (json/read-json (python-host/dispatch attacker
                                                                      "secret"
                                                                      (json/write-json-str
                                                                        {"session" victim
                                                                         "args" []})))]
                     (expect (nil? (get answer "value")))
                     (expect (str/includes? (str (get answer "error")) "no vis tool named")))
                   ;; the tool ran only for its own session
                   (expect (= [:ran] @served)))))

(defdescribe binding-without-an-interpreter-is-not-a-failure-test
             (it "binding without an interpreter is not a failure"
                 ;; Measured while loading a Python extension in a process that never started a
                 ;; sandbox: `bind!` marked itself done BEFORE binding, so the throw from a
                 ;; cdylib that had never been fetched left the flag set — the FIRST extension
                 ;; in such a process failed to load and every later one skipped the bind and
                 ;; only appeared to work. Binding needs an interpreter this process may simply
                 ;; not have; the extension host binds its own, in its own process.
                 (let [bound (deref #'python-host/bound)]
                   (try (reset! (deref #'python-host/bound) false)
                        (with-redefs [runtime/bind-host-with-call! (fn [_]
                                                           (throw (ex-info "no library here" {})))]
                          ;; a process with nothing to bind says so instead of throwing
                          (expect (false? (python-host/bind!))))
                        ;; and it stays retryable, so the next caller with an interpreter binds
                        (let [handed (atom nil)]
                          (with-redefs [runtime/bind-host-with-call! (fn [f]
                                                             (reset! handed f)
                                                             nil)]
                            (expect (true? (python-host/bind!)))
                            (expect (some? @handed))))
                        (finally (reset! (deref #'python-host/bound) bound))))))

(defdescribe concurrent-calls-keep-their-activity-sinks-test
  (it "keeps activity sinks separate while calls in the same session overlap"
      (let [session (block-session! {"publish" extension/publish-activity!})
            events (atom [])
            entered (promise)
            release (promise)
            publish (fn [call]
                      (json/read-json
                        (python-host/dispatch session call "publish"
                                              (json/write-json-str {"args" [{}]}))))
            one (future (binding [extension/*activity-content-sink* (fn [_] (swap! events conj :one))]
                          (python-host/conveying* session "one"
                            #(do (deliver entered true) @release (publish "one")))))]
        (try (expect (true? (deref entered 5000 nil)))
             (let [two (future (binding [extension/*activity-content-sink* (fn [_] (swap! events conj :two))]
                                 (python-host/conveying* session "two" #(publish "two"))))]
               (try (expect (= {"value" true} (deref two 5000 ::timeout)))
                    (expect (not (realized? one)))
                    (finally (future-cancel two))))
             (deliver release true)
             (expect (= {"value" true} (deref one 5000 ::timeout)))
             (expect (= [:two :one] @events))
             (finally (deliver release true)
                      (future-cancel one)
                      (python-host/forget-session! session)
                      (runtime/close-session! session))))))

(defdescribe
  nested-calls-restore-the-outer-activity-sink-test
  (it "nested calls restore the outer activity sink"
      ;; #283: a reentrant call must restore, not remove, the outer driver's frame.
      (let [session
            (block-session! {"publish" extension/publish-activity!})

            events
            (atom [])

            failure
            (ex-info "Inner call failed" {})]

        (try (binding [extension/*activity-content-sink* (fn [p]
                                                           (swap! events conj
                                                             [:outer (get p "headline")]))]
               (python-host/conveying
                 session
                 (expect (= {"value" true} (publish-from-callback session "Before inner call")))
                 (binding [extension/*activity-content-sink* (fn [p]
                                                               (swap! events conj
                                                                 [:inner (get p "headline")]))]
                   (expect (identical? failure
                                       (try (python-host/conveying
                                              session
                                              (publish-from-callback session "Inner call")
                                              (throw failure))
                                            (catch Throwable t t)))))
                 (expect (= {"value" true} (publish-from-callback session "After inner call")))))
             (expect (= [[:outer "Before inner call"] [:inner "Inner call"]
                         [:outer "After inner call"]]
                        @events))
             (expect (not (contains? @@#'python-host/frames session)))
             (finally (python-host/forget-session! session) (runtime/close-session! session))))))

(defdescribe different-contexts-remain-concurrent-test
             (it "different contexts remain concurrent"
                 ;; #283: serialize a context, not the entire extension host.
                 (let [one
                       (str "vis-host-concurrent-one-" (System/nanoTime))

                       two
                       (str "vis-host-concurrent-two-" (System/nanoTime))

                       entered
                       (promise)

                       release
                       (promise)

                       first-call
                       (start-call
                         #(python-host/conveying one (deliver entered true) @release :first))]

                   (try (expect (true? (deref entered 5000 nil)))
                        (let [second-call (start-call #(python-host/conveying two :second))]
                          (try (expect (= :second (deref (:result second-call) 5000 ::timeout)))
                               (expect (not (realized? (:result first-call))))
                               (finally (future-cancel (:result second-call)))))
                        (deliver release true)
                        (expect (= :first (deref (:result first-call) 5000 ::timeout)))
                        (doseq [session [one two]]
                          (expect (not (contains? @@#'python-host/frames session)))
                          (expect (not-any? #(= session (first %)) (keys @@#'python-host/frames))))
                        (finally (deliver release true) (future-cancel (:result first-call)))))))

(defdescribe failed-calls-release-the-context-test
  (it "a failing invocation releases its frame, not the other invocation's frame"
      (let [session "vis-host-frame-failure"
            failure (ex-info "Driver failed" {})]
        (python-host/conveying* session "outer"
          (fn []
            (expect (identical? failure
                                (try (python-host/conveying* session "inner" #(throw failure))
                                     (catch Throwable e e))))
            (expect (python-host/active-call? session "outer"))
            (expect (not (python-host/active-call? session "inner")))))
        (expect (not (python-host/active-call? session "outer"))))))

(defdescribe guest-thread-frame-test
             ;; #405: a guest thread that meets an active call must keep its limits.
             (it "keeps the guest-thread flag under the driver's conveyed frame"
                 (let [session
                       "guest-frame-fixture"

                       driver
                       (binding [python-host/*guest-thread?* false]
                         (clojure.lang.Var/getThreadBindingFrame))]

                   (swap! @#'python-host/frames assoc [session "call"] driver)
                   (try (expect (= {"value" true}
                                   (binding [python-host/*guest-thread?* true]
                                     (#'python-host/answer-in-frame
                                      session
                                      "call"
                                      "fixture"
                                      (fn []
                                        python-host/*guest-thread?*)
                                      []))))
                        (expect (= {"value" false}
                                   (#'python-host/answer-in-frame
                                    session
                                    "call"
                                    "fixture"
                                    (fn []
                                      python-host/*guest-thread?*)
                                    [])))
                        (finally (swap! @#'python-host/frames dissoc [session "call"]))))))
