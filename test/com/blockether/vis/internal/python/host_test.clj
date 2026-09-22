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
            [lazytest.experimental.interfaces.clojure-test :refer [deftest is testing]])
  (:import [java.lang Thread$State]))

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
    (is (nil? (get answer "error")) (str (get answer "error")))
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

(defn- parked?
  "Wait until a driver is blocked either on the context lock or on a test gate."
  [call]
  (when-let [^Thread thread (deref (:thread call) 5000 nil)]
    (let [deadline (+ (System/nanoTime) 5000000000)]
      (loop []

        (cond (#{Thread$State/WAITING Thread$State/TIMED_WAITING} (.getState thread)) true
              (>= (System/nanoTime) deadline) false
              :else (do (Thread/sleep 1) (recur)))))))

(defn- publish-from-callback
  "Dispatch without driver bindings, as on an extension worker's host-callback thread."
  [session headline]
  (binding [extension/*activity-content-sink* nil]
    (reply session "publish" [{"headline" headline}])))

(deftest dispatch-envelope-test
  (let [session (block-session! {"echo" (fn [x]
                                          {"said" x})
                                 "boom" (fn []
                                          (throw (ex-info "tool refused" {})))
                                 "faulty" (fn []
                                            (let [absent nil]
                                              (.length ^String absent)))})]
    (try (testing "a value comes back as data under `value`"
           (is (= {"value" {"said" "hi"}} (reply session "echo" ["hi"]))))
         (testing "a tool that throws is an error the guest can catch, with the tool's own message"
           (is (= {"error" "tool refused"} (reply session "boom" []))))
         (testing "a name this session never bound is refused by name"
           (is (= {"error" "no vis tool named `nope` in this session"} (reply session "nope" []))))
         (testing "a host NullPointerException says whose bug it is"
           (is (re-find #"internal tool fault" (get (reply session "faulty" []) "error"))))
         (testing "a session is only served its own bindings"
           (is (= {"error" "no vis tool named `echo` in this session"}
                  (reply "somebody-else" "echo" ["hi"]))))
         (finally (is (= 3 (python-host/forget-session! session)))
                  (runtime/close-session! session)))))

(deftest host-tool-through-the-interpreter-test
  (let [session (block-session! {"greet" (fn [who]
                                           (str "hello " who))})]
    (try (testing "a block awaits a Clojure function and reads its value as Python data"
           (is (= "hello world" (printed session "print(await greet('world'))"))))
         (testing "a failing tool arrives as a catchable exception carrying the host's message"
           (python-host/install-tools! session
                                       {"refuse" (fn []
                                                   (throw (ex-info "not today" {})))})
           (is (= "not today"
                  (printed session
                           (str "try:\n" "    await refuse()\n"
                                "except Exception as failure:\n" "    print(failure)")))))
         (finally (python-host/forget-session! session) (runtime/close-session! session)))))

(deftest two-sessions-one-name-test
  ;; One interpreter holds every session in the process, so the same tool name
  ;; bound in two of them has to reach two different functions.
  (let [one
        (block-session! {"whose" (fn []
                                   "first")})

        two
        (block-session! {"whose" (fn []
                                   "second")})]

    (try (is (= "first" (printed one "print(await whose())")))
         (is (= "second" (printed two "print(await whose())")))
         (finally (doseq [session [one two]]
                    (python-host/forget-session! session)
                    (runtime/close-session! session))))))

(deftest a-forged-session-buys-nothing-test
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

    (testing "the session that owns the tool is served"
      (is (= "SECRET" (get (reply victim "secret" []) "value"))))
    (testing "a caller that is not that session is refused, whatever the payload claims"
      (let [answer (json/read-json (python-host/dispatch attacker
                                                         "secret"
                                                         (json/write-json-str {"session" victim
                                                                               "args" []})))]
        (is (nil? (get answer "value")))
        (is (str/includes? (str (get answer "error")) "no vis tool named"))))
    (testing "the tool ran only for its own session" (is (= [:ran] @served)))))

(deftest binding-without-an-interpreter-is-not-a-failure-test
  ;; Measured while loading a Python extension in a process that never started a
  ;; sandbox: `bind!` marked itself done BEFORE binding, so the throw from a
  ;; cdylib that had never been fetched left the flag set — the FIRST extension
  ;; in such a process failed to load and every later one skipped the bind and
  ;; only appeared to work. Binding needs an interpreter this process may simply
  ;; not have; the extension host binds its own, in its own process.
  (let [bound (deref #'python-host/bound)]
    (try (reset! (deref #'python-host/bound) false)
         (with-redefs [runtime/bind-host! (fn [_]
                                            (throw (ex-info "no library here" {})))]
           (testing "a process with nothing to bind says so instead of throwing"
             (is (false? (python-host/bind!)))))
         (testing "and it stays retryable, so the next caller with an interpreter binds"
           (let [handed (atom nil)]
             (with-redefs [runtime/bind-host! (fn [f]
                                                (reset! handed f)
                                                nil)]
               (is (true? (python-host/bind!)))
               (is (some? @handed)))))
         (finally (reset! (deref #'python-host/bound) bound)))))

(deftest concurrent-calls-keep-their-activity-sinks-test
  ;; #283: a second driver replaced the first frame, then either driver removed it.
  (let [session
        (block-session! {"publish" extension/publish-activity!})

        events
        (atom [])

        first-entered
        (promise)

        first-release
        (promise)

        second-release
        (promise)

        second-entered
        (atom false)

        second-call
        (atom nil)

        first-call
        (start-call #(binding [extension/*activity-content-sink*
                               (fn [p]
                                 (swap! events conj [:first (get p "headline")]))]
                       (python-host/conveying session
                                              (deliver first-entered true)
                                              @first-release
                                              (publish-from-callback session "First call"))))]

    (try (is (true? (deref first-entered 5000 nil)))
         (reset! second-call (start-call #(binding [extension/*activity-content-sink*
                                                    (fn [p]
                                                      (swap! events conj
                                                        [:second (get p "headline")]))]
                                            (python-host/conveying
                                              session
                                              (reset! second-entered true)
                                              @second-release
                                              (publish-from-callback session "Second call")))))
         ;; Both implementations park here: the broken one has entered the body and
         ;; overwritten the frame; the fixed one is still waiting for the context.
         (is (parked? @second-call))
         (let [overlapped?
               @second-entered

               first-reply
               (do (deliver first-release true) (deref (:result first-call) 5000 ::timeout))

               second-reply
               (do (deliver second-release true) (deref (:result @second-call) 5000 ::timeout))]

           (is (= [[:first "First call"] [:second "Second call"]] @events)
               (str "Activity sink ownership: " (pr-str @events)))
           (is (false? overlapped?))
           (is (= {"value" true} first-reply))
           (is (= {"value" true} second-reply)))
         (is (not (contains? @@#'python-host/frames session)))
         (finally (deliver first-release true)
                  (deliver second-release true)
                  (doseq [call
                          [first-call @second-call]

                          :when call]

                    (future-cancel (:result call)))
                  (python-host/forget-session! session)
                  (runtime/close-session! session)))))

(deftest nested-calls-restore-the-outer-activity-sink-test
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
             (is (= {"value" true} (publish-from-callback session "Before inner call")))
             (binding [extension/*activity-content-sink* (fn [p]
                                                           (swap! events conj
                                                             [:inner (get p "headline")]))]
               (is (identical? failure
                               (try (python-host/conveying session
                                                           (publish-from-callback session
                                                                                  "Inner call")
                                                           (throw failure))
                                    (catch Throwable t t)))))
             (is (= {"value" true} (publish-from-callback session "After inner call")))))
         (is (= [[:outer "Before inner call"] [:inner "Inner call"] [:outer "After inner call"]]
                @events))
         (is (not (contains? @@#'python-host/frames session)))
         (finally (python-host/forget-session! session) (runtime/close-session! session)))))

(deftest different-contexts-remain-concurrent-test
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
        (start-call #(python-host/conveying one (deliver entered true) @release :first))]

    (try (is (true? (deref entered 5000 nil)))
         (let [second-call (start-call #(python-host/conveying two :second))]
           (try (is (= :second (deref (:result second-call) 5000 ::timeout)))
                (is (not (realized? (:result first-call))))
                (finally (future-cancel (:result second-call)))))
         (deliver release true)
         (is (= :first (deref (:result first-call) 5000 ::timeout)))
         (doseq [session [one two]]
           (is (not (contains? @@#'python-host/frames session)))
           (is (not (contains? @@#'python-host/frame-locks session))))
         (finally (deliver release true) (future-cancel (:result first-call))))))

(deftest failed-calls-release-the-context-test
  ;; #283: successful nesting and a failing outer call must both release their leases.
  (let [session
        (str "vis-host-failure-" (System/nanoTime))

        failure
        (ex-info "Driver failed" {})]

    (is (identical? failure
                    (try (python-host/conveying
                           session
                           (is (= :nested (python-host/conveying session :nested)))
                           (is (contains? @@#'python-host/frames session))
                           (is (= 1 (get-in @@#'python-host/frame-locks [session :users])))
                           (throw failure))
                         (catch Throwable t t))))
    (is (not (contains? @@#'python-host/frames session)))
    (is (not (contains? @@#'python-host/frame-locks session)))
    (is (= :recovered (python-host/conveying session :recovered)))
    (is (not (contains? @@#'python-host/frame-locks session)))))

(deftest interrupted-waiter-preserves-the-active-context-test
  ;; #283: cancelling a queued call must not remove the active frame or its lock.
  (let [session
        (str "vis-host-interrupted-" (System/nanoTime))

        entered
        (promise)

        release
        (promise)

        waiter-entered
        (atom false)

        waiter
        (atom nil)

        next-call
        (atom nil)

        holder
        (start-call #(python-host/conveying session (deliver entered true) @release :holder))]

    (try (is (true? (deref entered 5000 nil)))
         (let [frame
               (get @@#'python-host/frames session)

               lock
               (get-in @@#'python-host/frame-locks [session :lock])]

           (reset! waiter (start-call #(try (python-host/conveying session
                                                                   (reset! waiter-entered true)
                                                                   :unexpected)
                                            (catch InterruptedException _ :interrupted))))
           (is (parked? @waiter))
           (is (= 2 (get-in @@#'python-host/frame-locks [session :users])))
           (.interrupt ^Thread (deref (:thread @waiter) 5000 nil))
           (is (= :interrupted (deref (:result @waiter) 5000 ::timeout)))
           (is (false? @waiter-entered))
           (is (identical? frame (get @@#'python-host/frames session)))
           (is (identical? lock (get-in @@#'python-host/frame-locks [session :lock])))
           (is (= 1 (get-in @@#'python-host/frame-locks [session :users])))
           (reset! next-call (start-call #(python-host/conveying session :next)))
           (is (parked? @next-call))
           (is (not (realized? (:result @next-call))))
           (is (identical? lock (get-in @@#'python-host/frame-locks [session :lock])))
           (deliver release true)
           (is (= :holder (deref (:result holder) 5000 ::timeout)))
           (is (= :next (deref (:result @next-call) 5000 ::timeout))))
         (is (not (contains? @@#'python-host/frames session)))
         (is (not (contains? @@#'python-host/frame-locks session)))
         (finally (deliver release true)
                  (doseq [call
                          [holder @waiter @next-call]

                          :when call]

                    (future-cancel (:result call)))))))
