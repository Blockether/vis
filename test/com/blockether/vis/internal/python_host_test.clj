(ns com.blockether.vis.internal.python-host-test
  "The door from the sandbox back into Vis, measured from both sides.

   The envelope cases call the door directly, because the reply shape is a
   contract of its own: a tool that throws, a tool nobody bound and a host bug
   have to arrive as something the guest can catch and the model can read. The
   rest run a real block through the embedded CPython, which is the only way to
   prove that the session naming and the deferral survive the crossing."
  (:require [charred.api :as json]
            [clojure.string :as str]
            [com.blockether.vis-python-runtime :as runtime]
            [com.blockether.vis.internal.python-host :as python-host]
            [com.blockether.vis.internal.python-runtime :as python-runtime]
            [lazytest.experimental.interfaces.clojure-test :refer [deftest is testing]]))

(defn- reply
  "The reply map for one call to `tool` with `args` in `session`."
  [session tool args]
  (json/read-json (python-host/dispatch tool
                                        (json/write-json-str {"session" session "args" args}))))

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
