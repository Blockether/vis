(ns com.blockether.vis.internal.python.extension-tools-test
  "`vis.tools`: a Python extension calls another active session tool the way the
   model does and gets that extension's objects back as frozen classes. Boots real
   extension contexts on the shared engine, with no model in the loop."
  (:require [clojure.java.io :as io]
            [com.blockether.vis.internal.extension.core :as extension]
            [com.blockether.vis.internal.persistance.core :as ps]
            [com.blockether.vis.internal.persistance.sqlite.test-helpers :as h]
            [com.blockether.vis.internal.python.extensions :as pyx]
            [lazytest.core :refer [defdescribe expect it]])
  (:import [java.nio.file Files]
           [java.nio.file.attribute FileAttribute]))

(def ^:private svc-source
  (str
    "from __future__ import annotations\n"
    "from dataclasses import dataclass\n" "import blockether.vis.extension as vis\n"
    "\n" "\n"
    "@dataclass(frozen=True)\n" "class Browser:\n"
    "    \"A browser engine.\"\n" "    engine: str\n"
    "\n" "\n"
    "@dataclass(frozen=True)\n" "class Reservation:\n"
    "    \"A reserved browser.\"\n" "    id: str\n"
    "    profile: str\n" "    browser: Browser\n"
    "\n" "\n"
    "@dataclass(frozen=True)\n" "class Page:\n"
    "    \"Rows of one page.\"\n" "    __vis_sequence_field__ = \"rows\"\n"
    "    rows: tuple[Browser, ...]\n" "\n"
    "\n" "class Svc:\n"
    "    def reserve(self, label: str, *, profile: str = \"default\") -> Reservation:\n"
    "        \"Reserve a browser.\"\n"
    "        return Reservation(f\"r-{label}\", profile, Browser(\"chromium\"))\n" "\n"
    "    def page(self, reservation: str) -> Page:\n" "        \"List the rows of one page.\"\n"
    "        return Page((Browser(\"a\"), Browser(\"b\")))\n" "\n"
    "    def plain(self) -> dict:\n" "        \"Answer plain JSON data.\"\n"
    "        return {\"answer\": 42, \"nested\": {\"rows\": [1, 2]}}\n" "\n"
    "    def fail(self) -> str:\n" "        \"Fail on purpose.\"\n"
    "        raise ValueError(\"no browser available\")\n" "\n"
    "    def back(self) -> str:\n" "        \"Call the client back.\"\n"
    "        return vis.tools.client.probe()\n" "\n"
    "\n"
    "vis.register_extension(vis.Extension(name=\"svc\", description=\"Service fixture\", alias=\"svc\", symbols=[vis.Symbol(Svc(), name=\"svc\")]))\n"))

(def ^:private client-source
  (str
    "from __future__ import annotations\n" "import dataclasses\n"
    "import blockether.vis.extension as vis\n" "\n"
    "\n" "def _refusal(call):\n"
    "    try:\n" "        call()\n"
    "    except Exception as error:\n" "        return f\"{type(error).__name__}: {error}\"\n"
    "    return \"no refusal\"\n" "\n"
    "\n" "class Client:\n"
    "    def probe(self) -> dict:\n"
    "        \"Call svc through vis.tools and describe the answers.\"\n"
    "        first = vis.tools.svc.reserve(\"x-login\", profile=\"x-com\")\n"
    "        second = vis.tools[\"svc.reserve\"](\"other\")\n"
    "        page = vis.tools.svc.page(first.id)\n" "        plain = vis.tools.svc.plain()\n"
    "        return {\n" "            \"class\": type(first).__name__,\n"
    "            \"dataclass\": dataclasses.is_dataclass(first),\n"
    "            \"same_class\": type(first) is type(second),\n"
    "            \"id\": [first.id, first[\"id\"]],\n"
    "            \"profiles\": [first.profile, second.profile],\n"
    "            \"browser\": [type(first.browser).__name__, first.browser.engine],\n"
    "            \"frozen\": _refusal(lambda: setattr(first, \"id\", \"changed\")),\n"
    "            \"missing\": _refusal(lambda: first.missing),\n"
    "            \"rows\": [type(page).__name__, [row.engine for row in page], len(page), page[0].engine],\n"
    "            \"plain\": [isinstance(plain, dict), plain.answer, plain[\"nested\"].rows],\n"
    "            \"repr\": repr(first),\n"
    "        }\n" "\n"
    "    def loop(self) -> str:\n" "        \"Call this extension back through vis.tools.\"\n"
    "        return _refusal(lambda: vis.tools.client.probe())\n" "\n"
    "    def relay(self) -> str:\n" "        \"Call svc, which calls this extension back.\"\n"
    "        return _refusal(lambda: vis.tools.svc.back())\n" "\n"
    "    def unknown(self) -> str:\n" "        \"Call a misspelled tool.\"\n"
    "        return _refusal(lambda: vis.tools.svc.reserv(\"x\"))\n" "\n"
    "    def failing(self) -> str:\n" "        \"Call a tool that fails.\"\n"
    "        return _refusal(lambda: vis.tools.svc.fail())\n" "\n"
    "\n"
    "vis.register_extension(vis.Extension(name=\"client\", description=\"Client fixture\", alias=\"client\", symbols=[vis.Symbol(Client(), name=\"client\")]))\n"))

(defn- with-tools
  "Load both fixtures, run `f` with a map of their extensions by name while a bound
   session has them active, then unload them."
  [f]
  (let [dir
        (.toFile (Files/createTempDirectory "vis-tools" (make-array FileAttribute 0)))

        store
        (ps/db-create-connection! :memory)

        session-id
        (h/store-session! store {:title "Tools"})]

    (try (spit (io/file dir "svc.py") svc-source)
         (spit (io/file dir "client.py") client-source)
         (let [result
               (pyx/reload-python-extensions! {:dirs [(str dir)]})

               exts
               (into {}
                     (keep #(when (#{"svc" "client"} (:ext/name %)) [(:ext/name %) %]))
                     (extension/registered-extensions))]

           (expect (= 2 (:loaded result)))
           (binding [extension/*current-environment* {:db-info store
                                                      :session-id session-id
                                                      :extensions (atom (vec (vals exts)))
                                                      :active-extensions (atom (vec (vals exts)))}]
             (f exts)))
         (finally (pyx/reload-python-extensions! {:dirs []})
                  (ps/db-dispose-connection! store)
                  (doseq [file (reverse (file-seq dir))]
                    (io/delete-file file true))))))

(defn- call
  "Run the tool `sym` of `ext` as the model's call does."
  [ext sym]
  (extension/invoke-symbol-wrapper ext
                                   (some #(when (= sym (:ext.symbol/symbol %)) %)
                                         (extension/ext-symbols ext))
                                   []
                                   extension/*current-environment*))

(defdescribe
  extension-tool-call-test
  (it "answers another extension's objects as frozen classes of the same name"
      (with-tools
        (fn [exts]
          (expect
            (= {"class" "Reservation"
                "dataclass" true
                "same_class" true
                "id" ["r-x-login" "r-x-login"]
                "profiles" ["x-com" "default"]
                "browser" ["Browser" "chromium"]
                "frozen" "FrozenInstanceError: cannot assign to field 'id'"
                "missing" (str "AttributeError: Reservation has no field 'missing'; "
                               "available fields: id, profile, browser")
                "rows" ["Page" ["a" "b"] 2 "a"]
                "plain" [true 42 [1 2]]
                "repr"
                "Reservation(id='r-x-login', profile='x-com', browser=Browser(engine='chromium'))"}
               (dissoc (call (exts "client") 'client.probe) "op"))))))
  (it "shows each nested call as its own Activity under the calling tool"
      (with-tools (fn [exts]
                    (let [events (atom [])]
                      (binding [extension/*tool-event-sink* #(swap! events conj %)]
                        (call (exts "client") 'client.probe))
                      (let [[outer & nested] (filter #(= :start (:phase %)) @events)]
                        (expect (= :client/client.probe (:operation outer)))
                        (expect (= [:svc/svc.reserve :svc/svc.reserve :svc/svc.page :svc/svc.plain]
                                   (mapv :operation nested)))
                        (expect (every? #(= (:invocation-id outer) (:parent-invocation-id %))
                                        nested))
                        (expect (= (* 2 (inc (count nested)))
                                   (count (filter #(#{:start :terminal} (:phase %)) @events)))))))))
  (it "refuses a call back into a waiting extension instead of hanging"
      (with-tools
        (fn [exts]
          (doseq [sym '[client.loop client.relay]]
            (expect (= (str "VisToolError: Extension `client` is waiting on this vis.tools call; "
                            "calling back into it would never return")
                       (call (exts "client") sym)))))))
  (it "names similar tools for an unknown name and raises a tool's own failure"
      (with-tools
        (fn [exts]
          (expect
            (= (str
                 "VisToolError: vis.tools: no active tool is named `svc.reserv` in this session; "
                 "similar: svc.back, svc.fail, svc.page, svc.plain, svc.reserve")
               (call (exts "client") 'client.unknown)))
          (expect (= "VisToolError: ValueError: no browser available"
                     (call (exts "client") 'client.failing))))))
  (it "refuses a call without a bound session"
      (let [call-tool (get (pyx/host-doors nil "test" nil) "__vis_host_call_tool__")]
        (expect (= :session-not-bound
                   (try (call-tool "svc.reserve" [] {})
                        (catch clojure.lang.ExceptionInfo e (:error (ex-data e)))))))))
