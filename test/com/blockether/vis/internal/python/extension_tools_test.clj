(ns com.blockether.vis.internal.python.extension-tools-test
  "`vis.tools`: a Python extension calls another active session tool the way the
   model does and gets that extension's objects back as read-only records. Boots real
   extension contexts on the shared engine, with no model in the loop."
  (:require [clojure.java.io :as io]
            [com.blockether.vis.internal.extension.core :as extension]
            [com.blockether.vis.internal.foundation.core :as foundation]
            [com.blockether.vis.internal.foundation.mpl-capture :as mpl-capture]
            [com.blockether.vis.internal.persistance.core :as ps]
            [com.blockether.vis.internal.persistance.sqlite.test-helpers :as h]
            [com.blockether.vis.internal.python.extensions :as pyx]
            [lazytest.core :refer [defdescribe expect it]])
  (:import [java.nio.file Files]
           [java.nio.file.attribute FileAttribute]))

(def ^:private svc-source
  (str
    "from __future__ import annotations\n" "from dataclasses import dataclass\n"
    "import blockether.vis.extension as vis\n" "\n"
    "\n" "@dataclass(frozen=True)\n"
    "class Browser:\n" "    \"A browser engine.\"\n"
    "    engine: str\n" "\n"
    "\n" "@dataclass(frozen=True)\n"
    "class Webview:\n" "    \"An embedded engine with the fields of a browser.\"\n"
    "    engine: str\n" "\n"
    "\n" "@dataclass(frozen=True)\n"
    "class Reservation:\n" "    \"A reserved browser.\"\n"
    "    id: str\n" "    profile: str\n"
    "    browser: Browser\n" "\n"
    "\n" "@dataclass(frozen=True)\n"
    "class Page:\n" "    \"Rows of one page.\"\n"
    "    __vis_sequence_field__ = \"rows\"\n" "    rows: tuple[Browser, ...]\n"
    "\n" "\n"
    "@dataclass(frozen=True)\n" "class Status:\n"
    "    \"An environment status.\"\n" "    env: str\n"
    "\n" "    def __str__(self):\n"
    "        return f\"STATUS: {self.env}\"\n" "\n"
    "\n" "class Svc:\n"
    "    def reserve(self, label: str, *, profile: str = \"default\") -> Reservation:\n"
    "        \"Reserve a browser.\"\n"
    "        return Reservation(f\"r-{label}\", profile, Browser(\"chromium\"))\n" "\n"
    "    def page(self, reservation: str) -> Page:\n" "        \"List the rows of one page.\"\n"
    "        return Page((Browser(\"a\"), Browser(\"b\")))\n" "\n"
    "    def status(self) -> Status:\n" "        \"Answer a model with its own text.\"\n"
    "        return Status(\"DEV04\")\n" "\n"
    "    def name(self, reservation: Reservation) -> str:\n" "        \"Name a reservation.\"\n"
    "        return f\"{type(reservation).__name__}:{reservation.id}:{reservation.browser.engine}\"\n"
    "\n"
    "    def kinds(self, engines: list[Browser | Webview]) -> str:\n"
    "        \"Name the class of each engine.\"\n"
    "        return \",\".join(type(engine).__name__ for engine in engines)\n" "\n"
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
    "from __future__ import annotations\n" "import collections.abc\n"
    "import json\n" "import blockether.vis.extension as vis\n"
    "\n" "\n"
    "def _refusal(call):\n" "    try:\n"
    "        call()\n" "    except Exception as error:\n"
    "        return f\"{type(error).__name__}: {error}\"\n" "    return \"no refusal\"\n"
    "\n" "\n"
    "class Client:\n" "    def probe(self) -> dict:\n"
    "        \"Call svc through vis.tools and describe the answers.\"\n"
    "        first = vis.tools.svc.reserve(\"x-login\", profile=\"x-com\")\n"
    "        second = vis.tools[\"svc.reserve\"](\"other\")\n"
    "        page = vis.tools.svc.page(first.id)\n"
    "        plain = vis.tools.svc.plain()\n" "        return {\n"
    "            \"class\": type(first).__name__,\n"
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
    "    def records(self) -> dict:\n"
    "        \"Describe the record rules of a vis.tools answer.\"\n"
    "        first = vis.tools.svc.reserve(\"x\")\n" "        status = vis.tools.svc.status()\n"
    "        return {\n" "            \"mapping\": isinstance(first, collections.abc.Mapping),\n"
    "            \"json\": json.dumps({\"rows\": [first]}),\n"
    "            \"key\": _refusal(lambda: first[\"session\"]),\n"
    "            \"index\": _refusal(lambda: first[0]),\n"
    "            \"text\": [str(status), repr(status)],\n"
    "            \"argument\": vis.tools.svc.name(first),\n"
    "            \"nested\": vis.tools.svc.kinds([first.browser]),\n"
    "            \"passed\": vis.tools.svc.page(first.id),\n" "        }\n"
    "\n" "    def loop(self) -> str:\n"
    "        \"Call this extension back through vis.tools.\"\n"
    "        return _refusal(lambda: vis.tools.client.probe())\n"
    "\n" "    def relay(self) -> str:\n"
    "        \"Call svc, which calls this extension back.\"\n"
    "        return _refusal(lambda: vis.tools.svc.back())\n"
    "\n" "    def unknown(self) -> str:\n"
    "        \"Call a misspelled tool.\"\n"
    "        return _refusal(lambda: vis.tools.svc.reserv(\"x\"))\n"
    "\n" "    def failing(self) -> str:\n"
    "        \"Call a tool that fails.\"\n"
    "        return _refusal(lambda: vis.tools.svc.fail())\n"
    "\n" "    def shell_handle(self) -> dict:\n"
    "        \"Run a command with the session shell tool and drive its handle.\"\n"
    "        sh = vis.tools.shell({\"command\": \"echo handle\"})\n"
    "        sh.wait(20)\n" "        slow = vis.tools.shell({\"command\": \"sleep 30\"})\n"
    "        slow.stop()\n"
    "        return {\"status\": sh[\"status\"], \"out\": sh[\"out\"], \"logs\": sh.logs()[\"out\"],\n"
    "                \"stopped\": slow[\"status\"] != \"running\"}\n" "\n"
    "    def attach_probe(self) -> dict:\n" "        \"Store an artifact and read it back.\"\n"
    "        row = vis.attach(b\"a,b\\n1,2\\n\", \"totals.csv\", label=\"Totals\")\n"
    "        return {\n"
    "            \"row\": [row[\"filename\"], row[\"version\"], row[\"media_type\"], row[\"kind\"]],\n"
    "            \"bytes\": vis.read_attachment(\"totals.csv\").decode(),\n"
    "            \"listed\": [r[\"id\"] for r in vis.list_attachments(\"totals.csv\")] == [row[\"id\"]],\n"
    "            \"same\": vis.get_attachment(row)[\"id\"] == row[\"id\"],\n"
    "        }\n" "\n"
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
  (it "answers another extension's objects as read-only records of the same name"
      (with-tools
        (fn [exts]
          (expect
            (= {"class" "Reservation"
                "same_class" true
                "id" ["r-x-login" "r-x-login"]
                "profiles" ["x-com" "default"]
                "browser" ["Browser" "chromium"]
                "frozen"
                "AttributeError: Reservation is read-only; copy it with dict(r) to change it"
                "missing" (str "AttributeError: Reservation has no field 'missing'; "
                               "available fields: id, profile, browser")
                "rows" ["Page" ["a" "b"] 2 "a"]
                "plain" [true 42 [1 2]]
                "repr"
                "Reservation(id='r-x-login', profile='x-com', browser=Browser(engine='chromium'))"}
               (dissoc (call (exts "client") 'client.probe) "op"))))))
  (it
    "answers a record that is a mapping, serializes, and travels on as an argument or a result"
    ;; Regressions #462 (JSON), #367 (a model's own `str` text) and #256 (sequence field).
    (with-tools
      (fn [exts]
        (expect
          (=
            {"mapping" true
             "json"
             "{\"rows\": [{\"id\": \"r-x\", \"profile\": \"default\", \"browser\": {\"engine\": \"chromium\"}}]}"
             "key" (str "KeyError: \"Reservation has no field 'session'; "
                        "available fields: id, profile, browser\"")
             "index" "TypeError: Reservation field name must be a string"
             "text" ["STATUS: DEV04" "Status(env='DEV04')"]
             "argument" "Reservation:r-x:chromium"
             ;; Two classes fit the fields: the name that the record carries selects one (#427).
             "nested" "Browser"
             "passed" {"__vis_object__" "Page"
                       "__vis_attrs__"
                       {"rows" [{"__vis_object__" "Browser" "__vis_attrs__" {"engine" "a"}}
                                {"__vis_object__" "Browser" "__vis_attrs__" {"engine" "b"}}]}
                       "__vis_sequence_field__" "rows"}}
            (dissoc (call (exts "client") 'client.records) "op"))))))
  (it "drives the process of a shell answer through its handle"
      ;; The handle reads `_shell_wait` and `_shell_logs`, which only `python_execution` bound.
      (foundation/register!)
      (with-tools
        (fn [exts]
          (let [active (conj (vec (vals exts))
                             (some #(when (= "foundation-core" (:ext/name %)) %)
                                   (extension/registered-extensions)))]
            (binding [extension/*current-environment* (assoc extension/*current-environment*
                                                        :extensions (atom active)
                                                        :active-extensions (atom active))]
              (expect (= {"status" "exited" "out" "handle\n" "logs" "handle\n" "stopped" true}
                         (dissoc (call (exts "client") 'client.shell_handle) "op"))))))))
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
  (it
    "names similar tools for an unknown name and raises a tool's own failure"
    (with-tools
      (fn [exts]
        (expect
          (=
            (str
              "VisToolError: vis.tools: no active tool is named `svc.reserv` in this session; "
              "similar: svc.back, svc.fail, svc.kinds, svc.name, svc.page, svc.plain, svc.reserve, svc.status")
            (call (exts "client") 'client.unknown)))
        (expect (= "VisToolError: ValueError: no browser available"
                   (call (exts "client") 'client.failing))))))
  (it "stores an extension artifact in the calling block and reads it back"
      (with-tools (fn [exts]
                    (let [sink (atom [])]
                      (expect (= {"row" ["totals.csv" 1 "text/csv" "file"]
                                  "bytes" "a,b\n1,2\n"
                                  "listed" true
                                  "same" true}
                                 (binding [mpl-capture/*attachment-sink* sink]
                                   (dissoc (call (exts "client") 'client.attach_probe) "op"))))
                      (expect (= [["totals.csv" "Totals" "user"]]
                                 (mapv (juxt :filename :label :audience) @sink)))))))
  (it "refuses a call without a bound session"
      (let [call-tool (get (pyx/host-doors nil "test" nil) "__vis_host_call_tool__")]
        (expect (= :session-not-bound
                   (try (call-tool "svc.reserve" [] {})
                        (catch clojure.lang.ExceptionInfo e (:error (ex-data e)))))))))
