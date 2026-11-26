(ns com.blockether.vis.internal.foundation.mcp.apps-test
  "MCP Apps host conformance (extension `io.modelcontextprotocol/ui`, revision
   2026-01-26): tool visibility, view resources, the Content Security Policy, the
   app envelope that a tool call records, and the requests that a view may send."
  (:require [charred.api :as json]
            [clojure.string :as str]
            [com.blockether.vis.contract.document :as document]
            [com.blockether.vis.internal.config.scoped :as scoped]
            [com.blockether.vis.internal.foundation.mcp.apps :as apps]
            [com.blockether.vis.internal.foundation.mcp.client :as client]
            [com.blockether.vis.internal.foundation.mcp.core :as mcp]
            [com.blockether.vis.internal.foundation.mpl-capture :as mpl-capture]
            [lazytest.core :refer [defdescribe expect it]])
  (:import (java.nio.charset StandardCharsets)
           (java.util Base64)))

(def ^:private view-uri "ui://acme/deployments.html")

(def ^:private html "<!doctype html><html><head></head><body>Deployments</body></html>")

(def ^:private tools
  [{"name" "list_deployments"
    "inputSchema" {"type" "object"}
    "_meta" {"ui" {"resourceUri" view-uri}}}
   {"name" "promote" "inputSchema" {"type" "object"} "_meta" {"ui" {"visibility" ["app"]}}}
   {"name" "audit" "inputSchema" {"type" "object"} "_meta" {"ui" {"visibility" ["model"]}}}])

(defn- read-result
  ([] (read-result {}))
  ([meta-ui]
   {"contents" [{"uri" view-uri "mimeType" apps/mime-type "text" html "_meta" {"ui" meta-ui}}]}))

(defn- b64 [^String s] (.encodeToString (Base64/getEncoder) (.getBytes s StandardCharsets/UTF_8)))

(defn- decoded [^String s] (String. (.decode (Base64/getDecoder) s) StandardCharsets/UTF_8))

(defmacro ^:private with-server
  "Run `body` with one fake connected server `acme` that has `tools`. `calls`
   collects each tools/call and each request that reaches the server."
  [calls & body]
  `(with-redefs [scoped/resource-enabled? (constantly true)
                 mcp/ensure-connected! (fn [& _#] {:spec {}})
                 client/list-tools (constantly tools)
                 client/call-tool (fn [_# tool# args# & _#]
                                    (swap! ~calls conj ["tools/call" tool# args#])
                                    {"content" [{"type" "text" "text" "2 deployments"}]
                                     "structuredContent" {"count" 2}})
                 client/request (fn [_# method# params# & _#]
                                  (swap! ~calls conj [method# params#])
                                  (read-result {"csp" {"connectDomains" ["https://api.acme.example"]}
                                                "permissions" {"clipboardWrite" {}}
                                                "prefersBorder" true}))]
     ~@body))

(defdescribe tool-visibility-test
  (it "lets the model and the app call a tool without a declared visibility"
      (expect (apps/model-visible? {"name" "x"}))
      (expect (apps/app-callable? {"name" "x"})))
  (it "keeps an app-only tool away from the model and a model-only tool away from the app"
      (expect (not (apps/model-visible? (second tools))))
      (expect (apps/app-callable? (second tools)))
      (expect (apps/model-visible? (nth tools 2)))
      (expect (not (apps/app-callable? (nth tools 2)))))
  (it "reads the view of a tool from the nested and the deprecated flat key"
      (expect (= view-uri (apps/resource-uri (first tools))))
      (expect (= view-uri (apps/resource-uri {"_meta" {"ui/resourceUri" view-uri}})))
      (expect (nil? (apps/resource-uri {"_meta" {"ui" {"resourceUri" "https://acme.example/x"}}})))))

(defdescribe view-resource-test
  (it "takes the HTML from text or from a base64 blob"
      (expect (= html (get (apps/view-resource view-uri (read-result)) "html")))
      (expect (= html
                 (get (apps/view-resource view-uri
                                          {"contents" [{"uri" view-uri
                                                        "mimeType" apps/mime-type
                                                        "blob" (b64 html)}]})
                      "html"))))
  (it "refuses a resource that is not an MCP App view"
      (doseq [[uri result] [["https://acme.example/x" (read-result)]
                            [view-uri {"contents" [{"uri" view-uri "mimeType" "text/html" "text" html}]}]
                            [view-uri {"contents" []}]]]
        (expect (= :mcp-app/invalid-resource
                   (try (apps/view-resource uri result) nil
                        (catch clojure.lang.ExceptionInfo e (:type (ex-data e))))))))
  (it "keeps only the permissions that the host knows"
      (expect (= {"camera" {} "clipboardWrite" {}}
                 (get (apps/view-resource view-uri
                                          (read-result {"permissions" {"camera" {}
                                                                       "clipboardWrite" {}
                                                                       "usb" {}}}))
                      "permissions")))))

(defn- directive
  [policy name]
  (some #(when (str/starts-with? % (str name " ")) %) (str/split policy #"; ")))

(defdescribe content-security-policy-test
  (it "allows no network, no frame and no object when the view declares nothing"
      (let [policy (apps/content-security-policy (apps/declared-csp nil))]
        (expect (= "default-src 'none'" (directive policy "default-src")))
        (expect (= "connect-src 'none'" (directive policy "connect-src")))
        (expect (= "frame-src 'none'" (directive policy "frame-src")))
        (expect (= "object-src 'none'" (directive policy "object-src")))
        (expect (= "base-uri 'self'" (directive policy "base-uri")))
        (expect (= "img-src 'self' data:" (directive policy "img-src")))))
  (it "puts each declared domain in its directives"
      (let [policy (apps/content-security-policy
                     (apps/declared-csp {"connectDomains" ["https://api.acme.example"]
                                         "resourceDomains" ["https://cdn.acme.example"]
                                         "frameDomains" ["https://*.youtube.com"]
                                         "baseUriDomains" ["https://acme.example"]}))]
        (expect (= "connect-src https://api.acme.example" (directive policy "connect-src")))
        (expect (= "script-src 'self' 'unsafe-inline' https://cdn.acme.example"
                   (directive policy "script-src")))
        (expect (= "font-src 'self' https://cdn.acme.example" (directive policy "font-src")))
        (expect (= "frame-src https://*.youtube.com" (directive policy "frame-src")))
        (expect (= "base-uri https://acme.example" (directive policy "base-uri")))))
  (it "drops a declared value that would widen the policy beyond a domain"
      (expect (= {"connectDomains" ["https://ok.example"]}
                 (apps/declared-csp {"connectDomains" ["https://ok.example"
                                                       "https://a.example; script-src *"
                                                       "'unsafe-eval'"
                                                       "*"
                                                       "https://b.example 'unsafe-inline'"
                                                       42]})))))

(defdescribe app-from-a-tool-call-test
  (it "records the app envelope as a user attachment of the block and names it in the result"
      (let [calls (atom [])
            sink (atom [])]
        (with-server calls
                     (binding [mpl-capture/*attachment-sink* sink]
                       (let [r (#'mcp/mcp-call-impl {:session-id "s1"}
                                                    "acme"
                                                    "list_deployments"
                                                    {"env" "prod"})
                             app (get-in r [:result "app"])
                             [att] @sink
                             envelope (json/read-json (decoded (:base64 att)))]
                         (expect (= [["tools/call" "list_deployments" {"env" "prod"}]
                                     ["resources/read" {"uri" view-uri}]]
                                    @calls))
                         (expect (= {"resource_uri" view-uri "attachment_id" (:id att)} app))
                         (expect (= apps/envelope-media-type (:media-type att)))
                         (expect (= "user" (:audience att)))
                         (expect (nil? (document/explain-json "gateway" "mcp_app_envelope" envelope)))
                         (expect (= {"env" "prod"} (get envelope "arguments")))
                         (expect (= {"count" 2} (get-in envelope ["result" "structuredContent"])))
                         (expect (str/includes? (get envelope "content_security_policy")
                                                "connect-src https://api.acme.example"))
                         (expect (= "clipboard-write" (get envelope "allow"))))))))
  (it "keeps the tool result when the view cannot open"
      (let [sink (atom [])]
        (with-redefs [scoped/resource-enabled? (constantly true)
                      mcp/ensure-connected! (fn [& _] {:spec {}})
                      client/list-tools (constantly tools)
                      client/call-tool (fn [& _] {"content" [{"type" "text" "text" "ok"}]})
                      client/request (fn [& _] {"contents" [{"uri" view-uri "mimeType" "text/plain" "text" "x"}]})]
          (binding [mpl-capture/*attachment-sink* sink]
            (let [r (#'mcp/mcp-call-impl {:session-id "s1"} "acme" "list_deployments" {})]
              (expect (= "ok" (get-in r [:result "content" 0 "text"])))
              (expect (= view-uri (get-in r [:result "app" "resource_uri"])))
              (expect (string? (get-in r [:result "app" "error"])))
              (expect (empty? @sink))))))))

(defdescribe model-sees-no-app-only-tool-test
  (it "lists and calls only the tools whose visibility has model"
      (let [calls (atom [])]
        (with-server calls
                     (let [catalog (#'mcp/mcp-call-impl {:session-id "s1"} "acme")
                           refused (#'mcp/mcp-call-impl {:session-id "s1"} "acme" "promote" {})]
                       (expect (= ["list_deployments" "audit"]
                                  (mapv #(get % "name") (get-in catalog [:result "tools"]))))
                       (expect (false? (:success? refused)))
                       (expect (empty? @calls)))))))

(defdescribe app-request-test
  (it "sends a view's call of an app tool to its server"
      (let [calls (atom [])]
        (with-server calls
                     (expect (= {"count" 2}
                                (get (mcp/app-request nil "s1" "acme" "tools/call" {"name" "promote" "arguments" {"id" "d1"}})
                                     "structuredContent")))
                     (expect (= [["tools/call" "promote" {"id" "d1"}]] @calls)))))
  (it "refuses a model-only tool, an unknown tool and a request outside MCP Apps"
      (let [calls (atom [])]
        (with-server calls
                     (doseq [[method params] [["tools/call" {"name" "audit"}]
                                              ["tools/call" {"name" "nope"}]
                                              ["prompts/get" {"name" "x"}]
                                              ["sampling/createMessage" {}]]]
                       (expect (= 403
                                  (try (mcp/app-request nil "s1" "acme" method params) nil
                                       (catch clojure.lang.ExceptionInfo e (:status (ex-data e)))))))
                     (expect (empty? @calls)))))
  (it "lists only the tools that a view may call"
      (let [calls (atom [])]
        (with-server calls
                     (expect (= ["list_deployments" "promote"]
                                (mapv #(get % "name") (get (mcp/app-request nil "s1" "acme" "tools/list" {}) "tools"))))))))
