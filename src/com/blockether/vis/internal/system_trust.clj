(ns com.blockether.vis.internal.system-trust
  "Runtime TLS trust merged from a launcher-resolved PEM certificate bundle.

   The native image and JVM both retain their configured default roots. A PEM
   bundle discovered by `bin/vis-agent` is an additional trust source, which is
   required on WSL because Windows and the Linux distribution have independent
   certificate stores."
  (:require [clojure.string :as str])
  (:import [com.blockether.vispython Trust]))

(defn trust-manager-for-pem
  "Return the runtime-owned trust manager combining defaults and host PEM."
  [path]
  (Trust/managerForPem path))

(defn ssl-context-for-pem
  "Return the runtime-owned TLS context combining defaults and host PEM."
  [path]
  (Trust/contextForPem path))

(defn install!
  "Install host trust once at startup, shared by HTTPS and pip. Never pass a session CA."
  ([] (install! (System/getenv "VIS_SYSTEM_CA_CERT")))
  ([path] (when-not (str/blank? path) (Trust/installPem path)) path))
