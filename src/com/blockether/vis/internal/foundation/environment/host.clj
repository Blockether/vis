(ns com.blockether.vis.internal.foundation.environment.host
  "Host-side facts read from JDK system properties and process
   environment variables.

   No I/O, no shell-out, no third-party deps. Cheap to compute, safe
   to call from any thread, never throws."
  (:require [clojure.string :as string])
  (:import (java.time ZoneId ZonedDateTime)
           (java.time.format DateTimeFormatter)))

(defn- system-property ^String [^String key] (System/getProperty key))

(defn- system-getenv ^String [^String key] (System/getenv key))

(defn snapshot
  "Return a map of host facts captured from the running JVM:

   {:cwd         absolute working directory at JVM start
    :user        OS username
    :home        user's home directory
    :os-name     long os name (\"Mac OS X\")
    :os-version  os release version
    :os-arch     architecture (\"aarch64\", \"amd64\", ...)
    :shell       $SHELL or \"unknown\"
    :jvm         \"<vendor> <version>\"
    :locale      default locale tag (\"en-US\")
    :time        current local date+time with zone offset (ISO-8601)
    :timezone    current JVM zone id}

   Keys use hyphenated names (`:os-name`, not `:os.name`) so callers
   can `:keys`-destructure without hitting the JVM's restriction on
   dots in local-binding identifiers."
  []
  (let
    [cwd
     (system-property "user.dir")

     user
     (system-property "user.name")

     home
     (system-property "user.home")

     os-name
     (system-property "os.name")

     os-version
     (system-property "os.version")

     os-arch
     (system-property "os.arch")

     shell
     (or (system-getenv "SHELL") "unknown")

     jvm-name
     (system-property "java.vm.name")

     jvm-ver
     (system-property "java.version")

     locale
     (.toLanguageTag (java.util.Locale/getDefault))

     zone
     (ZoneId/systemDefault)

     now
     (ZonedDateTime/now zone)]

    {:cwd cwd
     :user user
     :home home
     :os-name os-name
     :os-version os-version
     :os-arch os-arch
     :shell shell
     :jvm (string/trim (str (or jvm-name "") " " (or jvm-ver "")))
     :locale locale
     :time (.format DateTimeFormatter/ISO_OFFSET_DATE_TIME now)
     :timezone (str zone)}))
