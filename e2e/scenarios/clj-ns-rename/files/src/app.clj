(ns app
  (:require [foo.bar :as fb]))

(defn run [] (foo.bar/h 1))

(defn run2 [] (fb/h 2))
