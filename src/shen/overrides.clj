(clojure.core/ns shen
  (:refer-clojure :only [])
  (:use [shen.primitives])
  (:require [clojure.core :as core]))

(def ^:dynamic *language* "Clojure")
(def ^:dynamic *implementation* (core/str "Clojure " (core/clojure-version)
                                          " [jvm "(System/getProperty "java.version")"]"))
(def ^:dynamic *port* "0.1.0-SNAPSHOT")
(def ^:dynamic *porters* "Håkan Råberg")

(def ^:dynamic *stinput* core/*in*)
(def ^:dynamic *home-directory* (System/getProperty "user.dir"))

(defun
 (intern "@p")
 (V706 V707)
 (core/object-array ['shen-tuple V706 V707]))

(defun
 variable?
 (V702)
 (and (core/symbol? V702) (Character/isUpperCase (.charAt (core/name V702) 0))))

(defun
  macroexpand
  (V510)
  (let
      Y
    (shen-compose (core/filter core/fn? (core/map value (value '*macros*))) V510)
    (if (= V510 Y) V510 (shen-walk macroexpand Y))))

(core/defn -main []
  (shen-shen))