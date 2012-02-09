(ns shen.primitives
  (:require [clojure.string :as string])
  (:require [clojure.walk])
  (:refer-clojure :exclude [set intern let pr type cond cons]))

(defmacro defun [F X & Y]
  (clojure.core/let [F (if (list? F) (eval F) F)]
    `(defn ~F
       ~@(for [p# (map #(take % X) (range 1 (count X)))]
           `(~(vec p#) (partial ~F ~@p#)))
       (~(vec X) ~@Y))))

(defmacro cond [& CS]
  `(clojure.core/cond ~@(apply concat CS)))

(defn- shen-symbol [X]
  (symbol (string/replace (name X) "/" "-slash-")))

(defn set [X Y]
  (clojure.core/intern (find-ns 'shen)
                       (shen-symbol X)
                       Y)
  Y)

(defn value [X]
  @(clojure.core/intern (find-ns 'shen) (shen-symbol X)))

(defn simple-error [String]
  (throw (RuntimeException. String)))

(defmacro trap-error [X F]
  `(try
     ~X
     (catch Throwable _#
       (.printStackTrace _#)
       (~F _#))))

(defn error-to-string [E]
  (if (instance? Throwable E)
    (str E)
    (throw (IllegalArgumentException. (str E " is not an exception")))))

(declare absvector? cons?)

(defn cons [X Y]
  (if (or (cons? Y) (list? Y))
    (clojure.core/cons X Y)
    (list X Y)))

(defn hd [X] (first X))

(defn tl [X] (rest X))

(defn cons? [X]
  (or (and (absvector? X) (not (empty? X)))
      (and (seq? X) (not (empty? X)))))

(defn intern [String]
  (clojure.core/intern (find-ns 'shen)
                       (shen-symbol String))
  (shen-symbol String))

(defn- shen-elim-define [X]
  (clojure.core/cond
   (and (seq? X)
        (= (first X) 'define)) (clojure.core/let [F ((value 'shen-shen->kl)
                                                     (second X)
                                                     (drop 2 X))]
                                                 (prn F)
                                                 F)
        (seq? X) (doall (map shen-elim-define X))
        :else X))

(defn eval-without-macros [X]
  (prn X)
  (clojure.core/let [kl (shen-elim-define X)]
                     (prn kl)
                     (eval kl)))

(defmacro lambda [X Y]
  `(fn [~X] ~Y))

(defmacro let [X Y Z]
  (clojure.core/let [X-safe (if (list? X) (gensym (eval X)) X)
                     Z (if (list? X) (clojure.walk/postwalk
                                      #(if (= X %) X-safe %) Z) Z)]
                    `(clojure.core/let [~X-safe ~Y] ~Z)))

(defmacro freeze [X]
  `(fn [] ~X))

(defn thaw [X]
  (X))

(defn absvector [N]
  (doto (object-array N) (java.util.Arrays/fill ())))

(defn absvector? [X]
  (if (nil? X)
    false
    (-> X clojure.core/type .isArray)))

(def shen-absarray? absvector?)

(defn address-> [Vector N Value]
  (aset Vector N Value)
  Vector)

(defn <-address [Vector N]
  (aget Vector N))

(defn n->string [N]
  (str (char N)))

(def byte->string n->string)

(defn pr [X S]
  (binding [*out* (if (or (= *in* S)
                          (instance? clojure.lang.LineNumberingPushbackReader S))
                    *out* S)]
    (print X)
    (flush)
    X))

(defn read-byte [S]
  (.read S))

(defn open [Type String Direction]
  (clojure.core/let [Path (clojure.java.io/file (resolve 'shen/*home-directory*) String)]
    (clojure.core/cond
     (= 'in Direction) (clojure.java.io/input-stream Path)
     (= 'out Direction) (clojure.java.io/output-stream Path)
     :else (throw (IllegalArgumentException. "invalid direction")))))

(defn type [X MyType]
  (cast MyType X))

(defn close [Stream]
  (.close Stream))

(defn pos [X N]
  (str (get X N)))

(defn tlstr [X]
  (subs X 1))

(defn cn [Str1 Str2]
  (str Str1 Str2))

(def internal-start-time (System/currentTimeMillis))

(defn get-time [Time]
   (if (= Time 'run)
       (* 1.0 (/ (- (System/currentTimeMillis) internal-start-time)
                 1000))
       (throw (IllegalArgumentException.
               (str "get-time does not understand the parameter " Time)))))
