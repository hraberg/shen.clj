(ns shen.primitives
  (:require [clojure.string :as string])
  (:refer-clojure :exclude [set intern let pr type cond cons]))

; Probably handle dynamic currying in here, and define primitves using it.
; Also: Lambda; TCO? And "Kl follows a dual namespace model"

(defmacro defun [F X & Y]
  (clojure.core/let [F (if (list? F) (eval F) F)]
    `(defn ~F
       ~@(for [p# (map #(take % X) (range 1 (count X)))]
           `(~(vec p#) (partial ~F ~@p#)))
       (~(vec X) (do ~@Y)))))

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

(declare absvector?)

(defn cons [X Y]
  (condp some [Y]
    seq? (clojure.core/cons X Y)
    absvector? (clojure.core/to-array (clojure.core/cons X Y))))

(defn hd [X] (first X))

(defn tl [X] (rest X))

(defn cons? [X]
  (or (absvector? X)
      (and (seq? X) (not (empty? X)))))

(defn intern [String]
  (clojure.core/intern (find-ns 'shen)
                       (shen-symbol String))
  (shen-symbol String))

(defn eval-without-macros [X]
  (eval X))

(defmacro lambda [X Y]
  `(fn [~X] ~Y))

(defmacro let [X Y Z]
  (clojure.core/let [X-safe (if (list? X) (gensym (eval X)) X)
                     Z (if (list? X) (clojure.walk/postwalk
                                      #(if (= X %) X-safe %) Z) Z)]
                    `(clojure.core/let [~X-safe ~Y] ~Z)))

(defn equal? [X Y] (= X Y))

(defmacro freeze [X]
  `(fn [] ~X))

(defn thaw [X]
  (X))

(defn absvector [N]
  (object-array N))

(defn absvector? [X]
  (-> X clojure.core/type .isArray))

(defn address-> [Vector N Value]
  (aset Vector N Value)
  Vector)

(defn <-address [Vector N]
  (aget Vector N))

(defn n->string [N]
  (str (char N)))

(defn pr [X S]
  (binding [*out* S]
    (clojure.core/println X)))

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

(defn multiply [X Y]
  (* (double X) (double Y)))

(defn add [X Y]
  (+ (double X) (double Y)))

(defn subtract [X Y]
  (- (double X) (double Y)))

(defn divide [X Y]
  (/ (double X) (double Y)))

(defn greater? [X Y]
  (> X Y))

(defn less? [X Y]
  (< X Y))

(defn less? [X Y]
  (>= X Y))

(defn less-than-or-equal-to? [X Y]
  (<= X Y))

(def ^:dynamic *stinput* *in*)
