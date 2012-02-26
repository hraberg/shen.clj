(ns shen.primitives
  (:require [clojure.core :as core])
  (:require [clojure.string :as string])
  (:require [clojure.walk :as walk])
  (:refer-clojure :exclude [set intern let pr type cond cons number? string? str + - * / > < >= <= = or and])
  (:gen-class))

(defmacro defun [F X & Y]
  (core/let [F (if (seq? F) (eval F) F)]
    `(defn ^:dynamic ~F
       ~@(for [p# (map #(take % X) (range 1 (count X)))]
           `(~(vec p#) (partial ~F ~@p#)))
       (~(vec X) ~@Y))))

(def number? core/number?)
(def string? core/string?)
(defn str [& X]
  (when (= (last X) ()) (throw (RuntimeException.)))
  (apply core/str X))

(doseq [op '[+ - * / > < >= <= =]
        :let [real-op (symbol "clojure.core" (name op))]]
  (eval `(defun ~op ~'[X Y] (~real-op ~'X ~'Y))))


(defn alias-vars [ns-map target-ns]
  (doseq [[k v] ns-map]
    (do
      (core/intern target-ns k v)
      (alter-meta! (find-var (symbol (name target-ns) (name k)))
                   merge (meta v)))))

(alias-vars (select-keys (ns-map 'clojure.core) '[and or]) 'shen.primitives)

(defn ^:private interned? [X]
  (and (seq? X) (= 'shen-symbol (first X))))

(def safe-tail-call '#{shen-reverse_help shen-read-file-as-bytelist-help})

(defn shen-kl-to-clj
  ([clj] (shen-kl-to-clj clj #{}))
  ([clj scope]
     (condp some [clj]
       scope (if (interned? clj) (list 'value clj)
                 clj)
       symbol? (condp = (name clj)
                 "true" true
                 "false" false
                 "nil" nil
                 (list 'quote clj))
       (every-pred
        seq?
        not-empty) (core/let [[fst snd trd & rst] clj
                              scope (condp get fst
                                      '#{defun} (into (conj scope snd) trd)
                                      '#{let lambda} (conj scope snd)
                                      scope)
                              fst (condp some [fst]
                                    (some-fn
                                     interned?
                                     scope) (if (safe-tail-call fst)
                                              'recur
                                              (list 'value fst))
                                     seq? (shen-kl-to-clj fst scope)
                                     fst)
                              snd (condp get fst
                                    '#{defun let lambda} snd
                                    (shen-kl-to-clj snd scope))
                              trd (if ('#{defun} fst) trd
                                      (shen-kl-to-clj trd scope))]
                             (take-while (complement nil?)
                                         (concat [fst snd trd]
                                                 (core/map #(shen-kl-to-clj % scope) rst))))
       clj)))

(defmacro cond [& CS]
  `(core/cond ~@(apply concat CS)))

(defn shen-symbol [X]
  (core/let [s (name X)]
    (symbol (if (= "/" s) s
                (string/replace s "/" "-slash-")))))

(defn set [X Y]
  (core/let [s (shen-symbol X)]
            @(core/intern (find-ns 'shen)
                          (shen-symbol X)
                          Y)))

(defn value [X]
  (if-let [v (and (symbol? X) (ns-resolve 'shen (shen-symbol X)))]
    @v
    X))

(defn simple-error [String]
  (throw (RuntimeException. String)))

(defmacro trap-error [X F]
  `(try
     ~X
     (catch Throwable _#
       (~F _#))))

(defn error-to-string [E]
  (if (instance? Throwable E)
    (with-out-str
      (.printStackTrace E))
    (throw (IllegalArgumentException. (str E " is not an exception")))))

(declare absvector? cons?)

(defn cons [X Y]
  (if (or (cons? Y) (seq? Y))
    (core/cons X Y)
    (list X Y)))

(defn hd [X] (first X))

(defn tl [X] (rest X))

(defn cons? [X]
  (and (seq? X) (not (empty? X))))

(defn intern [String]
  (core/let [s (shen-symbol String)]
;            (core/intern (find-ns 'shen) s)
            s))

(defn- shen-elim-define [X]
  (if (seq? X)
    (if ('#{define} (first X)) (core/let [KL ((value 'shen-shen->kl)
                                              (second X)
                                              (drop 2 X))
                                          F (shen-kl-to-clj KL)]
                                         (binding [*ns* (find-ns 'shen)]
                                           (eval F)))
        (map shen-elim-define X))
    X))

(defn eval-without-macros [X]
  (core/let [kl (shen-kl-to-clj (shen-elim-define X))]
            (binding [*ns* (find-ns 'shen)]
              (println kl (core/type kl))
              (eval kl))))

(defmacro lambda [X Y]
  `(fn [~X] ~Y))

(defmacro let [X Y Z]
  (core/let [X-safe (if (seq? X) (gensym (eval X)) X)
             Z (if (seq? X) (walk/postwalk
                             #(if (= X %) X-safe %) Z) Z)]
            `(core/let [~X-safe ~Y]
                       ~Z)))

(defmacro freeze [X]
  `(fn [] ~X))

(defn thaw [X]
  (X))

(defn absvector [N]
  (doto (make-array Object N) (java.util.Arrays/fill 'fail!)))

(defn absvector? [X]
  (if (nil? X)
    false
    (-> X core/type .isArray)))

(def shen-absarray? absvector?)

(defn address-> [Vector N Value]
  (core/aset Vector N Value)
  Vector)

(defn <-address [Vector N]
  (core/aget Vector N))

(defn n->string [N]
  (str (char N)))

(def byte->string n->string)

(defn pr [X S]
  (binding [*out* (if (= *in* S) *out*
                      S)]
    (core/print X)
    (flush)
    X))

(defn read-byte [S]
  (.read S))

(defn open [Type String Direction]
  (core/let [Path (clojure.java.io/file (value '*home-directory*) String)]
    (core/condp = Direction
     'in (clojure.java.io/input-stream Path)
     'out (clojure.java.io/output-stream Path)
     (throw (IllegalArgumentException. "invalid direction")))))

(defn type [X MyType]
  (cast MyType X))

(defn close [Stream]
  (.close Stream))

(defn pos [X N]
  (str (get X N)))

(defn tlstr [X]
  (subs X 1))

(defn cn [Str1 Str2]
  (if (string? Str1)
    (str Str1 Str2)
    (concat Str1 Str2)))

(def ^:private internal-start-time (System/currentTimeMillis))

(defn get-time [Time]
   (if (= Time 'run)
       (* 1.0 (/ (- (System/currentTimeMillis) internal-start-time)
                 1000))
       (throw (IllegalArgumentException.
               (str "get-time does not understand the parameter " Time)))))

(defmethod print-method (core/type (object-array 1)) [o ^java.io.Writer w]
  (print-method (vec o) w))

