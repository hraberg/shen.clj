(ns shen.primitives
  (:require [clojure.core :as core]
            [clojure.set :as set]
            [clojure.string :as string]
            [clojure.walk :as walk]
            [clojure.java.io :as io])
  (:refer-clojure :exclude [set intern let pr type cond cons str number? string? defmacro
                            + - * / > < >= <= = and or])
  (:import [java.io Reader Writer InputStream]
           [java.util Arrays])
  (:gen-class))

(def string? core/string?)
(def number? core/number?)

(core/defmacro and
  ([x] `(fn [y#] (core/and ~x y#)))
  ([x & xs] `(core/and ~x ~@xs)))

(core/defmacro or
  ([x] `(fn [y#] (core/or ~x y#)))
  ([x & xs] `(core/or ~x ~@xs)))

(defn ^:private and-fn
  ([x] (and x))
  ([x y] (and x y)))

(defn ^:private or-fn
  ([x] (or x))
  ([x y] (or x y)))

(defn ^:private partials [name parameters]
  (for [p (map #(take % parameters) (range 1 (count parameters)))]
    `(~(vec p) (partial ~name ~@p))))

(core/defmacro defun [F X & Y]
  (core/let [F (if (seq? F) (eval F) F)]
            `(do
               (defn ^:dynamic ~F
                 ~@(partials F X)
                 (~(vec X) ~@Y))
               ~F)))

(defn /
  ([X] (partial / X))
  ([X Y] (core/let [r (clojure.core// X Y)]
                   (if (ratio? r) (double r) r))))

(def ^:private array-class (Class/forName "[Ljava.lang.Object;"))

(defn =
  ([X] (partial = X))
  ([X Y]
     (if (and (identical? array-class (class X))
              (identical? array-class (class Y)))               ;
       (Arrays/equals #^"[Ljava.lang.Object;" X #^"[Ljava.lang.Object;" Y)
       (core/= X Y))))

(doseq [op '[> < >= <= + - *]
        :let [real-op (symbol "clojure.core" (name op))]]
  (eval `(defun ~op ~'[X Y] (~real-op ~'X ~'Y))))

(defn ^:private interned? [X]
  (and (seq? X) (= 'intern (first X))))

(def ^:private slash-dot (symbol "/."))

(defn ^:private recur?
  ([path] (partial recur? path))
  ([path fn]
     (or (= 'cond (last (drop-last path)))
         (set/superset? '#{defun cond if do let}
                        (core/set path)))))

(defn ^:private maybe-apply [kl path]
  (if (= 'cond (last path)) kl
      (list 'function kl)))

(defn shen-kl-to-clj
  ([kl] (shen-kl-to-clj kl #{} [] :unknown))
  ([kl scope] (shen-kl-to-clj kl scope [] :no-recur))
  ([kl scope path fn]
     (condp some [kl]
       scope kl
       symbol? (condp = (name kl)
                 "true" true
                 "false" false
                 (list 'quote kl))
       seq? (core/let [[fst snd trd & rst] kl
                       fn (if ('#{defun} fst) snd
                              fn)
                       scope (condp get fst
                               '#{defun} (into scope trd)
                               '#{let lambda} (conj scope snd)
                               scope)
                       fst (condp some [fst]
                             (every-pred
                              #{fn}
                              (recur? path)) 'recur
                             (some-fn
                              interned?
                              scope) (maybe-apply fst path)
                              seq? (maybe-apply (shen-kl-to-clj fst scope) path)
                              (if (= 'cond (last path))
                                (shen-kl-to-clj fst scope)
                                fst))
                       path (conj path fst)
                       snd (condp get fst
                             '#{defun let lambda} snd
                             '#{if} (shen-kl-to-clj snd scope)
                             (shen-kl-to-clj snd scope path fn))
                       trd (condp get fst
                             '#{defun} trd
                             '#{let} (shen-kl-to-clj trd scope)
                             (shen-kl-to-clj trd scope path fn))]
                      (take-while (complement nil?)
                                  (concat [fst snd trd]
                                          (map #(shen-kl-to-clj % scope path fn) rst))))
       kl)))

(defn intern [String]
  (symbol (condp = String
            "/" "/"
            "/." slash-dot
            (string/replace String "/" "-slash-"))))

(core/defmacro cond [& CS]
  `(core/cond ~@(apply concat CS)))

(defn set
  ([X] (partial set X))
  ([X Y]
     @(core/intern (the-ns 'shen)
                   (with-meta X {:dynamic true :declared true})
                   Y)))

(defn value [X]
  (core/let [v (and (symbol? X) (ns-resolve 'shen X))]
            (condp = X
              'and and-fn
              'or or-fn
              @v)))

(defn function [fn]
  (if (fn? fn) fn
      (value fn)))

(defn simple-error [String]
  (throw (RuntimeException. ^String String)))

(core/defmacro trap-error [X F]
  `(try
     ~X
     (catch Exception e#
       (~F e#))))

(defn error-to-string [E]
  (if (instance? Throwable E)
    (or (.getMessage ^Throwable E) (core/str E))
    (throw (IllegalArgumentException. ^String (core/str E " is not an exception")))))

(defn ^:private pair [X Y] [X Y])

(defn ^:private pair? [X]
  (and (vector? X) (= 2 (count X))))

(defn cons [X Y]
  (if (and (coll? Y)
           (not (pair? Y)))
    (core/cons X Y)
    (pair X Y)))

(defn hd [X] (first X))

(defn tl [X]
  (if (pair? X)
    (second X)
    (rest X)))

; is this safe?
(defn fail! [] (assert false))

(defn cons? [X]
  (and (coll? X) (not (empty? X))))

(defn str [X]
  (if-not (coll? X) (core/pr-str X)
          (throw (IllegalArgumentException.
                  (core/str X " is not an atom; str cannot convert it to a string.")))))

(defn ^:private vec-to-cons [[fst & rst]]
  (if fst (list 'cons fst (vec-to-cons rst))
      ()))

(defn ^:private cleanup-clj [clj]
  (condp some [clj]
    vector? (recur (vec-to-cons clj))
    coll? (if ('#{clojure.core/deref} (first clj))
            (symbol (core/str "@" (second clj)))
            clj)
    '#{λ} slash-dot
    char? (intern clj)
    clj))

(defn ^:private define* [name body]
  (core/let [kl ((value 'shen-shen->kl) name body)]
            (binding [*ns* (the-ns 'shen)]
              ((value 'eval) kl)
              name)))

(defn ^:private shen-elim-define [X]
  (if (seq? X)
    (if ('#{define} (first X)) (define* (second X) (drop 2 X))
        (map shen-elim-define X))
    X))

(defn eval-shen* [body]
  (core/let [body (walk/postwalk cleanup-clj body)]
            (binding [*ns* (the-ns 'shen)]
              (->> body
                   (map (value 'eval))
                   last))))

(core/defmacro eval-shen [& body]
  `(eval-shen* '~body))

(core/defmacro 神 [& body]
  `(eval-shen ~@body))

(core/defmacro define [name & body]
  `(core/let [fn# (eval-shen ~(concat ['define name] body))]
             (defn ~(with-meta name {:dynamic true})
               [& ~'args] (apply (value fn#) ~'args))))

(doseq [[name args] '{defmacro [name] defprolog [name] prolog? [] package [name exceptions]}]
  (eval
   `(core/defmacro ~name [~@args & ~'body]
      `(eval-shen ~(concat ['~name ~@args] ~'body)))))

(def ^:private missing-symbol-pattern #"Unable to resolve symbol: (.+) in this context")

(defn ^:private missing-symbol [s]
  (when-let [[_ sym] (re-find missing-symbol-pattern (or s ""))] sym))


(defn ^:private fn-to-symbol [fn]
  (-> fn class .getName
      (string/replace "_" "-")
      (string/split #"\$")
      last symbol))

(defn ^:private cleanup-return [x]
  (or (when (fn? x)
        (core/let [name (fn-to-symbol x)]
                  (when (fn? (ns-resolve 'shen name)) name)))
      x))

(defn ^:private eval-and-declare-missing [kl]
  (binding [*ns* (the-ns 'shen)]
    (try
      (cleanup-return (eval kl))
      (catch RuntimeException e
        (if-let [s (missing-symbol (.getMessage e))]
          (do
            (set (symbol s) nil)
            (eval-and-declare-missing kl))
          (throw e))))))

(defn eval-without-macros [X]
  (core/let [kl (shen-kl-to-clj (shen-elim-define (cleanup-clj X)))]
            (eval-and-declare-missing kl)))

(core/defmacro lambda [X Y]
  `(fn [~X & XS#] (core/let [result# ~Y]
                            (if XS# (apply result# XS#)
                                result#))))

(core/defmacro λ [X Y]
  `(lambda ~X ~Y))

(core/defmacro let [X Y Z]
  (core/let [X-safe (if (seq? X) (gensym (eval X)) X)
             Z (if (seq? X) (walk/postwalk
                             #(if (= X %) X-safe %) Z) Z)]
            `(core/let [~X-safe ~Y]
                       ~Z)))

(core/defmacro freeze [X]
 `(fn [] ~X))

(defn thaw [X] (X))

(defn absvector [N]
  (doto (object-array (int N)) (Arrays/fill 'fail!)))

(defn absvector? [X]
  (if-not X
    false
    (identical? array-class (core/class X))))

(defn <-address [#^"[Ljava.lang.Object;" Vector N]
  (aget Vector (int N)))

(defn address-> [#^"[Ljava.lang.Object;" Vector N Value]
  (aset Vector (int N) Value)
  Vector)

(defn n->string [N]
  (core/str (char N)))

(defn string->n [S]
  (core/int (first S)))

(def byte->string n->string)

(defn pr [X S]
  (binding [*out* (if (= *in* S) *out*
                      S)]
    (print X)
    (flush)
    X))

(defmulti read-byte class)

(defmethod read-byte InputStream [^InputStream S]
  (.read S))

(defmethod read-byte Reader [^Reader S]
  (.read S))

(defn open [Type String Direction]
  (core/let [Path (io/file (value '*home-directory*) String)]
            (condp = Direction
              'in (io/input-stream Path)
              'out (io/output-stream Path)
              (throw (IllegalArgumentException. "invalid direction")))))

(defn type [X MyType]
  (cast MyType X))

(defn close [^java.io.Closeable Stream]
  (.close Stream))

(defn pos [X N]
  (core/str (get X N)))

(defn tlstr [X]
  (subs X 1))

(defn cn
  ([Str1] (partial cn Str1))
  ([Str1 Str2]
     (core/let [strings (replace {() ""} [Str1 Str2])]
               (when-let [no-string (first (remove string? strings))]
                 (throw (IllegalArgumentException. (core/str no-string " is not a string"))))
               (apply core/str strings))))

(def ^:private internal-start-time (System/currentTimeMillis))

(defn get-time [Time]
  (if (= Time 'run)
    (* 1.0 (/ (- (System/currentTimeMillis) internal-start-time)
              1000))
    (throw (IllegalArgumentException.
            (core/str "get-time does not understand the parameter " Time)))))

(defmethod print-method array-class [o ^Writer w]
  (print-method (vec o) w))

(defn ^:private read-bytes [s]
  ((value (intern "@p")) (map int s) ()))

(defn parse-shen [s]
  (core/let [<st_input> (value 'shen-<st_input>)
             snd (value 'snd)]
            (-> s read-bytes <st_input> snd)))

(defn parse-and-eval-shen [s]
  (eval-shen* (parse-shen s)))

(defn reset-macros! []
  (set '*macros* (filter #(re-find #"shen-" (name %)) (value '*macros*))))

(defn exit
  ([] (exit 0))
  ([status] (System/exit status)))
