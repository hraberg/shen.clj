(ns shen.primitives
  (:require [clojure.core :as c]
            [clojure.set :as set]
            [clojure.string :as string]
            [clojure.walk :as walk]
            [clojure.java.io :as io])
  (:refer-clojure :exclude [set intern let pr type cond cons str number? string? defmacro
                            + - * / > < >= <= = and or])
  (:import [java.io Reader Writer InputStream OutputStream PrintWriter OutputStreamWriter]
           [java.util Arrays])
  (:gen-class))

(create-ns 'shen.globals)

(def string? c/string?)
(def number? c/number?)

(defn assert-boolean [x & [fmt]]
  (if (instance? Boolean x) x
      (throw (IllegalArgumentException. (format (c/or fmt "%s is not a boolean") x)))))

(c/defmacro if-kl
  ([test] `(c/let [test# ~test] (fn [then# else#] (if-kl test# then# else#))))
  ([test then] `(partial (if-kl ~test) ~then))
  ([test then else] `(if (assert-boolean ~test "boolean expected: not %s") ~then ~else)))

(c/defmacro and
  ([x] `(fn [y#]
          (boolean (c/and (assert-boolean ~x) (assert-boolean y#)))))
  ([x & [y & xs]]
     `(if-let [x# (assert-boolean ~x)]
        ~(if xs `(and ~y ~@xs) `(assert-boolean ~y))
        false)))

(c/defmacro or
  ([x] `(fn [y#]
          (boolean (c/or (assert-boolean ~x) (assert-boolean y#)))))
  ([x & [y & xs]]
     `(if-let [x# (assert-boolean ~x)]
        true
        ~(if xs `(or ~y ~@xs) `(assert-boolean ~y)))))

(defn ^:private and-fn
  ([x] (and x))
  ([x y] (and x y)))

(defn ^:private or-fn
  ([x] (or x))
  ([x y] (or x y)))

(defn ^:private partials [name parameters]
  (for [p (map #(take % parameters) (range 1 (count parameters)))]
    `(~(vec p) (partial ~name ~@p))))

(c/defmacro defun [F X & Y]
  (c/let [F (if (seq? F) (eval F) F)]
            `(do
               (defn ^:dynamic ~F
                 ~@(partials F X)
                 (~(vec X) ~@Y))
               ~F)))

(def ^:private array-class (Class/forName "[Ljava.lang.Object;"))

(defn =
  ([X] (partial = X))
  ([X Y]
     (c/cond
      (c/and (identical? array-class (class X))
                (identical? array-class (class Y))) (Arrays/equals #^"[Ljava.lang.Object;" X
                                                                   #^"[Ljava.lang.Object;" Y)
                (c/and (number? X) (number? Y)) (== X Y)
                :else (c/= X Y))))

(defn /
  ([X] (partial / X))
  ([X Y]
     (if (zero? Y) (throw (IllegalArgumentException. "division by zero"))
         (c/let [r (clojure.core// X Y)]
                   (if (ratio? r) (double r) r)))))

(defn ^:private alias-op [op real-op]
  (eval `(defun ~op ~'[X Y] (~real-op ~'X ~'Y))))

(doseq [op '[+ - *]]
  (alias-op op (symbol "clojure.core" (c/str (name op) "'"))))

(doseq [op '[> < >= <=]]
  (alias-op op (symbol "clojure.core" (name op))))

(defn ^:private interned? [X]
  (c/and (seq? X) (= 'intern (first X))))

(def ^:private slash-dot (symbol "/."))

(defn ^:private recur?
  ([path] (partial recur? path))
  ([path fn]
     (c/or (= 'cond (last (drop-last path)))
              (set/superset? '#{defun cond if do let}
                             (c/set path)))))

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
       seq? (c/let [[fst snd trd & rst] kl
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
                               '#{if} 'if-kl
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

(c/defmacro cond [[test expr] & clauses]
  (list 'if-kl test expr
        (when clauses
          (c/cons 'cond clauses))))

(defn set* [X Y ns]
  @(c/intern (the-ns ns)
                (with-meta X {:dynamic true :declared true})
                Y))

(defn set
  ([X] (partial set X))
  ([X Y] (set* X Y 'shen.globals)))

(defn ^:private value* [X ns]
  (c/let [v (c/and (symbol? X) (ns-resolve ns X))]
            (condp = X
              'and and-fn
              'or or-fn
              @v)))

(defn value [X] (value* X 'shen.globals))

(defn function [fn]
  (if (fn? fn) fn
      (value* fn 'shen)))

(defn simple-error [String]
  (throw (RuntimeException. ^String String)))

(c/defmacro trap-error [X F]
  `(try
     ~X
     (catch Exception e#
       (~F e#))))

(defn error-to-string [E]
  (if (instance? Throwable E)
    (c/or (.getMessage ^Throwable E) (c/str E))
    (throw (IllegalArgumentException. ^String (c/str E " is not an exception")))))

(defn ^:private pair [X Y] [X Y])

(defn ^:private pair? [X]
  (c/and (vector? X) (= 2 (count X))))

(defn cons [X Y]
  (if (c/and (coll? Y)
                (not (pair? Y)))
    (c/cons X Y)
    (pair X Y)))

(defn hd [X] (first X))

(defn tl [X]
  (if (pair? X)
    (second X)
    (rest X)))

(defn fail! [] (assert false))

(defn cons? [X]
  (c/and (coll? X) (not (empty? X))))

(defn str [X]
  (if-not (coll? X) (c/pr-str X)
          (throw (IllegalArgumentException.
                  (c/str X " is not an atom; str cannot convert it to a string.")))))

(defn ^:private seq-to-cons [[fst & rst] & [recursive?]]
  (if fst
    (list 'cons (if (c/and recursive? (sequential? fst))
                  (seq-to-cons fst recursive?)
                  fst)
          (seq-to-cons rst recursive?))
    ()))

(defn ^:private cleanup-clj [clj]
  (condp some [clj]
    vector? (recur (seq-to-cons clj))
    coll? (if ('#{clojure.core/deref} (first clj))
            (symbol (c/str "@" (second clj)))
            clj)
    '#{λ} slash-dot
    char? (intern clj)
    clj))

(defn ^:private define* [name body]
  (c/let [kl ((function 'shen-shen->kl) name body)]
            (binding [*ns* (the-ns 'shen)]
              ((function 'eval) kl)
              name)))

(defn ^:private shen-elim-define [X]
  (if (seq? X)
    (if ('#{define} (first X)) (define* (second X) (drop 2 X))
        (map shen-elim-define X))
    X))

(defn ^:private shen-proc-input+ [X]
  (if (seq? X)
    (if ('#{input+} (first X)) (c/let [[fst snd trd] X]
                                      (list fst snd
                                            (if (sequential? trd)
                                              (seq-to-cons trd :recursive)
                                              trd)))
        (map shen-proc-input+ X))
    X)
  X)

;; 87,100c87
;; <
;; < (DEFUN shen-proc-input+ (X)
;; <   (COND ((AND (CONSP X) (EQ (CAR X) 'input+))
;; <          (LIST (CAR X) (CADR X) (shen-iter-cons (CADDR X))))
;; <         ((CONSP X) (MAPCAR 'shen-proc-input+ X))
;; <         (T X)))
;; <
;; < (DEFUN shen-iter-cons (X)
;; <   (IF (CONSP X)
;; <       (LIST 'cons
;; <             (shen-iter-cons (CAR X))
;; <             (shen-iter-cons (CDR X)))
;; <       X))

(defn eval-shen* [body]
  (c/let [body (walk/postwalk cleanup-clj body)]
            (binding [*ns* (the-ns 'shen)]
              (->> body
                   (map (function 'eval))
                   last))))

(c/defmacro eval-shen [& body]
  `(eval-shen* '~body))

(c/defmacro 神 [& body]
  `(eval-shen ~@body))

(c/defmacro define [name & body]
  `(c/let [fn# (eval-shen ~(concat ['define name] body))]
             (defn ~(with-meta name {:dynamic true})
               [& ~'args] (apply (function fn#) ~'args))))

(doseq [[name args] '{defmacro [name] defprolog [name] prolog? [] package [name exceptions]}]
  (eval
   `(c/defmacro ~name [~@args & ~'body]
      `(eval-shen ~(concat ['~name ~@args] ~'body)))))

(def ^:private missing-symbol-pattern #"Unable to resolve symbol: (.+) in this context")

(defn ^:private missing-symbol [s]
  (when-let [[_ sym] (re-find missing-symbol-pattern (c/or s ""))] sym))


(defn ^:private fn-to-symbol [fn]
  (-> fn class .getName
      (string/replace "_" "-")
      (string/split #"\$")
      last symbol))

(defn ^:private cleanup-return [x]
  (c/or (when (fn? x)
             (c/let [name (fn-to-symbol x)]
                       (when (fn? (ns-resolve 'shen name)) name)))
           x))

(defn ^:private eval-and-declare-missing [kl]
  (binding [*ns* (the-ns 'shen)]
    (try
      (cleanup-return (eval kl))
      (catch RuntimeException e
        (if-let [s (missing-symbol (.getMessage e))]
          (do
            (set* (symbol s) nil 'shen)
            (eval-and-declare-missing kl))
          (throw e))))))

(defn eval-without-macros [X]
  (c/let [kl (shen-kl-to-clj (shen-elim-define (shen-proc-input+ (cleanup-clj X))))]
            (eval-and-declare-missing kl)))

(c/defmacro lambda [X Y]
  `(fn [~X & XS#] (c/let [result# ~Y]
                            (if XS# (apply result# XS#)
                                result#))))

(c/defmacro λ [X Y]
  `(lambda ~X ~Y))

(c/defmacro let [X Y Z]
  (c/let [X-safe (if (seq? X) (gensym (eval X)) X)
             Z (if (seq? X) (walk/postwalk
                             #(if (= X %) X-safe %) Z) Z)]
            `(c/let [~X-safe ~Y]
                       ~Z)))

(c/defmacro freeze [X]
 `(fn [] ~X))

(defn thaw [X] (X))

(defn absvector [N]
  (doto (object-array (int N)) (Arrays/fill 'fail!)))

(defn absvector? [X]
  (identical? array-class (c/class X)))

(defn <-address [#^"[Ljava.lang.Object;" Vector N]
  (aget Vector (int N)))

(defn address-> [#^"[Ljava.lang.Object;" Vector N Value]
  (aset Vector (int N) Value)
  Vector)

(defn n->string [N]
  (c/str (char N)))

(defn string->n [S]
  (c/int (first S)))

(def byte->string n->string)

(defmulti pr (fn [_ S] (class S)))

(defmethod pr Reader [X ^Reader S]
  (if (= *in* S)
    (pr X *out*)
    (throw (IllegalArgumentException. (str S)))))

(defmethod pr OutputStream [X ^OutputStream S]
  (pr X (OutputStreamWriter. S)))

(defmethod pr Writer [X ^Writer S]
  (binding [*out* S]
    (print X)
    (flush)
    X))

(defmulti read-byte class)

(defmethod read-byte InputStream [^InputStream S]
  (.read S))

(defmethod read-byte Reader [^Reader S]
  (.read S))

(defn open [Type String Direction]
  (condp = Type
    'file
    (c/let [Path (io/file (value '*home-directory*) String)]
              (condp = Direction
                'in (io/input-stream Path)
                'out (io/output-stream Path)
                (throw (IllegalArgumentException. "invalid direction"))))
    (throw (IllegalArgumentException. "invalid stream type"))))

(defn type [X MyType]
  (cast MyType X))

(defn close [^java.io.Closeable Stream]
  (.close Stream))

(defn pos [X N]
  (c/str (get X N)))

(defn tlstr [X]
  (subs X 1))

(defn cn
  ([Str1] (partial cn Str1))
  ([Str1 Str2]
     (c/let [strings (replace {() ""} [Str1 Str2])]
               (when-let [no-string (first (remove string? strings))]
                 (throw (IllegalArgumentException. (c/str no-string " is not a string"))))
               (apply c/str strings))))

(def ^:private internal-start-time (System/currentTimeMillis))

(defn get-time [Time]
  (if (= Time 'run)
    (/ (- (System/currentTimeMillis) internal-start-time) 1000)
    (throw (IllegalArgumentException.
            (c/str "get-time does not understand the parameter " Time)))))

(defmethod print-method array-class [o ^Writer w]
  (print-method (vec o) w))

(defn ^:private read-bytes [s]
  ((function (intern "@p")) (map int s) ()))

(defn parse-shen [s]
  (c/let [<st_input> (function 'shen-<st_input>)
             snd (function 'snd)]
            (-> s read-bytes <st_input> snd)))

(defn parse-and-eval-shen [s]
  (eval-shen* (parse-shen s)))

(defn reset-macros! []
  (set '*macros* (filter #(re-find #"shen-" (name %)) (value '*macros*))))

(defn exit
  ([] (exit 0))
  ([status] (System/exit status)))
