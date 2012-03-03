(ns shen.primitives
  (:require [clojure.core :as core])
  (:require [clojure.string :as string])
  (:require [clojure.walk :as walk])
  (:require [clojure.java.io :as io])
  (:refer-clojure :exclude [set intern let pr type cond cons str number? string? defmacro
                            + - * / > < >= <= = and or])
  (:import [java.io Writer]
           [java.util Arrays]))

(defn ^:private alias-vars [ns-map target-ns]
  (doseq [[k v] ns-map]
    (alter-meta! (core/intern target-ns k v) merge (meta v))))

(alias-vars (select-keys (ns-map 'clojure.core) '[and or string? number?]) 'shen.primitives)

(defn ^:private partials [name parameters]
  (for [p (map #(take % parameters) (range 1 (count parameters)))]
    `(~(vec p) (partial ~name ~@p))))

(core/defmacro defun [F X & Y]
  (core/let [F (if (seq? F) (eval F) F)]
            `(defn ^:dynamic ~F
               ~@(partials F X)
               (~(vec X) ~@Y))))

(doseq [op '[+ - * / > < >= <=]
        :let [real-op (symbol "clojure.core" (name op))]]
  (eval `(defun ~op ~'[X Y] (~real-op ~'X ~'Y))))

(declare absvector?)

(defn =
  ([X] (partial = X))
  ([X Y]
     (if (every? absvector? [X Y])
       (Arrays/equals X Y)
       (core/= X Y))))

(defn ^:private interned? [X]
  (and (seq? X) (= 'intern (first X))))

(def ^:private safe-tail-call '#{shen-reverse_help shen-read-file-as-bytelist-help})

(def ^:private slash-dot (symbol "/."))

(defn shen-kl-to-clj
  ([clj] (shen-kl-to-clj clj #{}))
  ([clj scope]
     (condp some [clj]
       scope clj
       symbol? (condp = (name clj)
                 "true" true
                 "false" false
                 "nil" nil
                 (list 'quote clj))
       seq? (core/let [[fst snd trd & rst] clj
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
                              seq? (list 'value (shen-kl-to-clj fst scope))
                              fst)
                       snd (if ('#{defun let lambda} fst) snd
                               (shen-kl-to-clj snd scope))
                       trd (if ('#{defun} fst) trd
                               (shen-kl-to-clj trd scope))]
                      (take-while (complement nil?)
                                  (concat [fst snd trd]
                                          (map #(shen-kl-to-clj % scope) rst))))
       clj)))

(defn intern [String]
  (core/let [s (name String)]
            (symbol (condp = s
                      "/" s
                      "/." slash-dot
                      (string/replace s "/" "-slash-")))))

(core/defmacro cond [& CS]
  `(core/cond ~@(apply concat CS)))

(defn set [X Y]
  @(core/intern (the-ns 'shen)
                (intern X)
                Y))

(defn value [X]
  (if-let [v (and (symbol? X) (ns-resolve 'shen (intern X)))]
    @v
    X))

(defn simple-error [String]
  (throw (RuntimeException. String)))

(core/defmacro trap-error [X F]
  `(try
     ~X
     (catch Throwable T#
       (~F T#))))

(defn error-to-string [E]
  (if (instance? Throwable E)
    (with-out-str
      (.printStackTrace E))
    (throw (IllegalArgumentException. (core/str E " is not an exception")))))

(defn ^:private pair [X Y]
  [X Y])

(defn ^:private pair? [X]
  (and (vector? X) (= 2 (count X))))

(defn cons [X Y]
  (if (coll? Y)
    (core/cons X Y)
    (pair X Y)))

(defn hd [X] (first X))

(defn tl [X]
  (if (pair? X)
    (second X)
    (rest X)))

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
    vector? (cleanup-clj (vec-to-cons clj))
    coll? (if ('#{clojure.core/deref} (first clj))
            (symbol (core/str "@" (second clj)))
            clj)
    '#{λ} slash-dot
    clj))

(defn ^:private define* [name body]
  (core/let [body (walk/postwalk cleanup-clj body)
             kl ((value 'shen-shen->kl) name body)]
            (binding [*ns* (the-ns 'shen)]
              ((value 'eval) kl))))

(defn ^:private shen-elim-define [X]
  (if (seq? X)
    (if ('#{define} (first X)) (define* (second X) (drop 2 X))
        (map shen-elim-define X))
    X))

(core/defmacro eval-shen [& body]
  (core/let [body (walk/postwalk cleanup-clj body)]
            `(binding [*ns* (the-ns '~'shen)]
               ((value 'eval) '~@body))))

(core/defmacro 神 [& body]
  `(eval-shen ~@body))

(core/defmacro define [name & body]
  `(eval-shen ~(cons 'define (cons name body))))

; is use of first wrong and a hint? macro never defined
(core/defmacro defmacro [name & body]
  `(first (eval-shen ~(cons 'defmacro (cons name body)))))

(defn eval-without-macros [X]
  (core/let [kl (shen-kl-to-clj (shen-elim-define (cleanup-clj X)))]
            (binding [*ns* (the-ns 'shen)]
              (eval kl))))

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

(defn thaw [X]
  (X))

(defn absvector [N]
  (doto (make-array Object N) (Arrays/fill 'fail!)))

(defn absvector? [X]
  (if-not X
    false
    (-> X core/type .isArray)))

(defn address-> [Vector N Value]
  (aset Vector N Value)
  Vector)

(defn <-address [Vector N]
  (aget Vector N))

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

(defn read-byte [S]
  (.read S))

(defn open [Type String Direction]
  (core/let [Path (io/file (value '*home-directory*) String)]
            (condp = Direction
              'in (io/input-stream Path)
              'out (io/output-stream Path)
              (throw (IllegalArgumentException. "invalid direction")))))

(defn type [X MyType]
  (cast MyType X))

(defn close [Stream]
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

(defmethod print-method (class (object-array 1)) [o ^Writer w]
  (print-method (vec o) w))

(defn ^:private read-bytes [s]
  ((value (intern "@p")) (map int s) ()))

(defn parse-shen [s]
  (core/let [<st_input> (value 'shen-<st_input>)
             snd (value 'snd)]
            (-> s read-bytes <st_input> snd first)))

(defn parse-and-eval-shen [s]
  ((value 'shen/eval) (parse-shen s)))
