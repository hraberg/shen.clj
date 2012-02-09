(ns shen.core
  (:use [clojure.java.io :only (file reader writer)]
        [clojure.pprint :only (pprint)]
        [clojure.set :only (intersection)])
  (:require [clojure.string :as string])
  (:require [shen.primitives :reload true])
  (:import [java.io StringReader PushbackReader]
           [java.util.regex Pattern])
  (:gen-class))

(def shen-namespaces '[sys core writer load macros prolog reader sequent
                       toplevel track t-star yacc declarations])

(def cleanup-symbols-pattern
  (re-pattern (str "(\\s+|\\()("
                   (string/join "|" (map #(Pattern/quote %) [":" ";" "{" "}" ":-" ":="
                                                             "/." "@p" "@s" "@v"
                                                             "shen-@s-macro"
                                                             "shen-@v-help"
                                                             "shen-i/o-macro"
                                                             "shen-put/get-macro"
                                                             "XV/Y" "-->"]))
                   ")(\\s*\\)|\\s+?)"
                   "(?!~)")))

(defn cleanup-symbols-before
  [kl] (string/replace kl
                       cleanup-symbols-pattern
                       "$1(intern \"$2\")$3"))

(defn cleanup-symbols-after
  ([clj] (cleanup-symbols-after clj #{}))
  ([clj scope]
     (condp some [clj]
       scope clj
       symbol? (list 'quote clj)
       list? (if (empty? clj)
               clj
               (let [[fst & rst] clj
                     scope (condp get fst
                             '#{defun} (into scope (flatten (take 2 rst)))
                             '#{let lambda} (conj scope (first rst))
                             scope)
                     fst (if (list? fst)
                           (if (= 'intern (first fst))
                             (list 'value fst)
                             (cleanup-symbols-after fst scope))
                           fst)]
                 (cons fst (map #(cleanup-symbols-after % scope) rst))))
       clj)))

(defn read-kl [kl]
  (with-open [r (PushbackReader. (StringReader. (cleanup-symbols-before kl)))]
    (doall
     (take-while (complement nil?)
                 (repeatedly #(read r false nil))))))

(defn read-kl-file [file]
  (try
    (cons (list 'clojure.core/println (str file)) (read-kl (slurp file)))
    (catch Exception e
      (println file e))))

(defn header [namespace exclusions]
  (list 'ns namespace
        '(:use [shen.primitives])
        (list :refer-clojure :exclude (vec exclusions))))

(def missing-declarations '#{shen-kl-to-lisp FORMAT READ-CHAR declare})

(defn declarations [clj]
  (into missing-declarations
        (filter symbol?
               (map second (filter #(= 'defun (first %)) clj)))))

(defn write-clj-file [dir name forms]
  (with-open [w (writer (file dir (str name ".clj")))]
    (binding [*out* w]
      (doseq [f forms]
        (pprint f)
        (println)))))

(defn ns-symbols [ns]
  (set (map first (ns-publics ns))))

(defn env []
  (for [[k v] '{*language* "Clojure"
                *implementation* (str "Clojure " (clojure.core/clojure-version)
                                      " [jvm "(System/getProperty "java.version")"]")
                *port* "0.1.0"
                *porters* "Håkan Råberg"
                *stinput* clojure.core/*in*}]
        `(clojure.core/intern *ns* (with-meta '~k {:dynamic true}) ~v)))

(defn kl-to-clj
  ([] (kl-to-clj "shen/klambda"
                 "shen/platforms/clj"))
  ([dir to-dir]
     (doall
      (.mkdirs (file to-dir))
      (let [shen (mapcat read-kl-file
                         (map #(file dir (str % ".kl")) shen-namespaces))
            dcl (declarations shen)
            exclusions (intersection (into (ns-symbols 'shen.primitives) dcl) (ns-symbols 'clojure.core))]
        (write-clj-file to-dir "shen"
                        (cons (header 'shen (sort exclusions))
                        (cons (cons 'clojure.core/declare dcl)
                              (concat (env) (remove string?
                                                    (map #(cleanup-symbols-after % dcl) shen))))))))))

(declare -main)

(defn install []
  (println "creating shen.clj")
  (time (kl-to-clj))
  (println "compiling: ")
  (time (compile 'shen)))

(defn -main []
  (try
    (require 'shen)
    (catch Exception _
      (install)))
  (binding [*ns* (find-ns 'shen)]
    (eval '(shen-shen))))
