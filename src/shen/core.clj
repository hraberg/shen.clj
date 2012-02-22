(ns shen.core
  (:use [clojure.java.io :only (file reader writer)]
        [clojure.pprint :only (pprint)]
        [clojure.set :only (intersection)])
  (:require [clojure.string :as string])
  (:require [shen.primitives :reload true])
  (:import [java.io StringReader PushbackReader]
           [java.util.regex Pattern]))

;; Contains duplications so globals gets built again once everything is defined properly.
(def shen-namespaces '[sys core writer load macros macros prolog reader sequent
                       toplevel track t-star yacc declarations declarations])

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

(defn cleanup-symbols
  [kl] (string/replace kl
                       cleanup-symbols-pattern
                       "$1(shen-symbol \"$2\")$3"))

(defn read-kl [kl]
  (with-open [r (PushbackReader. (StringReader. (cleanup-symbols kl)))]
    (doall
     (take-while (complement nil?)
                 (repeatedly #(read r false nil))))))

(defn read-kl-file [file]
  (try
    (cons (list 'clojure.core/println (str file)) (read-kl (slurp file)))
    (catch Exception e
      (println file e))))

(defn header [namespace exclusions]
  `(ns ~namespace
     (:use [shen.primitives])
     (:refer-clojure :exclude ~(vec exclusions))
     (:gen-class)))

(defn ns-symbols [ns]
  (set (map first (ns-publics ns))))

(def missing-declarations '#{shen-kl-to-lisp FORMAT READ-CHAR declare})

(defn declarations [clj]
  (into missing-declarations
        (map second (filter #(= 'defun (first %)) clj))))

(defn env []
  (for [[k v] '{*language* "Clojure"
                *implementation* (str "Clojure " (clojure.core/clojure-version)
                                      " [jvm "(System/getProperty "java.version")"]")
                *port* "3.0-SNAPSHOT"
                *porters* "Håkan Råberg"
                *stinput* clojure.core/*in*}]
    `(clojure.core/intern *ns* (with-meta '~k {:dynamic true}) ~v)))

(defn main-fn []
  '(defn -main []
     (shen-shen)))

(defn write-clj-file [dir name forms]
  (with-open [w (writer (file dir (str name ".clj")))]
    (binding [*out* w]
      (doseq [f forms]
        (pprint f)
        (println)))))

(defn kl-to-clj
  ([] (kl-to-clj "shen/klambda"
                 "shen/platforms/clj"))
  ([dir to-dir]
     (.mkdirs (file to-dir))
     (let [shen (mapcat read-kl-file
                        (map #(file dir (str % ".kl")) shen-namespaces))
           dcl (declarations shen)
           exclusions (intersection (into (ns-symbols 'shen.primitives) dcl) (ns-symbols 'clojure.core))]
       (write-clj-file to-dir "shen"
                       (concat [(header 'shen (sort exclusions))]
                               [(cons 'clojure.core/declare (filter symbol? dcl))]
                               (env)
                               (map #(shen.primitives/shen-kl-to-clj %)
                                    (remove string? shen))
                               [(main-fn)])))))

(defn install []
  (println "creating shen.clj")
  (time (kl-to-clj))
  (println "compiling: ")
  (time (compile 'shen)))

(kl-to-clj)