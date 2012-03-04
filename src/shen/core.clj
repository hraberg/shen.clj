(ns shen.core
  (:use [clojure.java.io :only (file reader writer)]
        [clojure.pprint :only (pprint)])
  (:require [clojure.string :as string])
  (:require [shen.primitives])
  (:import [java.io StringReader PushbackReader FileNotFoundException]
           [java.util.regex Pattern])
  (:gen-class))

(def shen-namespaces '[sys core writer load macros prolog reader sequent
                       toplevel track t-star printer yacc declarations types])

(def cleanup-symbols-pattern
  (re-pattern (str "(\\s+|\\()("
                   (string/join "|" (map #(Pattern/quote %) [":" ";" "{" "}" ":-" ":="
                                                             "/." "@p" "@s" "@v"
                                                             "shen-@s-macro"
                                                             "shen-@v-help"
                                                             "shen-i/o-macro"
                                                             "shen-put/get-macro"
                                                             "XV/Y"]))
                   ")(\\s*\\)|\\s+?)"
                   "(?!~)")))

(defn cleanup-symbols
  [kl] (string/replace kl
                       cleanup-symbols-pattern
                       "$1(intern \"$2\")$3"))

(defn read-kl [kl]
  (with-open [r (PushbackReader. (StringReader. (cleanup-symbols kl)))]
    (doall
     (take-while (complement nil?)
                 (repeatedly #(read r false nil))))))

(defn read-kl-file [file]
  (try
    (cons `(clojure.core/comment ~(str file)) (read-kl (slurp file)))
    (catch Exception e
      (println file e))))

(defn header [ns]
  `(ns ~ns
     (:refer-clojure :only [])
     (:use [shen.primitives])
     (:require [clojure.core :as ~'clj])))

(def missing-declarations '#{shen-kl-to-lisp FORMAT READ-CHAR})

(defn declarations [clj]
  (into missing-declarations
        (map second (filter #(= 'defun (first %)) clj))))

(defn write-clj-file [dir name forms]
  (with-open [w (writer (file dir (str name ".clj")))]
    (binding [*out* w]
      (doseq [f forms]
        (pprint f)
        (println)))))

(defn kl-to-clj
  ([] (kl-to-clj "shen/klambda"
                 *compile-path*))
  ([dir to-dir]
     (.mkdirs (file to-dir))
     (let [shen (mapcat read-kl-file
                        (map #(file dir (str % ".kl")) shen-namespaces))
           dcl (declarations shen)]
       (write-clj-file to-dir "shen"
                       (concat [(header 'shen)]
                               [`(clojure.core/declare ~@(filter symbol? dcl))]
                               (map #(shen.primitives/shen-kl-to-clj %)
                                    (remove string? shen))
                               ['(clojure.core/load "shen/overrides")])))))

(defn install []
  (try
    (require 'shen)
    (catch FileNotFoundException _
      (println "Creating shen.clj")
      (kl-to-clj))))

(defn swank [port]
  (try
    (require 'swank.swank)
    (with-out-str
      ((resolve 'swank.swank/start-repl) port))
    (println "Swank connection opened on" port)
    (catch FileNotFoundException _)))

(defn -main []
  (install)
  (require 'shen)
  (binding [*ns* (the-ns 'shen)]
    (swank 4005)
    ((resolve 'shen/-main))))

(when *compile-files*
  (install))

(defn repl? []
  (->> (Thread/currentThread) .getStackTrace seq
       (map str) (some (partial re-find #"clojure.main.repl"))))

(when (repl?)
  (-main))