(ns shen.install
  (:use [clojure.java.io :only (file reader writer)]
        [clojure.pprint :only (pprint)])
  (:require [clojure.string :as string]
            [shen.primitives])
  (:import [java.io StringReader PushbackReader FileNotFoundException]
           [java.util.regex Pattern])
  (:gen-class))

(def shen-namespaces '[sys writer declarations core load macros prolog reader sequent
                       toplevel track t-star printer yacc types])

(def kl-dir (->> ["../../K Lambda" "shen/klambda"]
                 (map file) (filter #(.exists %)) first))

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
    (cons `(c/comment ~(str file)) (read-kl (slurp file)))
    (catch Exception e
      (println file e))))

(defn header [ns]
  `(~'ns ~ns
     (:refer-clojure :only [])
     (:use [shen.primitives])
     (:require [clojure.core :as ~'c])
     (:gen-class)))

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

(defn project-version []
  (-> (slurp "project.clj") read-string (nth 2)))

(defn kl-to-clj
  ([] (kl-to-clj kl-dir
                 *compile-path*))
  ([dir to-dir]
     (.mkdirs (file to-dir))
     (let [shen (mapcat read-kl-file
                        (map #(file dir (str % ".kl")) shen-namespaces))
           dcl (declarations shen)]
       (write-clj-file to-dir "shen"
                       (concat [(header 'shen)]
                               [`(c/declare ~@(filter symbol? dcl))]
                               ['(c/intern 'shen.globals (c/with-meta '*language* {:dynamic true}) "Clojure")]
                               [(concat '(c/intern 'shen.globals (c/with-meta '*port* {:dynamic true}))
                                        [(project-version)])]
                               (map #(shen.primitives/shen-kl-to-clj %)
                                    (remove string? shen))
                               ['(c/load "shen/overwrite")]
                               ['(c/defn -main [] (shen-shen))])))))

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