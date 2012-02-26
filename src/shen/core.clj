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
     (:require [shen.primitives])
     (:refer-clojure :only [])
     (:require [clojure.core :as ~'clj])
     (:gen-class)))

(def missing-declarations '#{shen-kl-to-lisp FORMAT READ-CHAR})

(defn declarations [clj]
  (into missing-declarations
        (map second (filter #(= 'defun (first %)) clj))))

(defn env []
  (for [[k v] '{*language* "Clojure"
                *implementation* (clojure.core/str "Clojure " (clojure.core/clojure-version)
                                                   " [jvm "(System/getProperty "java.version")"]")
                *port* "0.1.0-SNAPSHOT"
                *porters* "Håkan Råberg"
                *stinput* clojure.core/*in*
                *home-directory* (System/getProperty "user.dir")}]
    `(clojure.core/intern (find-ns '~'shen) (with-meta '~k {:dynamic true}) ~v)))

(defn main-fn []
  '(clojure.core/defn -main []
     (shen-shen)))

(defn alias-vars []
  '(shen.primitives/alias-vars (clojure.core/ns-publics 'shen.primitives) 'shen))

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
                               [(alias-vars)]
                               (map #(shen.primitives/shen-kl-to-clj %)
                                    (remove string? shen))
                               (env)
                               ['(set '*macros* (clojure.core/map value *macros*))]
                               [(main-fn)])))))

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

(when (->> (Thread/currentThread) .getStackTrace seq
           (map str) (some (partial re-find #"clojure.main.repl")))
  (-main))
