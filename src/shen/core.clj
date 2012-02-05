(ns shen.core
  (:use [clojure.java.io :only (file reader writer)]
       [clojure.pprint :only (pprint)])
  (:require [clojure.string :as string])
  (:import [java.io StringReader PushbackReader]
           [java.util.regex Pattern]))

(def ^:dynamic *language* "Clojure")
(def ^:dynamic *implementation* (str "Clojure " (clojure-version)
                           " [jvm "(System/getProperty "java.version")"]"))
(def ^:dynamic *port* "0.1.0")
(def ^:dynamic *porters* "Håkan Råberg")


(def shen-namespaces '[core
                       declarations
                       load
                       macros
                       prolog
                       reader
                       sequent
                       sys
                       toplevel
                       track
                       t-star
                       types
                       writer
                       yacc.kl])


(def cleanup-symbols-pattern
  (re-pattern (str "(\\s+|\\()("
                   (string/join "|" (map #(Pattern/quote %) [":" ";" "{" "}"
                                                             "/." "@p" "@s" "@v"]))
                   ")(\\s*\\)|\\s+?)"
                   "(?!~)")))

(defn cleanup-symbols
  [kl] (string/replace kl
                       cleanup-symbols-pattern
                       "$1(clojure.core/symbol \"$2\")$3"))

(defn read-kl [kl]
  (with-open [r (PushbackReader. (StringReader. (cleanup-symbols kl)))]
    (doall
     (take-while (complement nil?)
                 (repeatedly #(read r false nil))))))

(defn read-kl-file [file]
  (try
    (read-kl (slurp file))
    (catch Exception e
      (println file e))))

(defn kl-files-in [dir]
  (filter #(re-find #".*.kl$" (str %))
          (file-seq (file dir))))

(defn read-all-kl-files
  ([] (read-all-kl-files "shen/klambda"))
  ([dir] (map read-kl-file (kl-files-in dir))))

(defn header [namespace]
  (list 'ns (symbol namespace)
        '(:use [shen.primitives])
        '(:refer-clojure :exclude [set intern let pr type cond])))

(defn write-clj-file [dir name forms]
  (with-open [w (writer (file dir (str name ".clj")))]
    (binding [*out* w]
      (doseq [form (cons (header name) forms)]
        (pprint form)
        (println)))))

(defn write-all-kl-files-as-clj
  ([] (write-all-kl-files-as-clj "shen/klambda" "shen/platforms/clj"))
  ([dir to-dir]
     (.mkdirs (file to-dir))
     (doseq [f (kl-files-in dir)]
       (let [name (string/replace (.getName f) #".kl$" "")]
         (write-clj-file to-dir name (read-kl-file f))))))
