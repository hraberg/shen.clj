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
                       yacc])


(def cleanup-symbols-pattern
  (re-pattern (str "(\\s+|\\()("
                   (string/join "|" (map #(Pattern/quote %) [":" ";" "{" "}" ":-"
                                                             "/." "@p" "@s" "@v"
                                                             "shen-@s-macro"]))
                   ")(\\s*\\)|\\s+?)"
                   "(?!~)")))

(defn cleanup-symbols-before
  [kl] (string/replace kl
                       cleanup-symbols-pattern
                       "$1(clojure.core/symbol \"$2\")$3"))

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
                             #{'defun} (into scope (flatten (take 2 rst)))
                             #{'let 'lambda} (conj scope (first rst))
                             scope)
                     fst (if (list? fst)
                           (if (= 'clojure.core/symbol (first fst))
                             (list 'clojure.core/resolve fst)
                             (cleanup-symbols-after fst scope))
                           fst)]
                 (cons fst (map #(cleanup-symbols-after % scope) rst))))
       clj)))

(defn read-kl [kl]
  (with-open [r (PushbackReader. (StringReader. (cleanup-symbols-before kl)))]
    (doall
     (map cleanup-symbols-after
          (take-while (complement nil?)
                      (repeatedly #(read r false nil)))))))

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

(defn header [namespace used-ns]
  (list 'ns namespace
        (cons :use
              (map vector (cons 'shen.primitives
                                (remove #{namespace} used-ns))))
        '(:refer-clojure :only [and or])))

(defn declarations [clj]
  (filter symbol?
          (map second (filter #(= 'defun (first %)) clj))))

(defn write-clj-file [dir name forms]
  (with-open [w (writer (file dir (str name ".clj")))]
    (binding [*out* w]
      (doseq [form (cons (header (symbol name) []) forms)]
        (pprint form)
        (println)))))

(defn write-all-kl-files-as-clj
  ([] (write-all-kl-files-as-clj "shen/klambda" "shen/platforms/clj"))
  ([dir to-dir]
     (.mkdirs (file to-dir))
     (doseq [f (kl-files-in dir)]
       (let [name (string/replace (.getName f) #".kl$" "")]
         (write-clj-file to-dir name (read-kl-file f))))))

(defn write-all-kl-files-as-single-clj
  ([] (write-all-kl-files-as-single-clj "shen/klambda" "shen/platforms/clj"))
  ([dir to-dir]
     (doall
      (.mkdirs (file to-dir))
      (let [shen (mapcat read-kl-file (kl-files-in dir))
            dcl (declarations shen)]
        (write-clj-file to-dir "shen" (cons (cons 'clojure.core/declare dcl) shen))))))
