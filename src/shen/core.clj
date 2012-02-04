(ns shen.core
  (use [clojure.java.io :only (file reader)]
       [clojure.pprint :only (pprint)])
  (require [clojure.string :as string])
  (import [java.io StringReader PushbackReader]
          [java.util.regex Pattern]))


(def cleanup-symbols-pattern
  (re-pattern (str "(\\s+)("
                   (string/join "|" (map #(Pattern/quote %) [":" ";" "{" "}" "/."]))
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

(defn read-all-kl-files
  ([] (read-all-kl-files "shen/klambda"))
  ([dir] (map read-kl-file (filter #(re-find #".*.kl$" (str %))
                                   (file-seq (file dir))))))

