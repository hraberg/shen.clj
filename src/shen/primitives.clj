(ns shen.primitives
  (:use [shen.backend :only (shen-kl-to-clojure)])
  (:refer-clojure :exclude [set intern let pr type cond]))

; Probably handle dynamic currying in here, and define primitves using it.
; Also: Lambda; TCO? And "Kl follows a dual namespace model"

(defmacro defun [F X Y]
  `(defn ~F
     ~@(for [p# (map #(take % X) (range 1 (count X)))]
         `(~(vec p#) (partial ~F ~@p#)))
     (~(vec X) ~Y)))

(defmacro cond [& CS]
  `(clojure.core/cond ~@(apply concat CS)))

;; (SETQ *user-syntax-in* NIL)

;; (DEFMACRO if (X Y Z)
;;   `(LET ((*C* ,X))
;;        (COND ((EQ *C* 'true) ,Y)
;;              ((EQ *C* 'false) ,Z)
;;              (T (ERROR "~S is not a boolean~%" *C*)))))

;; (DEFMACRO and (X Y) `(if ,X (if ,Y 'true 'false) 'false))

;; (DEFMACRO or (X Y) `(if ,X 'true (if ,Y 'true 'false)))

;; (DEFUN set (X Y) (SET X Y))

(defn set [X Y]
  (clojure.core/intern (if-let [ns (namespace X)]
                          (symbol ns) *ns*)
                       (symbol (name X))
                       Y)
  Y)

;; (DEFUN value (X) (SYMBOL-VALUE X))

(defn value [X]
  @(clojure.core/intern (if-let [ns (namespace X)]
                          (symbol ns) *ns*)
                        (symbol (name X))))

;; (DEFUN simple-error (String) (ERROR String))

(defn simple-error [String]
  (throw (RuntimeException. String)))

;; (DEFMACRO trap-error (X F)
;; `(HANDLER-CASE ,X (ERROR (Condition) (FUNCALL ,F Condition))))

(defmacro trap-error [X F]
  `(try
     ~X
     (catch Throwable _#
       (~F _#))))

;; (DEFUN error-to-string (E)
;;    (IF (TYPEP E 'CONDITION)
;;        (FORMAT NIL "~A" E)
;;        (ERROR "~S is not an exception~%" E)))

(defn error-to-string [E]
  (if (instance? Throwable E)
    (str E)
    (throw (IllegalArgumentException. (str E " is not an exception")))))

;; (DEFUN cons (X Y) (CONS X Y))

;; (DEFUN hd (X) (CAR X))

(defn hd [X] (first X))

;; (DEFUN tl (X) (CDR X))

(defn tl [X] (rest X))

;; (DEFUN cons? (X) (IF (CONSP X) 'true 'false))

(defn cons? [X] (if (seq X) true false))

;; ;(DEFUN intern (String) (INTERN String))

(defn intern [String] (symbol String))

;; (DEFUN intern (String) (INTERN (shen-process-intern String)))

;; (DEFUN shen-process-intern (S)
;;    (COND ((STRING-EQUAL S "") S)
;;          ((STRING-EQUAL (pos S 0) "#")
;;           (cn "_hash" (shen-process-intern (tlstr S))))
;;          ((STRING-EQUAL (pos S 0) "'")
;;           (cn "_quote" (shen-process-intern (tlstr S))))
;;          ((STRING-EQUAL (pos S 0) "`")
;;           (cn "_backquote" (shen-process-intern (tlstr S))))
;;          ((STRING-EQUAL (pos S 0) "|")
;;           (cn "bar!" (shen-process-intern (tlstr S))))
;;          (T (cn (pos S 0) (shen-process-intern (tlstr S))))))

;; (DEFUN eval-without-macros (X)
;;   (EVAL (shen-kl-to-lisp NIL (shen-elim-define X))))

(defn eval-without-macros [X]
  (eval (shen-kl-to-clojure X)))

;; ;(DEFUN shen-elim-define (X)
;;  ; (COND ((AND (CONSP X) (EQ (CAR X) 'define))
;;   ;       (COMPILE (EVAL (shen-kl-to-lisp NIL (shen-shen->kl ;(CADR X) (CDDR X))))))
;;  ;       ((CONSP X) (MAPCAR 'shen-elim-define X))
;;    ;     (T X)))

;; ;; Vasil's patch for defective 2.49
;; (DEFUN shen-elim-define (X)
;;  (COND ((AND (CONSP X) (EQ (CAR X) 'define))
;;          (LET ((F (EVAL (shen-kl-to-lisp NIL (shen-shen->kl (CADR X)
;; (CDDR X))))))
;;            (WITH-OUTPUT-TO-STRING (*ERROR-OUTPUT*)
;;                (COMPILE F))
;;                F))
;;        ((CONSP X) (MAPCAR 'shen-elim-define X))
;;        (T X)))

;; (DEFMACRO lambda (X Y) `(FUNCTION (LAMBDA (,X) ,Y)))

(defmacro lambda [X Y]
  `(fn [~X] ~Y))

;; (DEFMACRO let (X Y Z) `(LET ((,X ,Y)) ,Z))

(defmacro let [X Y Z]
  `(clojure.core/let [~X ~Y] ~Z))

;; (DEFUN equal? (X Y) (IF (shen-ABSEQUAL X Y) 'true 'false))

(defn equal? [X Y] (= X Y))

;; (DEFUN shen-ABSEQUAL (X Y)
;;   (COND ((AND (CONSP X) (CONSP Y) (shen-ABSEQUAL (CAR X) (CAR Y)))
;;          (shen-ABSEQUAL (CDR X) (CDR Y)))
;;         ((AND (STRINGP X) (STRINGP Y)) (STRING= X Y))
;;         ((AND (NUMBERP X) (NUMBERP Y)) (= X Y))
;;         ((AND (ARRAYP X) (ARRAYP Y)) (CF-VECTORS X Y (LENGTH X) (LENGTH Y)))
;;         (T (EQUAL X Y))))

;; (DEFUN CF-VECTORS (X Y LX LY)
;;    (AND (= LX LY)
;;         (CF-VECTORS-HELP X Y 0 (1- LX))))

;; (DEFUN CF-VECTORS-HELP (X Y COUNT MAX)
;;   (COND ((= COUNT MAX) (shen-ABSEQUAL (AREF X MAX) (AREF Y MAX)))
;;         ((shen-ABSEQUAL (AREF X COUNT) (AREF Y COUNT)) (CF-VECTORS-HELP X Y (1+ COUNT) MAX))
;;         (T NIL)))

;; (DEFMACRO freeze (X) `(FUNCTION (LAMBDA () ,X)))

(defmacro freeze [X]
  `(fn [] ~X))

;; (DEFUN thaw (X) (FUNCALL X))

(defn thaw [X]
  (X))

;; (DEFUN absvector (N) (MAKE-ARRAY N :INITIAL-ELEMENT 'fail!))

(defn absvector [N]
  [])

;; (DEFUN absvector? (X) (IF (ARRAYP X) 'true 'false))

(defn absvector? [X]
  (vector? X))

;; (DEFUN address-> (Vector N Value) (SETF (AREF Vector N) Value) Vector)

(defn address-> [Vector N Value]
  (assoc Vector N Value))

;; (DEFUN <-address (Vector N) (AREF Vector N))

(defn <-address [Vector N]
  (Vector N))

;; (DEFUN n->string (N) (FORMAT NIL "~C" (CODE-CHAR N)))

(defn n->string [N]
  (str (char N)))

;; (DEFUN pr (X S)
;;    (WRITE-STRING (shen-printer-routines X *printer*) S))

(defn pr [X S]
  (binding [*out* S]
    (clojure.core/println X)))

;; (DEFUN shen-printer-routines (X Routines)
;;   (IF (NULL Routines)
;;       X
;;       (shen-printer-routines (FUNCALL (CAR Routines) X) (CDR Routines))))

;; (DEFUN read-byte (S)
;;   (IF (CHAR-STREAM? S)
;;     (CHAR-CODE (READ-CHAR S))
;;     (READ-BYTE S NIL -1)))

(defn read-byte [S]
  (.read S))

;; (DEFUN CHAR-STREAM? (S)
;;   (SUBTYPEP (STREAM-ELEMENT-TYPE S) 'CHARACTER))

;; (DEFUN open (Type String Direction)
;;    (LET ((Path (FORMAT NIL "~A~A" *home-directory* String)))
;;         (COND ((EQ Type 'file) (file-stream Path Direction))
;;               (T (ERROR "invalid stream type")))))

(def ^:dynamic *home-directory* (System/getProperty "user.dir"))

(defn open [Type String Direction]
  (clojure.core/let [Path (clojure.java.io/file *home-directory* String)]
    (clojure.core/cond
     (= 'in Direction) (clojure.java.io/input-stream Path)
     (= 'out Direction) (clojure.java.io/output-stream Path)
     :else (throw (IllegalArgumentException. "invalid direction")))))

;; (DEFUN file-stream (String Direction)
;;      (COND ((EQ Direction 'in) (OPEN String :DIRECTION :INPUT :ELEMENT-TYPE 'UNSIGNED-BYTE))
;;            ((EQ Direction 'out) (OPEN String :DIRECTION :OUTPUT))
;;            (T (ERROR "invalid direction"))))

;; (DEFUN type (X MyType) (DECLARE (IGNORE MyType)) X)

(defn type [X MyType]
  (cast MyType X))

;; (DEFUN close (Stream) (CLOSE Stream) NIL)

(defn close [Stream]
  (.close Stream))

;; (DEFUN pos (X N) (COERCE (LIST (CHAR X N)) 'STRING))

(defn pos [X N]
  (get X N))

;; (DEFUN tlstr (X) (SUBSEQ X 1))

(defn tlstr [X]
  (subs X 1))

;; (DEFUN cn (Str1 Str2) (CONCATENATE 'STRING Str1 Str2))

(defn cn [Str1 Str2]
  (str Str1 Str2))

;; (DEFUN string? (S) (IF (STRINGP S) 'true 'false))

;; ;(DEFUN str (X) (COND ((NULL X) (ERROR "[] is not an atom in ;Shen; str cannot convert it to a string.~%"))
;;                      ;((SYMBOLP X) (STRING X))
;;                      ;((ATOM X) (FORMAT NIL "~S" X))
;;                     ; (T (ERROR "~S is not an atom; str cannot ;convert it to a string.~%" X))))

;; (DEFUN str (X)
;;   (COND ((NULL X) (ERROR "[] is not an atom in Shen; str cannot convert it to a string.~%"))
;;         ((SYMBOLP X) (shen-process-string (SYMBOL-NAME X)))
;;         ((NUMBERP X)
;;          (shen-process-number (FORMAT NIL "~A" X)))
;;         ((ATOM X) (FORMAT NIL "~S" X))
;;         (T (ERROR "~S is not an atom; str cannot convert it to a string.~%" X))))

;; (DEFUN shen-process-number (S)
;;   (COND ((STRING-EQUAL S "") "")
;;         ((STRING-EQUAL (pos S 0) "d")
;;          (IF (STRING-EQUAL (pos S 1) "0")
;;              ""
;;              (cn "e" (tlstr S))))
;;         (T (cn (pos S 0) (shen-process-number (tlstr S))))))

;; (DEFUN shen-process-string (X)
;;    (COND ((STRING-EQUAL X "") X)
;;          ((AND (> (LENGTH X) 4) (STRING-EQUAL X "_hash" :END1 5))
;;           (cn "#" (shen-process-string (SUBSEQ X 5))))
;;          ((AND (> (LENGTH X) 5)
;;                (STRING-EQUAL X "_quote" :END1 6))
;;           (cn "'" (shen-process-string (SUBSEQ X 6))))
;;          ((AND (> (LENGTH X) 9)
;;                (STRING-EQUAL X "_backquote" :END1 10))
;;           (cn "`" (shen-process-string (SUBSEQ X 10))))
;;          ((AND (> (LENGTH X) 3) (STRING-EQUAL X "bar!" :END1 4))
;;           (cn "|" (shen-process-string (SUBSEQ X 4))))
;;          (T (cn (pos X 0) (shen-process-string (tlstr X))))))

;; (DEFUN get-time (Time)
;;   (IF (EQ Time 'run)
;;       (* 1.0 (/ (GET-INTERNAL-RUN-TIME) INTERNAL-TIME-UNITS-PER-SECOND))
;;       (ERROR "get-time does not understand the parameter ~A~%" Time)))

(def internal-start-time (System/currentTimeMillis))

(defn get-time [Time]
   (if (= Time 'run)
       (* 1.0 (/ (- (System/currentTimeMillis) internal-start-time)
                 1000))
       (throw (IllegalArgumentException.
               (str "get-time does not understand the parameter " Time)))))

;; (DEFUN multiply (X Y)
;;   (* (double-precision X)(double-precision Y)))

(defn multiply [X Y]
  (* (double X) (double Y)))

;; (DEFUN add (X Y)
;;   (+ (double-precision X)(double-precision Y)))

(defn add [X Y]
  (+ (double X) (double Y)))


;; (DEFUN subtract (X Y)
;;   (- (double-precision X)(double-precision Y)))

(defn subtract [X Y]
  (- (double X) (double Y)))

(defn divide [X Y]
  (/ (double X) (double Y)))

;; (DEFUN divide (X Y)
;;   (LET ((Div (/ (double-precision X)
;;                 (double-precision Y))))
;;                       (IF (INTEGERP Div)
;;                            Div
;;                           (* (COERCE 1.0 'DOUBLE-FLOAT) Div))))

;; (DEFUN double-precision (X)
;;   (IF (INTEGERP X) X (COERCE X 'DOUBLE-FLOAT)))

;; (DEFUN greater? (X Y) (IF (> X Y) 'true 'false))

(defn greater? [X Y]
  (> X Y))

;; (DEFUN less? (X Y) (IF (< X Y) 'true 'false))

(defn less? [X Y]
  (< X Y))

;; (DEFUN greater-than-or-equal-to? (X Y) (IF (>= X Y) 'true 'false))

(defn less? [X Y]
  (>= X Y))

;; (DEFUN less-than-or-equal-to? (X Y) (IF (<= X Y) 'true 'false))

(defn less-than-or-equal-to? [X Y]
  (<= X Y))

;; (SETQ *stinput* *STANDARD-INPUT*)

(def ^:dynamic *stinput* *in*)

;; (DEFUN number? (N) (IF (NUMBERP N) 'true 'false))
