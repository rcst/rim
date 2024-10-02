; (defun arglist (fn) 
;   ; unfortunately function-lambda-expression
;   ; is not implemented in all Lisp implementation
;   ; it's not present in GCL
;   (cadr (function-lambda-expression fn)))
; 
; (defun keyarglist (fn) 
;   (cdr (member '&key (arglist fn))))
; 
; (defun member-keyarglist (x fn) 
;   ; x - item to be searched
;   ; fn - function
;   ; seaches for x in fn's keyword argument list
;   ; returns the tail of the keyword argument list if found
;   ; otherwise NIL
;   (let ((fun (cond
;                ((symbolp fn) (symbol-function fn))
;                ((functionp fn) fn)
;                (T (return-from member-keyarglist NIL)))))
;     (member x (keyarglist fun) :test #'eq :key #'car)))

(defparameter *with-r-list* T)

(defparameter *maxima-direct-ir-map*
  (let ((ht (make-hash-table)))
    (setf (gethash 'mtimes ht) '(op *))
    (setf (gethash 'mnctimes ht) '(op %*%))
    (setf (gethash 'mplus ht) '(op +))
    (setf (gethash 'mexpt ht) '(op ^))
    (setf (gethash 'rat ht) '(op /))
    (setf (gethash 'mquotient ht) '(op /))
    (setf (gethash 'msetq ht) '(op-no-bracket =))
    (setf (gethash 'mlist ht) '(struct-list))
    (setf (gethash 'mand ht) '(boolop &))
    (setf (gethash 'mor ht) '(boolop \|))
    (setf (gethash 'mnot ht) '(unary-op !))
    (setf (gethash 'mminus ht) '(unary-op -))
    (setf (gethash 'mgreaterp ht) '(comp-op >))
    (setf (gethash 'mequal ht) '(comp-op ==))
    (setf (gethash 'mnotequal ht) '(comp-op !=))
    (setf (gethash 'mlessp ht) '(comp-op <))
    (setf (gethash 'mgeqp ht) '(comp-op >=))
    (setf (gethash 'mleqp ht) '(comp-op <=))
    (setf (gethash '$floor ht) '(funcall (symbol "floor")))
    (setf (gethash '$fix ht) '(funcall (symbol "floor")))
    (setf (gethash '%fix ht) '(funcall (symbol "floor")))
    (setf (gethash '%sqrt ht) '(funcall (symbol "sqrt")))
    (setf (gethash '%log ht) '(funcall (symbol "log")))
    (setf (gethash '%gamma ht) '(funcall (symbol "gamma")))
    (setf (gethash 'mreturn ht) '(funcall (symbol "return")))
    (setf (gethash 'mabs ht) '(funcall (symbol "abs")))
    (setf (gethash '$invert ht) '(funcall (symbol "solve")))
    (setf (gethash '$determinant ht) '(funcall (symbol "det")))
    (setf (gethash '$transpose ht) '(funcall (symbol "t")))
    (setf (gethash 'mfactorial ht) '(funcall (symbol "factorial")))
    ht))

(defparameter *maxima-special-ir-map*
  (let ((ht (make-hash-table)))
    (setf (gethash 'mdefine ht) 'func-def-to-ir)
    (setf (gethash '$MATRIX ht) 'matrix-to-ir)
    (setf (gethash '$array ht) 'array-def-to-ir)
    (setf (gethash 'mprog ht) 'no-convert)
    ; (setf (gethash 'mprogn ht) 'mprogn-to-ir)
    (setf (gethash 'mcond ht) 'mcond-to-ir)
    (setf (gethash 'lambda ht) 'lambda-to-ir)
    ; (setf (gethash 'mdoin ht) 'for-list-to-ir)
    ; (setf (gethash 'mdo ht) 'for-loop-to-ir)
    ; (setf (gethash '%endcons ht) 'endcons-to-ir)
    ; (setf (gethash '$endcons ht) 'endcons-to-ir)
    ; (setf (gethash '$plot3d ht) 'plot-to-ir)
    ; (setf (gethash '$plot2d ht) 'plot-to-ir)
    ; (setf (gethash 'mexpt ht) 'mexpt-to-ir)
    ;(setf (gethash 'mfactorial ht) 'mfactorial-to-ir)
    ht))

(defun symbol-name-to-string (form)
  (string-left-trim "$%" (symbol-name form)))

(defun symbol-to-ir (form)
  `(symbol ,(maybe-invert-string-case (symbol-name-to-string form))))

(defun int32p (x)
  (and (integerp x)
       (< (abs x) (expt 2 32))))

;;; Generates IR for atomic forms
(defun atom-to-ir (form)
  (cond
    ((eq form 'nil) `(symbol "NULL"))
    ((eq form '$true) `(symbol "TRUE"))
    ((eq form 'T) T)
    ((stringp form) `(string ,form))
    ((and (not (symbolp form)) (floatp form)) `(num ,form))
    ((and (not (symbolp form)) (int32p form)) `(int ,form))
    ((and (not (symbolp form)) (integerp form)) `(num ,form))
    ((eq form '$%i) '(cplx 0 1)) ; iota complex number
    ((eq form '$%pi) '(symbol "pi")) ; Pi
    ((eq form '$%e) '(funcall (symbol "exp"))) ; Euler's Constant
    ((eq form '$inf) '(symbol "Inf"))
    (t (symbol-to-ir form))))

(defun cons-to-ir (form)
  (cond ((atom (caar form))
	 (let 
	     ((type (gethash (caar form) *maxima-direct-ir-map*)))
	   (cond 
	     (type (append type (mapcar #'maxima-to-ir (cdr form))))
	     ((setf type (gethash (caar form) *maxima-special-ir-map*))
	      (funcall type form))
             ((member 'ARRAY (car form) :test #'eq)
              (array-index-to-ir form))
	     (t 
	      `(funcall 
		 ,(symbol-to-ir (caar form)) 
		 ,@(mapcar 
		     #'maxima-to-ir 
		     (cdr form)))))))))

(defun maxima-to-ir (form)
  (cond ((atom form)
	 (atom-to-ir form))
	((and (consp form) (consp (car form)))
	 (cons-to-ir form))
	(t (cons 'no-convert form))))

; (defun mfactorial-to-ir (form)
;   `(funcall 'factorial ,@(mapcar #'maxima-to-ir (cdr form))))

; (defun mexpt-to-ir (form)
;   `(funcall (string "exp") ,@(mapcar #'maxima-to-ir (cdr form))))

(defun mlist-to-ir (form)
  `(STRUCT-LIST ,@(mapcar #'maxima-to-ir (cdr form))))

(defun mcond-to-ir (form)
  `(COND ,@(mapcar #'maxima-to-ir (cdr form))))

(defun lambda-to-ir (form)
  `(LAMBDA (FUNC-ARGS ,@(mapcar #'maxima-to-ir (cdadr form)))
     ,@(mapcar #'maxima-to-ir (cddr form))))

(defun func-def-to-ir (form)
  `(func-def ,(maxima-to-ir (caaadr form)) 
             (func-args ,@(mapcar (lambda (elm) (maxima-to-ir elm)) 
                                 (cdadr form))) 
             ,@(mapcar #'maxima-to-ir (cddr form))))

(defun matrix-to-ir (form)
  (cons 'MATRIX 
        (mapcar
          (lambda (elm) (maxima-to-ir elm))
          (cdr form))))

(defun array-def-to-ir (form)
  `(op-no-bracket = 
                  ,(maxima-to-ir (cadr form))
                  (ARRAY ,@(mapcar #'maxima-to-ir (cddr form)))))

(defun array-index-to-ir (form)
  `(ARRAY-INDEX ,(maxima-to-ir (caar form))
     ,@(mapcar (lambda (elm) (maxima-to-ir elm))
               (cdr form))))

(defvar *maxima-special-r-map* ())

(defparameter *ir-r-direct-templates* 
  (let ((ht (make-hash-table)))
    (setf (gethash 'symbol ht) 'symbol-to-r)
    (setf (gethash 'op ht) 'op-to-r)
    (setf (gethash 'boolop ht) 'boolop-to-r)
    (setf (gethash 'op-no-bracket ht) 'op-no-bracket-to-r)
    (setf (gethash 'comp-op ht) 'op-to-r)
    (setf (gethash 'unary-op ht) 'unary-op-to-r)
    (setf (gethash 'funcall ht) 'funcall-to-r)
    (setf (gethash 'int ht) 'int-to-r)
    (setf (gethash 'num ht) 'num-to-r)
    (setf (gethash 'cplx ht) 'cplx-to-r)
    (setf (gethash 'string ht) 'string-to-r)
    (setf (gethash 'struct-list ht) 'struct-list-to-r)
    (setf (gethash 'func-def ht) 'func-def-to-r)
    (setf (gethash 'lambda ht) 'lambda-to-r)
    (setf (gethash 'matrix ht) 'matrix-to-r)
    (setf (gethash 'cond ht) 'cond-to-r)
    (setf (gethash 'array ht) 'array-to-r)
    (setf (gethash 'array-index ht) 'array-index-to-r)
    ht))

(defun atom-to-r (form)
  (cond 
    ((eq form 'NIL) 'FALSE)
    ((eq form '$true) 'TRUE)
    (t form)))

(defun cons-to-r (form)
  (cond ((atom (caar form))
	 (let ((fel (gethash (car form) *ir-r-direct-templates*)))
	   (cond (fel 
		  (funcall fel form))
		 ;;((setf type (gethash (caar form *maxima-r-map*))) (funcall type form))
		 (t 'no-convert))))))

(defun ir-to-r (form)
  (typecase form
    (cons               
      (let ((type (gethash (car form) *ir-r-direct-templates*)))
        (cond
          (type (funcall type form))
          (t (format nil "no-convert: ~a" form)))))
    (t 
      (format nil "~a" form))))

(defun op-template (op)
  ;; replaces the control-string by the argument 
  ;; print (in brackets) all elements of the operator
  ;; separated by the operator as a string
  (format nil "~@?" "(~~{~~#[~~;~~a~~:;~~a ~a ~~]~~})"
	  op))

(defun op-no-bracket-template (op)
  (format nil "~@?" "~~{~~#[~~;~~a~~:;~~a ~a ~~]~~}"
	  op))

(defun op-to-r (form)
  (format nil (op-template (cadr form))
          (mapcar
            (lambda (elm) (ir-to-r elm))
            (cddr form))))

(defun boolop-to-r (form)
  (let ((*with-r-list* NIL))
   (format nil (op-template (cadr form))
           (mapcar
             (lambda (elm) (ir-to-r elm))
             (cddr form)))))

(defun op-no-bracket-to-r (form)
  (format nil (op-no-bracket-template (cadr form))
          (mapcar
            (lambda (elm) (ir-to-r elm))
            (cddr form))))

(defun symbol-to-r (form)
  (cadr form))

(defun unary-op-to-r (form)
 (format nil "(~a~a)" 
         (cadr form)
         (ir-to-r (caddr form))))

(defun funcall-to-r (form)
  (format nil "~a(~a)"
          (ir-to-r (cadr form))
          (ir-to-r (caddr form))))

(defun int-to-r (form)
  (format nil "~aL" (cadr form)))

(defun num-to-r (form)
  (format nil "~a" (cadr form)))

(defun cplx-to-r (form)
  (format nil "complex(1, ~a, ~a)"
          (cadr form)
          (caddr form)))

(defun string-to-r (form)
  (format nil "~c~a~c" #\" (cadr form) #\"))

(defun struct-list-to-r (form)
  (format nil "~:[(~{~a~^, ~})~;list(~{~a~^, ~})~]" 
          *with-r-list*
          (let ((*with-r-list* T))
            (mapcar
             (lambda (elm) (ir-to-r elm))
             (cdr form)))))

(defun func-args-to-r (form)
  (format nil "~{~a~^, ~}"
          (mapcar
            (lambda (elm) (ir-to-r elm))
            (cdr form))))

(defun func-def-to-r (form)
  (format nil "~a <- function(~a) { ~{~a~^~%~} }" 
          (ir-to-r (cadr form))
          (func-args-to-r (caddr form))
          (mapcar
            (lambda (elm) (ir-to-r elm))
            (cdddr form))))

(defun lambda-to-r (form)
  (format nil "function(~a) { ~a }"
        (func-args-to-r (cadr form))
        (ir-to-r (caddr form))))

(defun func-args-to-r (form)
  (format nil "~{~a~^, ~}"
          (mapcar
            (lambda (elm) (ir-to-r elm))
            (cdr form))))

(defun matrix-to-r (form)
  (format nil "matrix(data = c(~{~a~^, ~}), ncol = ~a, nrow = ~a)"
          (let
            ((ll NIL)) 
            (dolist 
              (i (cdr form) ll)
              (setf ll (append ll (mapcar 
                                    (lambda (d) 
                                      (ir-to-r d)) 
                                    (cdr i))))))
          (length (cdr form))
          (- (length (cadr form)) 1)))

; the last element in a COND form
; is alsways the default case
(defun cond-to-r (form)
  (cond ((= (length (cdr form)) 2) 
         (format nil "if(~a) { ~a }" 
                 (ir-to-r (cadr form)) 
                 (ir-to-r (caddr form))))
        ((= (length (cdr form)) 4) 
         (format nil "if(~a) { ~a } else { ~a }" 
                 (ir-to-r (cadr form)) 
                 (ir-to-r (caddr form))
                 (ir-to-r (car (last form)))))
        ((>= (length (cdr form)) 4)
         (format nil "if(~a) { ~a } else ~a"
                 (ir-to-r (cadr form))
                 (ir-to-r (caddr form))
                 (cond-to-r (cddr form))))))

(defun array-to-r (form)
  (format nil "array(dim = c(~{~a~^, ~}))"
          (mapcar 
            (lambda (elm) (ir-to-r elm))
            (cdr form))))

(defun array-index-to-r (form)
  (format nil "~a[~{~a~^,~}]"
          (ir-to-r (cadr form))
          (mapcar (lambda (elm) (ir-to-r elm))
                  (cddr form))))

; (defun stripdollar (form) 
;   (string-left-trim "$" (symbol-name form)))

(defun maxima2r (form)
  (ir-to-r (maxima-to-ir form)))

(defun maybe-invert-string-case (string)
  (let ((all-upper t)
	(all-lower t)
	(length (length string)))
    (dotimes (i length)
      (let ((ch (char string i)))
	(when (both-case-p ch)
	  (if (upper-case-p ch)
	      (setq all-lower nil)
	      (setq all-upper nil)))))
    (cond (all-upper
	   (string-downcase string))
	  (all-lower
	   (string-upcase string))
	  (t
	   string))))
