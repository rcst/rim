(defparameter simple-form '((MPLUS) ((MMINUS) $D) $C $B $A))

; a hashmap to list SEXPTYPE numbers
(defparameter *SXPS* 
  (let ((ht (make-hash-table)))
    (setf (gethash 'NILSXP ht) 0)
    (setf (gethash 'SYMSXP ht) 1)
    (setf (gethash 'LISTSXP ht) 2)
    (setf (gethash 'CLOSXP ht) 3)
    (setf (gethash 'ENVSXP ht) 4)
    (setf (gethash 'PROMSXP ht) 5)
    (setf (gethash 'LANGSXP ht) 6)
    (setf (gethash 'SPECIALSXP ht) 7)
    (setf (gethash 'BUILTINSXP ht) 8)
    (setf (gethash 'CHARSXP ht) 9)
    (setf (gethash 'LGLSXP ht) 10)
    (setf (gethash 'INTSXP ht) 13)
    (setf (gethash 'REALSXP ht) 14)
    (setf (gethash 'CPLXSXP ht) 15)
    (setf (gethash 'STRSXP ht) 16)
    (setf (gethash 'DOTSXP ht) 17)
    (setf (gethash 'ANYSXP ht) 18)
    (setf (gethash 'VECSXP ht) 19)
    (setf (gethash 'EXPRSXP ht) 20)
    (setf (gethash 'BCODESXP ht) 21)
    ht))

(defvar *maxima-special-r-map* ())

(defparameter *maxima-direct-r-map* 
  (let ((ht (make-hash-table)))
    (setf (gethash 'MPLUS ht) 'mplus-to-r)
    (setf (gethash 'MMINUS ht) 'mminus-to-r)
    ht))

(defun atom-to-r (form)
  (cond 
    ((eq form 'NIL) 'FALSE)
    ((eq form '$true) 'TRUE)
    (t form)))

(defun cons-to-r (form)
  (cond ((atom (caar form))
	 (let ((fel (gethash (caar form) *maxima-direct-r-map*)))
	   (cond (fel 
		  (funcall fel form))
		 ;;((setf type (gethash (caar form *maxima-r-map*))) (funcall type form))
		 (t 'no-convert))))))

(defun maxima-to-r (form)
  (cond ((atom form) 
	 (atom-to-r form))
	((and (consp form) (consp (car form))) 
	 (cons-to-r form))
	(t 
	 (cons 'no-convert form))))

(defun op-template (op)
  ;; replaces the control-string by the argument 
  ;; print (in brackets) all elements of the operator
  ;; separated by the operator as a string
  (format nil "~@?" "(~~{~~#[~~;~~a~~:;~~a ~a ~~]~~})"
	  op))

(defun op-no-bracket-template (op)
  (format nil "~@?" "~~{~~#[~~;~~a~~:;~~a ~a ~~]~~}"
	  op))

(defun mplus-to-r (form)
  (format nil (op-template "+") 
	  (mapcar 
	    (lambda (elm) (maxima-to-r elm)) 
	    (cdr form))))

(defun mminus-to-r (form)
  (format nil "-~A" (cadr form)))


(defun stripdollar (form) 
  (string-left-trim "$" (symbol-name form)))


;; +++++++++++++ RDS encoding ++++++++++++
(defun rds-character (x)
  ;; serializes a single character vector element
  (format nil "~A~%~A~%~A"
	  (logior (gethash 'CHARSXP *SXPS*) (expt 2 18)) 
	  (length x)
	  x))

(defun rds-string (x)
  ;; serializes a single string or a LIST of strings i.e., a character vector (in R)
  (if (listp x)
      (format nil "~A~%~A~%~{~A~^~%~}"
	      (gethash 'STRSXP *SXPS*)
	      (list-length x)
	      (mapcar #'rds_character x))
      (format nil "~A~%~A~%~A"
	      (gethash 'STRSXP *SXPS*)
	      1
	      (rds_character x))))

;; (rds_string '("hello" "world" "you"))

(defun rds-symbol (x)
  (format nil "~A~%~A"
	  (gethash 'SYMSXP *SXPS*)
	  (rds-character x)))

(defun rds-number (x numtype)
  ;; serializes a LIST of numbers one of integer, real or complex
  ;; defined by numtype
  (format nil "~A~%~A~%~{~A~^~%~}"
	  numtype
	  (length x)
	  x))

(defun rds-pairlist (x))

(defun rds-list (x))

(defun rds_language (sym)
; function that serializes a language object
  (format nil "~A~%~A~%262153~%~A~%~A" 
	  (gethash 'LANGSXP *SXPS*)
	  (gethash 'SYMSXP *SXPS*)
	  (length sym)
	  sym))

;; working examples of forms
;; ((MPLUS) ((MQUOTIENT) ((%LOG SIMP) ((MPLUS SIMP) 1 $X)) 2) ((MMINUS) ((MQUOTIENT) ((%LOG SIMP) ((MPLUS SIMP) -1 $X)) 2)))
