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
