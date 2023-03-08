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

