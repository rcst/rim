(defvar *maxima-special-r-map* ())

(defparameter *ir-r-direct-templates* 
  (let ((ht (make-hash-table)))
    (setf (gethash 'symbol ht) 'symbol-to-r)
    (setf (gethash 'op ht) 'op-to-r)
    (setf (gethash 'op-no-bracket ht) 'op-no-bracket-to-r)
    (setf (gethash 'unary-op ht) 'unary-op-to-r)
    (setf (gethash 'funcall ht) 'funcall-to-r)
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

; (defun ir-to-r (form)
;   (cond ((atom form) 
; 	 (atom-to-r form))
; 	((and (consp form) (consp (car form))) 
; 	 (cons-to-r form))
; 	(t 
; 	 (cons 'no-convert form))))

(defun ir-to-r (form)
  (typecase form
    (cons 
      (let ((type (gethash (car form) *ir-r-direct-templates*)))
        (cond
          (type (funcall type form))
          (t (format nil "no-convert: (~a)" form)))))
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

; (defun mplus-to-r (form)
;   (format nil (op-template "+") 
; 	  (mapcar 
; 	    (lambda (elm) (maxima-to-r elm)) 
; 	    (cdr form))))

; (defun mminus-to-r (form)
;   (format nil "-~A" (cadr form)))

(defun op-to-r (form)
  (format nil (op-template (cadr form))
          (mapcar
            (lambda (elm) (ir-to-r elm))
            (cddr form))))

(defun symbol-to-r (form)
  (cadr form))

(defun unary-op-to-r (form)
 (format nil "(~a~a)" 
         (cadr form)
         (ir-to-r (caddr form))))

(defun stripdollar (form) 
  (string-left-trim "$" (symbol-name form)))
