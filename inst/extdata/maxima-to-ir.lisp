(defparameter *maxima-direct-ir-map*
  (let ((ht (make-hash-table)))
    (setf (gethash 'mtimes ht) '(op *))
    (setf (gethash 'mplus ht) '(op +))
    (setf (gethash 'mexpt ht) '(op ^))
    (setf (gethash 'rat ht) '(op /))
    (setf (gethash 'mquotient ht) '(op /))
    (setf (gethash 'msetq ht) '(op-no-bracket <-))
    (setf (gethash 'mlist ht) '(struct-list))
    (setf (gethash 'mand ht) '(boolop (symbol "and")))
    (setf (gethash 'mor ht) '(boolop (symbol "or")))
    (setf (gethash 'mnot ht) '(funcall (symbol "not")))
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
    (setf (gethash 'mreturn ht) '(funcall (symbol "return")))
    (setf (gethash 'mabs ht) '(funcall (symbol "abs")))
    ht))

(defparameter *maxima-special-ir-map*
  (let ((ht (make-hash-table)))
    (setf (gethash 'mdefine ht) 'func-def-to-ir)
    ; (setf (gethash '%array ht) 'array-def-to-ir)
    ; (setf (gethash 'mprog ht) 'mprog-to-ir)
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
    (setf (gethash 'mfactorial ht) 'mfactorial-to-ir)
    ht))

(defun symbol-name-to-string (form)
  (string-left-trim "$" (symbol-name form)))

(defun symbol-to-ir (form)
  `(symbol ,(symbol-name-to-string form)))

;;; Generates IR for atomic forms
(defun atom-to-ir (form)
  (cond
    ((eq form 'nil) `(symbol "NULL"))
    ((eq form '$true) `(symbol "TRUE"))
    ((stringp form) `(string ,form))
    ((and (not (symbolp form)) (floatp form)) `(num ,form))
    ((and (not (symbolp form)) (integerp form)) `(int ,form))
    ((eq form '$%i) '(cplx 0 1)) ; iota complex number
    ((eq form '$%pi) '(num (symbol "pi") 0)) ; Pi
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

(defun mfactorial-to-ir (form)
  `(funcall (string "factorial") ,@(mapcar #'maxima-to-ir (cdr form))))

; (defun mexpt-to-ir (form)
;   `(funcall (string "exp") ,@(mapcar #'maxima-to-ir (cdr form))))

(defun mlist-to-ir (form)
  `(list ,@(mapcar #'maxima-to-ir (cdr form))))

;; the second element of the list
;; should be a pairlist
;; (defun lambda-to-ir (form)
;;   `(function ,@(mlist-to-pairlist (cadr form)) ,@(mapcar #'maxima-to-ir (cddr form))))

(defun lambda-to-ir (form)
  `(funcall (string "function") ,@(mapcar #'maxima-to-ir (cdr form))))

(defun func-def-to-ir (form)
  `(op-no-bracket <- 
		  ,(maxima-to-ir (caaadr form)) 
		  (funcall (string "function") 
			   ,(mlist-to-ir (cadr form))) 
		  ,@(mapcar #'maxima-to-ir (cddr form))))
