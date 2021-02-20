(in-package :maxima)

(defun mydispla (x)
	(princ "out;>>")
	(linear-displa x)
	(princ "<<out;")
)

(setf *maxima-prolog* (format nil "pid=~a~%" (getpid)))
(setf *alt-display2d* 'mydispla)
(setf *prompt-prefix* "prompt;>>")
(setf *prompt-suffix* "<<prompt;")
