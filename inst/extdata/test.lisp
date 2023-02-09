(load "maxima-to-ir.lisp")
; (load "ir-to-r.lisp")
(load "rds.lisp")

; test forms
(setf func-form '(($F SIMP) $X)) ; f(x)
(setf expt-form '((MEXPT) $E ((MEXPT SIMP) $X 2)))
(setf simple-form '((MPLUS) ((MMINUS) $D) $C $B $A))
(setf factoril-form '((MFACTORIAL SIMP) $A))
(setf cplx-form '((MQUOTIENT) ((MTIMES) 4 $%I) 3))

expt-form

(maxima-to-ir simple-form)
(maxima-to-ir func-form)
(maxima-to-ir expt-form)
(maxima-to-ir cplx-form)

(floatp 1.1)

(maxima-to-r simple-form)
(mplus-to-r '((MPLUS) A B C))

(cadr '((MPLUS) A B C))
(cddr '((MPLUS) A B C))

(untrace mplus-to-r)
(untrace)

(format nil (op-template "+") '(A B C))

;;; NOTES
;;;
;;; function maybe-invert-string-case turns the string
;;; FOO into foo
;;; foo into FOO
;;; Foo into Foo
;;; Lisp symbols are always upper case
;;; So, in a Maxima expression in LISP form
;;; all uppercase symbols are to be made lower case
;;; except for when the symbol is surrounded by vertical bars
