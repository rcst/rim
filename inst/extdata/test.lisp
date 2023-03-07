(load "inst/extdata/maxima-to-ir.lisp")
; (load "ir-to-r.lisp")
(load "inst/extdata/rds.lisp")

; test forms
(setf func-form '(($F SIMP) $X)) ; f(x)
(setf expt-form '((MEXPT) $E ((MEXPT SIMP) $X 2)))
(setf simple-form '((MPLUS) ((MMINUS) $D) $C $B $A))
(setf factorial-form '((MFACTORIAL SIMP) $A))
(setf cplx-form '((MQUOTIENT) ((MTIMES) 4 $%I) 3))
(setf funcdef-form '((MDEFINE SIMP) (($F) $X) ((%SIN) $X)))
(setf val-assign-form '((MSETQ SIMP) $A 4))
(setf list-form '((MLIST SIMP) 1 2 7 ((MPLUS SIMP) $X $Y)))
(setf lambda-form '((LAMBDA SIMP) ((MLIST) $I) ((MPLUS) $I 1)))
(setf adv-form '((MPLUS) ((MQUOTIENT) ((%LOG SIMP) ((MPLUS SIMP) 1 $X)) 2) ((MMINUS) ((MQUOTIENT) ((%LOG SIMP) ((MPLUS SIMP) -1 $X)) 2))))

expt-form

(maxima-to-ir simple-form)
(maxima-to-ir func-form)
(maxima-to-ir expt-form)
(maxima-to-ir cplx-form)
(maxima-to-ir factorial-form)
(maxima-to-ir funcdef-form)
(maxima-to-ir val-assign-form)
(maxima-to-ir list-form)
(maxima-to-ir lambda-form) ;; doesn't work yet
(maxima-to-ir adv-form)

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
