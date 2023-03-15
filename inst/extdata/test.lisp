; (load "maxima-to-ir.lisp")
; (load "ir-to-r.lisp")
(load "parser.lisp")
(load "test-forms.lisp")
; (load "rds.lisp")

(maxima-to-ir simple-form)
(maxima-to-ir func-form)
(maxima-to-ir expt-form)
(maxima-to-ir cplx-form)
(maxima-to-ir factorial-form)
(maxima-to-ir list-form)
(maxima-to-ir list-form2)
(maxima-to-ir funcdef-form)
(maxima-to-ir val-assign-form)
(maxima-to-ir lambda-form)
(maxima-to-ir adv-form)
(maxima-to-ir matrix-form)

(maxima-to-r simple-form)
(maxima-to-r func-form)
(maxima-to-r adv-form)
(maxima-to-r expt-form)
(maxima-to-r cplx-form)
(maxima-to-r factorial-form)
(maxima-to-r list-form)
(maxima-to-r list-form2)
(maxima-to-r funcdef-form)
(maxima-to-r val-assign-form)
(maxima-to-r matrix-form)
(maxima-to-r lambda-form)

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
