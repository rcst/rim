;imported just for testing purposes
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

; (load "maxima-to-ir.lisp")
; (load "ir-to-r.lisp")
(load "parser.lisp")
(load "test-forms.lisp")
; (load "rds.lisp")

(declaim (optimize (debug 3)))

(maxima-to-ir simple-form)
(maxima-to-ir func-form)
(maxima-to-ir expt-form)
(maxima-to-ir cplx-form)
(maxima-to-ir factorial-form)
(maxima-to-ir list-form)
(maxima-to-ir list-form2)
(maxima-to-ir funcdef-form)
(maxima-to-ir jfunc-form)
(maxima-to-ir val-assign-form)
(maxima-to-ir lambda-form)
(maxima-to-ir adv-form)
(maxima-to-ir matrix-form)
(maxima-to-ir pi-form)
(maxima-to-ir atan-form)
(maxima-to-ir compare-form)
(maxima-to-ir longcompare-form)
(maxima-to-ir matmult-form)
(maxima-to-ir determinant-form)
(maxima-to-ir mattp-form)
(maxima-to-ir create-array-form)
(maxima-to-ir set-array-form)
(maxima-to-ir index-array-form)

(maxima2r simple-form)
(maxima2r func-form)
(maxima2r adv-form)
(maxima2r expt-form)
(maxima2r cplx-form)
(maxima2r factorial-form)
(maxima2r list-form)
(maxima2r list-form2)
(maxima2r funcdef-form)
(maxima2r jfunc-form)
(maxima2r val-assign-form)
(maxima2r matrix-form)
(maxima2r matrix-compl)
(maxima2r lambda-form)
(maxima2r pi-form)
(maxima2r atan-form)
(maxima2r compare-form)
(maxima2r longcompare-form)
(maxima2r matmult-form)
(maxima2r determinant-form)
(maxima2r mattp-form)
(maxima2r create-array-form)
(maxima2r set-array-form)
(maxima2r index-array-form)

;;; TODO
(maxima-to-ir string-form)


(;;;) NOTES
;;;
;;; function maybe-invert-string-case turns the string
;;; FOO into foo
;;; foo into FOO
;;; Foo into Foo
;;; Lisp symbols are always upper case
;;; So, in a Maxima expression in LISP form
;;; all uppercase symbols are to be made lower case
;;; except for when the symbol is surrounded by vertical bars
