; test forms
(setf func-form '(($F SIMP) $X)) ; f(x)
(setf expt-form '((MEXPT) $E ((MEXPT SIMP) $X 2)))
(setf simple-form '((MPLUS) ((MMINUS) $D) $C $B $A))
(setf factorial-form '((MFACTORIAL SIMP) $A))
(setf cplx-form '((MPLUS) ((MTIMES) 3 $%I) 4))
(setf funcdef-form '((MDEFINE SIMP) (($F) $X) ((%SIN) $X)))
(setf val-assign-form '((MSETQ SIMP) $A 4))
(setf list-form '((MLIST SIMP) 1 2 7 ((MPLUS SIMP) $X $Y)))
(setf list-form2 '((MLIST SIMP) ((MSETQ SIMP) $A 100) ((MSETQ SIMP) $B 200)))
(setf lambda-form '((LAMBDA SIMP) ((MLIST) $I) ((MPLUS) $I 1)))
(setf adv-form '((MPLUS) ((MQUOTIENT) ((%LOG SIMP) ((MPLUS SIMP) 1 $X)) 2) ((MMINUS) ((MQUOTIENT) ((%LOG SIMP) ((MPLUS SIMP) -1 $X)) 2))))
(setf matrix-form '(($MATRIX SIMP) ((MLIST SIMP) 1 2) ((MLIST SIMP) 2 3)))
(setf matrix-form-2 '(($MATRIX SIMP) ((MLIST SIMP) 1 0 0) ((MLIST SIMP) 0 1 0) ((MLIST SIMP) 0 0 1)))
(setf array-function '((MDEFINE SIMP) (($C ARRAY) $X $Y) ((MQUOTIENT) $Y $X)))
(setf matrix-compl '(($MATRIX SIMP) ((1 2) (2 3))))
; lists directly under a logical operator should not be turned into R-lists
(setf boolexpror '((MOR SIMP) ((MLIST SIMP) ((MLESSP SIMP) 1 $X))((MLIST SIMP) ((MLESSP SIMP) $X -1))))

(maxima2r boolexpror)
(maxima2r matrix-form)
(maxima2r matrix-form-2)
(maxima2r lambda-form)
(maxima2r cplx-form)
(maxima2r expt-form)
(maxima2r factorial-form)
(maxima2r adv-form)
(maxima2r list-form)
(maxima2r list-form2)
(maxima2r val-assign-form)
(maxima2r funcdef-form)
(maxima2r simple-form)
(maxima2r array-function)

