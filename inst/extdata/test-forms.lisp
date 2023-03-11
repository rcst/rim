; test forms
(setf func-form '(($F SIMP) $X)) ; f(x)
(setf expt-form '((MEXPT) $E ((MEXPT SIMP) $X 2)))
(setf simple-form '((MPLUS) ((MMINUS) $D) $C $B $A))
(setf factorial-form '((MFACTORIAL SIMP) $A))
(setf cplx-form '((MQUOTIENT) ((MTIMES) 4 $%I) 3))
(setf funcdef-form '((MDEFINE SIMP) (($F) $X) ((%SIN) $X)))
(setf val-assign-form '((MSETQ SIMP) $A 4))
(setf list-form '((MLIST SIMP) 1 2 7 ((MPLUS SIMP) $X $Y)))
(setf list-form2 '((MLIST SIMP) ((MSETQ SIMP) $A 100) ((MSETQ SIMP) $B 200)))
(setf lambda-form '((LAMBDA SIMP) ((MLIST) $I) ((MPLUS) $I 1)))
(setf adv-form '((MPLUS) ((MQUOTIENT) ((%LOG SIMP) ((MPLUS SIMP) 1 $X)) 2) ((MMINUS) ((MQUOTIENT) ((%LOG SIMP) ((MPLUS SIMP) -1 $X)) 2))))
(setf matrix-form '(($MATRIX SIMP) ((MLIST SIMP) 1 2) ((MLIST SIMP) 2 3)))
(setf array-function '((MDEFINE SIMP) (($C ARRAY) $X $Y) ((MQUOTIENT) $Y $X)))


