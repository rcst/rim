(defun tex-stripdollar (sym)
  (let
    ((nn-list (extract-trailing-digits (symbol-name sym))))
    (if nn-list
      ;; SYM matches foo_mm_nn.
      (apply #'concatenate 'string (tex-array `((,(intern (first nn-list)) 'array) ,@(rest nn-list)) nil nil))
      ;; SYM is a simple symbol.
      (let ((s (maybe-invert-string-case (quote-% (stripdollar sym)))))
        (if (> (length s) 1)
          (concatenate 'string "\\textit{" s "}")
          s)))))

(defun tex-mlabel (x l r)
  (tex (caddr x)
       (append l
	       (if (cadr x)
		   (list (format nil "\\mathtt{(~A)}\\quad " (tex-stripdollar (cadr x))))
;-  		   (list (format nil "" (tex-stripdollar (cadr x))))
		   nil))
       r 'mparen 'mparen))
