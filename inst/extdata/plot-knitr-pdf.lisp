
;;by default denotes the current working directory
(defvar $plot2d_output_folder ".") 
(defvar $plot3d_output_folder ".") 
(defvar *builtin-plot2d* (symbol-function '$plot2d-impl))
(defvar *builtin-plot3d* (symbol-function '$plot3d-impl))

($set_random_state ($make_random_state ($absolute_real_time)))

;; if a random file name is used, every time the knitr engine is run
;; the same plot files with different names are saved
;; idea: generate hash from plot arguments string and use as filename
;;       however, this will overwrite the same file, but will work
;;       to avoid exhausting path lengths, only use first 6 digits of sha256sum
;;       maybe save plots in folder _maxima_files
(defmfun $plot2d (&rest args)
  (if (member '$pdf_file args)
    ;; Hmm. plot2d args already contains svg_file.
    ;; Just punt the call without modifying the arguments.
    (apply *builtin-plot2d* args)
    ;; Otherwise, append another argument for svg_file,
    ;; and then punt to built-in plot2d.
    (let*
      ((nnn ($substring ($sha256sum ($sconcat "plot2d" ($simplode args))) 1 7))
       (pdf-file ($sconcat $plot2d_output_folder "/plot2d-" nnn ".pdf"))
       (args-new (append args (list `((mlist) $pdf_file ,pdf-file)))))
      ;; (format t "plot2d: append pdf file ~s to arguments and call built-in plot2d.~%" pdf-file)
      ;; (format t "plot2d: new args: ~a~%" (mfuncall '$string (cons '(mlist) args-new)))
      (apply *builtin-plot2d* args-new))))

(defmfun $plot3d (&rest args)
  (if (member '$pdf_file args)
    ;; Hmm. plot2d args already contains svg_file.
    ;; Just punt the call without modifying the arguments.
    (apply *builtin-plot3d* args)
    ;; Otherwise, append another argument for svg_file,
    ;; and then punt to built-in plot2d.
    (let*
      ((nnn ($substring ($sha256sum ($sconcat "plot3d" ($simplode args))) 1 7))
       (pdf-file ($sconcat $plot3d_output_folder "/plot3d-" nnn ".pdf"))
       (args-new (append args (list `((mlist) $pdf_file ,pdf-file)))))
      ;; (format t "plot3d: append pdf file ~s to arguments and call built-in plot2d.~%" pdf-file)
      ;; (format t "plot3d: new args: ~a~%" (mfuncall '$string (cons '(mlist) args-new)))
      (apply *builtin-plot3d* args-new))))
