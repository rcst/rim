($load "draw")
;;by default denotes the current working directory
(defvar $plot_output_folder ".") 
(defvar *builtin-plot2d* (symbol-function '$plot2d-impl))
(defvar *builtin-plot3d* (symbol-function '$plot3d-impl))
(defvar *builtin-draw* (symbol-function '$draw))
(defvar *builtin-draw2d* (symbol-function '$draw2d))
(defvar *builtin-draw3d* (symbol-function '$draw3d))

;; ($set_random_state ($make_random_state ($absolute_real_time)))

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
      ((nnn ($substring ($sha256sum ($sconcat "/plot2d" ($simplode args))) 1 7))
       (pdf-file ($sconcat $plot_output_folder "/plot2d-" nnn ".pdf"))
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
       (pdf-file ($sconcat $plot_output_folder "/plot3d-" nnn ".pdf"))
       (args-new (append args (list `((mlist) $pdf_file ,pdf-file)))))
      ;; (format t "plot3d: append pdf file ~s to arguments and call built-in plot2d.~%" pdf-file)
      ;; (format t "plot3d: new args: ~a~%" (mfuncall '$string (cons '(mlist) args-new)))
      (apply *builtin-plot3d* args-new))))

(defmfun $draw (&rest args)
  (if (member '$file_name args)
    ;; Hmm. plot2d args already contains svg_file.
    ;; Just punt the call without modifying the arguments.
    (apply *builtin-draw* args)
    ;; Otherwise, append another argument for svg_file,
    ;; and then punt to built-in plot2d.
    (let*
      ((nnn ($substring ($sha256sum ($sconcat "draw" ($simplode args))) 1 7))
       (file_name ($sconcat $plot_output_folder "/draw-" nnn))
       (args-new (append args (list `((mequal) $file_name ,file_name))))
       (args-new (append args-new (list `((mequal) $terminal $pdf)))))
       ;; (format t "draw: append pdf file ~s to arguments and call built-in draw.~%" file_name)
       ;; (format t "draw: new args: ~a~%" (mfuncall '$string (cons '(mlist) args-new)))
      (apply *builtin-draw* args-new))))

(defmfun $draw2d (&rest args)
  (if (member '$file_name args)
    ;; Hmm. plot2d args already contains svg_file.
    ;; Just punt the call without modifying the arguments.
    (apply *builtin-draw2d* args)
    ;; Otherwise, append another argument for svg_file,
    ;; and then punt to built-in plot2d.
    (let*
      ((nnn ($substring ($sha256sum ($sconcat "draw2d" ($simplode args))) 1 7))
       (file_name ($sconcat $plot_output_folder "/draw2d-" nnn))
       (args-new (append args (list `((mequal) $file_name ,file_name))))
       (args-new (append args-new (list `((mequal) $terminal $pdf)))))
       ;; (format t "draw: append pdf file ~s to arguments and call built-in draw.~%" file_name)
       ;; (format t "draw: new args: ~a~%" (mfuncall '$string (cons '(mlist) args-new)))
      (apply *builtin-draw2d* args-new))))

(defmfun $draw3d (&rest args)
  (if (member '$file_name args)
    ;; Hmm. plot2d args already contains svg_file.
    ;; Just punt the call without modifying the arguments.
    (apply *builtin-draw3d* args)
    ;; Otherwise, append another argument for svg_file,
    ;; and then punt to built-in plot2d.
    (let*
      ((nnn ($substring ($sha256sum ($sconcat "draw3d" ($simplode args))) 1 7))
       (file_name ($sconcat $plot_output_folder "/draw3d-" nnn))
       (args-new (append args (list `((mequal) $file_name ,file_name))))
       (args-new (append args-new (list `((mequal) $terminal $pdf)))))
       ;; (format t "draw: append pdf file ~s to arguments and call built-in draw.~%" file_name)
       ;; (format t "draw: new args: ~a~%" (mfuncall '$string (cons '(mlist) args-new)))
      (apply *builtin-draw3d* args-new))))
