($load "draw")
;;by default denotes the current working directory
(defvar $plot_output_folder ".") 
(defvar *builtin-plot2d* (symbol-function '$plot2d-impl))
(defvar *builtin-plot3d* (symbol-function '$plot3d-impl))
(defvar *builtin-draw* (symbol-function '$draw))
(defvar *builtin-draw2d* (symbol-function '$draw2d))
(defvar *builtin-draw3d* (symbol-function '$draw3d))

(defmfun $plot2d (&rest args)
  (if (member '$pdf_file args)
    (apply *builtin-plot2d* args)
    (let*
      ((nnn ($substring ($sha256sum ($sconcat "/plot2d" ($simplode args))) 1 7))
       (pdf-file ($sconcat $plot_output_folder "/plot2d-" nnn ".pdf"))
       (args-new (append args (list `((mlist) $pdf_file ,pdf-file)))))
      (apply *builtin-plot2d* args-new))))

(defmfun $plot3d (&rest args)
  (if (member '$pdf_file args)
    (apply *builtin-plot3d* args)
    (let*
      ((nnn ($substring ($sha256sum ($sconcat "plot3d" ($simplode args))) 1 7))
       (pdf-file ($sconcat $plot_output_folder "/plot3d-" nnn ".pdf"))
       (args-new (append args (list `((mlist) $pdf_file ,pdf-file)))))
      (apply *builtin-plot3d* args-new))))

(defmfun $draw (&rest args)
  (if (member '$file_name args)
    (apply *builtin-draw* args)
    (let*
      ((nnn ($substring ($sha256sum ($sconcat "draw" ($simplode args))) 1 7))
       (file_name ($sconcat $plot_output_folder "/draw-" nnn))
       (args-new (append args (list `((mequal) $file_name ,file_name))))
       (args-new (append args-new (list `((mequal) $terminal $pdf)))))
      (apply *builtin-draw* args-new)
      `((mlist) ,file_name ,($sconcat file_name ".pdf")))))

(defmfun $draw2d (&rest args)
  (if (member '$file_name args)
    (apply *builtin-draw2d* args)
    (let*
      ((nnn ($substring ($sha256sum ($sconcat "draw2d" ($simplode args))) 1 7))
       (file_name ($sconcat $plot_output_folder "/draw2d-" nnn))
       (args-new (append args (list `((mequal) $file_name ,file_name))))
       (args-new (append args-new (list `((mequal) $terminal $pdf)))))
      (apply *builtin-draw2d* args-new)
      `((mlist) ,file_name ,($sconcat file_name ".pdf")))))

(defmfun $draw3d (&rest args)
  (if (member '$file_name args)
    (apply *builtin-draw3d* args)
    (let*
      ((nnn ($substring ($sha256sum ($sconcat "draw3d" ($simplode args))) 1 7))
       (file_name ($sconcat $plot_output_folder "/draw3d-" nnn))
       (args-new (append args (list `((mequal) $file_name ,file_name))))
       (args-new (append args-new (list `((mequal) $terminal $pdf)))))
      (apply *builtin-draw3d* args-new)
      `((mlist) ,file_name ,($sconcat file_name ".pdf")))))
