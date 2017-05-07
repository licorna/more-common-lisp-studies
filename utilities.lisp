;;
;; From Paul Graham's On Lisp
;;
(defmacro with-gensyms ((&rest names) &body body)
  `(let ,(loop for n in names collect `(,n (make-symbol ,(string n))))
     ,@body))

(defun as-keyword (sym) (intern (string sym) :keyword))

(defun mklist (x) (if (listp x) x (list x)))
;;
;; end of Graham's help
;;
