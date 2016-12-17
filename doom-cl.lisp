;;
;; Common Lisp Doom WAD File Reader
;; Simple exercise to study Common Lisp.
;; This version can read the first 12 bytes of the header and initializes
;; an object with this information.
;;
;; Completely based on Practical Common Lisp, chapter 24.
;;

;; 12-byte header
;; 4 byte (ascii string) with contents IWAD or PWAD
;; 4 byte (long) with number of lumps in file
;; 4 byte (long) with file offset of directory

;; Directory
;; 4 byte (long) file ofsset to start of lump
;; 4 byte (long) size of lump in bytes
;; 8 byte (ascii string) name of lump (padded with NULL bytes)


;; from the book
(defmacro with-gensyms ((&rest names) &body body)
  `(let ,(loop for n in names collect `(,n (make-symbol ,(string n))))
     ,@body))

;; from the book
(defmacro once-only ((&rest names) &body body)
  (let ((gensyms (loop for n in names collect (gensym (string n)))))
    `(let (,@(loop for g in gensyms collect `(,g (gensym))))
       `(let (,,@(loop for g in gensyms for n in names collect ``(,,g ,,n)))
          ,(let (,@(loop for n in names for g in gensyms collect `(,n ,g)))
             ,@body)))))

(defun as-keyword (sym) (intern (string sym) :keyword))

(defun slot->defclass-slot (spec)
  (let ((name (first spec)))
    `(,name :initarg ,(as-keyword name) :accessor ,name)))

(defgeneric read-value (type stream &key)
  (:documentation "Read a value of the given type from the stream."))

(defmethod read-value ((type (eql 'iso-8859-1-string)) in &key length)
  (with-output-to-string (s)
    (dotimes (i length)
      (write-char (code-char (read-byte in)) s))
    s))

(defmethod read-value ((type (eql 'u4)) in &key)
  (let ((numb 0))
    (setf (ldb (byte 8 0) numb) (read-byte in))
    (setf (ldb (byte 8 8) numb) (read-byte in))
    (setf (ldb (byte 8 16) numb) (read-byte in))
    (setf (ldb (byte 8 24) numb) (read-byte in))
    numb))

(defun mklist (x) (if (listp x) x (list x)))

(defun normalize-slot-spec (spec)
  (list (first spec) (mklist (second spec))))

(defun slot->read-value (spec stream)
  (destructuring-bind (name (type &rest args)) (normalize-slot-spec spec)
    `(setf ,name (read-value ',type ,stream ,@args))))

(defmacro define-binary-class (name slots)
  (with-gensyms (typevar objectvar streamvar)
    `(progn
       (defclass ,name ()
         ,(mapcar #'slot->defclass-slot slots))

       (defmethod read-value ((,typevar (eql ',name)) ,streamvar &key)
         (let ((,objectvar (make-instance ',name)))
           (with-slots ,(mapcar #'first slots) ,objectvar
             ,@(mapcar #'(lambda (x) (slot->read-value x streamvar)) slots))
           ,objectvar)))))

(define-binary-class wad-file
    ((identifier (iso-8859-1-string :length 4))
     (number-of-lumps u4)
     (directory-offset u4)
     ))

(defparameter *my-wad-file* "zdoom/doom2.wad")

(setf wad-object
      (read-value 'wad-file
                  (open *my-wad-file* :element-type '(unsigned-byte 8))))

(slot-value wad-object 'identifier)
(slot-value wad-object 'number-of-lumps)
(slot-value wad-object 'directory-offset)
