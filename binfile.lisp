;;
;; binfile.lisp
;; binary file reading from common-lisp
;;

(defgeneric read-value (type stream &key)
  (:documentation "Read a value of the given type from the stream."))

(defmethod read-value ((type (eql 'iso-8859-1-string)) in &key length)
  (remove #\Nul
          (with-output-to-string (s)
            (dotimes (i length)
              (write-char (code-char (read-byte in)) s)))))

(defun unsigned-to-signed (value size)
  "Transforms a unsigned integer to signed."
  (let ((max-signed (expt 2 (1- (* 8 size))))
        (to-subtract (expt 2 (* 8 size))))
    (if (>= value max-signed)
        (- value to-subtract)
        value)))

(defmethod read-value ((type (eql 'u4)) in &key)
  (let ((numb 0))
    (setf (ldb (byte 8 0) numb) (read-byte in))
    (setf (ldb (byte 8 8) numb) (read-byte in))
    (setf (ldb (byte 8 16) numb) (read-byte in))
    (setf (ldb (byte 8 24) numb) (read-byte in))
    numb))

(defmethod read-value ((type (eql 'u2)) in &key)
  (let ((numb 0))
    (setf (ldb (byte 8 0) numb) (read-byte in))
    (setf (ldb (byte 8 8) numb) (read-byte in))
    numb))

(defmethod read-value ((type (eql 's2)) in &key)
  (let ((numb 0))
    (setf (ldb (byte 8 0) numb) (read-byte in))
    (setf (ldb (byte 8 8) numb) (read-byte in))
    (unsigned-to-signed numb 2)))

(defmethod read-value ((type (eql 'noop)) in &key)
  "Used to add slots that won't be initialized."
  nil)

(defun normalize-slot-spec (spec)
  (list (first spec) (mklist (second spec))))

(defun slot->read-value (spec stream)
  (destructuring-bind (name (type &rest args)) (normalize-slot-spec spec)
    `(setf ,name (read-value ',type ,stream ,@args))))

(defun slot->defclass-slot (spec)
  (let ((name (first spec)))
    `(,name :initarg ,(as-keyword name) :accessor ,name)))

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
