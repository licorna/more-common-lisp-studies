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


(defparameter *my-wad-file* "doom2.wad")

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

(defmethod read-value ((type (eql 'noop)) in &key)
  "Used to add slots that won't be initialized."
  nil)

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

(defun read-current-lump (in)
  "Reads the LUMP in the current file position."
  (let ((result nil))
    (push (read-value 'u4 in) result)
    (push (read-value 'u4 in) result)
    (push (remove #\Nul (read-value 'iso-8859-1-string in :length 8)) result)
    result))

(defun read-lump (in wad-object lump-number)
  "This sets the file position to where the lump-number should start, and then
it reads from that place."
  (let ((absolute-position (+ (slot-value wad-object 'directory-offset)
                              (* lump-number 16))))
    (file-position in absolute-position)
    (read-current-lump in)))

(defun read-current-map (in current-lump)
  "Expects current lump to be a lump of type map. Will append every following
lump to this one, until finding a lump with size 0."
  (let ((map-lumps nil)
        (current nil)
        (i 0))
    (loop do
      (incf i)
      (setf current (read-current-lump in))
      (if (not (= (second current) 0))
          (push current map-lumps))
          while (and (not (= (second current) 0))
                     (< i 10)))
    (if (= (second current) 0)
        (file-position in (- (file-position in) 16)))  ;; not consume last lump
    (append current-lump (list map-lumps))))

(defun read-lump-meta (in wad-object)
  "Reads lumps from stream. Also moves stuff from maps into a "
  (let ((lumps nil)
        (directory-offset (slot-value wad-object 'directory-offset)))
    (file-position in directory-offset)
    (do ((i 2 (1+ i))
         (current))
        ((or (= i (slot-value wad-object 'number-of-lumps))
             (>= (file-position in) (file-length in))))
      (setf current (read-current-lump in))
      (if (eq (search "MAP" (first current)) 0)
          (push (read-current-map in current) lumps)
          (push current lumps)))
    lumps))


(define-binary-class wad-file
    ((identifier (iso-8859-1-string :length 4))
     (number-of-lumps u4)
     (directory-offset u4)
     (lumps noop)))


(setf in (open *my-wad-file* :element-type '(unsigned-byte 8)))
(setf wad-object
      (read-value 'wad-file in))
(setf (slot-value wad-object 'lumps) (read-lump-meta in wad-object))
(slot-value wad-object 'identifier)
(slot-value wad-object 'number-of-lumps)
(slot-value wad-object 'directory-offset)


;; very ugly implementation as I have to maintain the file descriptor open.
;; I'll try to implement something that reads everylump at the beggining and
;; then the file descriptor can be closed.
;; (read-lump in wad-object 0) ;; first lump
;; (read-lump in wad-object 1) ;; second lump
