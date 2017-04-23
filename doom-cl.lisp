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

(ql:quickload "alexandria")
;; (ql:quickload "sdl")


(defparameter *my-wad-file* "e1m4b.wad")

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
  (remove #\Nul
          (with-output-to-string (s)
            (dotimes (i length)
              (write-char (code-char (read-byte in)) s)))))

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

(defun unsigned-to-signed (value size)
  (let ((max-signed (expt 2 (1- (* 8 size))))
        (to-subtract (expt 2 (* 8 size))))
    (if (>= value max-signed)
        (- value to-subtract)
        value)))

(defmethod read-value ((type (eql 's2)) in &key)
  (let ((numb 0))
    (setf (ldb (byte 8 0) numb) (read-byte in))
    (setf (ldb (byte 8 8) numb) (read-byte in))
    (unsigned-to-signed numb 2)))

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
    (push (read-value 'iso-8859-1-string in :length 8) result)
    result))

(defun read-lump (in wad-object lump-number)
  "This sets the file position to where the lump-number should start, and then
it reads from that place."
  (let ((absolute-position (+ (slot-value wad-object 'directory-offset)
                              (* lump-number 16))))
    (file-position in absolute-position)
    (read-current-lump in)))

(defun thing-angle (angle)
  (case angle
    (0 'east)
    (45 'north-east)
    (90 'north)
    (135 'north-west)
    (180 'west)
    (225 'south-west)
    (270 'south)))

(defun read-thing0 (in)
  (list (read-value 's2 in) ;; x-position
        (read-value 's2 in) ;; y-position
        (thing-angle (read-value 'u2 in)) ;; angle
        (read-value 'u2 in) ;; type
        (read-value 'u2 in) ;; spawn-flags
        ))

(defun read-things (thing in)
  (let ((thing-size (second thing))
        (things nil))
    (file-position in (third thing))
    (dotimes (i (/ thing-size 10))
      (push (read-thing0 in) things))
    things))

(defun read-linedef0 (in)
  (list (read-value 'u2 in) ;; beginning vertex
        (read-value 'u2 in) ;; ending vertex
        (read-value 'u2 in) ;; flags
        (read-value 'u2 in) ;; line-type
        (read-value 'u2 in) ;; sector tag
        (read-value 'u2 in) ;; right sidef
        (read-value 'u2 in) ;; left sidef
        ))

(defun read-linedefs (linedef in)
  (let ((linedef-size (second linedef))
        (linedefs nil))
    (file-position in (third linedef))
    (dotimes (i (/ linedef-size 14))
      (push (read-linedef0 in) linedefs))
    linedefs))

(defun read-sidedef0 (in)
  (list (read-value 's2 in) ;; x offset
        (read-value 's2 in) ;; y offset
        (read-value 'iso-8859-1-string in :length 8) ;; upper-texture
        (read-value 'iso-8859-1-string in :length 8) ;; lower-texture
        (read-value 'iso-8859-1-string in :length 8) ;; middle-texture
        (read-value 'u2 in) ;; Sector reference
        ))

(defun read-sidedefs (sidef in)
  (let ((sidef-size (second sidef))
        (sidefs nil))
    (file-position in (third sidef))
    (dotimes (i (/ sidef-size 30))
      (push (read-sidedef0 in) sidefs))
    sidefs))

(defun read-vertex0 (in)
  (list (read-value 's2 in)  ;; x position
        (read-value 's2 in)  ;; y position
        ))

(defun read-vertexes (vertex in)
  (let ((vertex-size (second vertex))
        (vertexes nil))
    (file-position in (third vertex))
    (dotimes (i (/ vertex-size 4))
      (push (read-vertex0 in) vertexes))
    vertexes))

(defun read-segment0 (in)
  (list (read-value 'u2 in)
        (read-value 'u2 in)
        (read-value 's2 in)
        (read-value 'u2 in)
        (read-value 's2 in)
        (read-value 's2 in)))

(defun read-segs (segs in)
  (let ((segs-size (second segs))
        (segments nil))
    (file-position in (third segs))
    (dotimes (i (/ segs-size 12))
      (push (read-segment0 in) segments))
    segments))

(defun read-ssector0 (in)
  (list (read-value 'u2 in) ;; not sure about specs, it mentions short
        (read-value 'u2 in) ;; i assume 2 byte unsigned
        ))

(defun read-ssectors (ssector in)
  (let ((ssector-size (second ssector))
        (ssectors nil))
    (file-position in (third segs))
    (dotimes (i (/ ssector-size 4))
      (push (read-ssector0 in) ssectors))
    ssectors))

(defun process-lump (lump in)
  (let ((file-position-restore (file-position in))
        (result (alexandria:switch ((first lump) :test #'string=)
                  ("THINGS" (list "THINGS" (read-things lump in)))
                  ("LINEDEFS" (list "LINEDEFS" (read-linedefs lump in)))
                  ("SIDEDEFS" (list "SIDEDEFS" (read-sidedefs lump in)))
                  ("VERTEXES" (list "VERTEXES" (read-vertexes lump in)))
                  ("SEGS" (list "SEGS" (read-segs lump in)))
                  ("SSECTOR" (list "SSECTORS" (read-ssectors lump in)))
                  (t lump))))
    (file-position in file-position-restore)
    result))

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
          (push (process-lump current in) map-lumps))
          while (and (not (= (second current) 0))
                     (< i 10)))
    (if (= (second current) 0)
        (file-position in (- (file-position in) 16)))  ;; not consume last lump
    (append current-lump (list map-lumps))))

(defun read-maps (in wad-object)
  "Reads lumps from stream. Also moves stuff from maps into a "
  (let ((lumps nil)
        (directory-offset (slot-value wad-object 'directory-offset)))
    (file-position in directory-offset)
    (do ((i 2 (1+ i))
         (current))
        ((or (= i (slot-value wad-object 'number-of-lumps))
             (>= (file-position in) (file-length in))))
      (setf current (read-current-lump in))
      (if (eq (search "E1M4" (first current)) 0)
          (push (read-current-map in current) lumps)))
    lumps))


(define-binary-class wad-file
    ((identifier (iso-8859-1-string :length 4))
     (number-of-lumps u4)
     (directory-offset u4)
     (lumps noop)
     (maps noop)))

(defvar in)
(defvar wad-object)

(setf in (open *my-wad-file* :element-type '(unsigned-byte 8)))
(setf wad-object
      (read-value 'wad-file in))
(setf (slot-value wad-object 'maps) (read-maps in wad-object))
(slot-value wad-object 'identifier)
(slot-value wad-object 'number-of-lumps)
(slot-value wad-object 'directory-offset)
