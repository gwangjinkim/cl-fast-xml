
(ql:quickload :cl-ppcre)

(defpackage cl-fast-xml
  (:use :cl :cl-ppcre)
  (:export #:xml-to-hash
	   #:lists-to-xml
	   #:lists-to-xml-content))

(in-package :cl-fast-xml)

(defun join (lst &key (sep ""))
  "Python's `join` function for strings and list of strings."
  (format nil (format nil "~a~a~a" "~{~a~^" sep "~}") lst))

(defun split (str &key (sep " "))
  "Python's `split` function for strings handling regex patterns."
  (let ((result '())
	(start 0)
	(matches (cl-ppcre:all-matches sep str)))
    (loop for (match-start match-end) on matches by #'cddr
	  do (progn
	       (push (if (= (- match-start start) 0)
			 ""
			 (subseq str start match-start))
		     result)
	       (setf start match-end))
	  finally (progn
		    (push (subseq str match-end (length str)) result)
		    (return (nreverse result))))))
#|
;; at the end, I didn't used these definitions

(defun or/c (&rest predicates)
  "Returns a function that checks if any of the predicates return true for a given value."
  (lambda (value)
    (some (lambda (predicate) (funcall predicate value)) predicates)))

(defun cons/c (car-predicate cdr-predicate)
  "Returns a function that checks if a value is a cons cell with the car and cdr parts satisfying the given predicates."
  (lambda (value)
    (and (consp value)
         (funcall car-predicate (car value))
         (funcall cdr-predicate (cdr value)))))
|#

(defparameter *status-symbols*
  (list
   :traverse-start
   :key-waiting
   :key-start
   :key-reading
   :key-reading-end
   :attr-key-waiting
   :attr-key-reading
   :attr-key-end
   :attr-value-waiting
   :attr-value-reading
   :attr-value-end
   :key-end
   :key-value-reading
   :key-value-end-maybe
   :key-value-end
   :key-pair-end
   :traverse-end))

(defun statusp (status &optional (status-symbols *status-symbols*))
  (member status status-symbols))

(defparameter *special-chars*
  '(("&gt;" . ">")
    ("&lt;" . "<")
    ("&quot;" . "\"")
    ("&apos;" . "'")
    ("&amp;" . "&")))
;; the only problem is ampersand
;; because when translating back some translated contain an ampersand.
;; so when translating back, whe have to flip-reverse the alist.

(defun flip-alist (alist)
  (loop for (k . v) in alist
	collect (cons v k)))

(defun regex-replace-alist (str alist)
  "Regex replace alist (rules) in string."
  (let ((result str))
    (loop for (old . new) in alist
	  do (setf result (cl-ppcre:regex-replace-all old result new))
	  finally (return result))))

(defun from-special-chars (str &optional (special-chars *special-chars*))
  "Replace in string all special chars given in special-chars alist."
  (regex-replace-alist str special-chars))

(defun to-special-chars (str &optional (special-chars *special-chars*))
  "Replace in string all special chars given in special-chars alist back."
  (regex-replace-alist str (reverse (flip-alist special-chars))))

(defun defs-to-hash (def-list)
  (let ((def-hash (make-hash-table :test 'equal)))
    (loop for def in def-list
	  do (let ((items (cl-ppcre:split "\\." def))
		   (keys '()))
	       (loop for item in items
		     do (progn
			  (setf keys (cons item keys))
			  (let* ((key (join (reverse keys) :sep "."))
				 (type (gethash key def-hash nil)))
			    (if (string= def key)
				(if type
				    (cond ((eq type 'k)
					   (setf (gethash key def-hash) 'kv))
					  ((eq type 'v)
					   (setf (gethash key def-hash) 'v)))
				    (setf (gethash key def-hash) 'v))
				(if type
				    (when (eq type 'v)
				      (setf (gethash key def-hash) 'kv))
				    (setf (gethash key def-hash) 'k)))))))
	  finally (return def-hash))))

;; the following functions use ((key1 . count1) (key2 . count2)) as `keys`.
(defun from-keys-to-value-key (keys)
  (let ((_keys (mapcar (lambda (k) (format nil "~a~a" (car k) (cdr k))) keys)))
    (if (> (length _keys) 1)
	(join (reverse _keys) :sep ".")
	(car _keys))))

(defun from-keys-to-pure-key (keys)
  (let ((_keys (mapcar #'car keys)))
    (if (> (length _keys) 1)
	(join (reverse _keys) :sep ".")
	(car _keys))))

(defun from-keys-to-count-key (keys)
  (let ((result '()))
    (loop for (key . count) in keys
	  do (setf result (cons (if (null result)
					(format nil "~a" key)
					(format nil "~a~a" key count)) result))
	     finally (return (join result :sep ".")))))

;;;; status

(defun attr-key-reading (ch)
  (case ch
    (#\= '(:attr-key-end t t nil))
    (otherwise '(:attr-key-reading t nil t))))

(defun attr-key-waiting (ch)
  (case ch
    (#\space '(:attr-key-waiting t nil nil))
    (#\> '(:key-end t nil nil))
    (#\? '(:key-pair-end t nil nil))
    (#\/ '(:key-end nil nil nil))
    (#\< '(:key-waiting nil nil nil))
    (otherwise '(:attr-key-reading t nil t))))

(defun attr-value-end (ch)
  (case ch
    (#\? '(:key-pair-end t nil nil))
    (#\> '(:key-end t nil nil))
    (otherwise '(:attr-key-waiting t nil nil))))

(defun attr-value-reading (ch)
  (case ch
    (#\" '(:attr-value-end nil nil t))
    (otherwise '(:attr-value-reading t nil t))))

(defun attr-value-waiting (ch)
  (case ch
    (#\" '(:attr-value-reading t nil nil))
    (otherwise '(:attr-value-waiting t nil nil))))

(defun key-end (ch)
  (case ch
    (#\> '(:key-waiting t nil nil))
    (#\< '(:key-waiting nil nil nil))
    (#\/ '(:key-pair-end nil nil nil))
    (otherwise '(:key-value-reading nil nil nil))))

(defun key-reading-end (ch)
  (case ch
    (#\/ '(:key-value-end nil nil nil))
    (#\> '(:key-end t t nil))
    (otherwise '(:attr-key-waiting t nil nil))))

(defun key-reading (ch)
  (case ch
    (#\space '(:key-reading-end nil t nil))
    (#\newline '(:key-reading-end nil t nil))
    (#\return '(:key-reading-end nil t nil))
    (#\> '(:key-reading-end nil t nil))
    (#\/ '(:key-reading-end nil t nil))
    (otherwise '(:key-reading t nil t))))

(defun key-start (ch)
  (case ch
    (#\/ '(:key-pair-end t nil nil))
    (otherwise '(:key-reading t nil t))))

(defun key-value-end-maybe (ch)
  (case ch
    (#\> '(:key-value-end nil nil t))
    (otherwise '(:key-value-reading t nil t))))

(defun key-value-reading (ch)
  (case ch
    (#\< '(:key-value-end nil nil t))
    (#\/ '(:key-value-end-maybe t nil t))
    (otherwise '(:key-value-reading t nil t))))

(defun key-waiting (ch)
  (case ch
    (#\< '(:key-start t nil nil))
    (#\/ '(:key-pair-end nil nil nil))
    (otherwise '(:key-waiting t nil nil))))

(defun xml-port-to-hash (xml-port def-list)
  (let ((xml-hash (make-hash-table :test 'equal))
	(def-hash (defs-to-hash def-list)))
    (labels ((process (&key (status :traverse-start)
			    (ch (read-char xml-port nil :eof))
			    (count 0)
			    (keys '())
			    (chars '())
			    (waiting-pure-key nil)
			    (waiting-value-key nil))
	       (when (not (eql ch :eof))
		 (destructuring-bind (next-status read-char-p reserve-key-p reserve-char-p)
		     (case status
		       (:traverse-start '(:key-waiting nil nil nil))
		       (:key-waiting (key-waiting ch))
		       (:key-start (key-start ch))
		       (:key-reading (key-reading ch))
		       (:key-value-reading (key-value-reading ch))
		       (:key-value-end-maybe (key-value-end-maybe ch))
		       (:attr-key-waiting (attr-key-waiting ch))
		       (:attr-key-reading (attr-key-reading ch))
		       (:attr-value-waiting (attr-value-waiting ch))
		       (:attr-value-reading (attr-value-reading ch))
		       (:key-end
			(let* ((pure-key (from-keys-to-pure-key keys))
			       (value-key (from-keys-to-value-key keys)))
			  (when (gethash pure-key def-hash nil)
			    (let ((type (gethash pure-key def-hash)))
			      (when (or (eq type 'v) (eq type 'kv))
				(setf waiting-pure-key pure-key
				      waiting-value-key value-key)))))
			(key-end ch))
		       (:key-reading-end
			(let* ((pure-key (from-keys-to-pure-key keys))
			       (count-key (from-keys-to-count-key keys))
			       (key-count (format nil "~a's count" count-key))
			       (value-key (from-keys-to-value-key keys)))
			  (when (gethash pure-key def-hash nil)
			    (let ((type (gethash pure-key def-hash)))
			      (when (or (eq type 'v) (eq type 'kv))
				(setf waiting-pure-key pure-key
				      waiting-value-key value-key))
			      (setf (gethash key-count xml-hash)
				    (1+ (gethash key-count xml-hash 0))))))
			(key-reading-end ch))
		       (:key-pair-end
			(when waiting-pure-key
			  (setf (gethash waiting-value-key xml-hash)
			        (from-special-chars (coerce (reverse (cdr chars)) 'string))))
			(setf waiting-pure-key nil
			      waiting-value-key nil
			      keys (cdr keys))
			'(:key-waiting t nil nil))
		       (:attr-key-end
			(let* ((pure-key (from-keys-to-pure-key keys))
			       (value-key (from-keys-to-value-key keys)))
			  (when (gethash pure-key def-hash nil)
			    (let ((type (gethash pure-key def-hash)))
			      (when (or (eq type 'v) (eq type 'kv))
				(setf waiting-pure-key pure-key
				      waiting-value-key value-key)))))
			(setf keys (cdr keys))
			'(:attr-value-reading t nil nil))
		       (:attr-value-end
			(when waiting-pure-key
			  (when (gethash waiting-pure-key def-hash nil)
			    (let ((type (gethash waiting-pure-key def-hash)))
			      (when (or (eq type 'v) (eq type 'kv))
			      (setf (gethash waiting-value-key xml-hash)
				    (from-special-chars (coerce (reverse (cdr chars)) 'string)))))))
			(setf waiting-pure-key nil
			      waiting-value-key nil)
			(attr-value-end ch))
		       (:key-value-end
			(when waiting-pure-key
			  (setf (gethash waiting-value-key xml-hash)
			        (from-special-chars (coerce (reverse (cdr chars)) 'string))))
			(setf waiting-pure-key nil
			      waiting-value-key nil)
			'(:key-waiting nil nil nil)))
		   (process
		    :status next-status
		    :ch (if read-char-p (read-char xml-port nil :eof) ch)
		    :count (if read-char-p (1+ count) count)
		    :keys (if reserve-key-p
			      (let* ((key (coerce (reverse chars) 'string))
				     (_keys (cons (cons key 0) keys))
				     (count-key (from-keys-to-count-key _keys))
				     (key-count (format nil "~a's count" count-key)))
				(if (> (length key) 0)
				    (cons (cons key (1+ (gethash key-count xml-hash 0))) keys)
				    keys))
			      keys)
		    :chars (if reserve-char-p (cons ch chars) '())
		    :waiting-pure-key waiting-pure-key
		    :waiting-value-key waiting-value-key)))))
      (process))
    xml-hash))
		 
(defun xml-file-to-hash (xml-file def-list)
  (with-open-file (stream xml-file :direction :input)
    (xml-port-to-hash stream def-list)))

(defun add-xml-head (xml-str)
  (format nil "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"yes\"?>~%~%~a" xml-str))

(defun lists-to-compact-xml (xml-list)
  (add-xml-head (cl-ppcre:regex-replace-all ">\n *<" (lists-to-xml-content xml-list) "><")))

(defun lists-to-xml (xml-list)
  (add-xml-head (lists-to-xml-content xml-list)))

(defun string-or-symbol-p (x)
  (or (stringp x) (symbolp x)))

(defun attributep (node)
  (and (consp node)
       (string-or-symbol-p (car node))
       (string-or-symbol-p (cdr node))))

(defun lists-to-xml-content (xml-list)
  (with-output-to-string (out)
    (labels ((process (stream nodes prefix-spaces)
	       (when (and nodes (string-or-symbol-p (car nodes)))
		 (let* ((attributes (remove-if-not #'attributep (cdr nodes)))
			(children (remove-if #'attributep (cdr nodes)))
			(value-children (remove-if-not #'string-or-symbol-p children))
			(list-children (remove-if-not #'listp children)))
		   (format stream "~a<~a~a"
			   prefix-spaces
			   (car nodes)
			   (with-output-to-string (attributes-port)
			     (labels ((process-attributes (attributes)
					(when attributes
					  (format attributes-port " ~a=\"~a\"" (caar attributes) (to-special-chars (cdar attributes)))
					  (process-attributes (cdr attributes)))))
			       (process-attributes attributes))))
		   (cond ((null children) (format stream "/>~%"))
			 (t (format stream ">")
			    (if value-children
				(format stream "~a</~a>~%"
					(to-special-chars (reduce (lambda (x y) (concatenate 'string x y))
								  (mapcar (lambda (v) (format nil "~a" v))
									  value-children)))
					(car nodes))
				(format stream "~%~a~a</~a>~%"
					(with-output-to-string (children-port)
					  (labels ((process-children (children)
						     (when children
						       (process children-port (car children) (concatenate 'string prefix-spaces "  "))
						       (process-children (cdr children)))))
					    (process-children list-children)))
					prefix-spaces
					(car nodes)))))))))
      (process out xml-list ""))))

;; ok this format is like this:
;; first string in list is a tag.
;; then there can be but must not exist attributes in form of cons cells.
;; so children are everything non-attribute.
;; any string/symbol in (cdr nodes) is a value-child.
;; but any non-cons list is a list-child.

;; so in case there are no children, self-close.
;; in case there are children, close starting tag and at the end add closing tag.
;; in between process the children autonomously.
;; select attributes and add them.

;; non-value children are addere starting a new line and indenting them.

;; in the compact version, just delete whitespaces between tag close and open > <



