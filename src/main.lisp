(defpackage cl-spam-detector
  (:use :cl :cl-ppcre :trivial-hashtable-serialize)
  (:export :learning :single-scoring :multi-scoring))
(in-package :cl-spam-detector)

(defparameter *n* 5)

;;; reference: https://gitlab.com/ediethelm/trivial-hashtable-serialize
(defun default-deserialize-fn (line)
  "This function deserializes key/value pairs serialized by *DEFAULT-SERIALIZE-FN*."
  (declare (type string line))
  (let* ((pair (split-sequence:split-sequence #\$ line))
		 (key-str (car pair))
		 (value-str (cadr pair)))
    (values key-str (parse-integer (read-from-string value-str)))))

;;; Make :test as an option in make-hash-table
;;; reference: https://gitlab.com/ediethelm/trivial-hashtable-serialize
(defun load-hashtable (file-name &key
								   (if-does-not-exist :create)
								   (deserialize-fn #'default-deserialize-fn)
								   (test #'eql))
  (declare (type function deserialize-fn))
  (let ((table (make-hash-table :test test)))
    (with-open-file (fstream file-name :direction :input :if-does-not-exist if-does-not-exist)
      (unless fstream
		(error "Could not open file '~a'" file-name))

      (loop for line = (read-line fstream nil 'eof)
		 until (eq line 'eof)
		 do
		   (multiple-value-bind (key value) (funcall deserialize-fn line)
			 (setf (gethash key table) value))))

    (return-from load-hashtable table)))


;; str ops
(defun n-gram (text n)
  (cond ((<= n (length text))
		 (cons (subseq text 0 n)
			   (n-gram (subseq text 1) n)))
		(t '())))

;;; table ops
(defun read-table (ham-filepath spam-filepath)
  (let ((ham-table (if (probe-file ham-filepath)
					   (load-hashtable ham-filepath :test #'equal)
					   (make-hash-table :test #'equal)))
		(spam-table (if (probe-file spam-filepath)
						(load-hashtable spam-filepath :test #'equal)
						(make-hash-table :test #'equal))))
	(values ham-table
			spam-table)))

(defun save-table (ham-table ham-filepath spam-table spam-filepath)
  (trivial-hashtable-serialize:save-hashtable ham-table ham-filepath)
  (trivial-hashtable-serialize:save-hashtable spam-table spam-filepath))

(defun insert-table (table text)
  (let ((ng (n-gram text *n*)))
	(loop for gram in ng
	   do (cond ((null (gethash gram table))
				 (setf (gethash gram table) 1))
				(t (setf (gethash gram table) (1+ (gethash gram table))))))
	table))

(defun scoring (ham-table spam-table filepath)
  (let* ((ham-score 0)
		 (spam-score 0)
		 (text (format nil "~{~A~}" (mylib:read-file-to-list filepath)))
		 (ng (n-gram text *n*)))
	(loop for gram in ng
	   do (progn
			(setf ham-score (+ ham-score
							   (if (gethash gram ham-table)
								   (sqrt (gethash gram ham-table))
								   0)))
			(setf spam-score (+ spam-score
								(if (gethash gram spam-table)
									(sqrt (gethash gram spam-table))
									0)))))
	(values (- ham-score spam-score)
			ham-score
			spam-score)))

(defun multi-scoring (ham-filepath spam-filepath filepath-lst)
  (multiple-value-bind (ham-table spam-table)
	  (read-table ham-filepath spam-filepath)
	(loop for fp in filepath-lst
	   collect (scoring ham-table spam-table fp))))

(defun single-scoring (ham-filepath spam-filepath filepath)
  (multiple-value-bind (ham-table spam-table)
	  (read-table ham-filepath spam-filepath)
	(scoring ham-table spam-table filepath)
	))

(defun learning (type ham-filepath spam-filepath filepath-lst)
  (multiple-value-bind (ham-table spam-table)
	  (read-table ham-filepath spam-filepath)
	(let ((lst-count (length filepath-lst))
		  (count 1))
	  (loop for filepath in filepath-lst
		 do (let ((text (ppcre:regex-replace-all
						 "[^\\w]"
						 (format nil "~{~A~}" (mylib:read-file-to-list filepath))
						 "")))
			  (format t "~A ~A/~A~%" filepath count lst-count)
			  ;; ここloopごとに判定してるからかなり遅くなると思う。
			  (cond ((string= type "ham")
					 (insert-table ham-table text))
					((string= type "spam")
					 (insert-table spam-table text))
					(t (error (format nil "Type Not Found (~A)" type))))
			  (incf count))))
	(save-table ham-table ham-filepath spam-table spam-filepath)))
