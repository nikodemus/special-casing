;;;; SPECIAL-CASING is by Nikodemus Siivola <nikodemus@random-state.net>, 2012
;;;;
;;;; Permission is hereby granted, free of charge, to any person obtaining
;;;; a copy of this software and associated documentation files (the
;;;; "Software"), to deal in the Software without restriction, including
;;;; without limitation the rights to use, copy, modify, merge, publish,
;;;; distribute, sublicense, and/or sell copies of the Software, and to
;;;; permit persons to whom the Software is furnished to do so, subject to
;;;; the following conditions:
;;;;
;;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;;;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;;;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
;;;; IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
;;;; CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
;;;; TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
;;;; SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

;;; TODO:
;;;  * tests
;;;  * capitalization
;;;  * non-turkic parts of SpecialCasing.txt

(defpackage :special-casing
  (:use :cl :alexandria :split-sequence)
  (:shadow #:case)
  (:export
   #:list-all-languages
   #:find-language
   #:languagep
   #:language-name
   #:language-upcase
   #:language-downcase
   #:with-language))

(in-package :special-casing)

(defstruct (language (:copier nil)
                     (:conc-name lang-)
                     (:predicate languagep))
  (name (required-argument) :type symbol)
  (upcase (required-argument) :type function)
  (downcase (required-argument) :type function))

(defmethod print-object ((language language) stream)
  (print-unreadable-object (language stream :type t :identity nil)
    (princ (lang-name language) stream)))

(defvar *languages* (make-hash-table))

(defun find-language (language &optional errorp)
  (if (languagep language)
      language
      (or (gethash language *languages*)
          (when errorp
            (error "~S is not a known language." language)))))

(defun language-name (language)
  (lang-name (find-language language t)))

(defun list-all-languages ()
  (hash-table-values *languages*))

(defvar *language* :default)

(defmacro with-language ((language &key) &body body)
  `(let ((*language* (find-language ,language t)))
     ,@body))

(defun language-upcase (string &key (start 0) end (language *language*))
  (funcall (lang-upcase (find-language language t))
           string start (or end (length string))))

(defun language-downcase (string &key (start 0) end (language *language*))
  (funcall (lang-downcase (find-language language t))
           string start (or end (length string))))

;;; When the thing is built, not when a fasl is loaded -- which might not even
;;; be in the same directory. These functions don't end up in the runtime.
(eval-when (:compile-toplevel :execute)

  (defun parse-code (code)
    (parse-integer code :radix 16))

  (defun parse-codes (codes)
    (map 'string (lambda (code)
                   (code-char (parse-code code)))
         (split-sequence #\space codes :remove-empty-subseqs t)))

  (defparameter *special-casing* nil)

  (defstruct case code lower title upper conditions)

  (defun parse-casing-spec (line)
    (declare (string line))
    ;; Strip away comments
    (when-let (p (position #\# line))
      (setf line (subseq line 0 p)))
    (unless (emptyp line)
      (destructuring-bind (code lower title upper &optional conds &rest _)
          ;; Split into parts sans external whitespace
          (mapcar (lambda (e) (string-trim '(#\space) e))
                  (split-sequence #\; line))
        (declare (ignore _))
        (push (make-case :code (parse-code code)
                         :lower (parse-codes lower)
                         :title (parse-codes title)
                         :upper (parse-codes upper)
                         :conditions (split-sequence #\space conds))
              *special-casing*))))

  (with-open-file (f (merge-pathnames "SpecialCasing.txt"
                                      (or *compile-file-truename*
                                          *load-truename*)))
    (loop for line = (read-line f nil nil)
          while line
          do (handler-case
                 (parse-casing-spec line)
               (error (e)
                 (error "Error parsing line:~%~A~2%Line: ~S" e line)))))

  (defmacro upcase-turkic (string index start end stream casing)
    (declare (ignorable start end))
    (with-unique-names (code)
      `(let ((,code (char-code (char ,string ,index))))
         (cond
           ,@(mapcar (lambda (case)
                       `((eql ,code ,(case-code case))
                         (write-string ,(case-upper case) ,stream)))
                     (remove-if (lambda (case)
                                  (eql (code-char (case-code case))
                                       (ignore-errors (character (case-upper case)))))
                                casing))
           (t
            (write-char (char-upcase (code-char ,code)) ,stream))))))

  (defmacro downcase-turkic (string index start end stream casing)
    (declare (ignorable start end))
    (with-unique-names (code)
      `(let ((,code (char-code (char ,string ,index))))
         (cond
           ,@(mapcar (lambda (case)
                       `((and (eql ,code ,(case-code case))
                              ,@(when (member "After_I" (case-conditions case)
                                              :test #'string=)
                                  `((> ,index ,start)
                                    (eql #\I (char ,string (1- ,index)))))
                              ,@(when (member "Not_Before_Dot" (case-conditions case)
                                              :test #'string=)
                                  `((or (not (< (1+ ,index) ,end))
                                        (not (eql #\COMBINING_DOT_ABOVE
                                                  (char ,string (1+ ,index))))))))
                         ,@(unless (emptyp (case-lower case))
                             `((write-string ,(case-lower case) ,stream)))))
                     (remove-if (lambda (case)
                                  (eql (code-char (case-code case))
                                       (ignore-errors (character (case-lower case)))))
                                casing))
           (t
            (write-char (char-downcase (code-char ,code)) ,stream)))))))

(macrolet ((build ()
             (let* ((id "tr")
                    (turkish (remove-if-not
                              (lambda (case)
                                ;; FIXME: We're not implementing the
                                ;; unconditional rules from the file right now
                                ;; -- need to check that it's TRT first!
                                (member id (case-conditions case)
                                        :test #'string=))
                              *special-casing*)))
               ;; Number of special cases for Turkish seems small enough that we'll
               ;; open code them.
               `(progn
                  (defun turkish-upcase (string start end)
                    (declare (string string) (array-index start end))
                    ;; FIXME: Check bounds outside the loop.
                    (with-output-to-string (s)
                      (loop for i from start below end
                            do (upcase-turkic string i start end s ,turkish))))
                  (defun turkish-downcase (string start end)
                    (declare (string string) (array-index start end))
                    ;; FIXME: Check bounds outside the loop.
                    (with-output-to-string (s)
                      (loop for i from start below end
                            do (downcase-turkic string i start end s ,turkish))))))))
  (build))

(defun default-upcase (string start end)
  (string-upcase string :start start :end end))

(defun default-downcase (string start end)
  (string-downcase string :start start :end end))

(macrolet ((def (name upcase downcase)
             `(setf (gethash ',name *languages*)
                    (make-language :name ',name
                                   :upcase #',upcase
                                   :downcase #',downcase))))
  (def :default default-upcase default-downcase)
  (def :turkish turkish-upcase turkish-downcase))

(eval-when (:compile-toplevel :execute)
  (setf *special-casing* nil))
