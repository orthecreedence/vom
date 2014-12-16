(defpackage :vom
  ;; DON'T use :cl, otherwise most of the implementations bitch about using
  ;; error and warn functions
  (:use)
  ;; import everything from cl that we actually need. while obnoxious, it makes
  ;; sure vom runs smoothly on most (all?) implementations.
  (:import-from #:cl
                #:t #:nil
                #:defpackage #:in-package #:*package* #:package-name #:find-package
                #:eval-when
                #:eval
                #:lambda #:defun #:multiple-value-list #:defmacro
                #:defvar #:defparameter
                #:declare #:optimize #:type #:ignore
                #:keyword #:integer
                #:assert
                #:member
                #:macro-function
                #:documentation
                #:let #:let* #:progn #:multiple-value-bind
                #:&rest #:&key
                #:if #:when #:unless #:cond
                #:loop #:dolist
                #:car #:cdr #:cddr
                #:write-sequence #:format
                #:get-universal-time
                #:get-decoded-time
                #:string #:string-downcase #:make-string #:concatenate
                #:symbolp
                #:intern
                #:setf #:getf
                #:max #:min
                #:eq
                #:+ #:- #:> #:< #:<= #:>=
                #:apply #:funcall
                #:append #:list #:length)
  (:shadow #:error
           #:warn
           #:debug)
  (:export #:config
           #:*log-stream*
           #:*log-hook*
           
           #:emerg
           #:alert
           #:crit
           #:error
           #:warn
           #:notice
           #:info
           #:debug
           #:debug1
           #:debug2
           #:debug3
           #:debug4))
(in-package :vom)

;; define our *levels* and *max-level-name-length* before the define-level macro
;; is defined (so it can access them)
(eval-when (:load-toplevel :compile-toplevel)
  (defparameter *levels* '(:off 0)
    "Holds the log level mappings (keyword -> value).")
  
  (defparameter *max-level-name-length* 0
    "Holds the number of characters in the longest log-level name."))

(defvar *config* '(t :warn)
  "Holds the logging config as a plist. Holds package -> level mappings, using
   T as the default (used if logging from a package that hasn't been
   configured).")

(defvar *log-stream* t
  "Holds the default stream we're logging to.")

(defvar *log-hook*
  (lambda (log-level package-keyword package-log-level)
    (declare (ignore log-level package-keyword package-log-level))
    *log-stream*)
  "Holds a function that, given a log-level, a package name, and the effective
   log-level for that package, returns one or more (via (values ...)) streams
   that this log will be sent to.")

(defun config (package-keyword level-name)
  "Configure the log level for a package (or use t for the package name to set
   the default log level). The log level is given as a keyword."
  (assert (member level-name *levels*))
  (cond ((eq package-keyword t)
         (setf (getf *config* t) level-name))
        ((symbolp package-keyword)
         (let* ((name (find-package package-keyword))
                (package-name (string (if name
                                          (package-name name)
                                          package-keyword))))
           (setf (getf *config* (intern package-name :keyword)) level-name)))))

(defun pretty-time ()
  "Convert a timestamp to a HH:MM:SS time."
  (multiple-value-bind (second minute hour)
      (get-decoded-time)
    (format nil "~2,'0D:~2,'0D:~2,'0D" hour minute second)))

(defun do-log (level-name log-level package-keyword format-str &rest args)
  "The given data to the current *log-stream* stream."
  (declare (optimize (cl:speed 3) (cl:safety 0) (cl:debug 0))
           (type keyword level-name package-keyword)
           (type integer log-level)
           (type string format-str)
           (type list args))
  (let* ((package (find-package package-keyword))
         (package-name (when package
                         (intern (package-name package) :keyword)))
         (package-level (getf *config* package-name))
         (package-level (if package-level
                            package-level
                            (getf *config* t)))
         (package-level-value (getf *levels* package-level 0))
         (log-streams (multiple-value-list
                        (funcall *log-hook*
                                 log-level
                                 package-keyword
                                 package-level-value))))
    (when (<= log-level package-level-value)
      (let* ((level-str (string level-name))
             (format-str (concatenate 'string "~a<~a> [~a] ~a - " format-str "~%"))
             (logline (apply 'format
                             (append (list
                                       nil
                                       format-str)
                                     (list
                                       (make-string (- *max-level-name-length* (length level-str))
                                                    :initial-element #\space)
                                       level-str
                                       (pretty-time)
                                       (string-downcase (string package-keyword)))
                                     args))))
        (dolist (stream log-streams)
          (write-sequence logline (if (eq stream t)
                                      cl:*standard-output*
                                      stream)))))))

(defmacro define-level (name level-value)
  "Define a log level."
  (let ((macro-name (intern (format nil "LOG-~a" (string name))))
        (log-sym (intern (string name))))
    `(progn
       (setf (getf *levels* ,name) ,level-value)
       (defmacro ,macro-name (format-str &rest args)
         ,(format nil "Log output to the ~s log level (~a)" name level-value)
         (let ((pkg (intern (package-name *package*) :keyword)))
           `(do-log ,,name ,,level-value ,pkg ,format-str ,@args)))
       (setf (documentation ',log-sym 'cl:function) (documentation ',macro-name 'cl:function))
       (setf (macro-function ',log-sym) (macro-function ',macro-name))
       (setf *max-level-name-length* (max *max-level-name-length*
                                          (length (string ,name)))))))

(define-level :emerg 1)
(define-level :alert 2)
(define-level :crit 3)
(define-level :error 4)
(define-level :warn 5)
(define-level :notice 6)
(define-level :info 7)
(define-level :debug 8)
(define-level :debug1 9)
(define-level :debug2 10)
(define-level :debug3 11)
(define-level :debug4 12)

