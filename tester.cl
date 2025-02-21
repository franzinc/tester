;; tester.cl
;; A test harness for Allegro CL.
;; See the file LICENSE for the full license governing this code.

#+(version= 11 0)
(sys:defpatch "tester" 1
  "v1: fix compilation issues with util.test:with-tests."
  :type :system
  :post-loadable t)

(defpackage :util.test
  (:use :common-lisp :excl)
  (:shadow #:test)
  (:export
;;;; Control variables:
   #:*break-on-test-failures*
   #:*error-protect-tests*
   #:*test-errors*
   #:*test-successes*
   #:*test-unexpected-failures*
   #:*test-report-thread*

;;;; The test macros:
   #:test
   #:test-error
   #:test-no-error
   #:test-warning
   #:test-no-warning
   
   #:with-tests
   #:inc-test-counter
   ))

(in-package :util.test)

#+allegro
(defvar *test-syms* nil)

#+allegro
(defun set-test-syms ()
  ;; this can't be done at compile time because the :test package is
  ;; defined after we are loaded.
  (let ((p (find-package :test)))
    (when (and (null *test-syms*) p)
      (setq *test-syms*
	(list (find-symbol (symbol-name :.total-errors.) p)
	      (find-symbol (symbol-name :.total-successes.) p)
	      #+(version>= 10 1)
	      (find-symbol (symbol-name :.total-unexpected-failures.) p))))))

(defvar *break-on-test-failures* nil
  "When a test failure occurs, common-lisp:break is called, allowing
interactive debugging of the failure.")

(defvar *test-errors* 0
  "The value is the number of test errors which have occurred.

If an application increments or decrements this variable, it should 
use the inc-test-counter macro instead of incf or decf.
")
(defvar *test-successes* 0
  "The value is the number of test successes which have occurred.

If an application increments or decrements this variable, it should 
use the inc-test-counter macro instead of incf or decf.
")
(defvar *test-unexpected-failures* 0
  "The value is the number of unexpected test failures which have occurred.

If an application increments or decrements this variable, it should 
use the inc-test-counter macro instead of incf or decf.
")

(defvar *error-protect-tests* nil
  "Protect each test from errors.  If an error occurs, then that will be
taken as a test failure unless test-error is being used.")

(defvar *test-report-thread* nil
  "When non-nil, include the process-name and thread id in messages.")


(defmacro report-error ((&key (stream '*error-output*)) &body body)
  ;; internal macro
  #-smp-macros
  `(progn (report-thread ,stream) ,@body)
  #+smp-macros
  `(with-locked-stream (,stream :non-smp :without-scheduling)
     (report-thread ,stream)
     ,@body)
  )

(defun report-thread (stream)
  ;; internal utility
  (when *test-report-thread*
    (format stream "~&~%***** test report from ~A[~A] *****~%"
	    (mp:process-name  mp:*current-process*)
	    #-(version>= 10 1)    ;; bug24565
	    (sys::thread-bindstack-index
	     (mp:process-thread mp:*current-process*))
	    #+(version>= 10 1)
	    (mp:process-sequence mp:*current-process*))))  ;; bug24565

(defmacro inc-test-counter (var &optional (increment 1))
  " This macro is used internally to increment the three counters
used by the tester.  Applications that explicitly increment the
counters should also use this macro instead of incf or decf.

The macro is necessary because the global counters must be updated 
atomically in a multi-threaded application running in SMP
implementations of ACL.  If the counters are bound on the stack, 
they must be updated as ordinary special variables.
"

  ;; This is safe in virtual threads or in os-threads with a locked heap.
  #-smp
  `(incf ,var ,increment)

  ;; In smp we need to be more careful.
  #+smp
  `(inc-test-counter-fn ',var ,increment)

  )

#+smp
(defun inc-test-counter-fn (var increment)
  ;; internal function
  (multiple-value-bind (val status)
      (excl::symeval-in-thread var (excl::current-thread)) ;; [bug20172]
    (declare (ignore val))
    (if (null status)
	;; A global counter must be updated atomically.
	(incf-atomic (sys:global-symbol-value var) increment)
      ;; A stack-bound counter is only seen in one thread.
      (incf (symbol-value var) increment))))


(defmacro test-values-errorset (form &optional announce catch-breaks)
  ;; internal macro
  (let ((g-announce (gensym))
	(g-catch-breaks (gensym)))
    `(let* ((,g-announce ,announce)
	    (,g-catch-breaks ,catch-breaks))
       (handler-case (cons t (multiple-value-list ,form))
	 (condition (condition)
	   (with-unreachable-code-allowed
	       (if* (and (null ,g-catch-breaks)
			 (typep condition 'simple-break))
		  then (break condition)
		elseif ,g-announce
		  then (report-error ()
			 (format *error-output* "~&Condition type: ~a~%"
				 (class-of condition))
			 (format *error-output* "~&Message: ~a~%" condition))))
	   condition)))))

(defmacro test-values (form &optional announce catch-breaks)
  ;; internal macro
  (if* *error-protect-tests*
     then `(test-values-errorset ,form ,announce ,catch-breaks)
     else `(cons t (multiple-value-list ,form))))

(defmacro test (expected-value test-form
		&key (test #'eql test-given)
		     (multiple-values nil multiple-values-given)
		     (fail-info nil fail-info-given)
		     (known-failure nil known-failure-given)

;;;;;;;;;; internal, undocumented keywords:
;;;; Note about these keywords: if they were documented, we'd have a
;;;; problem, since they break the left-to-right order of evaluation.
;;;; Specifically, errorset breaks it, and I don't see any way around
;;;; that.  `errorset' is used by the old test.cl module (eg,
;;;; test-equal-errorset).
		     errorset
		     reported-form
		     (wanted-message nil wanted-message-given)
		     (got-message nil got-message-given))
  "Perform a single test.  `expected-value' is the reference value for the
test.  `test-form' is a form that will produce the value to be compared to
the expected-value.  If the values are not the same, then an error is
logged, otherwise a success is logged.

Normally the comparison of values is done with `eql'.  The `test' keyword
argument can be used to specify other comparison functions, such as eq,
equal,equalp, string=, string-equal, etc.

Normally, only the first return value from the test-form is considered,
however if `multiple-values' is t, then all values returned from test-form
are considered.

`fail-info' allows more information to be printed with a test failure.

`known-failure' marks the test as a known failure.  This allows for
programs that do regression analysis on the output from a test run to
discriminate on new versus known failures."
  `(test-check
    :expected-result ,expected-value
    :test-results
    (,(if errorset 'test-values-errorset 'test-values) ,test-form t)
    ,@(when test-given `(:predicate ,test))
    ,@(when multiple-values-given `(:multiple-values ,multiple-values))
    ,@(when fail-info-given `(:fail-info ,fail-info))
    ,@(when known-failure-given `(:known-failure ,known-failure))
    :test-form ',(if reported-form reported-form test-form)
    ,@(when wanted-message-given `(:wanted-message ,wanted-message))
    ,@(when got-message-given `(:got-message ,got-message))))

(defmethod conditionp ((thing condition)) t)
(defmethod conditionp ((thing t)) nil)

(defmacro test-error (form &key announce
				catch-breaks
				(fail-info nil fail-info-given)
				(known-failure nil known-failure-given)
				(condition-type ''simple-error)
				(include-subtypes nil include-subtypes-given)
				(format-control nil format-control-given)
				(format-arguments nil format-arguments-given))
  "Test that `form' signals an error. The order of evaluation of the
arguments is keywords first, then test form.

If `announce' is non-nil, then cause the error message to be printed.

The `catch-breaks' is non-nil then consider a call to common-lisp:break an
`error'.

`fail-info' allows more information to be printed with a test failure.

`known-failure' marks the test as a known failure.  This allows for
programs that do regression analysis on the output from a test run to
discriminate on new versus known failures.

If `condition-type' is non-nil, it should be a symbol naming a condition
type, which is used to check against the signalled condition type.  The
test will fail if they do not match.

`include-subtypes', used with `condition-type', can be used to match a
condition to an entire subclass of the condition type hierarchy.

`format-control' and `format-arguments' can be used to check the error
message itself."
  (let ((g-announce (gensym))
	(g-catch-breaks (gensym))
	(g-fail-info (gensym))
	(g-known-failure (gensym))
	(g-condition-type (gensym))
	(g-include-subtypes (gensym))
	(g-format-control (gensym))
	(g-format-arguments (gensym))
	(g-c (gensym)))
    `(let* ((,g-announce ,announce)
	    (,g-catch-breaks ,catch-breaks)
	    ,@(when fail-info-given `((,g-fail-info ,fail-info)))
	    ,@(when known-failure-given `((,g-known-failure ,known-failure)))
	    (,g-condition-type ,condition-type)
	    ,@(when include-subtypes-given
		`((,g-include-subtypes ,include-subtypes)))
	    ,@(when format-control-given
		`((,g-format-control ,format-control)))
	    ,@(when format-arguments-given
		`((,g-format-arguments ,format-arguments)))
	    (,g-c (test-values-errorset ,form ,g-announce ,g-catch-breaks)))
       (test-check
	:predicate #'eq
	:expected-result t
	:test-results
	(with-unreachable-code-allowed
	    (test-values (and (conditionp ,g-c)
			      ,@(if* include-subtypes-given
				   then `((if* ,g-include-subtypes
					     then (typep ,g-c ,g-condition-type)
					     else (eq (class-of ,g-c)
						      (find-class
						       ,g-condition-type))))
				   else `((eq (class-of ,g-c)
					      (find-class ,g-condition-type))))
			      ,@(when format-control-given
				  `((or
				     (null ,g-format-control)
				     (string=
				      (concatenate 'simple-string
					"~1@<" ,g-format-control "~:@>")
				      (simple-condition-format-control ,g-c)))))
			      ,@(when format-arguments-given
				  `((or
				     (null ,g-format-arguments)
				     (equal
				      ,g-format-arguments
				      (simple-condition-format-arguments ,g-c))))))
			 t))
	:test-form ',form
	,@(when fail-info-given `(:fail-info ,g-fail-info))
	,@(when known-failure-given `(:known-failure ,g-known-failure))
	:condition-type ,g-condition-type
	:condition ,g-c
	,@(when include-subtypes-given
	    `(:include-subtypes ,g-include-subtypes))
	,@(when format-control-given
	    `(:format-control ,g-format-control))
	,@(when format-arguments-given
	    `(:format-arguments ,g-format-arguments))))))

(defmacro test-no-error (form &key announce
				   catch-breaks
				   (fail-info nil fail-info-given)
				   (known-failure nil known-failure-given))
  "Test that `form' does not signal an error.  The order of evaluation of
the arguments is keywords first, then test form.

If `announce' is non-nil, then cause the error message to be printed.

The `catch-breaks' is non-nil then consider a call to common-lisp:break an
`error'.

`fail-info' allows more information to be printed with a test failure.

`known-failure' marks the test as a known failure.  This allows for
programs that do regression analysis on the output from a test run to
discriminate on new versus known failures."
  (let ((g-announce (gensym))
	(g-catch-breaks (gensym))
	(g-fail-info (gensym))
	(g-known-failure (gensym))
	(g-c (gensym)))
    `(let* ((,g-announce ,announce)
	    (,g-catch-breaks ,catch-breaks)
	    ,@(when fail-info-given `((,g-fail-info ,fail-info)))
	    ,@(when known-failure-given `((,g-known-failure ,known-failure)))
	    (,g-c (test-values-errorset ,form ,g-announce ,g-catch-breaks)))
       (test-check
	:predicate #'eq
	:expected-result t
	:test-results (test-values (not (conditionp ,g-c)))
	:test-form ',form
	:condition ,g-c
	,@(when fail-info-given `(:fail-info ,g-fail-info))
	,@(when known-failure-given `(:known-failure ,g-known-failure))))))

(defvar *warn-cookie* (cons nil nil))

(defmacro test-warning (form &key fail-info known-failure)
  "Test that `form' signals a warning.  The order of evaluation of
the arguments is keywords first, then test form.

`fail-info' allows more information to be printed with a test failure.

`known-failure' marks the test as a known failure.  This allows for
programs that do regression analysis on the output from a test run to
discriminate on new versus known failures."
  (let ((g-fail-info (gensym))
	(g-known-failure (gensym))
	(g-value (gensym)))
    `(let* ((,g-fail-info ,fail-info)
	    (,g-known-failure ,known-failure)
	    (,g-value (test-values-errorset ,form nil t)))
       (test
	*warn-cookie*
	(if* (or (typep ,g-value 'simple-warning) (typep ,g-value 'warning))
	   then *warn-cookie*
	   else ;; test produced no warning
		nil)
	:test #'eq
	:reported-form ,form ;; quoted by test macro
	:wanted-message "a warning"
	:got-message "no warning"
	:fail-info ,g-fail-info
	:known-failure ,g-known-failure))))

(defmacro test-no-warning (form &key fail-info known-failure)
  "Test that `form' does not signal a warning.  The order of evaluation of
the arguments is keywords first, then test form.

`fail-info' allows more information to be printed with a test failure.

`known-failure' marks the test as a known failure.  This allows for
programs that do regression analysis on the output from a test run to
discriminate on new versus known failures."
  (let ((g-fail-info (gensym))
	(g-known-failure (gensym))
	(g-value (gensym)))
    `(let* ((,g-fail-info ,fail-info)
	    (,g-known-failure ,known-failure)
	    (,g-value (test-values-errorset ,form nil t)))
       (test
	*warn-cookie*
	(if* (or (typep ,g-value 'simple-warning) (typep ,g-value 'warning))
	   then nil ;; test produced warning
	   else *warn-cookie*)
	:test #'eq
	:reported-form ',form
	:wanted-message "no warning"
	:got-message "a warning"
	:fail-info ,g-fail-info
	:known-failure ,g-known-failure))))

(defvar *announce-test* nil) ;; if true announce each test that was done

(defun test-check (&key (predicate #'eql)
			expected-result test-results test-form
			multiple-values fail-info known-failure
			wanted-message got-message condition-type condition
			include-subtypes format-control format-arguments
		   &aux fail predicate-failed got wanted)
  ;; for debugging large/complex test sets:
  (when *announce-test*
    (report-error ()
      (format *error-output* "~&Just did test ~s~%" test-form))
    (when (not (eq *error-output* *standard-output*))
      (report-error (:stream *standard-output*)
	(format t "~&Just did test ~s~%" test-form)
	(force-output))))
  
  ;; this is an internal function
  (flet ((check (expected-result result)
	   (let* ((results
		   (multiple-value-list
		    (errorset (funcall predicate expected-result result) t)))
		  (failed (null (car results))))
	     (if* failed
		then (setq predicate-failed t)
		     nil
		else (cadr results)))))
    (when (conditionp test-results)
      (setq condition test-results)
      (setq test-results nil))
    (when (null (car test-results))
      (setq fail t))
    (if* (and (not fail) (not multiple-values))
       then ;; should be a single result
	    ;; expected-result is the single result wanted
	    (when (not (and (cdr test-results)
			    (check expected-result (cadr test-results))))
	      (setq fail t))
	    (when (and (not fail) (cddr test-results))
	      (setq fail 'single-got-multiple))
       else ;; multiple results wanted
	    ;; expected-result is a list of results, each of which
	    ;; should be checked against the corresponding test-results
	    ;; using the predicate
	    (do ((got (cdr test-results) (cdr got))
		 (want expected-result (cdr want)))
		((or (null got) (null want))
		 (when (not (and (null want) (null got)))
		   (setq fail t)))
	      (when (not (check (car got) (car want)))
		(return (setq fail t)))))
    (if* fail
       then (report-error ()
	      (when (not known-failure)
		(format *error-output*
			"~& * * * UNEXPECTED TEST FAILURE * * *~%")
		(inc-test-counter *test-unexpected-failures*))
	      (format *error-output* "~&Test failed: ~@[known failure: ~*~]~s~%"
		      known-failure test-form)
	      (if* (eq 'single-got-multiple fail)
		 then (format
		       *error-output*
		       "~
Reason: additional value were returned from test form.~%")
	       elseif predicate-failed
		 then (format *error-output* "Reason: predicate error.~%")
	       elseif (null (car test-results))
		 then (format *error-output* "~
Reason: an error~@[ (of type `~s')~] was detected.~%"
			      (when condition (class-of condition)))
	       elseif condition
		 then (if* (not (conditionp condition))
			 then (format *error-output* "~
Reason: expected but did not detect an error of type `~s'.~%"
				      condition-type)
		       elseif (null condition-type)
			 then (format *error-output* "~
Reason: detected an unexpected error of type `~s':
        ~a.~%"
				      (class-of condition)
				      condition)
		       elseif (not (if* include-subtypes
				      then (typep condition condition-type)
				      else (eq (class-of condition)
					       (find-class condition-type))))
			 then (format *error-output* "~
Reason: detected an incorrect condition type.~%")
			      (format *error-output*
				      "  wanted: ~s~%" condition-type)
			      (format *error-output*
				      "     got: ~s~%" (class-of condition))
		       elseif (and format-control
				   (not (string=
					 (setq got
					   (concatenate 'simple-string
					     "~1@<" format-control "~:@>"))
					 (setq wanted
					   (simple-condition-format-control
					    condition)))))
			 then ;; format control doesn't match
			      (format *error-output* "~
Reason: the format-control was incorrect.~%")
			      (format *error-output* "  wanted: ~s~%" wanted)
			      (format *error-output* "     got: ~s~%" got)
		       elseif (and format-arguments
				   (not (equal
					 (setq got format-arguments)
					 (setq wanted
					   (simple-condition-format-arguments
					    condition)))))
			 then (format *error-output* "~
Reason: the format-arguments were incorrect.~%")
			      (format *error-output* "  wanted: ~s~%" wanted)
			      (format *error-output* "     got: ~s~%" got)
			 else ;; what else????
			      (error "internal-error"))
		 else (let ((*print-length* 50)
			    (*print-level* 10))
			(if* wanted-message
			   then (format *error-output*
					"  wanted: ~a~%" wanted-message)
			   else (if* (not multiple-values)
				   then (format *error-output*
						"  wanted: ~s~%"
						expected-result)
				   else (format
					 *error-output*
					 "  wanted values: ~{~s~^, ~}~%"
					 expected-result)))
			(if* got-message
			   then (format *error-output*
					"     got: ~a~%" got-message)
			   else (if* (not multiple-values)
				   then (format *error-output* "     got: ~s~%"
						(second test-results))
				   else (format
					 *error-output*
					 "     got values: ~{~s~^, ~}~%"
					 (cdr test-results))))))
	      (when fail-info
		(format *error-output* "Additional info: ~a~%" fail-info))
	      )
	    (incf *test-errors*)
	    (when *break-on-test-failures*
	      (break "~a is non-nil." '*break-on-test-failures*))
       else (when known-failure
	      (report-error ()
		(format *error-output*
			"~&Expected test failure for ~s did not occur.~%"
			test-form)
		(when fail-info
		  (format *error-output* "Additional info: ~a~%" fail-info))
		)
	      (setq fail t))
	    (inc-test-counter *test-successes*))
    (not fail)))

(defmacro with-tests ((&key (name "unnamed")) &body body)
  #+allegro (set-test-syms)
  (let ((g-name (gensym)))
    `(flet ((doit () ,@body))
       (let ((,g-name ,name)
	     (*test-errors* 0)
	     (*test-successes* 0)
	     (*test-unexpected-failures* 0))
	 (report-error ()
	   (format *error-output* "Begin ~a test~%" ,g-name))
	 (if* *break-on-test-failures*
	    then (doit)
	    else (handler-case (doit)
		   (error (c)
		     (report-error ()
		       (format
			*error-output*
			"~
~&Test ~a aborted by signalling an uncaught error:~%~a~%"
			,g-name c)))))
	 (let ((state (sys:gsgc-switch :print)))
	   (report-error (:stream *standard-output*)
	     (setf (sys:gsgc-switch :print) nil)
	     (format t "~&**********************************~%" ,g-name)
	     (format t "End ~a test~%" ,g-name)
	     (format t "Errors detected in this test: ~s " *test-errors*)
	     (cond ((plusp *test-unexpected-failures*)
		    (format t "UNEXPECTED: ~s" *test-unexpected-failures*))
		   ((plusp *test-errors*)
		    (format t "(all known failures)")))
	     (format t "~%Successes this test: ~s~%" *test-successes*)
	     #+allegro
	     (when *test-syms*
	       (set (first *test-syms*)
		    (+ (symbol-value (first *test-syms*))
		       *test-errors*))
	       (set (second *test-syms*)
		    (+ (symbol-value (second *test-syms*))
		       *test-successes*))
	       #+(version>= 10 1)
	       (set (third *test-syms*)
		    (+ (symbol-value (third *test-syms*))
		       *test-unexpected-failures*)))
	     (setf (sys:gsgc-switch :print) state)))))))

(provide :tester #+module-versions 1.1)

;;; For Gnu Emacs.
;;; Local Variables: ***
;;; eval: (put 'report-error 'fi:common-lisp-indent-hook 1) ***
;;; End: ***
