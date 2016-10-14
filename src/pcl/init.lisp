;;;; This file defines the initialization and related protocols.

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.

;;;; This software is derived from software originally released by Xerox
;;;; Corporation. Copyright and release statements follow. Later modifications
;;;; to the software are in the public domain and are provided with
;;;; absolutely no warranty. See the COPYING and CREDITS files for more
;;;; information.

;;;; copyright information from original PCL sources:
;;;;
;;;; Copyright (c) 1985, 1986, 1987, 1988, 1989, 1990 Xerox Corporation.
;;;; All rights reserved.
;;;;
;;;; Use and copying of this software and preparation of derivative works based
;;;; upon this software are permitted. Any distribution of this software or
;;;; derivative works must comply with all applicable United States export
;;;; control laws.
;;;;
;;;; This software is made available AS IS, and Xerox Corporation makes no
;;;; warranty about the software, its performance or its conformity to any
;;;; specification.

(in-package "SB-PCL")

(defmethod make-instance ((class symbol) &rest initargs)
  (apply #'make-instance (find-class class) initargs))

(defmethod make-instance ((class class) &rest initargs)
  (declare (inline ensure-class-finalized))
  (let ((instance-or-nil (maybe-call-ctor class initargs)))
    (when instance-or-nil
      (return-from make-instance instance-or-nil)))
  (ensure-class-finalized class)
  (let ((class-default-initargs (class-default-initargs class)))
    (when class-default-initargs
      (setf initargs (default-initargs initargs class-default-initargs)))
    (when initargs
      (when (eq **boot-state** 'complete)
        (check-mi-initargs class initargs)))
    (let ((instance (apply #'allocate-instance class initargs)))
      (apply #'initialize-instance instance initargs)
      instance)))

(defun default-initargs (supplied-initargs class-default-initargs)
  (loop for (key nil fun) in class-default-initargs
        when (eq (getf supplied-initargs key '.not-there.) '.not-there.)
          append (list key (funcall fun)) into default-initargs
        finally
          (return (append supplied-initargs default-initargs))))

(defmethod initialize-instance ((instance slot-object) &rest initargs)
  (apply #'shared-initialize instance t initargs))

(defmethod reinitialize-instance ((instance slot-object) &rest initargs)
  ;; the ctor machinery allows us to track when memoization of
  ;; validity of initargs should be cleared.
  (check-ri-initargs instance initargs)
  (apply #'shared-initialize instance nil initargs)
  instance)


;; UNDERSTANDING
;;
;; Q: In the original code, the expansion in SB-PCL::WITH-TYPE-CHECKED
;;    uses THE to check for the type, and there is a comment about it:
;;
;;    ;; To handle FUNCTION types reasonable, we use SAFETY 3 and
;;    ;; THE instead of e.g. CHECK-TYPE.
;;
;;    After running the test suite, I do get an error for the :TYPE
;;    :FUNCTION test in clos-typechecking.impure.lisp.
;;
;;    Why wouldn't CHECK-TYPE handle FUNCTION types reasonably?
;;
;;    Because the list form of the FUNCTION type specifier should not
;;    be passed to TYPEP.
;;
;;    Issue FUNCTION-TYPE writeup states:
;;
;;    "Also, page 47 of CLtL states that the FUNCTION type specifier
;;    can only be used for declaration and not discrimination."
;;
;;    In the proposal, point 2c states:
;;
;;    "Clarify that the list form of the FUNCTION type specifier may
;;    still only be used for declaration."
;;
;;    CLtL states:
;;
;;    "Note that the distinction between declaration and
;;    discrimination remains useful, if only so that we may remark
;;    that the specialized (list) form of the function type specifier
;;    may still be used only for declaration and not for
;;    discrimination."
;;
;;    "X3J13 voted in June 1988 (FUNCTION-TYPE) to clarify that while
;;    the specialized form of the function type specifier (a list of
;;    the symbol function possibly followed by argument and value type
;;    specifiers) may be used only for declaration, the symbol form
;;    (simply the name function) may be used for discrimination."
;;
;;    I note that in the original code the slow-slot-typecheck
;;    generated in SB-PCL::GENERATE-SLOTD-TYPECHECK does use TYPEP
;;    with the typespec passed as-is.  It may be considered a bug.
;;
;;    Still, we can use the symbol form for discrimination and the
;;    list form for declaration?
;;
;;    It seems SBCL can take a STRICT flag if we use
;;    SB-KERNEL::%%TYPEP.  The type specifier then needs to be passed
;;    to SB-KERNEL::SPECIFIER-TYPE (or rather,
;;    SB-C::CAREFUL-SPECIFIER-TYPE).  Since I don't know if the
;;    resulting object is externalizable, I do it in runtime, and that
;;    is inefficient.
;;
;;    The :TYPE :FUNCTION test seems buggy, as it supplies (LAMBDA ()
;;    1) and expects it to pass as type (FUNCTION (FIXNUM) FIXNUM).
;;
;;    Since the functions SB-PCL::GENERATE-SLOTD-TYPECHECK generates
;;    are no longer based on just a type specifier, but also on
;;    class/slot names, SB-PCL::**TYPECHECK-CACHE** is no longer
;;    useful.
;;
;; Q: If I evaluate MAKE-INSTANCE in the REPL, it leads to slot
;;    assignment code generated by
;;    SB-PCL::MAKE-OPTIMIZED-STD-WRITER-METHOD-FUNCTION.
;;
;;    If I call MAKE-INSTANCE inside a function, it leads to slot
;;    assignment code generated by SB-PCL::SLOT-INIT-FORMS.
;;
;;    Why are there two code paths?
;;
;;    I'm guessing the compiled MAKE-INSTANCE is translated to
;;    something more efficient.
;;
;; Q: In the original code, SB-PCL::GENERATE-SLOTD-TYPECHECK generates
;;    two kinds of typecheck functions, a slow-slot-typecheck and a
;;    fast one.  The fast one again uses THE to do the type checking,
;;    but a bit different from the one in SB-PCL::WITH-TYPE-CHECKED.
;;
;;    Is the THE type test comparable in speed to a TYPEP type test
;;    with a literal type specifier?
;;
;;    Should the expansions be different?
;;
;; Q: Apparently there are more places that use the
;;    SB-PCL::SLOT-INFO-TYPECHECK path:
;;
;;    * SB-PCL::SET-SLOT-VALUE
;;    * SB-PCL::SAFE-SET-SLOT-VALUE
;;    * (SETF SB-PCL::SLOT-VALUE-USING-CLASS)
;;
;;    When are these used?
;;
;;    A simple (SETF (SLOT-VALUE INSTANCE SLOT) 42) seems to lead to
;;    the first.
;;
;; Q: The SB-PCL::SLOT-INFO-TYPECHECK method is used in all cases
;;    except the SB-PCL::SLOT-INIT-FORMS case.  Why is this latter
;;    case different?
;;
;;    I guess for efficiency reasons.
;;
;; IMPLEMENTATION
;;
;; Q: Should I attempt to minimize code size in the expansions?
;;
;; Q: Is SB-KERNEL::READ-EVALUATED-FORM the function to use in
;;    SB-PCL::CHECK-SLOT-ASSIGNMENT-TYPE-ERROR?
;;

(define-condition slot-assignment-type-error (reference-condition type-error)
  ((class-name :initarg :class-name :reader slot-assignment-type-error-class-name)
   (slot-name :initarg :slot-name :reader slot-assignment-type-error-slot-name))
  (:default-initargs :references (list '(:ansi-cl :section (7 5 3))))
  (:report (lambda (condition stream)
             (let ((class-name (slot-assignment-type-error-class-name condition))
                   (slot-name (slot-assignment-type-error-slot-name condition))
                   (datum (type-error-datum condition))
                   (expected-type (type-error-expected-type condition)))
               (format stream
                       "~@<Invalid slot assignment: the value ~S ~
                       is not of type ~S, which is expected for ~
                       values of the slot ~S defined in class ~S .~@:>"
                       datum expected-type slot-name class-name)))))

(defun check-slot-assignment-type-error (class-name slot-name value type)
  (let ((condition
          (make-condition
           'slot-assignment-type-error
           :class-name class-name
           :slot-name slot-name
           :datum value
           :expected-type type)))
    (restart-case (error condition)
      (store-value (value)
        :report "Supply a new value."
        :interactive sb-kernel::read-evaluated-form
        value))))

(defmacro with-type-checked ((type safe-p class-name slot-name) &body body)
  (if safe-p
      (let ((result (gensym)))
        `(let ((,result (progn ,@body)))
           (do ()
               ((sb-kernel::%%typep ,result (sb-c::careful-specifier-type ',type) nil))
             (setf ,result
                   (check-slot-assignment-type-error
                    ',class-name
                    ',slot-name
                    ,result
                    ',type)))
           (the ,type ,result)))
      `(progn ,@body)))

(defvar *typecheck-stack* nil)

(defun generate-slotd-typecheck (slotd info)
  (let* ((type (slot-definition-type slotd))
         (class (slot-definition-class slotd))
         (class-name (class-name class))
         (slot-name (slot-definition-name slotd))
         (cookie (cons class (slot-definition-name slotd))))
    (declare (dynamic-extent cookie))
    (when (and (neq t type) (safe-p class))
      (or
       ;; It is possible for compilation of a typecheck to trigger class
       ;; finalization, which in turn may trigger compilation of a
       ;; slot-typechecking function -- detects and break those cycles.
       ;;
       ;; We use the slow function here, but the outer call will replace it
       ;; with the fast one.
       (when (member cookie *typecheck-stack* :test #'equal)
         (setf (slot-info-typecheck info)
               (named-lambda slow-slot-typecheck (value)
                 (do ()
                     ((sb-kernel::%%typep value (sb-c::careful-specifier-type type) nil))
                   (setf value
                         (check-slot-assignment-type-error
                          class-name
                          slot-name
                          value
                          type)))
                 value)))
       ;; The normal, good case: compile an efficient typecheck function.
       (let ((*typecheck-stack* (cons cookie *typecheck-stack*)))
         (handler-bind (((or style-warning compiler-note) #'muffle-warning))
           (let ((fun (compile
                       nil
                       `(named-lambda (slot-typecheck ,type) (value)
                          (declare (optimize (sb-c:store-coverage-data 0)
                                             (sb-c::verify-arg-count 0)))
                          (with-type-checked (,type t ,class-name ,slot-name)
                            value)))))
             (setf (slot-info-typecheck info) fun))))))))

(define-condition slotd-initialization-error (reference-condition error)
  ((initarg :initarg :initarg :reader slotd-initialization-error-initarg)
   (kind :initarg :kind :reader slotd-initialization-error-kind)
   (value :initarg :value :initform nil :reader slotd-initialization-error-value))
  (:default-initargs :references (list '(:amop :initialization slot-definition)))
  (:report (lambda (condition stream)
             (let ((initarg (slotd-initialization-error-initarg condition))
                   (kind (slotd-initialization-error-kind condition))
                   (value (slotd-initialization-error-value condition)))
               (format stream
                       "~@<Invalid ~S initialization: the initialization ~
                        argument ~S was ~
                        ~[missing~*~;not a symbol: ~S~;constant: ~S~].~@:>"
                       'slot-definition initarg
                       (getf '(:missing 0 :symbol 1 :constant 2) kind)
                       value)))))

(define-condition slotd-initialization-type-error (slotd-initialization-error type-error)
  ((value :initarg :datum))
  (:report (lambda (condition stream)
             (let ((initarg (slotd-initialization-error-initarg condition))
                   (datum (type-error-datum condition))
                   (expected-type (type-error-expected-type condition)))
               (format stream
                       "~@<Invalid ~S initialization: the initialization ~
                        argument ~S was ~S, which is not of type ~S.~@:>"
                       'slot-definition initarg
                       datum expected-type)))))

(defmethod initialize-instance :before ((slotd slot-definition)
                                        &key (name nil namep)
                                          (initform nil initformp)
                                          (initfunction nil initfunp)
                                          (type nil typep)
                                          (allocation nil allocationp)
                                          (initargs nil initargsp)
                                          (documentation nil docp))
  (declare (ignore initform initfunction type))
  (unless namep
    (error 'slotd-initialization-error :initarg :name :kind :missing))
  (unless (symbolp name)
    (error 'slotd-initialization-type-error :initarg :name :datum name :expected-type 'symbol))
  (when (and (constantp name)
             ;; KLUDGE: names of structure slots are weird, and their
             ;; weird behaviour gets grandfathered in this way.  (The
             ;; negative constraint is hard to express in normal
             ;; CLOS method terms).
             (not (typep slotd 'structure-slot-definition)))
    (error 'slotd-initialization-error :initarg :name :kind :constant :value name))
  (when (and initformp (not initfunp))
    (error 'slotd-initialization-error :initarg :initfunction :kind :missing))
  (when (and initfunp (not initformp))
    (error 'slotd-initialization-error :initarg :initform :kind :missing))
  (when (and typep (not t))
    ;; FIXME: do something.  Need SYNTACTICALLY-VALID-TYPE-SPECIFIER-P
    )
  (when (and allocationp (not (symbolp allocation)))
    (error 'slotd-initialization-type-error :initarg :allocation :datum allocation :expected-type 'symbol))
  (when initargsp
    (unless (typep initargs 'list)
      (error 'slotd-initialization-type-error :initarg :initarg :datum initargs :expected-type 'list))
    (do ((is initargs (cdr is)))
        ((atom is)
         (unless (null is)
           (error 'slotd-initialization-type-error :initarg :initarg :datum initargs :expected-type '(satisfies proper-list-p))))
      (unless (symbolp (car is))
        (error 'slotd-initialization-type-error :initarg :initarg :datum is :expected-type '(or null (cons symbol))))))
  (when docp
    (unless (typep documentation '(or null string))
      (error 'slotd-initialization-type-error :initarg :documentation :datum documentation :expected-type '(or null string)))))

(defmethod initialize-instance :before ((dslotd direct-slot-definition)
                                        &key
                                          (readers nil readersp)
                                          (writers nil writersp))
  (macrolet ((check (arg argp)
               `(when ,argp
                  (unless (typep ,arg 'list)
                    (error 'slotd-initialization-type-error
                           :initarg ,(keywordicate arg)
                           :datum ,arg :expected-type 'list))
                  (do ((as ,arg (cdr as)))
                      ((atom as)
                       (unless (null as)
                         (error 'slotd-initialization-type-error
                                :initarg ,(keywordicate arg)
                                :datum ,arg :expected-type '(satisfies proper-list-p))))
                    (unless (valid-function-name-p (car as))
                      (error 'slotd-initialization-type-error
                             :initarg ,(keywordicate arg)
                             :datum ,arg :expected-type '(or null (cons (satisfies valid-function-name-p)))))))))
    (check readers readersp)
    (check writers writersp)))

(defmethod initialize-instance :after ((slotd effective-slot-definition) &key)
  (let ((info (make-slot-info :slotd slotd)))
    (generate-slotd-typecheck slotd info)
    (setf (slot-definition-info slotd) info)))

;;; FIXME: Do we need (SETF SLOT-DEFINITION-TYPE) at all?
(defmethod (setf slot-definition-type) :after (new-type (slotd effective-slot-definition))
  (generate-slotd-typecheck slotd (slot-definition-info slotd)))

(defmethod update-instance-for-different-class
    ((previous standard-object) (current standard-object) &rest initargs)
  ;; First we must compute the newly added slots. The spec defines
  ;; newly added slots as "those local slots for which no slot of
  ;; the same name exists in the previous class."
  (let ((added-slots '())
        (current-slotds (class-slots (class-of current)))
        (previous-slotds (class-slots (class-of previous))))
    (dolist (slotd current-slotds)
      (when (and (eq (slot-definition-allocation slotd) :instance)
                 (not (member (slot-definition-name slotd) previous-slotds
                              :key #'slot-definition-name)))
        (push (slot-definition-name slotd) added-slots)))
    (when initargs
      (let ((call-list (list (list* 'update-instance-for-different-class previous current initargs)
                             (list* 'shared-initialize current added-slots initargs))))
        (declare (dynamic-extent call-list))
        (check-initargs-1 (class-of current) initargs call-list)))
    (apply #'shared-initialize current added-slots initargs)))

(defmethod update-instance-for-redefined-class
    ((instance standard-object) added-slots discarded-slots property-list
     &rest initargs)
  (when initargs
    (check-initargs-1
     (class-of instance) initargs
     (list (list* 'update-instance-for-redefined-class
                  instance added-slots discarded-slots property-list initargs)
           (list* 'shared-initialize instance added-slots initargs))))
  (apply #'shared-initialize instance added-slots initargs))

(defmethod shared-initialize ((instance slot-object) slot-names &rest initargs)
  (flet ((initialize-slot-from-initarg (class instance slotd)
           (let ((slot-initargs (slot-definition-initargs slotd)))
             (doplist (initarg value) initargs
               (when (memq initarg slot-initargs)
                 (setf (slot-value-using-class class instance slotd)
                       value)
                 (return t)))))
         (initialize-slot-from-initfunction (class instance slotd)
           ;; CLHS: If a before method stores something in a slot,
           ;; that slot won't be initialized from its :INITFORM, if any.
           (let ((initfun (slot-definition-initfunction slotd)))
             (if (typep instance 'structure-object)
                 ;; We don't have a consistent unbound marker for structure
                 ;; object slots, and structure object redefinition is not
                 ;; really supported anyways -- so unconditionally
                 ;; initializing the slot should be fine.
                 (when initfun
                   (setf (slot-value-using-class class instance slotd)
                         (funcall initfun)))
                 (unless (or (not initfun)
                             (slot-boundp-using-class class instance slotd))
                   (setf (slot-value-using-class class instance slotd)
                         (funcall initfun)))))))
    (let ((class (class-of instance)))
      (loop for slotd in (class-slots class)
            unless (initialize-slot-from-initarg class instance slotd)
            do
            (when (or (eq t slot-names)
                      (memq (slot-definition-name slotd) slot-names))
              (initialize-slot-from-initfunction class instance slotd))))
    instance))

;;; If initargs are valid return nil, otherwise signal an error.
(defun check-initargs-1 (class initargs call-list
                         &optional (plist-p t) (error-p t))
  (multiple-value-bind (legal allow-other-keys)
      (check-initargs-values class call-list)
    (unless allow-other-keys
      (if plist-p
          (check-initargs-2-plist initargs class legal error-p)
          (check-initargs-2-list initargs class legal error-p)))))

(defun check-initargs-values (class call-list)
  (let ((methods (mapcan (lambda (call)
                           (if (consp call)
                               (copy-list (compute-applicable-methods
                                           (gdefinition (car call))
                                           (cdr call)))
                               (list call)))
                         call-list))
        (legal (apply #'append (mapcar #'slot-definition-initargs
                                       (class-slots class)))))
    ;; Add to the set of slot-filling initargs the set of
    ;; initargs that are accepted by the methods. If at
    ;; any point we come across &allow-other-keys, we can
    ;; just quit.
    (dolist (method methods)
      (multiple-value-bind (llks nreq nopt keys)
          (analyze-lambda-list (if (consp method)
                                   (early-method-lambda-list method)
                                   (method-lambda-list method)))
        (declare (ignore nreq nopt))
        (when (ll-kwds-allowp llks)
          (return-from check-initargs-values (values nil t)))
        (setq legal (append keys legal))))
    (values legal nil)))

(define-condition initarg-error (reference-condition program-error)
  ((class :reader initarg-error-class :initarg :class)
   (initargs :reader initarg-error-initargs :initarg :initargs))
  (:default-initargs :references (list '(:ansi-cl :section (7 1 2))))
  (:report (lambda (condition stream)
             (format stream "~@<Invalid initialization argument~P: ~2I~_~
                             ~<~{~S~^, ~} ~@:>~I~_in call for class ~S.~:>"
                     (length (initarg-error-initargs condition))
                     (list (initarg-error-initargs condition))
                     (initarg-error-class condition)))))

(defun check-initargs-2-plist (initargs class legal &optional (error-p t))
  (let ((invalid-keys ()))
    (unless (getf initargs :allow-other-keys)
      ;; Now check the supplied-initarg-names and the default initargs
      ;; against the total set that we know are legal.
      (doplist (key val) initargs
        (unless (or (memq key legal)
                    ;; :ALLOW-OTHER-KEYS NIL gets here
                    (eq key :allow-other-keys))
          (push key invalid-keys)))
      (when (and invalid-keys error-p)
        (error 'initarg-error :class class :initargs invalid-keys)))
    invalid-keys))

(defun check-initargs-2-list (initkeys class legal &optional (error-p t))
  (let ((invalid-keys ()))
    (unless (memq :allow-other-keys initkeys)
      ;; Now check the supplied-initarg-names and the default initargs
      ;; against the total set that we know are legal.
      (dolist (key initkeys)
        (unless (memq key legal)
          (push key invalid-keys)))
      (when (and invalid-keys error-p)
        (error 'initarg-error :class class :initargs invalid-keys)))
    invalid-keys))
