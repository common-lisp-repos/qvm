;;;; debugger.lisp
;;;;
;;;; Author: Juan M. Bello-Rivas

(in-package #:qvm-app)

(defparameter *threshold* (* 1e3 double-float-epsilon))

(defparameter *qvm* nil "Current QVM.")

(defparameter *display* nil "Current display setting.")

(defparameter *source-filename* nil "Currently loaded source file.")

(defparameter *breakpoints* nil "List of breakpoints.")

(defparameter *debugger-prompt* "(qvm-debugger) ")

(defun error-unless-qvm-exists ()
  (unless *qvm*
    (error "No program has been loaded.")))

(defstruct command
  (name nil :type string)
  (function nil :type function)
  (documentation nil :type string))

(defparameter *commands* nil
  "Association list of available commands.")

(defmacro define-debugger-command (name lambda-list &body body)
  "Define a new command function named %NAME (note that the % character is prepended to NAME) with the specified LAMBDA-LIST and BODY.

Documentation strings are mandatory in debugger commands and they should be formatted as follows:

1. First line containing usage information.
2. Empty line.
3. Longer description in a line by itself.
4. Optional additional documentation."
  (unless (stringp (first body))
    (error "Documentation string missing."))
  (let ((documentation (pop body))
        (lowercase-name (string-downcase name))
        (symbol (intern (concatenate 'string "%" (symbol-name name)))))
    `(prog1
       (defun ,symbol ,lambda-list
         ,@body)

       (setf *commands* (acons ,lowercase-name
                               (make-command :name ,lowercase-name
                                             :function (symbol-function ',symbol)
                                             :documentation ,documentation)
                               *commands*)))))

(defun %get-command (command-name)
  (alexandria:assoc-value *commands* command-name :test #'uiop:string-prefix-p))

(defun get-command (command-name)
  "Return COMMAND structure corresponding to COMMAND-NAME. Signal an error if no such command exists."
  (let ((command (%get-command command-name)))
    (unless command
      (error "No such command ~S exists." command-name))
    command))

(defun command-exists-p (command-name)
  (not (null (%get-command command-name))))

(defun eval-command (command-name &optional args)
  (let ((function (command-function (get-command command-name))))
    (if (and args (string/= args ""))
        (funcall function args)
        (funcall function))))

(defun command-description (command)
  "Return command description string found in the second non-empty line of the docstring corresponding to COMMAND."
  (let* ((documentation (command-documentation command))
         (substrings (nth-value 1 (cl-ppcre:scan-to-strings ".*\\n\\w*\\n(.*)" documentation))))
    (unless (= (length substrings) 1)
      (error "Invalid documentation string for ~S." (command-name command)))
    (aref substrings 0)))

(define-debugger-command help (&optional command-name)
  "Usage: help [COMMAND]

Display help message."
  (if command-name
      (format t "~A~%" (command-documentation (get-command command-name)))
      (loop :for (command-name . command) :in *commands* :do
        (format t "~&~A~40T~A~%" command-name (command-description command)))))

(define-debugger-command reset ()
  "Usage: reset

Reset the current QVM to the zero ket and set the program counter to zero."
  (error-unless-qvm-exists)
  (qvm::reset-quantum-state *qvm*)
  (qvm::reset-classical-memory *qvm*)
  (setf (qvm::pc *qvm*) 0))

(define-debugger-command list ()
  "Usage: list

Disassemble (list) the currently loaded program."
  (error-unless-qvm-exists)
  (flet ((get-marker-index (instruction-index)
           (if *qvm*
               (boolean-bit (= instruction-index (qvm::pc *qvm*)))
               0)))
    (loop :for instruction-index :from 0
          :for instruction :across (qvm::program *qvm*) :do
            (format t "~&~[ ~;*~]~D~10T~/cl-quil:instruction-fmt/~%"
                    (get-marker-index instruction-index) instruction-index instruction))))

(define-debugger-command run ()
  "Usage: run

Run the current program from the beginning."
  (cond
    ((and *qvm* (plusp (length (qvm::program *qvm*))))
     (loop :initially (%reset)
           :for pc := (qvm::pc *qvm*)
           :for breakpoint-p := (find pc *breakpoints*)
           :until (or (program-finished-p) breakpoint-p)
           :do (setf *qvm* (qvm:transition *qvm* (qvm::current-instruction *qvm*)))
           :finally (when breakpoint-p
                      (format t "Stopped at breakpoint before instruction:~%~
                              ~D~10T~/cl-quil:instruction-fmt/~%"
                              pc (qvm::current-instruction *qvm*)))
                    (return *qvm*))
     (when *display*
       (display-amplitudes)))
    (t
     (error "No program has been loaded."))))

(defun program-finished-p ()
  (or (null (qvm::pc *qvm*))
      (>= (qvm::pc *qvm*)
          (qvm::loaded-program-length *qvm*))))

(define-debugger-command step ()
  "Usage: step

Run the next instruction and stop."
  (error-unless-qvm-exists)
  (cond
    ((program-finished-p)
     (format t "Finished program execution.~%"))
    (t
     (quil::print-instruction (qvm::current-instruction *qvm*))
     (terpri)
     (setf *qvm* (qvm:transition *qvm* (qvm::current-instruction *qvm*)))
     (when *display*
       (display-amplitudes)))))

(define-debugger-command continue ()
  "Usage: continue

Resume program execution from the current instruction."
  (error-unless-qvm-exists)
  (cond
    ((program-finished-p)
     (format t "Finished program execution.~%"))
    (t
     (loop :for pc := (qvm::pc *qvm*)
           :for counter :from 0
           :for breakpoint-p := (and (find pc *breakpoints*)
                                     (plusp counter))
           :until (or (program-finished-p) breakpoint-p) :do
             (setf *qvm* (qvm:transition *qvm* (qvm::current-instruction *qvm*)))
           :finally (when breakpoint-p
                      (format t "Stopping at breakpoint in instruction ~D:~%~/quil:instruction-fmt/~%"
                              pc (qvm::current-instruction *qvm*)))
                    (return *qvm*)))))

(define-debugger-command load (filename)
  "Usage: load FILENAME

Load a program instantiating a suitable QVM."
  (unless filename
    (error "File name not specified."))
  (let* ((program (quil:read-quil-file (string-trim " " filename)))
         (number-of-qubits (quil:qubits-needed program)))
    (format t "Read ~A using ~D qubits.~%" filename number-of-qubits)
    (setf *source-filename* filename
          *qvm* (qvm:make-qvm number-of-qubits
                              :classical-memory-model (qvm:memory-descriptors-to-qvm-memory-model
                                                       (quil:parsed-program-memory-definitions program)))
          *breakpoints* nil)
    (qvm:load-program *qvm* program))
  (setf *breakpoints* nil))

(define-debugger-command reload ()
  "Usage: reload

Re-read the file corresponding to the previously loaded program."
  (unless *source-filename*
    (error "No program has been loaded."))
  (format t "Reloading ~S.~%" *source-filename*)
  (%load *source-filename*))

(define-debugger-command break (instruction-index)
  "Usage: break INSTRUCTION-INDEX

Set up a breakpoint at the specified instruction."
  (error-unless-qvm-exists)
  (let ((idx (parse-integer instruction-index)))
    (unless (and (integerp idx)
                 (not (minusp idx))
                 (< idx (length (qvm::program *qvm*))))
      (error "Invalid instruction index ~S." idx))
    (pushnew idx *breakpoints*)
    (setf *breakpoints* (sort *breakpoints* #'<))))

(defun display-amplitudes ()
  (format t "Amplitudes:~%")
  (let ((nq (qvm:number-of-qubits *qvm*)))
    (qvm:map-amplitudes
     *qvm*
     (let ((i 0))
       (lambda (z)
         (let ((p (* 100 (qvm:probability z))))
           (when (> p *threshold*)
             (format t
                     "|~v,'0B>: ~/qvm-app::pprint-complex/, ~64TP=~5F~%"
                     nq i z p)))
         (incf i))))))

(define-debugger-command display ()
  "Usage: display

Show the contents of the wavefunction. You can enable running this command after every step by running the \"display\" command."
  (error-unless-qvm-exists)
  (unless *display*
    (format t "Enabling register display.~%")
    (setf *display* t)))

(define-debugger-command delete ()
  "Usage: delete

Delete breakpoints and disable register display."
  (error-unless-qvm-exists)
  (setf *display* nil
        *breakpoints* nil))

(defun parse-string (string)
  "Return a list of two elements. The first is the (lowercase) command at the beginning and the second is the rest of the string without the command and surrounding whitespace."
  (multiple-value-bind (start end reg-start reg-end)
      (cl-ppcre:scan "(\\w+)\\s*(.*)$" string)
    (when (and start end)
      (list (string-downcase (subseq string (aref reg-start 0) (aref reg-end 0)))
            (subseq string (aref reg-start 1) (aref reg-end 1))))))

(define-debugger-command info (args)
  "Usage: info COMMAND where COMMAND is 'qvm', 'registers', or 'classical'.

Show internal state information."
  (let ((subcommand (first (parse-string args))))
    (cond
      ((uiop:string-prefix-p subcommand "qvm")
       (if *qvm*
           (format t "QVM is ~A with ~D qubits.~%" *qvm* (qvm:number-of-qubits *qvm*))
           (format t "No QVM has been instantiated.~%")))
      ((uiop:string-prefix-p subcommand "registers")
       (when *qvm*
         (format t "Program counter: ~D~%" (qvm::pc *qvm*))
         (display-amplitudes)))
      ((uiop:string-prefix-p subcommand "classical")
       (when *qvm*
         (qvm-app::print-classical-memory *qvm*)))
      ((uiop:string-prefix-p subcommand "breakpoints")
       (format t "~@(~R~) breakpoint~:P currently set.~%" (length *breakpoints*))
       (loop :for breakpoint :in *breakpoints* :do
         (format t "~&~D~10T~/cl-quil:instruction-fmt/~%" breakpoint (elt (qvm::program *qvm*) breakpoint))))
      (t
       (error "Invalid info command.")))))

(defun formedit (&key prompt1 prompt2)
  (declare (ignorable prompt2))
  (write-string prompt1)
  (force-output)
  (handler-case (read-line)
    (end-of-file (x)
      (declare (ignorable x))
      (when (y-or-n-p "~&Do you want to exit the debugger?")
        (uiop:quit)))))

(defun debugger ()
  (loop :with previous-command := nil
        :with previous-args := nil
        :for string := (formedit :prompt1 *debugger-prompt*)
        :for (command args) := (parse-string string)
        :until (uiop:string-prefix-p command "quit")
        :if command :do
          (handler-case
              (progn
                (eval-command command args)
                (setf previous-command command
                      previous-args args))
            (error (x)
              (format t "Error: ~A~%" x))
            (end-of-file (x)
              (declare (ignorable x))
              (quit-nicely)))
        :else :do
          (when previous-command
            (eval-command previous-command previous-args))
        :finally (quit-nicely)))
