;;
;;  token-stream  -  Lexer classes for cl-stream
;;
;;  Copyright 2017,2018 Thomas de Grivel <thoxdg@gmail.com>
;;
;;  Permission to use, copy, modify, and distribute this software for any
;;  purpose with or without fee is hereby granted, provided that the above
;;  copyright notice and this permission notice appear in all copies.
;;
;;  THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
;;  WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
;;  MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
;;  ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
;;  WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
;;  ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
;;  OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
;;

(in-package :token-stream)

(defclass token ()
  ((string :initarg :string
           :initform ""
	   :accessor token-string
	   :type string)
   (line :initarg :line
	 :reader token-line
	 :type (integer 0))
   (character :initarg :character
	      :reader token-character
	      :type (integer 0))))

(defclass lexer-token (token)
  ((start :initarg :start
          :accessor token-start)))

(defvar *buffer-size* 64)
(declaim (type fixnum *buffer-size*))

(defun make-buffer ()
  (make-array `(,*buffer-size*)
              :element-type 'character
              :adjustable t
              :fill-pointer 0))

(defclass lexer (super-stream input-stream)
  ((line :initarg :line
         :initform 0
         :accessor lexer-line
         :type fixnum)
   (character :initarg :character
              :initform -1
              :accessor lexer-character
              :type fixnum)
   (input-ended :initform nil
                :accessor lexer-input-ended
                :type boolean)
   (buffer :initform (make-buffer)
           :accessor lexer-buffer
           :type string)
   (match :initform (make-instance 'lexer-token
                                   :line 0
                                   :character 0
                                   :start 0)
          :accessor lexer-match
          :type lexer-token)
   (stack :initform ()
          :accessor lexer-stack
          :type list)
   (eof-p :initform nil
          :accessor lexer-eof-p
          :type boolean)))

(defgeneric discard-token (lexer)
  (:documentation "Discard token from lexer stack top."))

(defgeneric lexer-input (lexer)
  (:documentation "Read a character from lexer in and put it into lexer
buffer."))

(defgeneric lexer-input-n (lexer n)
  (:documentation "Ensure at least N characters are in lexer buffer."))

(defgeneric lexer-match-char (lexer index)
  (:documentation "Ensure input and return the character from lexer
buffer for matching at index."))

(defgeneric lexer-match-start (lexer)
  (:documentation "Return token start for lexer match."))

(defgeneric (setf lexer-match-start) (value lexer)
  (:documentation "Set token start for lexer match."))

(defgeneric lexer-push-extend (lexer character)
  (:documentation "Put a character into lexer buffer, extending it by
*BUFFER-SIZE* if necessary."))

(defgeneric lexer-token (lexer)
  (:documentation "Consumes and returns a token."))

(defgeneric make-token (lexer symbol &rest initargs)
  (:documentation "Create a token of given class from lexer stack
top."))

(defgeneric match (lexer thing)
  (:documentation "Advance lexer match start if THING is matched in
lexer buffer."))

(defgeneric match-not (lexer thing))

(defgeneric match-option (lexer thing))

(defgeneric match-times (lexer thing min max))

(defgeneric match-until (lexer thing)
  (:documentation "Advance lexer match start until THING is matched in
lexer buffer."))

(defgeneric pop-token (lexer)
  (:documentation "Return the last token on lexer stack."))

(defgeneric push-token (lexer)
  (:documentation "Push a copy of the lexer match onto the lexer
stack."))

;;  Stream methods

(defmethod stream-element-type ((lx lexer))
  'token)

;;  Input

(defmethod lexer-push-extend ((lx lexer) item)
  (let* ((buffer (lexer-buffer lx))
         (fp (the fixnum (fill-pointer buffer)))
         (new-fp (1+ fp)))
    (if (= fp (array-dimension buffer 0))
        (let ((new-buffer (adjust-array buffer
                                        (the fixnum (+ fp *buffer-size*))
                                        :fill-pointer new-fp)))
          (setf (lexer-buffer lx) new-buffer))
        (setf (fill-pointer buffer) new-fp))
    (locally (declare (optimize (safety 0)))
      (setf (aref buffer fp) item))
    fp))

(defmethod lexer-input ((lx lexer))
  (let ((in (stream-underlying-stream lx)))
    (multiple-value-bind (item state) (stream-read in)
      (ecase state
        ((nil) (let* ((pos (the fixnum (lexer-push-extend lx item)))
                      (buf (lexer-buffer lx)))
                 (declare (type vector buf))
                 (cond ((or (and (char= #\Newline item)
                                 (or (not (< 0 pos))
                                     (char/= #\Return
                                             (char buf (1- pos)))))
                            (char= #\Return item))
                        (setf (lexer-character lx) 0)
                        (incf (the fixnum (lexer-line lx))))
                       (t
                        (incf (the fixnum (lexer-character lx)))))
                 (values item nil)))
        ((:eof) (setf (lexer-input-ended lx) t)
         (values nil :eof))
        ((:non-blocking)
         (signal (make-condition 'non-blocking :stream lx)))))))

(defmethod lexer-match-start ((lx lexer))
  (token-start (lexer-match lx)))

(defmethod (setf lexer-match-start) ((value integer) (lx lexer))
  (setf (token-start (lexer-match lx)) (the fixnum value)))

(defmethod lexer-input-n ((lx lexer) (n integer))
  (declare (type fixnum n))
  (loop
     (let ((length (- (the fixnum (fill-pointer (lexer-buffer lx)))
                      (the fixnum (lexer-match-start lx)))))
       (declare (type fixnum length))
       (when (or (lexer-input-ended lx)
                 (<= n length))
         (return))
       (lexer-input lx))))

(defmethod lexer-match-char ((lx lexer) (index integer))
  (declare (type fixnum index))
  (lexer-input-n lx (the fixnum (1+ index)))
  (let ((buf (lexer-buffer lx))
        (match-index (+ (the fixnum (lexer-match-start lx))
                        index)))
    (declare (type (vector character) buf)
             (type fixnum match-index))
    (char buf match-index)))

;;  Tokenizer

(defmethod push-token ((lx lexer))
  (let* ((match (lexer-match lx))
         (line (token-line match))
         (character (token-character match))
         (start (token-start match))
         (token (make-instance 'lexer-token
                               :line line
                               :character character
                               :start start)))
    (push token (lexer-stack lx))))

(defun subseq* (sequence start &optional end)
  (subseq sequence start end))

(defmethod pop-token ((lx lexer))
  (assert (lexer-stack lx))
  (let* ((buffer (lexer-buffer lx))
	 (fp (fill-pointer buffer))
	 (token (pop (lexer-stack lx)))
	 (match-start (lexer-match-start lx))
         (string (subseq* buffer (token-start token) match-start)))
    (setf (token-string token) string)
    (when (endp (lexer-stack lx))
      (replace buffer buffer :start2 match-start :end2 fp)
      (setf (lexer-match-start lx) 0
	    (fill-pointer buffer) (- fp match-start)))
    token))

(defmethod make-token ((lx lexer) (class symbol) &rest initargs)
  (let ((lt (pop-token lx)))
    (apply #'make-instance class
	   :string (token-string lt)
	   :line (token-line lt)
	   :character (token-character lt)
	   initargs)))

(defmethod discard-token ((lx lexer))
  (pop (lexer-stack lx))
  (when (endp (lexer-stack lx))
    (let* ((buffer (lexer-buffer lx))
           (fp (fill-pointer buffer))
           (match-start (lexer-match-start lx)))
      (replace buffer buffer :start2 match-start :end2 fp)
      (setf (lexer-match-start lx) 0
            (fill-pointer buffer) (- fp match-start))))
  nil)

;;  Matcher

(defmethod match ((lx lexer) (s string))
  (let* ((length (length s))
         (match-start (lexer-match-start lx))
         (match-end (+ match-start length))
         (buffer (lexer-buffer lx)))
    (declare (type fixnum length match-start match-end)
             (type (vector character) buffer))
    (lexer-input-n lx length)
    (when (and (<= match-end (length buffer))
               (string= s buffer :start2 match-start :end2 match-end))
      (incf (the fixnum (lexer-match-start lx)) length))))

(defmethod match ((lx lexer) (c character))
  (when (char= c (lexer-match-char lx 0))
    (incf (the fixnum (lexer-match-start lx)))))

(defmethod match-until ((lx lexer) (s string))
  (lexer-input-n lx (length s))
  (loop
     (let ((match (match lx s)))
       (when match
         (return match)))
     (when (lexer-input-ended lx)
       (return))
     (lexer-input lx)
     (incf (the fixnum (lexer-match-start lx)))))

(defmethod match-until ((lx lexer) (f function))
  (loop
     (let ((match (funcall f lx)))
       (when match
         (return match)))
     (when (lexer-input-ended lx)
       (return))
     (lexer-input lx)
     (incf (the fixnum (lexer-match-start lx)))))

(defmethod match-option ((lx lexer) (f function))
  (or (funcall f lx)
      (lexer-match-start lx)))

(defmethod match-not ((lx lexer) (f function))
  (let ((match-start (lexer-match-start lx)))
    (cond ((or (funcall f lx)
               (lexer-input-ended lx))
           (setf (lexer-match-start lx) match-start)
           nil)
          (t
           (incf (the fixnum (lexer-match-start lx)))))))

(defmacro match-sequence (lexer &body body)
  (let ((lx (gensym "LX-"))
	(match-start (gensym "MATCH-START-"))
	(result (gensym "RESULT-")))
    `(let* ((,lx ,lexer)
	    (,match-start (lexer-match-start ,lx))
	    (,result (progn ,@body)))
       (cond (,result
	      ,result)
	     (t
	      (setf (lexer-match-start ,lx) ,match-start)
	      nil)))))

(defmethod match-times ((lx lexer) (f function) (min integer) (max integer))
  (declare (type fixnum min max))
  (match-sequence lx
    (let ((n 0))
      (loop
         (unless (< n max)
           (return (lexer-match-start lx)))
         (unless (funcall f lx)
           (if (< n min)
               (return nil)
               (return (lexer-match-start lx))))
         (incf n)))))

(defmethod match-times ((lx lexer) (f function) (min integer) (max null))
  (declare (type fixnum min))
  (match-sequence lx
    (let ((n 0))
      (declare (type fixnum n))
      (loop
         (unless (funcall f lx)
           (if (< n min)
               (return nil)
               (return (lexer-match-start lx))))
         (incf n)))))

(defmethod stream-read ((lx lexer))
  (if (lexer-eof-p lx)
      (values nil :eof)
      (handler-case (values (lexer-token lx) nil)
        (end-of-file () (values nil :eof))
        (non-blocking () (values nil :non-blocking)))))
