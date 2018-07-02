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

(defclass lexer (matcher-stream)
  ((match :initform (make-instance 'lexer-token
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

(defgeneric lexer-token (lexer)
  (:documentation "Consumes and returns a token."))

(defgeneric make-token (lexer symbol &rest initargs)
  (:documentation "Create a token of given class from lexer stack
top."))

(defgeneric pop-token (lexer)
  (:documentation "Return the last token on lexer stack."))

(defgeneric push-token (lexer)
  (:documentation "Push a copy of the lexer match onto the lexer
stack."))

;;  Stream methods

(defmethod stream-element-type ((lx lexer))
  'token)

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
  (let* ((buffer (stream-input-buffer lx))
	 (fp (fill-pointer buffer))
	 (token (pop (lexer-stack lx)))
	 (match-start (matcher-start lx))
         (string (subseq* buffer (token-start token) match-start)))
    (setf (token-string token) string)
    (when (endp (lexer-stack lx))
      (replace buffer buffer :start2 match-start :end2 fp)
      (setf (matcher-start lx) 0
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
    (let* ((buffer (stream-input-buffer lx))
           (fp (fill-pointer buffer))
           (match-start (matcher-start lx)))
      (replace buffer buffer :start2 match-start :end2 fp)
      (setf (matcher-start lx) 0
            (fill-pointer buffer) (- fp match-start))))
  nil)

(defmethod stream-read ((lx lexer))
  (if (lexer-eof-p lx)
      (values nil :eof)
      (handler-case (values (lexer-token lx) nil)
        (end-of-file () (values nil :eof))
        (non-blocking () (values nil :non-blocking)))))
