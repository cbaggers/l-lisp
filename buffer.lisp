;;; *** buffer.lisp ***
;;;
;;; This file is part of L-Lisp by Knut Arild Erstad.
;;; Implements a growing sequence as a list of vectors.

(in-package :l-systems)

;; *** Buffer data structure ***
(defstruct (buffer (:constructor internal-make-buffer))
  (inc-size 0 :type fixnum)     ; size of each "minibuffer"
  (list nil)                    ; list of minibuffers
  (current nil)                 ; current minibuffer (for fast access)
  (size 0 :type fixnum)         ; total number of elements
  (current-size 0 :type fixnum) ; number of elements in current buffer
  (list-size 0 :type fixnum))   ; length of list

(defun make-buffer (&optional (inc-size 4096))
  (internal-make-buffer :inc-size inc-size
                        :current-size inc-size))

(declaim (inline buffer-push))
(defun buffer-push (elt buffer)
  "Add a new element to the end of the buffer (like VECTOR-PUSH-EXTEND)."
  (declare (optimize (speed 3) (safety 0)))
  (incf (buffer-size buffer))
  (if (>= (buffer-current-size buffer) (buffer-inc-size buffer))
    (let ((newbuf (make-array (buffer-inc-size buffer))))
      (setf (buffer-current buffer) newbuf)
      (push newbuf (buffer-list buffer))
      (incf (buffer-list-size buffer))
      (setf (buffer-current-size buffer) 1)
      (setf (svref newbuf 0) elt))
    (let ((pos (buffer-current-size buffer)))
      (incf (buffer-current-size buffer))
      (setf (svref (buffer-current buffer) pos) elt))))

(defun buffer->vector (buffer)
  "Returns simple-vector of the buffer elements."
  (declare (optimize (speed 3) (safety 0)))
  (let ((vector (make-array (buffer-size buffer)))
        (i 0)
        (list-count (buffer-list-size buffer)))
    (declare (type fixnum i list-count))
    (dolist (v (reverse (buffer-list buffer)))
      (let ((vsize (if (zerop (decf list-count))
                     (buffer-current-size buffer)
                     (buffer-inc-size buffer))))
        (dotimes (vpos vsize)
          (setf (svref vector i) (svref v vpos))
          (incf i))))
    vector))

(defmacro dobuffer ((var buffer &optional (result nil)) &body body)
  (let ((vec (gensym))
        (vec-size (gensym))
        (vec-pos (gensym))
        (list-count (gensym))
        (buf (gensym)))
    `(let* ((,buf ,buffer)
            (,list-count (buffer-list-size ,buf)))
       (dolist (,vec (reverse (buffer-list ,buf)) ,result)
         (let ((,vec-size (if (zerop (decf ,list-count))
                            (buffer-current-size ,buf)
                            (buffer-inc-size ,buf))))
           (dotimes (,vec-pos ,vec-size)
             (let ((,var (svref ,vec ,vec-pos)))
               ,@body)))))))
