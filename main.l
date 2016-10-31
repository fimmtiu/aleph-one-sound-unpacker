#!/usr/bin/env sbcl --script

;;;; A simple script to read a Marathon sound file.

;;; Works with all Aleph One-compatible sound files, although a couple of
;;; the M1A1 sounds are garbled. (Not sure if that's a problem with this
;;; script or the data.)

(load "./util.l")
(load "./permutation.l")
(load "./sound.l")
(load "./soundfile.l")

;;; Reads all the sounds from the given sound file and write the resulting
;;; WAV files into a directory called "sounds".
(defun read-and-convert-sounds (input-file)
  (ensure-directories-exist "sounds/")
  (with-open-file (stream input-file :element-type 'unsigned-byte)
    (let ((sounds (read-sounds stream)))
      (loop for sound across (remove-if #'empty-p sounds) do
           (print-sound-data sound)
           (loop for perm in (permutations sound)
                 for i from 1 do
                (let ((name (format nil "sounds/~A_~A.wav" (code sound) i)))
                  (with-open-file (stream name :element-type '(unsigned-byte 8) :direction :output :if-exists :overwrite :if-does-not-exist :create)
                    (convert-to-wav perm stream))))))))

(defvar *input-file* (second *posix-argv*))
(unless *input-file*
  (format t "Usage: aleph-one-sound-unpacker <sound-file>~%")
  (quit))

(read-and-convert-sounds *input-file*)
