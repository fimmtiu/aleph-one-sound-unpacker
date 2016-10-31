;;;; This represents a single type of sound corresponding to some in-game event.
;;;; Each may have several permutations, but most only have one.

(defconstant unused-sound-bytes 8)
(defconstant max-permutations 5)

;;; Most of this is game data that's not relevant to this script, but what
;;; the heck...
(defclass sound ()
  ((code :accessor code)
   (behaviour-index :accessor behaviour-index)
   (flags :accessor flags)
   (chance :accessor chance)
   (low-pitch :accessor low-pitch)
   (high-pitch :accessor high-pitch)
   (permutations-played :accessor permutations-played)
   (single-length :accessor single-length)
   (total-length :accessor total-length)
   (last-played :accessor last-played)
   (permutations :accessor permutations :initform '())))

;;; Read a single sound and all its permutations from the given stream.
(defun read-sound (stream)
  (let ((sound (make-instance 'sound))
        (offsets (make-array max-permutations))
        (permutations-count 0)
        (group-offset 0))

    ;; Read the header information about the sound.
    (setf (code sound) (read-int16 stream))
    (setf (behaviour-index sound) (read-int16 stream))
    (setf (flags sound) (read-uint16 stream))
    (setf (chance sound) (read-uint16 stream))
    (setf (low-pitch sound) (read-int32 stream))
    (setf (high-pitch sound) (read-int32 stream))
    (setf permutations-count (read-int16 stream))
    (setf (permutations-played sound) (read-int16 stream))
    (setf group-offset (read-int32 stream))
    (setf (single-length sound) (read-int32 stream))
    (setf (total-length sound) (read-int32 stream))
    (dotimes (i max-permutations)
      (setf (aref offsets i) (read-int32 stream)))
    (setf (last-played sound) (read-uint32 stream))
    (stream-skip stream unused-sound-bytes)

    ;; Read each of the sound's permutations (including the raw sound data).
    (dotimes (i permutations-count)
      (let ((original-position (file-position stream)))
        (file-position stream (+ group-offset (aref offsets i)))
        (setf (permutations sound)
              (nconc (permutations sound) (list (read-permutation stream))))
        (file-position stream original-position)))

    sound))

;;; Is this an empty slot in the sound file? (There are tons, which we can
;;; safely ignore.)
(defun empty-p (sound)
  (eql (code sound) -1))

;;; Just for debugging; I don't care if it's ugly.
(defun print-sound-data (sound)
  (format t "Sound:~%    Code: ~A~%    Behaviour Index: ~A~%    Flags: ~A~%    Chance: ~A~%    Low Pitch: ~A~%    High Pitch: ~A~%    Permutations Played: ~A~%    Single Length: ~A~%    Total Length: ~A~%    Last Played: ~A~%    Permutations:~%" (code sound) (behaviour-index sound) (flags sound) (chance sound) (low-pitch sound) (high-pitch sound) (permutations-played sound) (single-length sound) (total-length sound) (last-played sound))
  (dolist (perm (permutations sound))
    (print-permutation-data perm)))
