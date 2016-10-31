;;;; Code for reading the header information from a Marathon sound file.

(defconstant snd2-tag (four-chars-to-int "snd2"))
(defconstant unused-header-bytes 248)

;;; Reads all sounds from the given stream and returns a vector.
(defun read-sounds (stream)
  ;; First, sanity-check the header.
  (let ((version (read-int32 stream))
        (tag (read-uint32 stream)))
    (unless (or (eql version 0) (eql version 1))
      (error (format nil "Bogus version: ~A" version)))
    (unless (eql tag snd2-tag)
      (error (format nil "Bogus tag: ~A" tag))))

  ;; Grab the source count and number of sounds in each source.
  (let ((source-count (read-int16 stream))
        (sound-count (read-int16 stream)))
    (if (zerop sound-count)
        (setf sound-count source-count
              source-count 1))
    (let* ((total-sound-count (* source-count sound-count))
           (sounds (make-array total-sound-count)))

      ;; Skip the unused header space.
      (stream-skip stream unused-header-bytes)

      ;; Fill the vector with all of the sounds.
      (dotimes (i total-sound-count)
        (setf (aref sounds i) (read-sound stream)))
      sounds)))
