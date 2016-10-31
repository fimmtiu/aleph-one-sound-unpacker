;;;; Random utility functions for the rest of the code.

;;; Read an unsigned 16-bit integer from the given stream.
(defun read-uint16 (stream)
  (let ((n 0))
    (setf (ldb (byte 8 8) n) (read-byte stream))
    (setf (ldb (byte 8 0) n) (read-byte stream))
    n))

;;; Read a signed 16-bit integer from the given stream.
(defun read-int16 (stream)
  (let ((n (read-uint16 stream)))
    (if (> (logand n #x8000) 0)
        (- n #x10000)
        n)))

;;; Read an unsigned 32-bit integer from the given stream.
(defun read-uint32 (stream)
  (let ((n 0))
    (dolist (offset '(24 16 8 0))
      (setf (ldb (byte 8 offset) n) (read-byte stream)))
    n))

;;; Read a signed 32-bit integer from the given stream.
(defun read-int32 (stream)
  (let ((n (read-uint32 stream)))
    (if (> (logand n #x80000000) 0)
        (- n #x100000000)
        n)))

;;; Writes a little-endian signed 16-bit integer to the given stream.
(defun write-le-int16 (int stream)
  (write-byte (logand int #xFF) stream)
  (write-byte (ash (logand int #xFF00) -8) stream))

;;; Writes a little-endian signed 32-bit integer to the given stream.
(defun write-le-int32 (int stream)
  (write-byte (logand int #xFF) stream)
  (write-byte (ash (logand int #xFF00) -8) stream)
  (write-byte (ash (logand int #xFF0000) -16) stream)
  (write-byte (ash (logand int #xFF000000) -24) stream))

;;; Writes a big-endian signed 32-bit integer to the given stream.
(defun write-be-int32 (int stream)
  (write-byte (ash (logand int #xFF000000) -24) stream)
  (write-byte (ash (logand int #xFF0000) -16) stream)
  (write-byte (ash (logand int #xFF00) -8) stream)
  (write-byte (logand int #xFF) stream))

;; If 'vec' is an unsigned byte vector comprising a little-endian integer
;; 'len' bytes long, assemble those bytes into an integer.
(defun int-from-le-vector (vec offset len)
  (let ((int 0))
    (loop for i from 0 to (1- len)
          for bits-to-shift = 0 then (+ bits-to-shift 8)
          for byte = (elt vec (+ offset i)) do
         (setf int (logior int (ash byte bits-to-shift))))
    int))

;;; Converts the unsigned representation of a twos-complement signed byte
;;; into an actual signed byte. (129 -> -127, 255 -> -1, etc.)
(defun twos-complement-to-signed-byte (byte)
  (if (> byte #x7F)
      (- (mod byte #x80) #x80)
      byte))

;;; A "fixed" integer, in MacOS parlance, is a 32-bit integer with 16
;;; integer bits (sometimes signed, though not in our case), and 16 bits
;;; representing the fractional part of the number. Basically a speed hack
;;; to do fractional math without having to do floating-point numbers.
(defun fixed-to-float (fixed)
  (let ((int-part (ash fixed -16))
        (fraction-part (logand fixed #xFFFF)))
    (float (+ int-part (/ fraction-part #x8000)))))

;;; Found myself typing this much too often.
(defun stream-skip (stream offset)
  (file-position stream (+ offset (file-position stream))))

;;; Packs a four-character string into a 4-byte integer.
(defun four-chars-to-int (str)
  (unless (eql (length str) 4)
    (error (format nil "String is longer than 4 characters: ~A" str)))
  (let ((int 0))
    (loop for offset in '(24 16 8 0)
          for character across str do
         (setf int (logior int (ash (char-code character) offset))))
    int))
