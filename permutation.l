;;;; Code to read a "permutation", which is a single sound effect. When a
;;;; sound event occurs in-game, one of several slightly different
;;;; permutations will get played at random.
;;;;
;;;; Terminology: A "sample" is a single fragment of audio data. A "frame"
;;;; is all the samples needed to play all the channels of a sound at a
;;;; given moment. (Sample and frame are identical for single-channel
;;;; sounds.) We play the frames back at a speed of "sample-rate" frames
;;;; per second.

;; The string "twos" packed into a 32-bit integer.
(defconstant twos-tag (four-chars-to-int "twos"))
(defconstant standard-header-tag #x0)
(defconstant extended-header-tag #xFF)
(defconstant compressed-header-tag #xFE)
(defconstant fixed-compression-tag -1)

;;; The audio data for a single permutation.
(defstruct (permutation (:conc-name nil))
  (sample-bits 8)
  (channel-count 1)
  (signed-eight-bit-p nil)
  (little-endian-p nil)
  (sample-rate)
  (base-frequency)
  (data))

(defun bytes-per-frame (perm)
  (* (/ (sample-bits perm) 8) (channel-count perm)))

(defun frame-count (perm)
  (/ (length (data perm)) (bytes-per-frame perm)))

;;; There's two different kinds of permutations. "Standard" ones are simple
;;; pre-System 7 'snd ' resources that use unsigned eight-bit samples.
(defun read-standard-permutation (stream)
  (let ((perm (make-permutation))
        (length nil)
        (encoding nil))
    ;; Skip the first 4 unused bytes ("samplePtr" according to IM:S)
    (stream-skip stream 4)

    (setf length (read-int32 stream))
    (setf (sample-rate perm) (fixed-to-float (read-uint32 stream)))
    (stream-skip stream 8)   ; skip the loopStart and loopEnd
    (setf encoding (read-byte stream))
    (setf (base-frequency perm) (read-byte stream))
    (unless (eql encoding standard-header-tag)
      (error (format nil "Not a standard header tag: ~A" encoding)))

    ;; Read the actual sound data.
    (let ((vec (make-array length :element-type '(unsigned-byte 8))))
      (read-sequence vec stream)
      (setf (data perm) vec))

    perm))

;;; It's not actually compressed, thank goodness. The "twos" tag just means
;;; that it's a series of signed twos-complement integers.
(defun read-compressed-header (perm stream)
  (stream-skip stream 14)
  (let ((compression-format (read-uint32 stream)))
    (unless (eql compression-format twos-tag)
      (error (format nil "Unknown compression format: ~A" compression-format))))
  (stream-skip stream 12)
  (let ((compression-type (read-int16 stream)))
    (unless (eql compression-type fixed-compression-tag)
      (error (format nil "Unknown compression type: ~A" compression-type))))
  (stream-skip stream 4)
  (setf (signed-eight-bit-p perm) t))

;;; Converts this permutation's samples in-place from signed 8-bit
;;; (-128..127) to unsigned 8-bit (0..255).
(defun convert-to-unsigned-eight-bit (perm)
  (setf (signed-eight-bit-p perm) nil)
  (loop for value across (data perm)
        for i from 0 do
       (setf (elt (data perm) i)  (+ (twos-complement-to-signed-byte value) #x80))))

;;; Converts this permutation's samples from unsigned 8-bit (0..255) to
;;; little-endian signed 16-bit (-32,768..32,767).
(defun convert-to-le-sixteen-bit (perm)
  (setf (sample-bits perm) 16)
  (setf (little-endian-p perm) t)
  (let ((new-vec (make-array (* (length (data perm)) 2) :element-type '(unsigned-byte 8))))
    (loop for value across (data perm)
          for i from 0 by 2
          for sixteen-bit-value = (- (* (/ value #xFF) #xFFFF) #x8000) do
         (setf (elt new-vec i) (logand sixteen-bit-value #xFF))
         (setf (elt new-vec (1+ i)) (logand (ash sixteen-bit-value -8) #xFF)))
    (setf (data perm) new-vec)))

;;; Converts this permutation's samples in-place from big-endian signed
;;; 16-bit to little-endian signed 16-bit.
(defun convert-to-little-endian (perm)
  (setf (little-endian-p perm) t)
  (loop for i from 0 to (- (length (data perm)) 2) by 2 do
       (rotatef (elt (data perm) i) (elt (data perm) (1+ i)))))

;;; Converts the sound to a 16-bit little-endian representation, which makes
;;; converting it to WAV much simpler.
(defun normalize (perm)
  (when (signed-eight-bit-p perm)
    (convert-to-unsigned-eight-bit perm))
  (when (eql (sample-bits perm) 8)
    (convert-to-le-sixteen-bit perm))
  (unless (little-endian-p perm)
    (convert-to-little-endian perm))
  perm)

;;; An "extended" permutation is a Mac sound resource that allows for
;;; compression and multi-channel audio. (See Inside Macintosh: Sound.)
(defun read-extended-permutation (stream)
  (let ((perm (make-permutation))
        (length nil)
        (frame-count nil)
        (encoding nil))

    ;; Skip the first 4 unused bytes ("samplePtr" according to IM:S)
    (stream-skip stream 4)

    (setf (channel-count perm) (read-int32 stream))
    (setf (sample-rate perm) (fixed-to-float (read-uint32 stream)))
    (stream-skip stream 8)   ; skip the loopStart and loopEnd
    (setf encoding (read-byte stream))
    (setf (base-frequency perm) (read-byte stream))
    (setf frame-count (read-int32 stream))

    ;; If it's "compressed", read the compression header, otherwise skip it.
    (if (eql encoding compressed-header-tag)
        (read-compressed-header perm stream)
        (stream-skip stream 22))

    (setf (sample-bits perm) (read-int16 stream))
    (setf length (* frame-count (bytes-per-frame perm)))

    ;; Read the actual sound data.
    (let* ((vec (make-array length :element-type '(unsigned-byte 8))))
      (read-sequence vec stream)
      (setf (data perm) vec))

    (normalize perm)))

;;; Peek at the encoding, which is 20 bytes into the header. (That's a
;;; silly place for it...)
(defun get-encoding (stream)
  (stream-skip stream 20)
  (let ((encoding (read-byte stream)))
    (stream-skip stream -21)
    encoding))

;;; Reads a single permutation from the given stream.
(defun read-permutation (stream)
  (let ((encoding (get-encoding stream)))
    (if (eql encoding standard-header-tag)
        (read-standard-permutation stream)
        (read-extended-permutation stream))))

;;; Writes the permutation to the given stream in WAV format.
(defun convert-to-wav (perm stream)
  (let ((rounded-sample-rate (round (sample-rate perm))))
    ;; File header
    (write-be-int32 (four-chars-to-int "RIFF") stream)
    (write-le-int32 (+ (length (data perm)) 36) stream)
    (write-be-int32 (four-chars-to-int "WAVE") stream)

    ;; It's tragic that WAV only has sixteen bits for the sample rate. Think of
    ;; the minute fractions of accuracy we're missing!

    ;; Subchunk 1 (sound metadata)
    (write-be-int32 (four-chars-to-int "fmt ") stream)
    (write-le-int32 16 stream)   ; subchunk 1 size
    (write-le-int16 1 stream)    ; audio format (PCM)
    (write-le-int16 (channel-count perm) stream)    ; number of channels
    (write-le-int32 rounded-sample-rate stream)  ; sample rate
    (write-le-int32 (* rounded-sample-rate (bytes-per-frame perm)) stream)  ; byte rate
    (write-le-int16 (bytes-per-frame perm) stream)  ; block align
    (write-le-int16 (sample-bits perm) stream)  ; bits per sample

    ;; Subchunk 2 (raw sound data)
    (write-be-int32 (four-chars-to-int "data") stream)
    (write-le-int32 (* (frame-count perm) (bytes-per-frame perm)) stream)

    (if (eql (sample-bits perm) 8)
        (loop for value across (data perm) do
             (write-byte value stream))
        (loop for i from 0 to (- (length (data perm)) 2) by 2
              for value = (int-from-le-vector (data perm) i 2) do
             (write-le-int16 value stream)))))

;;; Just for debugging; I don't care if it's ugly.
(defun print-permutation-data (perm)
  (format t "        Bits per sample: ~A~%        Channels: ~A~%        Signed 8-bit? ~A~%        Little-endian? ~A~%        Bytes per frame: ~A~%        Sample rate: ~A~%        Base frequency: ~A~%        Length: ~A~%~%" (sample-bits perm) (channel-count perm) (signed-eight-bit-p perm) (little-endian-p perm) (bytes-per-frame perm) (sample-rate perm) (base-frequency perm) (length (data perm))))
