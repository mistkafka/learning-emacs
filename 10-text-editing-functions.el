;;
;; 10 Text Editing Functions
;;
;;

;; 1. Insert Text
(insert "I love you!")

;; 2. Delete Text
(delete-char 9)
(delete-region 59 896)

;; 3. Grab Text from Buffer to String
(buffer-substring-no-properties 1 200)

;; get thing at point. The "thing" can be
;; word, symbol, line, filename, URL and others

;; return string that's the current word
(defun do-thing-at-point-word ()
  (interactive)
  (message "%s" (thing-at-point 'word)))

(defun do-thing-at-point-symbol ()
  (interactive)
  (message "%s" (thing-at-point 'symbol)))

(defun do-thing-at-point-line ()
  (interactive)
  (message "%s" (thing-at-point 'line)))

;; get the start and end positions of word
(defun do-buounds-of-thing-at-point ()
  (interactive)
  (let ((rslt (bounds-of-thing-at-point)))
    (message "%S" rslt)))

(with-temp-buffer
  (insert "sdafasd asdkfasdfjasfsdf askdf")
  (goto-char (point-min))
  (buffer-string))
