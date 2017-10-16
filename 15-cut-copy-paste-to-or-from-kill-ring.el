;;
;; 15. Cut Copy Paste to/from Kill-ring

;; 1. Cut Text to kill-ring
(kill-region 500 6000)

;; 2. Copy Text to kill-ring
(kill-ring-save (point-min) (point-max))
(kill-new "a string ")

;; ex:
(defun my-copy-all ()
    "Put the whole buffer content into the `kill-ring'.
(respects `narrow-to-region')"
    (interactive)
    (kill-new (buffer-string))
    (message "Buffer content copied."))

;; 3. Paste from kill-ring
(yank)
