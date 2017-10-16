
;;
;; Get Universal Argument
;;
;; More Info, see: http://ergoemacs.org/emacs/elisp_universal_argument.html

;; Problem
;; You have written a emacs command. You want the command's behavior to be different if user presses.

;; Solution			       
;; To make your command aware of universal argument, there are 3 simple ways:
;; - the global variable current-prefix-arg holds the value of universal argument.
;; - Add (interactive "P") to your function. It will pass the value of current-prefix-arg to your function's first argument.
;; - Add (interactive "p") to your function. It will pass converted numerical value of current-prefix-arg to your function's first argument.


;; Example:
(defun f (x)
  "print argument received"
  (interactive "P")
  (message "%s" x))

(defun g ()
  "print `current-prefix-arg'"
  (interactive)
  (message "%s" current-prefix-arg))
