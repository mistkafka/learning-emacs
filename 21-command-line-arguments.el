;;
;; Elisp Script's Command Line Arguments
;; 

(mapc
 (lambda (arg)
   (message "argv: %s" arg))
 argv)
