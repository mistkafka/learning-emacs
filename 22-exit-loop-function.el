;;
;; Exit Loop/Function
;;
;; Elisp Use catch/throw to exit Loop/Function.
;;

;; 1. Basic Example
(defun test-exit-func ()
  "Example. using catch/throw to exit function"
  (interactive)
  (catch 'user-exit
    (if (y-or-n-p "exit?")
	(progn
	  (message "existing")
	  (throw 'user-exit 3))		; if yes, exit right away, return 3 to catch
      (progn
	(message "went on")
	4))))

(defun test-exit-func ()
  "example"
  (interactive)
  (if (y-or-n-p "invoke user-error to exit?")
      (user-error "Error, baseuse: %s" "you said so!")
    (progn
      (message "went go"))))

;; 2. Exit a map
(progn
  (setq myList [0 1 2 3 4 5])
  (catch 'found-the-number
    (mapc
     (lambda (x)
       (message "%s" x)
       (when (equal x 3)
	 (throw 'found-the-number x)))
     myList)
    nil))				; ==> 3

;; 3. Exit a while loop by flag variable
(let ((found-number-p nil)
      (i 0))
  (while (not found-number-p)
    (setq i (+ i 1))
    (when (= i 3)
      (setq found-number-p t)))
  (message "found the number %d" i ))
