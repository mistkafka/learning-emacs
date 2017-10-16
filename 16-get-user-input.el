;;
;; 16 Get User Input
;;
;;

;; 1. Read X
(defun ff ()
  "Prompt user to enter a file name, with completion and history support."
  (interactive)
  (message "String is %s" (read-file-name "Enter file name:")))

;; More Action:
;;
;; `read-string'
;; `read-file-name'
;; `read-directory-name'
;; `read-regexp'

;; 2. Select from a List, ido-completing-read.
(require 'ido)
(setq choices '("cat" "dog" "doc" "dragon" "tiger"))
(defun my-pick-one ()
  "Prompt user to pick a choice from a list."
  (interactive)
  (message "%s" (ido-completing-read "Open bookmark:" choices)))

;; 3. Query User for Yes/No, y-or-n-p
(if (y-or-n-p "Do it?")
    (progn
      (message "yes"))
  (progn
    (message "no")))

