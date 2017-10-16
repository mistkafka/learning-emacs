;;
;; Get Buffer String
;;

;; 1. Get Current Word
(current-word)				; Depend on major mode's syntax table

(defun my-get-word ()
  "print the word under cursor.
Word here is any A to Z, a to z, and low line _"
  (interactive)
  (let (
        p1
        p2
        (case-fold-search t))
    (save-excursion
      (skip-chars-backward "_a-z0-9" )
      (setq p1 (point))
      (skip-chars-forward "_a-z0-9" )
      (setq p2 (point))
      (message "%s" (buffer-substring-no-properties p1 p2)))))

;; 2. Get Current Line
(buffer-substring-no-properties (line-beginning-position) (line-end-position))

;; Get Thing at Point

;; 'word 'symbol 'list 'sexp 'defun 'filename 'url 'email 'sentence 'whitespace
;; 'line 'number 'page

(setq str (thing-at-point 'filename))

;; 3. Example, Grab Between Matching Pairs
(defun my-select-inside-quotes ()
  "Select text between double straight quotes
on each side of cursor."
  (interactive)
  (let (p1 p2)
    (skip-chars-backward "^\"")
    (setq p1 (point))
    (skip-chars-forward "^\"")
    (setq p2 (point))

    (goto-char p1)
    (push-mark p2)
    (setq mark-active t)))

