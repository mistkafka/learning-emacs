;;
;; Cursor Position Functions
;;
;;

;;;; 1. Get Cursor Position

;; Get current cursor position
(point)					; ==> 70

;; returns the position of the beginning/end of region
(region-beginning)
(region-end)

;; return buffer's max and min point
(point-max)
(point-min)


;;;; 2. Move Cursor and Search

;; move cursor to position 1
(goto-char 1)

;; move cursor by 9 chars
(forward-char 9)
(backward-char 9)

;; move cursor to the first char that's not a newline or tab
(skip-chars-forward "\n\t")
(skip-chars-backward "\n\t")

;; move cursor to the location of string "cat"
(search-forward "cat")
(search-backward "cat")

;; move to the beginning/end of line
(beginning-of-line)
(end-of-line)



;;;; 3. Preserve Cursor Position
(save-excursion
  (narrow-to-region pos1 pos2)
  )
