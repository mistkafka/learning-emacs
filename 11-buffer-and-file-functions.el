;;
;; 11 Buffer and File Functions
;;

;; 1. Buffers

;; Return the name of current buffer
(buffer-name)

;; return the full path of the file
(buffer-file-name)

;; switch to a given buffer
(set-buffer "*scratch*")

;; close a given buffer
(kill-buffer "*Help*")

;; use a temp buffer to manipulate string
(with-temp-buffer
  (insert "askasfsd")
  (buffer-string))


;; 2. Files

;; open a file (return a buffer)
(find-file "~/tmp/hehe.txt")

;; save current buffer
(save-buffer)

;; like "Save As". Save current buffer, close it, and open the new saved
(write-file (buffer-file-name))

;; append text between positions to file
(append-to-file (point-min) (point-max) "~/tmp/hehe.txt")
