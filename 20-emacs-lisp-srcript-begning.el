;;
;; 20 Emacs Lisp CMD Programming
;;
;; There are some difference between Emacs Command Programming
;; and Emacs Command Line Programming. Such as, in I/O, you shouldn't
;; use `find-file' and `write-file' function. They are for Emacs Editor, is
;; too heavy and too slow for command line. You should use `with-temp-buffer'
;; , `with-temp-file' or `write-region' and so on.
;;

;; 1. Read File(Read-Only)
;;
;; Use `insert-file-contents' to read file content to temp buffer.
;; 
(defun show-file-content (fPath &optional start end)
  "Show file content at path FPATH.
If START and END is gaven, just show the start to end"
  (with-temp-buffer
    (insert-file-contents fPath)
    (unless (and start end)
      (setq start (point-min)
	    end   (point-max)))
    (message "%s" (buffer-substring start end))))

(show-file-content (buffer-file-name))


;; 2. Modify Files

;; 2.1 Automitic Save File. `with-temp-file'
(defun append-hello-to-file (fPath)
  "Append 'hello' to the file."
  (with-temp-file fPath
    (insert-file-contents fPath)
    (goto-char (point-max))
    (insert "hello")))

(append-hello-to-file (buffer-file-name)) ; need re-read this file from disk to show the result

;; 2.2 Save file only when file has changed. Manual use `write-region'.
(defun my-process-file (fPath)
  "Process the file at path FPATH..."
  (let ((file-changed-p nil))
    (with-temp-buffer
      (insert-file-contents fPath)
      ;; process text
      ;; set file-changed-p to t or nil
      (when file-changed-p (write-region 1 (point-max) fPath)))))

;; 3. Get File Contents
(defun get-string-from-file (fPath)
  "Return filePath's file content."
  (with-temp-buffer
    (insert-file-contents fPath)
    (buffer-string)))

(get-string-from-file (buffer-file-name))

(defun read-lines (fPath)
  "Return a list of lines of a file at FPATH"
  (split-string (get-string-from-file fPath)))
(read-lines (buffer-file-name))

;; 4. File Name Manipulate file names.
(setq f (buffer-file-name))

(file-name-directory f)			; get file's dir path
(file-name-nondirectory f)		; get file name

(file-name-extension f)			; get suffix
(file-name-sans-extension f)		; remove suffix

(file-relative-name f)			; get relative path
(expand-file-name f)			; get full path

;; 5. File and Directory

(file-exists-p FILENAME)
(rename-file FILE NEWNAME &optional OK-IF-ALREADY-EXISTS)
(copy-file FILE NEWNAME &optional OK-IF-ALREADY-EXISTS KEEP-TIME PRESERVE-UID-GID)
(delete-file FILE)
(set-file-modes FILE MODE)

;; get list of file names
(directory-files DIR &optional FULL MATCH NOSORT)

;; create a dir. Non existent paren dirs will be created
(make-directory DIR &optional PARENTS)

;; copy/delete whole dir
(delete-directory DIRECTORY &optional RECURSIVE)
(copy-directory DIR NEWNAME &optional KEEP-TIME PARENTS)

;; Example: make backup file.
(defun make-backup ()
  "Make a backup copy of current buffer's file.
Create a backup of current buffer's file.
The new file name is the old file name with trailing “~”, in the same dir.
If such a file already exists, append more “~”.
If the current buffer is not associated with a file, its a error."
  (interactive)
  (let* ((fName (buffer-file-name))
	 (backupName fName))
    (if (null fName)
	(message "current buffer is not a file.")
      (progn
	(while (file-exists-p backupName)
	  (setq backupName (format "%s~" backupName)))
	(copy-file fName backupName))
      (message "backup file: %s as file :%s" fName backupName))))

;; 6. Get Current Elisp Script's Filename in Runtime
(or load-file-name buffer-file-name)	; execute by evil-buffer, the load-file-name will be null

;; 7. Call Shell Command

;; sync execute 
(shell-command COMMAND &optional OUTPUT-BUFFER ERROR-BUFFER) ; associate in buffer
(shell-command-to-string COMMAND)			     ; return execute resulat as string

;; async execute (use process)
(start-process-shell-command NAME BUFFER COMMAND)            ; associate in buffer, start a new process
(start-process NAME BUFFER PROGRAM &rest PROGRAM-ARGS)	     ; more oriangle version of start-process-shell-command

;; TODO: async execute (use Emacs 26's async function)

