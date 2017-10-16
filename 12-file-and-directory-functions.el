;; 
;; 12. File and Directory Functions
;;

;; 1. {Rename, Copy, Delete} Files


(rename-file "~/tmp/hehe.txt" "~/tmp/hehe2.txt")

(copy-file "~/tmp/hehe2.txt" "~/tmp/hehe3.txt")

(delete-file "~/tmp/hehe3.txt")

(copy-directory "~/tmp" "~/tmp2")

(delete-directory "~/tmp2" t)


;; 2. File Name Manipulation

;; get the dir path part
(file-name-directory "~/tmp/hehe.txt")

;; get filename part
(file-name-nondirectory (buffer-file-name))

;; get filename's extension
(file-name-extension (buffer-file-name))

;; get filename without extension
(file-name-sans-extension (buffer-file-name))

;; get relative path
(file-relative-name "~/home/hehe/bb/cat.jpg" "~/home/")

;; get full path
(expand-file-name "hehe.txt")
