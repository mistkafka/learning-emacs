;;
;; 18. replace found text
;;

;; emacs lisp有一个match概念，match是一个全局的东西，它提供给后面的math消费者使用
;; 正则表达式以及一些search函数会产生match
;;
(let ((case-fold-search t))
  (goto-char (point-min))
  (while (search-forward "case-fold-search" nil t)
    (replace-match "CASE-FOLD-SEARCH1")))
