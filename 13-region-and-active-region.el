;;
;; 13. Region, Active Region
;;
;; Region: 指的是最后一个marked position到光标所在位置
;; Active Region: 当变量mark-active为`t'时，region会被高亮
;;
;; 一般情况下，`set-mark-command'这个命令会把mark-active设置为t，直到执行某个命令后再设置为nil
;;
;; 其它应用下的“文本选择”基本等于active region


;; 1. check active region, use-region-p
(defun my-is-region-acitve ()
  "print whether region is active."
  (interactive)
  (if (use-region-p)
      (message "region active")
    (message "region not active")))


;; 2. Create Active Region
(defun my-select-line ()
  "Select current line."
  (interactive)
  (let (p1 p2)
    (setq p1 (line-beginning-position))
    (setq p2 (line-end-position))
    (goto-char p1)
    (push-mark p2)
    (setq mark-active t)))

;; 3. Pass Region Begin/End Positions to Function Arguments
;; (interactive "r")
(defun ff (p1 p2)
  "sample code, print region begin/end positions"
  (interactive "r")
  (message "Region starts: %d, end at: %d" p1 p2))

;; 4. Example: Get Active Region or Current {Word, Line}
;;
;;  Better command will acts on Select Text First

(defun downcase-word-or-region ()
  "Downcase current word or region."
  (interactive)
  (let (pos1 pos2 bds)
    (if (use-region-p)
	(setq pos1 (region-beginning) pos2 (region-end))
      (progn
	(setq bds (bounds-of-thing-at-point 'symbol))
	(setq pos1 (car bds) pos2 (cdr bds))))
    (downcase-region pos1 pos2)))
