;; 这篇我没怎么看懂，就自己去查阅一些资料来补一补list与vector的概念，主要见：
;; http://ergoemacs.org/emacs/elisp_list_vs_vector.html
;;
;; 总体而言，list类似于数据结构中的链表，而vector则对应数组

;; 1. 创建List（CONS CELL一章有说过）
(list 1 3 5 7)				; ==> (1 3 5 7)
'(1 . (3 . (5 . (7 . nil))))		; ==> (1 3 5 7), CONS CELL表示法
(1 2 5 7)				; ==> 非法的表示方式, s表达式会自求值，所以这里会被当做函数调用
'(1 2 5 7)				; ==> (1 2 5 7)

;; 1.1 number range (number-sequence start &optional end step)
(number-sequence 1 3)			; ==> (1, 2, 3)
(number-sequence 1 8 2)			; ==> (1 3 5 7)
(number-sequence 5)			; ==> (5)

(number-sequence 1.1 3)			; ==> (1.1 2.1)

;; 2. 访问list
(setq alist '(1 2 3))

(car alist)				; ==> 1, LIST本质是CONS CELL

(nth 0 alist)				; ==> 1, nth函数
(nth 2 alist)				; ==> 3
(elt alist 2)				; ==> 2, elt更通用，可以用在list与vector里

(last alist)				; ==> (3)
(last alist 2)				; ==> (2 3)
(car (last alist))			; ==> 3, 通过last与car组合获取最后一个元素

;; 3. 获取list子序列
(cdr alist)				; ==> (2 3), cdr操作
(nthcdr 2 alist)			; ==> (3),   nth-cdr操作
(butlast alist 1)			; ==> (1, 2), 从后往前取子序列

;; 4. prepend与append
(cons 'a alist)				; ==> (a 1 2 3)
(append alist '(9))			; ==> (1 2 3 9), 特别注意:: append的两个参数都要是list，其实append相当于其它语言里的concat

;; 5. 修改list内容
(push 0 alist)				; ==> (0 1 2 3), 把0前插到alist中（修改了alist）
alist					; ==> (0 1 2 3)
(pop alist)				; ==> 0
alist					; ==> (1 2 3)

(setq alist '(1 2 3 4 5 7))
(nbutlast alist 2)			; ==> (1 2 3 4), 从后面删除n个元素
alist					; ==> (1 2 3 4)


;; 6. 创建Vector
(setq x 7)
[1 x 7]					; ==> [1 x 7], 用[]表示的元素不会自求值
(vector 1 x 7)				; ==> [1 7 7], vector下的元素会自求值(evaluated)
(make-vector 9 x)			; ==> [7 7 7 7 7 ... 7]

;; 7. 访问Vector
(setq arr [1 2 3 4 5 6 7 8 9])
(aref arr 0)				; ==> 1
(aref arr (1- (length arr)))		; ==> 9
(elt arr 0)				; ==> 1

;; 8. change vector
(aset arr 0 "b")			; ==> "b"
arr					; ==> ["b" 2 3 4 ... 9]

;; 9. join vector and cover list to vector
(vconcat [3 4] ["a" "b"])		; ==> [3 4 "a" "b"]
(vconcat [3 4] '("a" "b"))		; ==> [3 4 "a" "b"]
(vconcat '("a" "b") '("c" "d"))		; ==> ["a" "b" "c" "d"]
(vconcat [3 4] "ab")			; ==> [3 4 97 98], 字符串是元素全是字符的vector，而字符又是数字

;; 10. cover vector to list
(append [1 2 3] nil)			; ==> (1 2 3)
(append [1 2 3] [4 5])			; ==> (1 2 3 . [4 5]), 特别注意了！
(append [1 2 3] [4 5] '(6))		; ==> (1 2 3 4 5 6 6), 奇淫巧技

;; 11. Alist: Association List
;; Association List 是指元素是 CONS CELL对的List
;; 可以有重复的key
(setq alist '(("mary" . 24)
	      ("joah" . 24)
	      ("smith" . 33)))

;; Get Alist Element by key
;; 获取第一个找到的CONS CELL
(assoc "joah" alist)			; ==> ("joah" . 24)

;; Get Alist Element by value
(rassoc 24 alist)			; ==> ("mary" . 24)
(rassq 24 alist)			; ==> ("mary" . 24)

;; 12. plist: Property List
;; Property List的结构为： '(key1 value1 key2 value2 ... keyN valueN), key是symbol
;; 主要应用：1. Symbole's的list 2. Text Property。数据量不大的list
(setq plist '(x 1 y 2 z 3))
(plist-get plist 'x)			; ==> 1
(plist-get plist 'z)			; ==> 3
(plist-get plist y)			; ==> nil,

;; 判断key是否存在
(plist-member plist y)			; ==> nil
(plist-member plist 'y)			; ==> (y 2 z 3)

;; 修改plist
(plist-put plist 'y 9)			; ==> (x 1 y 9 z 3)

;; 13. mapcar, 相当于js中的map
(mapcar '1+ [1 2 3])			; ==> (2 3 4)

(setq alist '((1 2) (3 4) (5 6)))
(mapcar 'car alist)			; ==> (1 3 5)

(mapcar
 (lambda (x) (* x x))
 '(1 2 3 4))				; ==> (1 4 9 16)

;; 14. mapc，相当于js中的forEach，没有返回值
(mapc
 (lambda (x) (insert (number-to-string (* x x))))
 '(1 2 3 4))

;; 15. dolist 与mapcar,mapc类似，只不过dolist只能接收list序列，
;; 并且使用使用表达式而不是函数来处理序列的元素
;; 
;; (dolist (VAR LIST RESULT) BODY) --> RESULT
;; (dolist (VAR LIST) BODY) --> nil
(let ((xlist (number-sequence 97 122)))
  (dolist (n xlist) (insert n)))

;; 16. dotimes
;;
;; (dotimes (VAR COUNT) BODY)
;; (dotimes (VAR COUNT RESULT) BODY)
;;
;; 与dolist类似，不过接收的是一个COUNT整数，VAR从0开始计数
(dotimes (i 4)
  (insert (number-to-string i)))


;; 17. filter a list
;;
;; emacs lisp竟然没有天然的filter函数，需要引入cl-lib库
(cl-remove-if-not 'numberp '(1 "a" 2))	; ==> (1 2)
(cl-remove-if 'numberp '(1 "a" 2))	; ==> ("a")













