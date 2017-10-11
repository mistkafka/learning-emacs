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
