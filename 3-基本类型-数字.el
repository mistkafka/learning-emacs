;; 0. Primitive Types
;; Emacs Lisp的基本数据类型称为“Primitive Types”，原始类型。包括：
;; 整数
;; 浮点数
;; cons
;; 符号（Symbol）
;; 字符串
;; 向量
;; 散列表（hash-table）
;; 内建函数（defun、defvar之类的）
;; byte-code function
;; 其它特殊类型。如，缓冲区（buffer）

;; 1. 数字
;; Emacs Lisp 有两种数字类型：整数、浮点数

;; 1.1 数的进制
;; 形式
;;
;; '#' + 具体进制（如，8表示八进制，17表示17进制） + 'r' + 具体数字
;;
(message "Number is:%d" #2r11)          ; ==> Number is:3
(message "Number is:%d" #8r11)          ; ==> Number is:9
(message "Number is:%d" #16ra0)         ; ==> Number is:160

;; 进制最高是36，因为英文字母最多36个
;; 二进制、八进制、16进制还可以另外用#b #o #x来表示
;; 十进制是默认进制，如果没有"#进制r数字“的形式，就是数字

;; 1.2 数的断言
;; emacs lisp有很多断言(predicate)函数，以字母p结尾，如果函数是单个单词，就直接以'p'
;; 结尾；如果是多个单词，则以'-p'结尾。
;; 下面是3个关于数的断言：是否是整数、浮点数、数字

(integerp 1)                            ; ==> t
(floatp 1)                              ; ==> nil
(floatp 0.0)                            ; ==> t
(numberp 1)                             ; ==> t



;; 1.3 数的比较
;;
;; （为什么浮点数的比较是不靠谱的？）
;;
;; 浮点数的相等测试是不可靠的，是要在一定误差内进行比较
(setq foo (- (+ 1.0 1.0e-3) 1.0))       ; ==> 0.0009999999999998899
(setq bar 1.0e-3)                       ; ==> 0.001
(= foo bar)                             ; ==> nil

(defvar fuzz-factor 1.0e-6
  "两个数相等的最大误差")
(defun approx-equal (x y)
  "判断X跟Y是否近视相等"
  (or (and (= x 0) (= y 0))
      (< (abs (- x y))
         fuzz-factor)))

(approx-equal foo bar)                  ; ==> t

;; 判等运算还有一个eql函数，它与'='函数的区别是：不仅数值要相等，而且类型也要相等
(= 1 1.0)                               ; ==> t
(eql 1 1.0)                             ; ==> nil



;; 1.4 数的转换
;; 整数转浮点数只需要float函数即可，
;; 而浮点数转整数因为有小数取舍的问题，所有相关的函数也比较多。

(float 1)                               ; ==> 1.0

;; truncate 像0靠拢取整
(truncate 1.9)                          ; ==> 1
(truncate -3.9)                         ; ==> -3
;; 向上/向下取整
(ceiling 1.1)                           ; ==> 2
(floor 1.99999)                         ; ==> 1
;; 四舍五入
(round 1.4999)                          ; ==> 1
(round 1.5)                             ; ==> 2

;; 数的范围
;; Emacs Lisp中，整数的范围是依机器的位数来定的
;; 可以通过 most-positive-fixnum与most-negative-fixnum来确定
;;
;; 而浮点数的范围，几乎可以认为是无限大的！所以当把一个超过整数范
;; 围的浮点数转成整数时，会报错！
(message "%d" most-positive-fixnum)     ; ==> 真的好大
(message "%d" most-negative-fixnum)     ; ==> 真的好小
(round 1e1000)                          ; 报错，range error



;; 1.5 数的运算
;; emacs lisp的数字运算需要注意一些点，比较零散：

;; 整数相除得整数
(/ 6 5)                                 ; ==> 1，而不是1.2
(/ 6 5.0)                               ; ==> 1.2

;; 没有++跟--，只有1+跟1-。为什么没有++跟--呢？
(setq foo 10)                           ; ==> 10
(setq foo (1+ foo))                     ; ==> 11
(setq foo (1- foo))                     ; ==> 10
;; 我自己写一个。 我很好奇这么简单，emacs lisp为什么不内建？
(defun -- (n)
  "对n进行自减1"
  (setq n (1- n)))
(defun ++ (n)
  "对n进行自增1"
  (setq n (1+ n)))
(-- 7)                                  ; ==> 7
(++ 7)                                  ; ==> 8

;; mod运算，emacs lisp有两种%跟mod, 区别见下面的例子
(% 6 5)                                 ; ==> 1
(% 6.0 5)                               ; 报错：Wrong type argument

(mod 6 5)                               ; ==> 1
(mod 6.0 5)                             ; ==> 1.0
(mod 6.3 5)                             ; ==> 1.299999999

;; 1.6 更多
;; 随机数 random
;; 三角函数
;; 指数、对数运算、开方
;; 位运算
;;
;; 这些用到，再来系统的过吧。
