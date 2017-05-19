;; 1. 基本的函数声明

;; 1.1
;; defun关键字
;; 参数
;; 参数在文档中用大写
;; 返回值：函数体的最后一个表达式
(defun say-hello-to (somebody)
     "Say Hello to SOMEBODY"
     (message "Hello, %s" somebody))

;; 1.2
;; 函数调用
;; 用C-h f查看光标处的函数文档
(say-hello-to "Jolie")



;; 2. 变量

;; 2.1 变量的基本声明
;; 无需声明，直接赋值
;; 赋值方式：setq
(setq my-name "mistkafka")
(message my-name)                       ; ==> "mistkafka"

;; 2.2 变量的文档
;; 用defvar进行声明，可以给变量写文档
;; 用C-h v查看光标处的变量文档

;; 2.2.1 基本形式
;; 形式：(defvar variable-name value "document string")
(defvar custom-config-path "~/.spacemacs.d/custom.el"
  "指定自定义配置的路径，需要绝对路径")
(message custom-config-path)

;; 2.2.2 defvar的安全性
;; defvar 不会改变已经声明的变量的值，但文档依然会生效
(setq str "hello, defvar")
(defvar str "haha"
  "这是变量str，我希望它的默认值是\"haha\"")
(message str)                           ; ==> "hello, defvar"

;; 2.3 局部变量
;; setq、defvar、defun声明出来的都是全局变量、函数,
;; 用let或者let* 可以进行局部变量的绑定

;; 2.3.1 基本使用
;; 基本形式:
;;
;; (let (bindings)
;;   body)
;;
;; 其中bindings是一个列表，列表的项有两种形式：
;;   1. (var value)
;;   2. 或者直接一个 var，其value默认是nil
(defun calc-circle-area (radix)
  (let ((PI 3.1415926)
        area)
    (setq area (* PI radix radix))
    (message "半径为%.2f的圆的面积是%.2f" radix area)))

(calc-circle-area 3)

;; 2.3.2 let*
;; let*与let的区别是，在bindings中，可以使用前面的声明项的变量
(defun calc-circle-area (radix)
  (let* ((PI 3.1415926)
        (area (* PI radix radix)))
    (message "半径为%.2f的圆的面积是%.2f" radix area)))
(calc-circle-area 3)



;; 3. 匿名函数lambda
;; 依然不知其特殊用处，只知道其匿名
;; 基本形式：
;;
;; (lambda (arguments-list)
;;   "Document String"
;;   Body)
;;
(lambda (name)
  "Say hello to NAME"
  (message "Hello, %s!" name))

;; 3.1 把lambda当做变量
;; 上面那个函数一声明，就丢失了对它的引用
;; 谁都调用不到，失去了任何意义。我们可以
;; 用setq来保存lambda
(setq say-hello (lambda (name)
                  "Say hello to NAME"
                  (message "Hello, %s!" name)))

;; 3.2 用funcall来调用lambda
(funcall say-hello "Jolie")             ; "Hello, Jolie!"
(funcall calc-circle-area 3)            ; calc-circle-area 是volid，无法调用！不知道为什么要有这种限制



;; 4. 控制结构

;; 4.1 顺序结构
;; defun、let等语句下，表达式是依次执行的。
;; 但默认情况下并非如此（我还不知道什么情况下不是），所以需要progn这个表达式。
;; 我的理解是：program * n，这个表达式下的表达式是顺序执行的。
;; 形式：
;;
;; (progn A B C D ...)
;;

;; 4.2 分支结构（条件判断）
;; lisp有两大类分支结构：单条件的if（包括when、unless）和多条件的cond（对应其他语言的switch）

;; 4.2.1 if语句
;; 形式:
;;
;; (if condition then else)
;;
(defun my-max (a b)
  (if (>= a b) a b))

(my-max 3 4)                            ; ==> 4

;; 4.2.2 when与unless语句
;; when就是条件为真时执行
;; unless就是条件为假时执行
;; 使用when跟unless的好处是，它们相比if更加语义化

;; 4.2.3 cond语句
;; 形式:
;; (cond (case1 do-when-case1)
;;       (case2 do-when-case2)
;;       (case3 do-when-case3)
;;       (t do-when-none-meet))
(defun fib (n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (t (+ (fib (- n 1))
              (fib (- n 2))))))
(fib 10)                                ; ==> 55



;; 5. 循环
;; 形式：
;; (while cond
;;   body)
(defun my-sum (n)
  (let ((sum 0))
    (while (> n 0)
      (setq sum (+ sum n))
      (setq n (- n 1)))
    sum))
(my-sum 5)                              ; ==> 15


;; 6. 逻辑运算符
;; 使用and、or、not
;; 可以使用and来代替when，用or来代替unless，此时不关心返回值，
;; 而是为了语句运行时产生的副作用

;; 用or给say-hello设置默认值
(defun say-hello (&optional name)
  (or name (setq name "Emacser"))
  (message "Hello, %s!" name))
(say-hello)                             ; ==> "Hello, Emacser!"



;; 后记：学到这里，瞬间明朗了很多！
