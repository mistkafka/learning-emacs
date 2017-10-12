;; Symbol 符号类型
;; 
;; Emacs Lisp的Symbol类型类似于其它语言的变量名。不同的是，在Lisp中，"变量名"本身也是一种类型。

;; 1. 合法的Symbol是怎样的？几乎所有的字符都是合法的Symbol
foo					; ==> 名为 `foo' 的符号
Foo					; ==> 名为 `Foo' 的符号
1+					; ==> 名为 `1+' 的符号
\1					; ==> 名为 `1' 的符号，名与面量冲突时，需要用'\'来加与区分，但是不表示转义

;; 2. 自求值（evaluated）与Symbol
;;
;; Lisp有自求值的概念，Symbol也是一种数据类型，所以它求值的时候就是表示它所“指向”的值
;; 所以上面那些表示方法都无法表示Symbol本身，而是表示Symbol所指向的值。为了获取Symbol
;; 本身，我们需要“中止”自求值的过程，使用Special Form "quote"来中止。
(quote foo)				; foo
(quote 1+)				; 1+
(symbolp (quote foo))			; ==> t
(symbolp foo)				; ==> void错误

'foo					; foo, 'x 相当于 (quote x)

;; 3. 符号类型数据结构（Symbole's Cell）
;;
;; Symbol的存储结构为四个“格子”(Cell)

;; "Print Name" 存储Symbol的“打印名称”，与Symbol一样的字符串。不可变更！
(symbol-name 'sin)			; ==> "sin"

;; "value" 存储Symbol所代表的“值”
(symbol-value 'sin)			; ==> void错误
(set 'sin 1)				; 给`sin'赋值1
(symbol-value 'sin)			; ==> 1

;; "function" 存储Symbol的函数定义、宏或者可以被执行的对象
(defun hello-world ()
  (message "hello, world"))
(symbol-function 'hello-world)		; ==> (lambda nil (message "hello, world"))

;; "property list" 存储一些Symbol的meta信息，如font face spec等
(symbol-plist 'sin)			; ==> (side-effect-free t)

;; 4. 什么时候使用Symbol？
;;
;; Symbol一般用于高级编程（这里不讨论）。日常的函数调用中，也会涉及。一些函数会希望你传入的值是Symbol，而有些函数则
;; 会自动把参数给转成Symbol。具体使用时，需要查阅函数的定义

;; set就需要手动传入Symbol
(set x 1)				; ==> 报错，第二个参数必须是Symbol
(set 'x 1)				; ==> 1

;; setq则自动把第一个参数Symbol化（这很符合我们的日常使用习惯）
;; 现在终于知道为什么set要叫做setq了。
(setq x 1)

;; 5. 修改Symbol的四个“Cell”

;; 5.1 Name Cell不能修改

;; 5.2 Value Cell，使用set/setq
(setq y "yes")
(set 'y "yes")

;; 使用boundp来检查value cell是否为void（是empty而不是nil）
(boundp 'xsdf)				; ==> nil
(boundp 'y)				; ==> t, 上面执行过(setq y "yes")

;; 5.3 Function Cell，使用defun（宏的定义应该也可以）
(defun z () 4)
(fboundp 'z)				; ==> t

;; 5.4 Property Cell, 见 http://ergoemacs.org/emacs/elisp_symbol_plist.html

;; 6. String <--> Symbol的相互转换
(symbol-name 'defun)			; ==> "defun", Symbol --> String

(intern "someting")			; ==> something，如果symbol不存在过，则创建并存入 "obarray"。obarray相当于Symbol的符号表，这里不展开讨论
(intern-soft "asdfasdf")		; ==> nil，如果symbole不存在过，则返回nil
