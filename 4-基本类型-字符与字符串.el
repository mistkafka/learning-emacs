;; 2. 字符
;; emacs lisp的字符本质上是数字

;; 2.1 字符的表示，以'?'开头
?A                                      ; ==>65


;; 2.3 对于各类标点符号，最好加上转义字符\。如果字符没有转义含义，加了转移符也没事
?\\                                     ; 表示\
?\+                                     ; ==> 43
?+                                      ; ==> 43


;; 2.4 控制符有多种表达方式，'C-i'也是合法的！
?\C-i                                   ; ==> 9
?\^I                                    ; ==> 9


;; 2.5 Meta修饰符(一般是Alt键)
;; Meta的作用是把一个字符的第27位从0变成1，所以才称之为“修饰符”
?a                                      ; ==> 97
(logior (lsh 1 27) ?a)                  ; ==> 134217825
?\M-a                                   ; ==> 134217825


;; 2.6 Predicate(断言)
;; 字符串stringp，没有字符断言，因为字符就是整数
(stringp "hello, world")                ; ==> t
(stringp ?A)                            ; ==> nil

(string-or-null-p "hello")              ; ==> t
(string-or-null-p nil)                  ; ==> t
(string-or-null-p 2)                    ; ==> nil

(char-or-string-p "hello")              ; ==> t
(char-or-string-p ?A)                   ; ==> t
(char-or-string-p 3)                    ; ==> t, 因为字符就是整数
(char-or-string-p nil)                  ; ==> nil


;; 2.7 构造函数
(make-string 5 ?x)                      ; ==> "xxxxx"
(string ?a ?b ?c)                       ; ==> "abc"

(substring "0123456" 0 3)               ; ==> "012",  sbustring的[起点, 终点) 左闭右开
(substring "0123456" -3 -1)             ; ==> "45"
(substring "0123456" -3)                ; ==> "456"

(concat "abc" "123" "一二三")            ; ==> "abc123一二三"


;; 2.8 字符串的比较

;; char-equal比较字符，当case-fold-search为t时，这个比较忽略大小写！！！
(let ((case-fold-search nil))
  (char-equal ?a ?A))                   ; ==> nil

(let ((case-fold-search t))
  (char-equal ?a ?A))                   ; ==> t

;; 字符串比较用string-equal或者string=(alias)、string-less或者string<(alias)，没有string-more或者string>
(string= "abc" "abC")                   ; ==> nil
(string= "abc" "abc")                   ; ==> t

(string< "a" "b")                       ; ==> t
(string< "b" "a")                       ; ==> nil
(string< "a" "")                        ; ==> t, 空字符串总是最小
(string< "" "")                         ; ==> nil, 可以用此表达式来检查字符串是否为空

(defun string-empty-p (str)
  "return t if STR is empty"
  (not (string< "" str)))
(string-empty-p "")                     ; ==> t
(string-empty-p "a")                    ; ==> nil


;; 2.9 cover函数
(char-to-string ?A)                     ; ==> "A"
(string-to-char "abc")                  ; ==> 97，这里只会转换字符串的首字符

(number-to-string 1234)                 ; ==> "1234"
(number-to-string #b1011)               ; ==> "11", 对于其它进制的数字会以十进制转成字符串，其它进制使用format函数
(format "%#b" 1)                        ; ==> 报错，不知道为什么不支持二进制
(format "%#o" 323)                      ; ==> 0503
(format "%#x" 323)                      ; ==> 0x143

(string-to-number "123")                ; ==> 123
(string-to-number "11" 2)               ; ==> 3，按二进制去处理字符串“11”

(downcase "ABC")                        ; ==> "abc"
(upcase "abc")                          ; ==> "ABC"
(capitalize "word")                     ; ==> "Word"
(upcase-initials "i'm mistkafka!")      ; ==> "I'm mistkafka!"


;; 2.10 find & replace
(string-match "34" "012345678")         ; ==> 3, 返回匹配到的起始位置
(string-match "0" "01230123" 2)         ; ==> 4, 2表示查找的起始位置

;; 2.10.1 正则表达式的group
;; 用match-data获取最后一个正则表达式的group信息，
;; 每两个数字表示一个group的起始与终止位置
(progn
  (string-match "hello" "say hello")
  (match-data))                          ; ==> (4 9), 4 9表示group 0的起始位置 终止位置
(progn
  (string-match "he\\(ll\\)o" "say hello")
  (match-data))                          ; ==> (4 9 6 8), 6 8表示group 0的起始位位置 终止位置

;; 用match-beginning与match-end来获取最后一个正则表达式的group的起始于终止位置
(progn
  (string-match ".+\\(rld\\)" "hello, world!")
  (match-data))                         ;  ==> (0 12 9 12), 有两个group

(progn
  (string-match ".+\\(rld\\)" "hello, world!")
  (match-beginning 0))                  ; ==> 0, group 0的起始位置是0

(progn
  (string-match ".+\\(rld\\)" "hello, world!")
  (match-beginning 1))                  ; ==> 9, group 1的起始位置是9

(progn
  (string-match ".+\\(rld\\)" "hello, world!")
  (match-beginning 2))                  ; ==> nil, 没有group 2


;; 一个例子：找出字符串中的所有'hello'的'll'的起始位置，
;; 注意：如果字符串中有这样的子串 'abcllabc'，它的'll'并非我们要寻找的字符串，
;; 只有构成了'hello'的ll才是我们要找的。
(let ((start 0))
  (while (string-match "he\\(ll\\)o" "hello, hi, world, tall, hello, all, haha, not ll" start)
    (princ (format "found 'hello's 'll' at %d\n" (match-beginning 1)))
    (setq start (match-end 0))))        ; 2跟26两个位置时符合要求的‘ll'

;; 替换
(let ((str "hi, world"))
  (string-match "hi" str)
  (replace-match "hello" nil nil str 0)) ; ==> "hello, world"，很奇怪为什么replace-match只给一个参数不work？replace-match的后续参数明明都是optional的
