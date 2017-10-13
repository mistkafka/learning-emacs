;; Hash Table
;;
;; Hash表

;; 1. Create Hash Table
(setq myHash (make-hash-table :test 'equal))
;; the :test 'equal is to specify what function to use to test key existence.

;; 2. Add Entry
(puthash "joe" "19" myHash)

;; 3. Remove Entry
(remhash "joe" myHash)

;; 4. Get Key's Value
(puthash "joe" "19" myHash)
(gethash "joe" myHash)			; ==> "19"

;; 5. Count Length
(hash-table-count myHash)		; ==> 1

;; 6. Remove All Entries
(clrhash myHash)

;; 7. Check If a Key Exists
;; emacs lisp使用gethash来检验key是否存在，这貌似有点不妥。
(let ((myHash (make-hash-table :test 'equal)))
  (puthash 'aa 9 myHash)
  (puthash 'bb nil myHash)
  (gethash 'cc myHash)			;nil
  (gethash 'bb myHash)			;nil
  
  (gethash 'bb myHash 19)		; ==> nil，虽然gethash给了缺省值19，但是依然返回nil，说明bb这个key是存在的
  (gethash 'cc myHash "no exists")	; ==> "no exists"
  (hash-table-count myHash))		; ==> 2，说明其实有两个key/value，一个是aa另一个是bb，但是这里用gethash检验bb时，却一样返回nil


;; 8. Apply a Function to All Entries
;;
;; (maphash FUNCTION HASH-TABLE)
;; 类似于与mapc与mapcar
;; 我们来实现一个一个hash-table-keys函数
(defun hash-table-keys (hash-table)
  (let ((keys '()))
    (maphash
     (lambda (k v) (push k keys))
     hash-table)
    keys))

(let ((myHash (make-hash-table :test 'equal)))
  (puthash "jolie" "176..." myHash)
  (puthash "kafka" "180..." myHash)
  (puthash "fangrong" "133..." myHash)
  (hash-table-keys myHash))		; ==> ("fangrong" "kafka" "jolie")


;; 9. Get All Keys, Get All Values
;;
;; use subr-x libiary's function:
;;   `hash-table-keys'
;;   `hash-table-values'
(require 'subr-x)
(let ((myHash (make-hash-table :test 'equal)))
  (puthash "jolie" "176..." myHash)
  (puthash "kafka" "180..." myHash)
  (puthash "fangrong" "133..." myHash)
  (hash-table-keys myHash))		; ==> ("fangrong" "kafka" "jolie")

(let ((myHash (make-hash-table :test 'equal)))
  (puthash "jolie" "176..." myHash)
  (puthash "kafka" "180..." myHash)
  (puthash "fangrong" "133..." myHash)
  (hash-table-values myHash))		; ==> ("176..." "180..." "133...")
