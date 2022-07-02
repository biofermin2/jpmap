(in-package :jpmap)

(defun parse-line (line &optional (sep #\,))
  "https://qiita.com/inutomo0123/items/0a7665d02501053b9a06
https://www.youtube.com/watch?v=pglVFT-sEcEを参考。
行毎にS式に変換"
  (read-from-string
   (format nil "(~a)"
	   (substitute #\space sep
		       (string-right-trim '(#\return) line))))) ; => PARSE-LINE

(defun divide (lst n)
  "https://shammerism.hatenadiary.com/entry/20130222/p1を参考"
  (when lst
    (values (subseq lst 0 n)
	    (subseq lst n))))		; => DIVIDE

(defun put-ht (k v ht)
  (setf (gethash k ht) v))		; => PUT-HT

(defun puts-ht (lst ht)
  (loop :for i :in lst
	:count i :into cnt
     :do (put-ht cnt i ht)))		; => PUTS-HT

(defun print-ht (ht)
  "ハッシュ表の出力関数"
  (maphash #'(lambda (k v) (print (list k v))) ht)) ; => PRINT-HT

(defun cut (file &key to key val (title t) (use nil) &aux (k-len (length key)))
  "ファイルから必要な列のデータを抽出し、
ハッシュテーブルにセット。その際タイトル行があれば省く"
  (with-open-file (in file)
    (loop :for line = (read-line in nil nil)
       :while line
       :collect (multiple-value-bind (k v)
		    (divide (loop :for i :in (append key val)
			       :collect (nth i (parse-line line))) k-len)
		  (cond ((and (not title) use)
			 (error "no header.you should delete use keyword from this function."))
			((or (and title use) (and (not title) (not use)))
			 (put-ht k v to))
			((and title (not use))
			 (setq header (list k v)
			       title nil))))))) ; => CUT

(defun ht2tree (ht)
  "ハッシュ表内のデータをツリー構造に変換"
  (loop :for (k1 k2 k3) :being the hash-keys :of ht :using (hash-value v)
     :collect (cons k2 (cons k3 v)) :into acc
     :finally (format t "~a" (cons k1 (group-by acc))))) ; => HT2TREE

