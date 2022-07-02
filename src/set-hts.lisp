(in-package :jpmap)

(defun adr3->2 (adr3-name)
  (caar (seek adr3-name (format nil "~a" adr-tree) :skin 1 :str nil))) ; => ADR3->2

(defun show-adr2-name (n)
  "番号を入れたら行政区名を表示する。"
  (nth (1- n) adr2-name))  ; => SHOW-ADR2-NAME



;; ok [2022-05-24]
(defun comb (lst)
  (flet ((pair (l)
	   (let ((key (car l))
		 (rest (cdr l)))
	     (mapcar #'(lambda (x) (list key x)) rest))))
    (mapcon #'pair lst)))		; => COMB

(defun make-adr1-ht ()
  (loop :for i :in prefs
     :count i :into cnt
     :do (put-ht cnt (make-area :name i :gm "") adr1-ht))) ; => MAKE-ADR1-HT

(defun adr1-name (n)
  (area-name (gethash n adr1-ht)))	; => ADR1-NAME


(defun min-town (n adr2-ht adr3-ht)
  "同一行政区の中で一番distの合計が小さかった町村名"
  (check-type n integer)
  (let (adr2-v)
    (loop :for v :being the hash-values :of adr3-ht :using (hash-key k)
       :when (eq (car k) n)
       :minimize (adr3-dist v) :into mini
       :finally (progn
		  (maphash
		   #'(lambda (k v)
		       (when (eq (adr3-dist v) mini)
			 (setf adr2-v (make-adr2 :name (show-adr2-name n) :gm k :lat (adr3-lat v) :lon (adr3-lon v)))))
		   adr3-ht)
		  (put-ht n adr2-v adr2-ht))))) ; => MIN-TOWN


(defun make-adr3-ht (adr2-ht adr3-ht)
  "keyを作成し、htにkeyと構造体のvalueを追加
町村のハッシュテーブルの作成"
     ;; (make-adr3-ht adr2-ht adr3-ht)	; => NIL
     ;; (hash-table-count adr3-ht)		; => 3140
  (loop :for i :in adr3/adr2 ;行政区単位の町村グループ抽出  
     :count i :into id2 ;行政番号をセット
     :do (loop :for (name lat lon) :in i ;グループから町村データを１つずつ抽出
	    :count name :into id3 ;町村番号を作成
	    :do (put-ht (cons id2 id3) (make-adr3 :name name :lat (read-from-string lat) :lon (read-from-string lon) :dist 0) adr3-ht)
	    :finally (progn
		      (set-dist id2 adr3-ht)
		      (min-town id2 adr2-ht adr3-ht)
		      ))))		; => MAKE-ADR3-HT

(defun pp (key1 key2 ht)
  "三平方の定理で、ベクトルの大きさを求める。Pythagorean proposition" 
  (let ((v1 (gethash key1 ht)) ;key1に対応する構造体をv1にセット
	(v2 (gethash key2 ht))) ;key2に対応する構造体をv2にセット
    (sqrt (+ (expt (- (adr3-lat v1) (adr3-lat v2)) 2)  
	     (expt (- (adr3-lon v1) (adr3-lon v2)) 2))))) ; => PP

(defun pp2 (key1 key2 ht)
  "三平方の定理で、ベクトルの大きさを求める。Pythagorean proposition" 
  (let ((v1 (gethash key1 ht)) ;key1に対応する構造体をv1にセット
	(v2 (gethash key2 ht))) ;key2に対応する構造体をv2にセット
    (sqrt (+ (expt (- (adr2-lat v1) (adr2-lat v2)) 2)  
	     (expt (- (adr2-lon v1) (adr2-lon v2)) 2))))) ; => PP2

(defun set-dist (n adr3-ht)
  "keyを元に組み合わせの全パターンを作成、"
  (check-type n integer)
  (loop :for (id2 . id3) :being the hash-keys :of adr3-ht :using (hash-value v)
     :when (eq id2 n) ;行政区番号がnと等しい時、
     :collect (cons id2 id3) :into group ;その行政区番号と各町村番号のconsを順次groupに格納
     :finally (loop :for (k1 k2) :in (comb group) ; 同一行政区番号の組み合わせパターンからkeyを２つ取り出す。
		 :do (let ((dist (pp k1 k2 adr3-ht))) ;2地点間の距離を計算しそれぞれの地点に距離を追加
		       (incf (adr3-dist (gethash k1 adr3-ht)) dist)
		       (incf (adr3-dist (gethash k2 adr3-ht)) dist))))) ; => SET-DIST

(defun set-dist2 (adr2-ht)
  "keyを元に組み合わせの全パターンを作成、"
  (loop :for id2 :being the hash-keys :of adr2-ht :using (hash-value v)
     :collect id2 :into group ;その行政区番号と各町村番号のconsを順次groupに格納
     :finally (loop :for (k1 k2) :in (comb group)
		 :do (let ((dist (pp2 k1 k2 adr2-ht))) ;2地点間の距離を計算しそれぞれの地点に距離を追加
		       (incf (adr2-dist (gethash k1 adr2-ht)) dist)
		       (incf (adr2-dist (gethash k2 adr2-ht)) dist))))) ; => SET-DIST2

