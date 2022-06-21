(in-package :jpmap)

(defstruct area
  (name nil))				; => AREA

(defstruct (ward (:include area))
  (lat 0)
  (lon 0))				; => WARD

(defstruct (town (:include ward))
  (dist 0))				; => TOWN

(defparameter pref-tree nil "ツリー作成用の変数") ; => PREF-TREE


(defparameter prefs-name (car pref-tree) "都道府県名") ; => PREFS-NAME

(defun prefs-name (n)
  (area-name (gethash n prefs-ht)))	; => PREFS-NAME

(defparameter prefs-ht (make-hash-table)) ; => PREFS-HT

(defparameter prefs '(北海道 青森県 岩手県 宮城県 秋田県
			  山形県 福島県 茨城県 栃木県 群馬県
			  埼玉県 千葉県 東京都 神奈川 新潟県
			  富山県 石川県 福井県 山梨県 長野県
			  岐阜県 静岡県 愛知県 三重県 滋賀県
			  京都府 大阪府 兵庫県 奈良県 和歌山県
			  鳥取県 島根県 岡山県 広島県 山口県
			  徳島県 香川県 愛媛県 高知県 福岡県
			  佐賀県 長崎県 熊本県 大分県 宮崎県
		      鹿児島県 沖縄県))	; => PREFS


(defparameter towns/ward (mapcar #'cdr (cdr pref-tree))) ; => TOWNS/WARD

(defun make-prefs-ht ()
  (loop :for i :in prefs
     :count i :into cnt
     :do (put-ht cnt (make-area :name i) prefs-ht))) ; => MAKE-PREFS-HT



;; 行政区名
(defparameter ward-name (mapcar #'car (cdr pref-tree))) ; => WARD-NAME

(defun show-ward-name (n)
  (nth (1- n) ward-name))    ; => SHOW-WARD-NAME


(defun put-ht (k v ht)
  (setf (gethash k ht) v))		; => PUT-HT

(defun puts-ht (lst ht)
  (loop :for i :in lst
	:count i :into cnt
     :do (put-ht cnt i ht)))		; => PUTS-HT

;; ok [2022-05-24]
(defun comb (lst)
  (flet ((pair (l)
	   (let ((key (car l))
		 (rest (cdr l)))
	     (mapcar #'(lambda (x) (list key x)) rest))))
    (mapcon #'pair lst)))		; => COMB

(defparameter ward-ht (make-hash-table)) ; => WARD-HT

(defparameter town-ht (make-hash-table)) ; => TOWN-HT

(defun min-town (n ward-ht town-ht)
  (check-type n integer)
  (loop :for v :being the hash-values :of town-ht :using (hash-key k)
       :when (eq (car k) n)
     :minimize (town-dist v) :into mini
     :finally (progn
		(maphash
		 #'(lambda (k v)
		     (when (eq (town-dist v) mini)
		       (setf ward-v (make-ward :name (show-ward-name n) :lat (town-lat v) :lon (town-lon v)))))
		 town-ht)
		(put-ht n ward-v ward-ht)))) ; => MIN-TOWN



(defun make-towns-ht (ward-ht town-ht)
  (loop :for i :in towns/ward
     :count i :into wid
     :do (loop :for (name lat lon) :in i
	    :count name :into tid
	    :do (put-ht (cons wid tid) (make-town :name name :lat lat :lon lon) town-ht)
	    :finally (progn
		       (set-dist wid town-ht)
		       (min-town wid ward-ht town-ht))))) ; => MAKE-TOWNS-HT



(defun pp (t1 t2 ht)
  "三平方の定理で、ベクトルの大きさを求める。Pythagorean proposition" 
  (let ((town1 (gethash t1 ht))
	(town2 (gethash t2 ht)))
    (sqrt (+ (expt (- (town-lat town1) (town-lat town2)) 2)
	     (expt (- (town-lon town1) (town-lon town2)) 2))))) ; => PP


(defun set-dist (n town-ht)
  (check-type n integer)
  (loop :for (wid . tid) :being the hash-keys :of ht :using (hash-value v)
     :when (eq wid n)
     :collect (cons wid tid) :into group
     :finally (loop :for (t1 t2) :in (comb group)
		 :do (let ((v (pp t1 t2 ht)))
		       (incf (town-dist (gethash t1 town-ht)) v)
		       (incf (town-dist (gethash t2 town-ht)) v))))) ; => SET-DIST

