;;;; jpmap.lisp

;; (ql:quickload '(:dexador :unio :group-by) :silent t) ; => (:DEXADOR :UNIO :GROUP-BY)

;;; 大域変数定義
(defpackage :jpmap
  (:use :cl :unio :group-by)
  (:shadowing-import-from :dex :get)
  (:export :main :show-map :load-db))		; => #<PACKAGE "JPMAP">
(in-package :jpmap)			; => #<PACKAGE "JPMAP">

;; (defparameter csv-file
;;   "~/howm/junk/location14-info/01000-14.0b/01_2020.csv" "処理元のcsvデータ") ; => CSV-FILE

(defvar header "タイトル行格納域")	; => HEADER

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


;;; 構造体定義
(defstruct area
  name
  gm)					; => AREA

(defstruct (adr2 (:include area))
  (lat 0)
  (lon 0)
  (dist 0))				; => ADR2

(defstruct (adr3 (:include adr2)))	; => ADR3

;;; 関数定義
(defun system (control-string &rest args)
  "Interpolate ARGS into CONTROL-STRING as if by FORMAT, and
synchronously execute the result using a Bourne-compatible shell, with
output to *verbose-out*.  Returns the shell's exit code."
  (let ((command (apply #'format nil control-string args)))
    (format t "; $ ~A~%" command)
    #+sbcl
    (sb-impl::process-exit-code
     (sb-ext:run-program
      "/usr/bin/zsh"
      (list  "-c" command)
      :input nil :output *standard-output*))
    #+(or cmu scl)
    (ext:process-exit-code
     (ext:run-program
      "/bin/sh"
      (list  "-c" command)
      :input nil :output *verbose-out*))
    #+clisp             ;XXX not exactly *verbose-out*, I know
    (ext:run-shell-command  command :output :terminal :wait t)
    ))					; => SYSTEM

(defun parse-line (line &optional (sep #\,)) ;
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
			       :collect (nth i (parse-line line)))
			    k-len)
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
     :finally (return (cons k1 (group-by acc))))) ; => HT2TREE

;;======================================================= 

;; (defun adr3->2 (adr3-name)
;;   (caar (seek adr3-name (format nil "~a" adr-tree) :skin 1 :str nil))) ; => ADR3->2
;;これは関数名をparentに変更すべき
;; (defun parent (area-name)
;;   (caar (seek area-name (format nil "~a" adr-tree) :skin 1 :str nil))) ; => PARENT

;; (parent "金蔵")				; => 輪島市

;; 行政区名

(defun show-adr2-name (n adr2-name)
  "番号を入れたら行政区名を表示する。"
  (nth (1- n) adr2-name))		; => SHOW-ADR2-NAME

;; ok [2022-05-24]
(defun comb (lst)
  (flet ((pair (l)
	   (let ((key (car l))
		 (rest (cdr l)))
	     (mapcar #'(lambda (x) (list key x)) rest))))
    (mapcon #'pair lst)))		; => COMB

(defun make-adr1-ht (adr1-ht)
  (loop :for i :in prefs
     :count i :into cnt
     :do (put-ht cnt (make-area :name i :gm "") adr1-ht))) ; => MAKE-ADR1-HT

(defun min-town (n adr2-ht adr3-ht adr2-name)
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
			 (setf adr2-v (make-adr2 :name (show-adr2-name n adr2-name) :gm k :lat (adr3-lat v) :lon (adr3-lon v)))))
		   adr3-ht)
		  (put-ht n adr2-v adr2-ht))))) ; => MIN-TOWN

(defun make-adr3-ht (adr3/adr2 adr2-ht adr3-ht adr2-name)
  "keyを作成し、htにkeyと構造体のvalueを追加
町村のハッシュテーブルの作成"
  (loop :for i :in adr3/adr2 ;行政区単位の町村グループ抽出  
     :count i :into id2 ;行政番号をセット
     :do (loop :for (name lat lon) :in i ;グループから町村データを１つずつ抽出
	    :count name :into id3 ;町村番号を作成
	    :do (put-ht (cons id2 id3) (make-adr3 :name name :lat (read-from-string lat) :lon (read-from-string lon) :dist 0) adr3-ht)
	    :finally (progn
		      (set-dist id2 adr3-ht)
		      (min-town id2 adr2-ht adr3-ht adr2-name)
		      ))))		; => MAKE-ADR3-HT

(defun pp (key1 key2 ht)
  "三平方の定理で、ベクトルの大きさを求める。Pythagorean proposition" 
  (let ((v1 (gethash key1 ht)) ;key1に対応する構造体をv1にセット
	(v2 (gethash key2 ht))) ;key2に対応する構造体をv2にセット
    (sqrt (+ (expt (- (adr3-lat v1) (adr3-lat v2)) 2)  
	     (expt (- (adr3-lon v1) (adr3-lon v2)) 2))))) ; => PP

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

(defun pp2 (key1 key2 ht)
  "三平方の定理で、ベクトルの大きさを求める。Pythagorean proposition" 
  (let ((v1 (gethash key1 ht)) ;key1に対応する構造体をv1にセット
	(v2 (gethash key2 ht))) ;key2に対応する構造体をv2にセット
    (sqrt (+ (expt (- (adr2-lat v1) (adr2-lat v2)) 2)  
	     (expt (- (adr2-lon v1) (adr2-lon v2)) 2))))) ; => PP2

(defun load-db (file ht)
  "ロード元のファイルを読み込み登録用の辞書テーブルに展開する"
  (let ((ifile (merge-pathnames (make-pathname
				  :directory '(:relative "howm" "junk")
				  :name (string-downcase file)
				  :type "db"
				  :version :newest)
				 (user-homedir-pathname))))
    (with-open-file (in ifile :if-does-not-exist nil)
      (with-standard-io-syntax
        (setf ht (read in))))))		; => LOAD-DB

(defun save-db (ht file)
  "ハッシュテーブルの保存"
  (let ((ofile (merge-pathnames (make-pathname
				   :directory '(:relative "howm" "junk")
				   :name (string-downcase file)
				   :type "db"
				   :version :newest)
				  (user-homedir-pathname))))
    (with-open-file (out ofile
                         :direction :output
                         :if-exists :supersede
			 :if-does-not-exist :create
                         :external-format :utf-8)
      (with-standard-io-syntax
        (print ht out)))))		; => SAVE-DB



;;--
(defun center-pt (z)
  (expt 2 (+ z 7)))			; => CENTER-PT

(defun total-px (z)
  (* (center-pt z) 2))			; => TOTAL-PX

(defun px/deg (z)
  (/ (total-px z) 360))			; => PX/DEG

(defun lon->x (lon z)
  (floor (/ (+ (center-pt z) (* lon (px/deg z)) 0.5) 256))) ; => LON->X

(defun px/rad (z)
  (/ (total-px z) (* 2 pi)))		; => PX/RAD

(defun siny (lat)
  (min (max (sin (* lat (/ pi 180))) -0.9999) 0.9999)) ; => SINY

(defun lat->y (lat z)
  (floor
   (/ (+ 0.5 (- (center-pt z)
		(* 0.5 (log (/ (+ 1 (siny lat))
			       (- 1 (siny lat))))
		   (px/rad z)))) 256)))	; => LAT->Y

(defun area->num (area group)
  (loop :for i :in group
     :count i :into cnt
     :do (when (string-equal area i)
	   (return cnt))))		; => AREA->NUM

(defun lon/lat (n adr2-ht)
  (let* ((v (gethash n adr2-ht))
	 (lon (adr2-lon v))
	 (lat (adr2-lat v)))
    (values lon lat)))			; => LON/LAT

(defun download-file (uri &optional (filename))
  (with-open-file (out filename :direction :output
		       :if-exists :supersede
		       :element-type '(unsigned-byte 8))
    (with-open-stream (input (dex:get uri :want-stream t :connect-timeout nil))
      (loop :for b := (read-byte input nil -1)
	 :until (minusp b)
	 :do (write-byte b out)))))	; => DOWNLOAD-FILE

(defun show-map (muni n adr2-ht adr2-name &optional (map-type "std"))
  "z (ズームレベル) は 4～10"
  (multiple-value-bind (lon lat)
      (lon/lat (area->num muni adr2-name) adr2-ht)
    (let* ((gsi-url "https://cyberjapandata.gsi.go.jp/xyz/")
	   (z (parse-integer n))
	   (x (lon->x lon z))
	   (y (lat->y lat z))
;;	   (map-file (format nil "~a~a" (truename "./") "map.png"))
	   (out-file (format nil "~a-~a.png" muni z))
	   ;(cross-file (format nil "~a~a" (truename "./") "cross.png"))
	   (map-layer (format nil "~a~a/~a/~a/~a.png" gsi-url map-type z x y)))
      (download-file map-layer out-file)
 ;     (system (format nil "~a ~a ~a" "composite -geometry +128+128 -compose over" map-file cross-file out-file))
      (system (format nil "xdg-open ~a~a" (truename "./") out-file))))) ; => SHOW-MAP

;; main

(defun main (csv-file)
  (let ((ht (make-hash-table :test #'equal)))
    (cut csv-file :to ht :key '(1 3 5) :val '(6 7))
    (let* ((adr-tree (ht2tree ht))
	   (adr1-name (car adr-tree))
	   (adr2-name (mapcar #'car (cdr adr-tree)))
	   (adr3/adr2 (mapcar #'cdr (cdr adr-tree)))
	   (adr1-ht (make-hash-table :test #'equal))
	   (adr2-ht (make-hash-table :test #'equal))
	   (adr3-ht (make-hash-table :test #'equal)))
      (make-adr1-ht adr1-ht)
      (make-adr3-ht adr3/adr2 adr2-ht adr3-ht adr2-name)
      (set-dist2 adr2-ht)
      (save-db adr-tree (format nil "~a-~a" "adr-tree" adr1-name))
;;      (save-db adr1-ht (format nil "~a-~a" "adr1" adr1-name))
      (save-db adr2-ht (format nil "~a-~a" "adr2" adr1-name))
      (save-db adr3-ht (format nil "~a-~a" "adr3" adr1-name))
;;      (show-map area z adr2-ht adr2-name "std")
      )))				; => MAIN

;; (main csv-file)				; => #<HASH-TABLE :TEST EQUAL :COUNT 25976 {1004CAD623}>


