(in-package :jpmap)


(defvar header "タイトル行格納域")	; => HEADER
(defparameter ht (make-hash-table :test #'equal) "ハッシュ表") ; => HT
;; (defparameter csv-file
;;   "~/howm/junk/csv-to-tree/location14-info/17000-14.0b/17_2020.csv" "処理元のcsvデータ") ; => CSV-FILE

(defvar adr-tree "ツリー作成用の変数")	; => ADR-TREE
(defvar adr1-name (car adr-tree) "都道府県名") ; => ADR1-NAME
;; 行政区名
(defparameter adr2-name (mapcar #'car (cdr adr-tree))) ; => ADR2-NAME

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

(defparameter adr1-ht (make-hash-table :test #'equal)) ; => ADR1-HT

(defstruct area
  name
  gm)					; => AREA

(defstruct (adr2 (:include area))
  (lat 0)
  (lon 0)
  (dist 0))				; => ADR2


(defstruct (adr3 (:include adr2)))	; => ADR3

(defparameter adr3/adr2 (mapcar #'cdr (cdr adr-tree))
  "行政区毎の町村名")			; => ADR3/ADR2

(defparameter adr2-ht (make-hash-table :test #'equal) "住所区分２行政区のハッシュテーブル") ; => ADR2-HT
(defparameter adr3-ht (make-hash-table :test #'equal) "住所区分３町村のハッシュテーブル") ; => ADR3-HT

(defun main (&rest argv)
  (cut argv :to ht :key '(1 3 5) :val '(6 7))
;; (hash-table-count ht)				; => 3140
  ;; 都道府県内の市町村ツリーの作成
  (setf adr-tree (ht2tree ht))		; => (NIL)  (make-prefs-ht)
  (make-adr1-ht)						   ; => NIL
  (make-adr3-ht adr2-ht adr3-ht)		; => NIL
  ;; (hash-table-count adr3-ht)		; => 0
  ;; (hash-table-count adr2-ht)		; => 0
  ;; (print-ht adr3-ht)			; => NIL
  ;; (print-ht adr2-ht)			; => NIL
  (set-dist2 adr2-ht)
)	; => MAIN

(main)					; => 


