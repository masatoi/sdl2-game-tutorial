;; ベースウィンドウのX/Y座標及び、幅/高さ
(defparameter *base-win-x* 25)
(defparameter *base-win-y* 345)
(defparameter *base-win-w* 590)
(defparameter *base-win-h* 110)

;; テキスト表示位置
(defparameter *text-x-pos* 40)
(defparameter *1st-line*   360)
(defparameter *2nd-line*   390)
(defparameter *3rd-line*   420)

;; ポーズアニメーション
(defparameter *pause-x*     305)
(defparameter *pause-y*     445)
(defparameter *pause-frame* 6)

;; テキストファイルへのパス
(defparameter *text-file-path* "Material/text/message-text.txt")

;; 最大テキストメッセージ数
(defparameter *max-text-num* 0)

;; メッセージ管理用配列
(defparameter *text-message-test* (make-array `(1 3) :initial-element nil :adjustable t))

;; メッセージウィンドウクラス
(defclass class-msgwin ()
  ((syswin-tex
    :initarg  :syswin-tex
    :initform (error "Must supply a syswin-tex"))
   (str-tex
    :initarg  :str-tex
    :initform nil)
   (pause-tex
    :initarg  :pause-tex
    :initform (error "Must supply a pause-tex"))
   (pause-clip
    :initarg  :pause-clip
    :initform nil)
   (font
    :initarg  :font
    :initform (error "Must supply a font"))))

;; テキストファイルからテキストを読み込み配列へ格納する
(defmethod load-text ()
  (let ((count1 0)
        (count2 0))
    (with-open-file (in *text-file-path* :if-does-not-exist nil)
      (when in
        (loop for line = (read-line in nil)
           while line do (progn
                           (setf (aref *text-message-test* count1 count2) (format nil "~a " line))
                           (if (< count2 2)
                               (incf count2)
                               (progn
                                 (incf count1)
                                 (setf *max-text-num* (+ count1 1))
                                 (adjust-array *text-message-test* `(,*max-text-num* 3))
                                 (setf count2 0)))))))))

(defmethod msg-view (obj renderer frames tick-per-frame text-count &key 1st 2nd 3rd)
  (with-slots (syswin-tex str-tex pause-tex pause-clip font) obj
    ;; ベースウィンドウ表示
    (system-window-render syswin-tex *base-win-x* *base-win-y* *base-win-w* *base-win-h*)

    ;; 1行目テキスト表示
    (when 1st
      (setf str-tex (tex-load-from-string renderer font 1st))
      (tex-render str-tex *text-x-pos* *1st-line*))

    ;; 2行目テキスト表示
    (when 2nd
      (setf str-tex (tex-load-from-string renderer font 2nd))
      (tex-render str-tex *text-x-pos* *2nd-line*))

    ;; 3行目テキスト表示
    (when 3rd
      (setf str-tex (tex-load-from-string renderer font 3rd))
      (tex-render str-tex *text-x-pos* *3rd-line*))

    (when (< text-count (- *max-text-num* 2))
      ;; ポーズアニメーション表示
      (tex-render pause-tex *pause-x* *pause-y* :clip pause-clip)

      ;; ポーズアニメーション更新
      (when (zerop (rem frames tick-per-frame))
        (setf (sdl2:rect-y pause-clip) (* (rem frames *pause-frame*) (sdl2:rect-height pause-clip)))))))
