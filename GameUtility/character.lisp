(defclass class-character ()
  ((clip
    :initarg  :clip
    :initform nil)
   (move
    :initarg  :move
    :initform 0)
   (x-index
    :initarg  :x-index
    :initform 0)
   (y-index
    :initarg  :y-index
    :initform 0)
   (x-pos
    :initarg  :x-pos
    :initform 0)
   (y-pos
    :initarg  :y-pos
    :initform 0)
   (direction
    :initarg  :direction
    :initform 0)
   (move-flg
    :initarg  :move-flg
    :initform nil)
   (location
    :initarg  :location
    :initform nil)))

(defmethod chk-key (obj keysym)
  (with-slots (move direction move-flg) obj
    (unless move-flg
      (setf move-flg t)
      (setf move 0)
      (case (sdl2:scancode keysym)
        (:scancode-up    (setf direction 3))
        (:scancode-down  (setf direction 0))
        (:scancode-left  (setf direction 1))
        (:scancode-right (setf direction 2))))))

(defmethod move-character (obj)
  (with-slots (move x-index y-index x-pos y-pos direction move-flg location) obj
    (when move-flg
      (case direction
        ;; 下方向へ移動
        (0 (cond ((or (= y-index (- +map-height+ 1)) (= (aref location (+ y-index 1) x-index) 1))
                  (setf move-flg nil)) ; 最端部か移動先が通行不可の場合、移動中フラグのクリアのみ行う
                 ((> (incf move) 32)   ; 移動量を更新し、キャラクターが32ピクセル分移動したかチェック
                  (setf move-flg nil)  ; 移動中フラグをクリア
                  (incf y-index))      ; キャラの居るマップ座標を更新
                 (t (incf y-pos))))    ; 現在座標を新更
        ;; 左方向へ移動
        (1 (cond ((or (= x-index 0) (= (aref location y-index (- x-index 1)) 1))
                  (setf move-flg nil))
                 ((> (incf move) 32)
                  (setf move-flg nil)
                  (decf x-index))
                 (t (decf x-pos))))
        ;; 右方向へ移動
        (2 (cond ((or (= x-index (- +map-width+ 1)) (= (aref location y-index (+ x-index 1)) 1))
                  (setf move-flg nil))
                 ((> (incf move) 32)
                  (setf move-flg nil)
                  (incf x-index))
                 (t (incf x-pos))))
        ;; 上方向へ移動
        (3 (cond ((or (= y-index 0) (= (aref location (- y-index 1) x-index) 1))
                  (setf move-flg nil))
                 ((> (incf move) 32)
                  (setf move-flg nil)
                  (decf y-index))
                 (t (decf y-pos))))))))

(defmethod change-direction (obj)
  (with-slots (clip direction) obj
    (case direction
      (0 (setf (sdl2:rect-y clip) 0))
      (1 (setf (sdl2:rect-y clip) 32))
      (2 (setf (sdl2:rect-y clip) 64))
      (3 (setf (sdl2:rect-y clip) 96)))))
