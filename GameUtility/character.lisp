(defclass class-character ()
  ((clip
    :initarg  :clip
    :initform nil)
   (move
    :initarg  :move
    :initform 0)
   (x-pos
    :initarg  :x-pos
    :initform 0)
   (y-pos
    :initarg  :y-pos
    :initform 0)
   (x-index
    :initarg  :x-index
    :initform 0)
   (y-index
    :initarg  :y-index
    :initform 0)
   (direction
    :initarg  :direction
    :initform 0)
   (move-flg
    :initarg  :move-flg
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
  (with-slots (move x-pos y-pos x-index y-index direction move-flg) obj
    (when move-flg
      (case direction
        ;; 下方向へ移動
        (0 (if (> (incf move) 32)
               (progn
                 (setf move-flg nil)
                 (incf y-index))
               (incf y-pos)))
        ;; 左方向へ移動
        (1 (if (> (incf move) 32)
               (progn
                 (setf move-flg nil)
                 (decf x-index))
               (decf x-pos)))
        ;; 右方向へ移動
        (2 (if (> (incf move) 32)
               (progn
                 (setf move-flg nil)
                 (incf x-index))
               (incf x-pos)))
        ;; 上方向へ移動
        (3 (if (> (incf move) 32)
               (progn
                 (setf move-flg nil)
                 (decf y-index))
               (decf y-pos)))))))

(defmethod change-direction (obj)
  (with-slots (clip direction) obj
    (case direction
      (0 (setf (sdl2:rect-y clip) 0))
      (1 (setf (sdl2:rect-y clip) 32))
      (2 (setf (sdl2:rect-y clip) 64))
      (3 (setf (sdl2:rect-y clip) 96)))))
