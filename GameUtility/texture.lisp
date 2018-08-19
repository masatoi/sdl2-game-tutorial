;; テクスチャクラス
(defclass class-texture ()
  ((renderer
    :initarg  :renderer
    :initform (error "Must supply a renderer"))
   (width
    :initform 0)
   (height
    :initform 0)
   (texture
    :initform nil)))

;; 画像を読み取る
(defmethod tex-load-from-file (renderer filepath)
  (let ((tex (make-instance 'class-texture :renderer renderer)))
    (with-slots (renderer width height texture) tex
      (let ((surface (sdl2-image:load-image filepath)))
        (setf width  (sdl2:surface-width surface))
        (setf height (sdl2:surface-height surface))
        (sdl2:set-color-key surface :true (sdl2:map-rgb (sdl2:surface-format surface) 0 0 0))
        (setf texture (sdl2:create-texture-from-surface renderer surface))))
    tex))

;; 文字列の読み込み
(defmethod tex-load-from-string (renderer font text)
  (let ((tex (make-instance 'class-texture :renderer renderer)))
    (with-slots (renderer width height texture) tex
      (let ((surface  (sdl2-ttf:render-utf8-solid font text #xFF #xFF #xFF 0)))
        (setf width   (sdl2:surface-width surface))
        (setf height  (sdl2:surface-height surface))
        (setf texture (sdl2:create-texture-from-surface renderer surface))))
    tex))

;; レンダリング処理
(defmethod tex-render (tex x y &key clip)
  (with-slots (renderer width height texture) tex
    (sdl2:render-copy renderer
                      texture
                      :source-rect clip
                      :dest-rect   (sdl2:make-rect x
                                                   y
                                                   (if clip (sdl2:rect-width  clip) width)
                                                   (if clip (sdl2:rect-height clip) height)))))

(defmethod tex-render2 (tex x y w h &key clip)
  (with-slots (renderer texture) tex
    (sdl2:render-copy renderer
                      texture
                      :source-rect clip
                      :dest-rect   (sdl2:make-rect x y w h))))

;; システムウィンドウレンダリング処理
(defmethod system-window-render (tex x y w h &key alpha)
  (with-slots (width height texture) tex
    (when alpha
      (sdl2:set-texture-blend-mode texture :blend)
      (sdl2:set-texture-alpha-mod  texture alpha))
    (let* ((width-size   (/ width  3))
           (height-size  (/ height 3))
           (right-pos    (- width (* (/ width 3) 3)))
           (center-pos   (- width (* (/ width 3) 2)))
           (left-pos     (- width (* (/ width 3) 1)))
           (upper-left   (sdl2:make-rect right-pos  right-pos  width-size height-size))
           (upper-right  (sdl2:make-rect left-pos   right-pos  width-size height-size))
           (bottom-left  (sdl2:make-rect right-pos  left-pos   width-size height-size))
           (bottom-right (sdl2:make-rect left-pos   left-pos   width-size height-size))
           (upper        (sdl2:make-rect center-pos right-pos  width-size height-size))
           (bottom       (sdl2:make-rect center-pos left-pos   width-size height-size))
           (left         (sdl2:make-rect right-pos  center-pos width-size height-size))
           (right        (sdl2:make-rect left-pos   center-pos width-size height-size))
           (center       (sdl2:make-rect center-pos center-pos width-size height-size)))
      ;; Four Corners
      (tex-render tex x                      y                       :clip upper-left  )
      (tex-render tex (+ x (- w width-size)) y                       :clip upper-right )
      (tex-render tex x                      (+ y (- h height-size)) :clip bottom-left )
      (tex-render tex (+ x (- w width-size)) (+ y (- h height-size)) :clip bottom-right)
      ;; Upper/Bottom
      (tex-render2 tex (+ x width-size) y                       (- w (* width-size 2)) height-size :clip upper )
      (tex-render2 tex (+ x width-size) (+ y (- h height-size)) (- w (* width-size 2)) height-size :clip bottom)
      ;; Right/Left
      (tex-render2 tex x                      (+ y height-size) width-size (- h (* height-size 2)) :clip left  )
      (tex-render2 tex (+ x (- w width-size)) (+ y height-size) width-size (- h (* height-size 2)) :clip right )
      ;; Center
      (tex-render2 tex (+ x width-size) (+ y height-size) (- w (* width-size 2)) (- h (* height-size 2)) :clip center))))
