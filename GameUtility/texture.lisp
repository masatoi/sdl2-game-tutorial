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
