(defpackage #:sdl2-game-tutorial/utils
  (:use #:cl)
  (:export #:with-window-renderer
           #:*screen-width*
           #:*screen-height*
           #:create-image-texture
           #:create-string-texture
           #:system-window-render
           #:frame-incf))
(in-package #:sdl2-game-tutorial/utils)

;; ウィンドウのサイズ
(defparameter *screen-width*  640) ; 幅
(defparameter *screen-height* 480) ; 高さ

;; SDL2ライブラリ初期化＆終了処理
(defmacro with-window-renderer ((window renderer title) &body body)
  ;; SDLの初期化と終了時の処理をまとめて実行
  `(sdl2:with-init (:video)
     ;; ウィンドウ作成処理を実行
     (sdl2:with-window (,window
                        :title ,title       ; タイトル
                        :w ,*screen-width*  ; 幅
                        :h ,*screen-height* ; 高さ
                        :flags '(:shown))   ; :shownや:hiddenなどのパラメータを設定できる
       ;; ウィンドウの2Dレンダリングコンテキストを生成
       (sdl2:with-renderer (,renderer
                            ,window
                            :index -1
                            ;; レンダリングコンテキストを生成するときに使われるフラグの種類
                            ;; :software      : ソフトウェア レンダラー
                            ;; :accelerated   : ハードウェア アクセラレーション
                            ;; :presentvsync  : 更新周期と同期
                            ;; :targettexture : テクスチャへのレンダリングに対応
                            :flags '(:accelerated :presentvsync))
         ;; sdl2-imageを初期化(扱う画像形式はPNG ※他にもJPGとTIFが使える)
         (sdl2-image:init '(:png))
         ;; sdl2-ttfを初期化
         (sdl2-ttf:init)
         ,@body
         ;; sdl2-image終了処理
         (sdl2-image:quit)
         ;; sdl2-ttf終了処理
         (sdl2-ttf:quit)))))

(defun create-image-texture (renderer
                             image-file-path
                             &key (r 0) (g 0) (b 0))
  (let* ((surface (sdl2-image:load-image image-file-path))
         (width (sdl2:surface-width surface))
         (height (sdl2:surface-height surface)))
    (sdl2:set-color-key surface :true (sdl2:map-rgb (sdl2:surface-format surface) r g b))
    (values (sdl2:create-texture-from-surface renderer surface) width height)))

(defun create-string-texture (renderer
                              font-file-path
                              string
                              &key (r 0) (g 0) (b 0) (a 0))
  (let* ((font (sdl2-ttf:open-font font-file-path 50))
         (surface (sdl2-ttf:render-utf8-solid font string r g b a))
         (width (sdl2:surface-width surface))
         (height (sdl2:surface-height surface))
         (texture (sdl2:create-texture-from-surface renderer surface)))
    (values texture width height)))

;; システムウィンドウレンダリング処理
(defun system-window-render (renderer
                             texture
                             width
                             height
                             &key (x 25) (y 345) (w 590) (h 110) (alpha 255))
  (let* ((w2 (/ width  3))
         (h2 (/ height 3))
         (w3 (- w (* w2 2)))
         (h3 (- h (* h2 2)))
         (x2 (+ x (- w w2)))
         (y2 (+ y (- h h2)))
         (x3 (+ x w2))
         (y3 (+ y h2))
         (x4 (+ x (- w w2)))
         (y4 (+ y (- h h2)))
         (r  (- width (* w2 3)))
         (c  (- width (* w2 2)))
         (l  (- width (* w2 1)))
         (upper-left-s   (sdl2:make-rect r  r  w2 h2))
         (upper-left-d   (sdl2:make-rect x  y  w2 h2))
         (upper-right-s  (sdl2:make-rect l  r  w2 h2))
         (upper-right-d  (sdl2:make-rect x2 y  w2 h2))
         (bottom-left-s  (sdl2:make-rect r  l  w2 h2))
         (bottom-left-d  (sdl2:make-rect x  y2 w2 h2))
         (bottom-right-s (sdl2:make-rect l  l  w2 h2))
         (bottom-right-d (sdl2:make-rect x2 y2 w2 h2))
         (upper-s        (sdl2:make-rect c  r  w2 h2))
         (upper-d        (sdl2:make-rect x3 y  w3 h2))
         (bottom-s       (sdl2:make-rect c  l  w2 h2))
         (bottom-d       (sdl2:make-rect x3 y4 w3 h2))
         (left-s         (sdl2:make-rect r  c  w2 h2))
         (left-d         (sdl2:make-rect x  y3 w2 h3))
         (right-s        (sdl2:make-rect l  c  w2 h2))
         (right-d        (sdl2:make-rect x4 y3 w2 h3))
         (center-s       (sdl2:make-rect c  c  w2 h2))
         (center-d       (sdl2:make-rect x3 y3 w3 h3)))
    (when alpha
      (sdl2:set-texture-blend-mode texture :blend)
      (sdl2:set-texture-alpha-mod  texture alpha))
    ;; Four Corners
    (sdl2:render-copy renderer texture :source-rect upper-left-s :dest-rect upper-left-d)
    (sdl2:render-copy renderer texture :source-rect upper-right-s :dest-rect upper-right-d)
    (sdl2:render-copy renderer texture :source-rect bottom-left-s :dest-rect bottom-left-d)
    (sdl2:render-copy renderer texture :source-rect bottom-right-s :dest-rect bottom-right-d)
    ;; Upper/Bottom
    (sdl2:render-copy renderer texture :source-rect upper-s :dest-rect upper-d)
    (sdl2:render-copy renderer texture :source-rect bottom-s :dest-rect bottom-d)
    ;; Right/Left
    (sdl2:render-copy renderer texture :source-rect left-s :dest-rect left-d)
    (sdl2:render-copy renderer texture :source-rect right-s :dest-rect right-d)
    ;; Center
    (sdl2:render-copy renderer texture :source-rect center-s :dest-rect center-d)))

;; フレーム数インクリメント
(defmacro frame-incf (frame)
  `(if (= ,frame most-positive-fixnum)
       (setf ,frame 1)
       (incf ,frame)))
