;;; 07：システムウィンドウ表示

(defpackage #:sdl2-game-tutorial/07-system-window
  (:use #:cl
        #:sdl2-game-tutorial/utils)
  (:export #:main))
(in-package #:sdl2-game-tutorial/07-system-window)

;; 画像ファイルへのパス
(defparameter *image-file-path* "Material/graphics/system/systemwindow.png")

;; システムウィンドウレンダリング処理
(defun system-window-render (renderer
                             texture
                             width
                             height
                             &key (x 25) (y 345) (w 590) (h 110) (alpha 255))
  (when alpha
    (sdl2:set-texture-blend-mode texture :blend)
    (sdl2:set-texture-alpha-mod  texture alpha))
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

(defun main ()
  (with-window-renderer (window renderer "SDL2 Tutorial 07")
    (multiple-value-bind (texture width height)
        ;; 画像ファイル読み込み、画像情報の取得などを行う
        (create-image-texture renderer *image-file-path*)
      ;; イベントループ(この中にキー操作時の動作や各種イベントを記述していく)
      (sdl2:with-event-loop (:method :poll)
        ;; キーが押下されたときの処理
        (:keydown (:keysym keysym)
                  ;; keysymをスキャンコードの数値(scancode-value)に変換して、キー判定処理(scancode=)を行う
                  (if (sdl2:scancode= (sdl2:scancode-value keysym) :scancode-escape)
                      (sdl2:push-event :quit))) ; Escキーが押下された場合、quitイベントをキューに加える
        ;; この中に描画処理など各種イベントを記述していく
        (:idle ()
               ;; 作図操作(矩形、線、およびクリア)に使用する色を設定
               (sdl2:set-render-draw-color renderer 0 0 0 255)
               ;; 現在のレンダーターゲットを上記で設定した色で塗りつぶして消去
               (sdl2:render-clear renderer)

               ;; レンダリング処理
               (system-window-render renderer texture width height)

               ;; レンダリングの結果を画面に反映
               (sdl2:render-present renderer))
        ;; 終了イベント
        (:quit () t)))))
