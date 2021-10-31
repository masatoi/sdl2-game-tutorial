;;; 06：画像を動かす

(defpackage #:sdl2-game-tutorial/06-move-image
  (:use #:cl
        #:sdl2-game-tutorial/utils)
  (:export #:main))
(in-package #:sdl2-game-tutorial/06-move-image)

;; 画像ファイルへのパス
(defparameter *image-file-path* (expand-path "Material/graphics/picture/cat.png"))

(defun main ()
  (with-window-renderer (window renderer "SDL2 Tutorial 06")
    (let ((tex nil) (w 0) (h 0)
          (x-pos 0) (y-pos 0))
      (flet ((image-texture (renderer img &key (r 0) (g 0) (b 0))
               (multiple-value-bind (texture width height)
                   (create-image-texture renderer img :r r :g g :b b)
                 (setf tex texture w width h height))))
        (image-texture renderer *image-file-path*)
        ;; イベントループ(この中にキー操作時の動作や各種イベントを記述していく)
        (sdl2:with-event-loop (:method :poll)
          ;; キーが押下されたときの処理
          (:keydown (:keysym keysym)
                    ;; keysymをスキャンコードの数値(scancode-value)に変換して、キー判定処理(scancode=)を行う
                    (if (sdl2:scancode= (sdl2:scancode-value keysym) :scancode-escape)
                        (sdl2:push-event :quit)  ; Escキーが押下された場合、quitイベントをキューに加える
                        (case (sdl2:scancode keysym)
                          (:scancode-up    (when (> y-pos 0) (decf y-pos)))
                          (:scancode-down  (when (< y-pos (- *screen-height* h)) (incf y-pos)))
                          (:scancode-left  (when (> x-pos 0) (decf x-pos)))
                          (:scancode-right (when (< x-pos (- *screen-width* w)) (incf x-pos))))))
          ;; この中に描画処理など各種イベントを記述していく
          (:idle ()
                 ;; 作図操作(矩形、線、およびクリア)に使用する色を設定
                 (sdl2:set-render-draw-color renderer 0 0 0 255)
                 ;; 現在のレンダーターゲットを上記で設定した色で塗りつぶして消去
                 (sdl2:render-clear renderer)

                 ;; レンダリング処理
                 (sdl2:render-copy renderer
                                   tex
                                   :dest-rect (sdl2:make-rect x-pos y-pos w h))

                 ;; レンダリングの結果を画面に反映
                 (sdl2:render-present renderer))
          ;; 終了イベント
          (:quit () t))))))
