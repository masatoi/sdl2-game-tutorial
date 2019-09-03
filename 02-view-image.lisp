;;; 02：画像を表示する

(defpackage #:sdl2-game-tutorial/02-view-image
  (:use #:cl
        #:sdl2-game-tutorial/utils)
  (:export #:main))
(in-package #:sdl2-game-tutorial/02-view-image)

;; 画像ファイルへのパス
(defparameter *image-file-path* "Material/graphics/picture/cat.png")

(defun create-texture (renderer surface)
  (sdl2:set-color-key surface :true (sdl2:map-rgb (sdl2:surface-format surface) 0 0 0))
  (sdl2:create-texture-from-surface renderer surface))

(defun main ()
  (with-window-renderer (window renderer "SDL2 Tutorial 02")
    ;; 画像ファイル読み込み、画像情報の取得などを行う
    (let* ((surface (sdl2-image:load-image *image-file-path*))
           (width (sdl2:surface-width surface))
           (height (sdl2:surface-height surface))
           (texture (create-texture renderer surface)))
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
               (let ((x-pos (- (/ *screen-width*  2) (floor width  2)))  ; テキストの表示位置(X座標)計算
                     (y-pos (- (/ *screen-height* 2) (floor height 2)))) ; テキストの表示位置(Y座標)計算
                 (sdl2:render-copy renderer
                                   texture
                                   :dest-rect (sdl2:make-rect x-pos y-pos width height)))
               ;; レンダリングの結果を画面に反映
               (sdl2:render-present renderer))
        ;; 終了イベント
        (:quit () t)))))
