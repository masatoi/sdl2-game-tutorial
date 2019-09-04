;;; 07：システムウィンドウ表示

(defpackage #:sdl2-game-tutorial/07-system-window
  (:use #:cl
        #:sdl2-game-tutorial/utils)
  (:export #:main))
(in-package #:sdl2-game-tutorial/07-system-window)

;; 画像ファイルへのパス
(defparameter *image-file-path* "Material/graphics/system/systemwindow.png")

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
