;;; 08：FPSタイマー

(defpackage #:sdl2-game-tutorial/08-fps-timer
  (:use #:cl
        #:sdl2-game-tutorial/utils)
  (:import-from #:sdl2-game-tutorial/fps
                #:fps
                #:timer-start
                #:timer-get-ticks)
  (:export #:main))
(in-package #:sdl2-game-tutorial/08-fps-timer)

;; フォントファイルへのパス
(defparameter *font-file-path* (expand-path "Material/fonts/ipaexg.ttf"))

(defun main ()
  (with-window-renderer (window renderer "SDL2 Tutorial 08")
    (let* ((fps-timer (make-instance 'fps))
           (cap-timer (make-instance 'fps))
           (fixed-fps 60)
           (tick-per-frame (floor 1000 fixed-fps))
           (frames 1))

      (timer-start fps-timer)

      ;; イベントループ(この中にキー操作時の動作や各種イベントを記述していく)
      (sdl2:with-event-loop (:method :poll)
        ;; キーが押下されたときの処理
        (:keydown (:keysym keysym)
                  ;; keysymをスキャンコードの数値(scancode-value)に変換して、キー判定処理(scancode=)を行う
                  (if (sdl2:scancode= (sdl2:scancode-value keysym) :scancode-escape)
                      (sdl2:push-event :quit))) ; Escキーが押下された場合、quitイベントをキューに加える
        ;; この中に描画処理など各種イベントを記述していく
        (:idle ()
               (timer-start cap-timer)
               
               ;; 作図操作(矩形、線、およびクリア)に使用する色を設定
               (sdl2:set-render-draw-color renderer 0 0 0 255)
               ;; 現在のレンダーターゲットを上記で設定した色で塗りつぶして消去
               (sdl2:render-clear renderer)

               ;; レンダリング処理
               (multiple-value-bind (texture width height)
                   (create-string-texture renderer
                                          *font-file-path*
                                          (format nil "FPS : ~a" (floor frames (1+ (floor (timer-get-ticks fps-timer) 1000))))
                                          :r 255 :g 255 :b 255)
                 (let ((x-pos (- (/ *screen-width*  2) (floor width  2)))  ; テキストの表示位置(X座標)計算
                       (y-pos (- (/ *screen-height* 2) (floor height 2)))) ; テキストの表示位置(Y座標)計算
                   (sdl2:render-copy renderer
                                     texture
                                     :dest-rect (sdl2:make-rect x-pos y-pos width height))))

               ;; 遅延処理
               (let ((time (timer-get-ticks cap-timer)))
                 (when (< time tick-per-frame)
                   (sdl2:delay (floor (- tick-per-frame time)))))

               ;; フレーム数をインクリメント
               (frame-incf frames)

               ;; レンダリングの結果を画面に反映
               (sdl2:render-present renderer))
        ;; 終了イベント
        (:quit () t)))))
