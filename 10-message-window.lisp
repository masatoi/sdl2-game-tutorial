;;; 10：メッセージウィンドウ

(defpackage :sdl2-game-tutorial/10-message-window
  (:use #:cl
        #:sdl2-game-tutorial/utils)
  (:export :main))
(in-package :sdl2-game-tutorial/10-message-window)

;; 画像ファイルへのパス
(defparameter *img-syswin* (expand-path "Material/graphics/system/systemwindow.png"))
(defparameter *img-pause*  (expand-path "Material/graphics/system/text-pause.png"))

;; フォントファイルへのパス
(defparameter *font-file-path* (expand-path "Material/fonts/ipaexg.ttf"))

(defun main ()
  (with-window-renderer (window renderer "SDL2 Tutorial 10")
    ;; 画像ファイル読み込み、画像情報の取得などを行う
    (let* ((msg-window     (make-instance 'class-msgwin
                                          :syswin-tex (tex-load-from-file renderer *img-syswin*)
                                          :pause-tex  (tex-load-from-file renderer *img-pause*)
                                          :pause-clip (sdl2:make-rect 0 0 30 16)
                                          :font       (sdl2-ttf:open-font *font-file-path* 20)))
           ;; メッセージ処理
           (event-flg      nil)
           (text-count     0)
           ;; FPS用変数
           (fps-timer      (make-instance 'fps-timer))
           (cap-timer      (make-instance 'fps-timer))
           (fixed-fps      60)
           (tick-per-frame (floor 1000 fixed-fps))
           (frames         0))

      ;; テキストファイルからテキストを読み込み配列へ格納する
      (load-text)
      (timer-start fps-timer)
      
      ;; イベントループ(この中にキー操作時の動作や各種イベントを記述していく)
      (sdl2:with-event-loop (:method :poll)
        ;; キーが押下されたときの処理
        (:keydown (:keysym keysym)
                  ;; keysymをスキャンコードの数値(scancode-value)に変換して、キー判定処理(scancode=)を行う
                  (if (sdl2:scancode= (sdl2:scancode-value keysym) :scancode-escape)
                      ;; Escキーが押下された場合、quitイベントをキューに加える
                      (sdl2:push-event :quit)
                      ;; その他のキー入力処理
                      (progn
                        (case (sdl2:scancode keysym)
                          (:scancode-z (progn (if event-flg
                                                  (if (< text-count (- *max-text-num* 2))
                                                      (incf text-count)
                                                      (progn (setf text-count 0) (setf event-flg nil)))
                                                  (setf event-flg t))))
                          (:scancode-x (progn (setf event-flg nil)
                                              (setf text-count 0)))))))
        ;; この中に描画処理など各種イベントを記述していく
        (:idle ()
               (timer-start cap-timer)
               
               ;; 作図操作(矩形、線、およびクリア)に使用する色を設定
               (sdl2:set-render-draw-color renderer 0 0 0 255)
               ;; 現在のレンダーターゲットを上記で設定した色で塗りつぶして消去
               (sdl2:render-clear renderer)

               ;; レンダリング処理
               (when event-flg
                 (msg-view msg-window renderer frames tick-per-frame text-count
                           :1st (aref *text-message-test* text-count 0)
                           :2nd (aref *text-message-test* text-count 1)
                           :3rd (aref *text-message-test* text-count 2)))
               
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
