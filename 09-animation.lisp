;;; 09：アニメーション

(defpackage #:sdl2-game-tutorial/09-animation
  (:use #:cl
        #:sdl2-game-tutorial/utils)
  (:import-from #:sdl2-game-tutorial/fps
                #:fps
                #:timer-start
                #:timer-get-ticks)
  (:export #:main))
(in-package #:sdl2-game-tutorial/09-animation)

;; 画像ファイルへのパス
(defparameter *img-player* (expand-path "Material/char-obj-chip/player.png"))
(defparameter *img-trap*   (expand-path "Material/char-obj-chip/trap.png"))

(defun tex-render (renderer texture x y clip)
  (sdl2:render-copy renderer
                    texture
                    :source-rect clip
                    :dest-rect (sdl2:make-rect x y
                                               (sdl2:rect-width clip)
                                               (sdl2:rect-height clip))))

(defun main ()
  (with-window-renderer (window renderer "SDL2 Tutorial 09")
    (let* ((player-texture nil) (player-width 0) (player-height 0)
           (trap-texture nil) (trap-width 0) (trap-height 0)
           ;; 3つ並んだ絵の内、左の絵を選択
           ;; 歩行グラフィックは、縦横32×32
           ;; マップ配置アニメオブジェクトは、縦横48×48
           (clip1          (sdl2:make-rect   0   0 32 32)) ; 歩行グラフィック(下向き)
           (clip2          (sdl2:make-rect   0  32 32 32)) ; 歩行グラフィック(左向き)
           (clip3          (sdl2:make-rect   0  64 32 32)) ; 歩行グラフィック(右向き)
           (clip4          (sdl2:make-rect   0  96 32 32)) ; 歩行グラフィック(上向き)
           (tclip1         (sdl2:make-rect   0   0 48 48)) ; マップ配置アニメオブジェクト(煙1)
           (tclip2         (sdl2:make-rect   0  48 48 48)) ; マップ配置アニメオブジェクト(煙2)
           (tclip3         (sdl2:make-rect   0  96 48 48)) ; マップ配置アニメオブジェクト(煙3)
           (tclip4         (sdl2:make-rect   0 144 48 48)) ; マップ配置アニメオブジェクト(爆発)
           (tclip5         (sdl2:make-rect 144   0 48 48)) ; マップ配置アニメオブジェクト(炎)
           (tclip6         (sdl2:make-rect 144  48 48 48)) ; マップ配置アニメオブジェクト(噴水)
           (tclip7         (sdl2:make-rect 144  96 48 48)) ; マップ配置アニメオブジェクト(竜巻)
           (tclip8         (sdl2:make-rect 144 144 48 48)) ; マップ配置アニメオブジェクト(光)
           ;; アニメーション用変数
           (current-sprite-frame 1) ; 現在表示している絵 (0:左, 1:真ん中, 2:右)
           (prev-sprite-frame 0)    ; 一つ前に表示していた絵 (0:左, 1:真ん中, 2:右)
           ;; FPS用変数
           (fps-timer      (make-instance 'fps))
           (cap-timer      (make-instance 'fps))
           (fixed-fps      60)
           (tick-per-frame (floor 1000 fixed-fps))
           (frames         1))
      (multiple-value-bind (texture width height)
          (create-image-texture renderer *img-player*)
        (setf player-texture texture
              player-width width
              player-height height))
      (multiple-value-bind (texture width height)
          (create-image-texture renderer *img-trap*)
        (setf trap-texture texture
              trap-width width
              trap-height height))
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
               ;; 歩行グラフィック表示
               (tex-render renderer player-texture 240 200 clip1)
               (tex-render renderer player-texture 272 200 clip2)
               (tex-render renderer player-texture 304 200 clip3)
               (tex-render renderer player-texture 336 200 clip4)

               ;; マップ配置アニメオブジェクト表示
               (tex-render renderer trap-texture 180 150 tclip1)
               (tex-render renderer trap-texture 130 188 tclip2)
               (tex-render renderer trap-texture 130 246 tclip3)
               (tex-render renderer trap-texture 180 280 tclip4)
               (tex-render renderer trap-texture 380 150 tclip5)
               (tex-render renderer trap-texture 430 188 tclip6)
               (tex-render renderer trap-texture 430 246 tclip7)
               (tex-render renderer trap-texture 380 280 tclip8)

               ;; 遅延処理
               (let ((time (timer-get-ticks cap-timer)))
                 (when (< time tick-per-frame)
                   (sdl2:delay (floor (- tick-per-frame time)))))

               ;; フレーム数をインクリメント
               (frame-incf frames)

               ;; アニメーション更新
               ;; 真ん中からスタートし、右➔真ん中➔左➔真ん中➔右➔真ん中・・・
               (when (zerop (rem frames tick-per-frame))
                 (cond ((or (= current-sprite-frame 2) (= current-sprite-frame 0))
                        (setf prev-sprite-frame current-sprite-frame)
                        (setf current-sprite-frame 1))
                       ((= prev-sprite-frame 2) (setf current-sprite-frame 0))
                       ((= prev-sprite-frame 0) (setf current-sprite-frame 2)))

                 (setf (sdl2:rect-x clip1) (* current-sprite-frame (sdl2:rect-width clip1)))
                 (setf (sdl2:rect-x clip2) (* current-sprite-frame (sdl2:rect-width clip2)))
                 (setf (sdl2:rect-x clip3) (* current-sprite-frame (sdl2:rect-width clip3)))
                 (setf (sdl2:rect-x clip4) (* current-sprite-frame (sdl2:rect-width clip4)))

                 (setf (sdl2:rect-x tclip1) (* current-sprite-frame (sdl2:rect-width tclip1)))
                 (setf (sdl2:rect-x tclip2) (* current-sprite-frame (sdl2:rect-width tclip2)))
                 (setf (sdl2:rect-x tclip3) (* current-sprite-frame (sdl2:rect-width tclip3)))
                 (setf (sdl2:rect-x tclip4) (* current-sprite-frame (sdl2:rect-width tclip4)))

                 (setf (sdl2:rect-x tclip5) (+ (* current-sprite-frame (sdl2:rect-width tclip5)) 144))
                 (setf (sdl2:rect-x tclip6) (+ (* current-sprite-frame (sdl2:rect-width tclip6)) 144))
                 (setf (sdl2:rect-x tclip7) (+ (* current-sprite-frame (sdl2:rect-width tclip7)) 144))
                 (setf (sdl2:rect-x tclip8) (+ (* current-sprite-frame (sdl2:rect-width tclip8)) 144)))

               ;; レンダリングの結果を画面に反映
               (sdl2:render-present renderer))
        ;; 終了イベント
        (:quit () t)))))
