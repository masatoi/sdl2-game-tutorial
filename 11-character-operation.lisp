;;; 11：キャラクター操作

(defpackage :sdl2-game-tutorial/11-character-operation
  (:use :cl)
  (:import-from :sdl2)
  (:import-from :sdl2-image)
  (:import-from :sdl2-ttf)
  (:import-from :sdl2-game-tutorial/utils :expand-path :load-file)
  (:export :main))
(in-package :sdl2-game-tutorial/11-character-operation)

;; 外部ファイルをロード
(load-file #P"GameUtility/fps-timer.lisp")
(load-file #P"GameUtility/map-field.lisp")
(load-file #P"GameUtility/character.lisp")

;; ウィンドウのサイズ
(defconstant +screen-width+  640) ; 幅
(defconstant +screen-height+ 480) ; 高さ

;; 画像ファイルへのパス
(defparameter *img-player* (expand-path "Material/char-obj-chip/player.png"))

;; フレーム数インクリメント
(defmacro frame-incf (frame)
  `(if (= ,frame most-positive-fixnum)
       (setf ,frame 1)
       (incf ,frame)))

;; SDL2ライブラリ初期化＆終了処理
(defmacro with-window-renderer ((window renderer) &body body)
  ;; SDLの初期化と終了時の処理をまとめて実行
  `(sdl2:with-init (:video)
     ;; ウィンドウ作成処理を実行
     (sdl2:with-window (,window
                        :title "SDL2 Tutorial 01" ; タイトル
                        :w     +screen-width+     ; 幅
                        :h     +screen-height+    ; 高さ
                        :flags '(:shown))         ; :shownや:hiddenなどのパラメータを設定できる
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
         (sdl2-image:init '(:png)) ; sdl2-imageを初期化(扱う画像形式はPNG ※他にもJPGとTIFが使える)
         (sdl2-ttf:init)           ; sdl2-ttfを初期化
         ,@body
         (sdl2-image:quit)         ; sdl2-image終了処理
         (sdl2-ttf:quit)))))       ; sdl2-ttf終了処理

(defun main ()
  (with-window-renderer (window renderer)
    (let* ((player-img  (tex-load-from-file renderer *img-player*))
           (player-char (make-instance 'class-character
                                       :clip    (sdl2:make-rect 0 0 32 32)
                                       :location *null-map*))
           ;; アニメーション用変数
           (cur-sprite-frame  1) ; 現在表示している絵 (0:左, 1:真ん中, 2:右)
           (prev-sprite-frame 0) ; 一つ前に表示していた絵 (0:左, 1:真ん中, 2:右)
           ;; FPS用変数
           (fps-timer      (make-instance 'fps-timer))
           (cap-timer      (make-instance 'fps-timer))
           (fixed-fps      60)
           (tick-per-frame (floor 1000 fixed-fps))
           (frames         1))

      (timer-start fps-timer)
      
      ;; イベントループ(この中にキー操作時の動作や各種イベントを記述していく)
      (sdl2:with-event-loop (:method :poll)
        ;; キーが押下されたときの処理
        (:keydown (:keysym keysym)
                  ;; keysymをスキャンコードの数値(scancode-value)に変換して、キー判定処理(scancode=)を行う
                  (if (sdl2:scancode= (sdl2:scancode-value keysym) :scancode-escape)
                      (sdl2:push-event :quit)  ; Escキーが押下された場合、quitイベントをキューに加える
                      (chk-key player-char keysym)))
        ;; この中に描画処理など各種イベントを記述していく
        (:idle ()
               (timer-start cap-timer)
               
               ;; 作図操作(矩形、線、およびクリア)に使用する色を設定
               (sdl2:set-render-draw-color renderer 0 0 0 255)
               ;; 現在のレンダーターゲットを上記で設定した色で塗りつぶして消去
               (sdl2:render-clear renderer)
               
               ;; キャラクタ移動
               (move-character   player-char)
               (change-direction player-char)
               
               ;; レンダリング処理
               (with-slots (x-pos y-pos clip) player-char
                 (tex-render player-img x-pos y-pos :clip clip))
               
               ;; 遅延処理
               (let ((time (timer-get-ticks cap-timer)))
                 (when (< time tick-per-frame)
                   (sdl2:delay (floor (- tick-per-frame time)))))

               ;; フレーム数をインクリメント
               (frame-incf frames)

               ;; アニメーション更新
               (with-slots (clip move-flg) player-char
                 (when move-flg
                   (when (zerop (rem frames tick-per-frame))
                     (cond ((or (= cur-sprite-frame 2) (= cur-sprite-frame 0))
                            (setf prev-sprite-frame cur-sprite-frame)
                            (setf cur-sprite-frame 1))
                           ((= prev-sprite-frame 2) (setf cur-sprite-frame 0))
                           ((= prev-sprite-frame 0) (setf cur-sprite-frame 2)))
                     (setf (sdl2:rect-x clip) (* cur-sprite-frame (sdl2:rect-width clip))))))
               
               ;; レンダリングの結果を画面に反映
               (sdl2:render-present renderer))
        ;; 終了イベント
        (:quit () t)))))
