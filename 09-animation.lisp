;;; 09：アニメーション

;; SDL2ライブラリのロード
(ql:quickload :sdl2)         ; SDL2ライブラリ
(ql:quickload :sdl2-image)   ; 画像ファイル読み込み、描画関連のライブラリ
(ql:quickload :sdl2-ttf)     ; フォントの描画関連のライブラリ

;; 外部ファイルをロード
(load "GameUtility/texture.lisp"   :external-format :utf-8)
(load "GameUtility/fps-timer.lisp" :external-format :utf-8)

;; ウィンドウのサイズ
(defconstant +screen-width+  640) ; 幅
(defconstant +screen-height+ 480) ; 高さ

;; 画像ファイルへのパス
(defparameter *img-player* "Material/char-obj-chip/player.png")
(defparameter *img-trap*   "Material/char-obj-chip/trap.png")

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
    (let* ((player-img     (tex-load-from-file renderer *img-player*))
           (trap-img       (tex-load-from-file renderer *img-trap*))
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
               (tex-render player-img 240 200 :clip clip1)
               (tex-render player-img 272 200 :clip clip2)
               (tex-render player-img 304 200 :clip clip3)
               (tex-render player-img 336 200 :clip clip4)

               ;; マップ配置アニメオブジェクト表示
               (tex-render trap-img 180 150 :clip tclip1)
               (tex-render trap-img 130 188 :clip tclip2)
               (tex-render trap-img 130 246 :clip tclip3)
               (tex-render trap-img 180 280 :clip tclip4)
               (tex-render trap-img 380 150 :clip tclip5)
               (tex-render trap-img 430 188 :clip tclip6)
               (tex-render trap-img 430 246 :clip tclip7)
               (tex-render trap-img 380 280 :clip tclip8)

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

;; main関数を呼び出して実行
(main)
