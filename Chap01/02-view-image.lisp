;;; 02：画像を表示する

;; SDL2ライブラリのロード
(ql:quickload :sdl2)         ; SDL2ライブラリ
(ql:quickload :sdl2-image)   ; 画像ファイル読み込み、描画関連のライブラリ
(ql:quickload :sdl2-ttf)     ; フォントの描画関連のライブラリ

;; ウィンドウのサイズ
(defconstant +screen-width+  640) ; 幅
(defconstant +screen-height+ 480) ; 高さ

;; 画像ファイルへのパス
(defparameter *image-file-path* "../Material/graphics/picture/cat.png")

;; テクスチャクラス
(defclass class-texture ()
  ((renderer
    :initarg  :renderer
    :initform (error "Must supply a renderer"))
   (width
    :initform 0)
   (height
    :initform 0)
   (texture
    :initform nil)))

;; 画像を読み取る
(defmethod tex-load-from-file (renderer filepath)
  (let ((tex (make-instance 'class-texture :renderer renderer)))
    (with-slots (renderer width height texture) tex
      (let ((surface (sdl2-image:load-image filepath)))
        (setf width  (sdl2:surface-width surface))
        (setf height (sdl2:surface-height surface))
        (sdl2:set-color-key surface :true (sdl2:map-rgb (sdl2:surface-format surface) 0 0 0))
        (setf texture (sdl2:create-texture-from-surface renderer surface))))
    tex))

;; レンダリング処理
(defmethod tex-render (tex x y)
  (with-slots (renderer width height texture) tex
    (sdl2:render-copy renderer texture :dest-rect (sdl2:make-rect x y width height))))

;; SDL2ライブラリ初期化＆終了処理
(defmacro with-window-renderer ((window renderer) &body body)
  ;; SDLの初期化と終了時の処理をまとめて実行
  `(sdl2:with-init (:video)
     ;; ウィンドウ作成処理を実行
     (sdl2:with-window (,window
                        :title "SDL2 Tutorial 01" ; タイトル
                        :w     +screen-width+     ; 幅
                        :h     +screen-height+    ; 高さ
                        :flags '(shown))          ; :shownや:hiddenなどのパラメータを設定できる
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
    ;; 画像ファイル読み込み、画像情報の取得などを行う
    (let ((img-tex (tex-load-from-file renderer *image-file-path*)))
      ;; イベントループ(この中にキー操作時の動作や各種イベントを記述していく)
      (sdl2:with-event-loop (:method :poll)
        ;; キーが押下されたときの処理
        (:keydown (:keysym keysym)
                  ;; keysymをスキャンコードの数値(scancode-value)に変換して、キー判定処理(scancode=)を行う
                  (if (sdl2:scancode= (sdl2:scancode-value keysym) :scancode-escape)
                      (sdl2:push-event :quit))) ; Escキーが押下された場合、quitイベントをキューに加える
        ;; この中に描画処理など各種イベントを記述していく
        (:idle ()
               (sdl2:set-render-draw-color renderer 0 0 0 255) ; 作図操作(矩形、線、およびクリア)に使用する色を設定
               (sdl2:render-clear renderer)                    ; 現在のレンダーターゲットを上記で設定した色で塗りつぶして消去

               ;; レンダリング処理
               (with-slots (renderer width height texture) img-tex
                 (let ((x-pos   (- (/ +screen-width+  2) (floor width  2)))          ; テキストの表示位置(X座標)計算
                       (y-pos   (- (/ +screen-height+ 2) (floor height 2))))         ; テキストの表示位置(Y座標)計算
                   (tex-render img-tex x-pos y-pos)))
               
               (sdl2:render-present renderer))                 ; レンダリングの結果を画面に反映
        ;; 終了イベント
        (:quit () t)))))

;; main関数を呼び出して実行
(main)
