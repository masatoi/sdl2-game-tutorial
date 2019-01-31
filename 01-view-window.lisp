;;; 01：ウィンドウを表示する

(defpackage :sdl2-game-tutorial/01-view-window
  (:use :cl)
  (:import-from :sdl2)
  (:import-from :sdl2-image)
  (:import-from :sdl2-ttf)
  (:export :main))
(in-package :sdl2-game-tutorial/01-view-window)

;; ウィンドウのサイズ
(defconstant +screen-width+  640) ; 幅
(defconstant +screen-height+ 480) ; 高さ

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

             ;; ここでレンダリング処理
             
             (sdl2:render-present renderer))                 ; レンダリングの結果を画面に反映
      ;; 終了イベント
      (:quit () t))))

;; main関数を呼び出して実行
(main)
