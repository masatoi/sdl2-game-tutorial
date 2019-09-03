(defpackage #:sdl2-game-tutorial/utils
  (:use #:cl)
  (:export #:with-window-renderer
           #:*screen-width*
           #:*screen-height*))
(in-package #:sdl2-game-tutorial/utils)

;; ウィンドウのサイズ
(defparameter *screen-width*  640) ; 幅
(defparameter *screen-height* 480) ; 高さ

;; SDL2ライブラリ初期化＆終了処理
(defmacro with-window-renderer ((window renderer title) &body body)
  ;; SDLの初期化と終了時の処理をまとめて実行
  `(sdl2:with-init (:video)
     ;; ウィンドウ作成処理を実行
     (sdl2:with-window (,window
                        :title ,title       ; タイトル
                        :w ,*screen-width*  ; 幅
                        :h ,*screen-height* ; 高さ
                        :flags '(:shown))   ; :shownや:hiddenなどのパラメータを設定できる
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
         ;; sdl2-imageを初期化(扱う画像形式はPNG ※他にもJPGとTIFが使える)
         (sdl2-image:init '(:png))
         ;; sdl2-ttfを初期化
         (sdl2-ttf:init)
         ,@body
         ;; sdl2-image終了処理
         (sdl2-image:quit)
         ;; sdl2-ttf終了処理
         (sdl2-ttf:quit)))))
