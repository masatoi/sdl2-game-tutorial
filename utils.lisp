(defpackage #:sdl2-game-tutorial/utils
  (:use #:cl)
  (:export #:with-window-renderer
           #:*screen-width*
           #:*screen-height*
           #:create-image-texture
           #:create-string-texture))
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

(defun create-image-texture (renderer
                             image-file-path
                             &key (r 0) (g 0) (b 0))
  (let* ((surface (sdl2-image:load-image image-file-path))
         (width (sdl2:surface-width surface))
         (height (sdl2:surface-height surface)))
    (sdl2:set-color-key surface :true (sdl2:map-rgb (sdl2:surface-format surface) r g b))
    (values (sdl2:create-texture-from-surface renderer surface) width height)))

(defun create-string-texture (renderer
                              font-file-path
                              string
                              &key (r 0) (g 0) (b 0) (a 0))
  (let* ((font (sdl2-ttf:open-font font-file-path 50))
         (surface (sdl2-ttf:render-utf8-solid font string r g b a))
         (width (sdl2:surface-width surface))
         (height (sdl2:surface-height surface))
         (texture (sdl2:create-texture-from-surface renderer surface)))
    (values texture width height)))
