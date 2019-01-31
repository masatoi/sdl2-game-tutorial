(defpackage :sdl2-game-tutorial/app
  (:use :cl)
  (:import-from :sdl2-game-tutorial/01-view-window))
(in-package :sdl2-game-tutorial/app)

;; 01：ウィンドウを表示する
(sdl2-game-tutorial/01-view-window:main)
