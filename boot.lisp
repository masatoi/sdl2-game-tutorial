(defpackage #:sdl2-game-tutorial
  (:use #:cl)
  (:import-from #:sdl2-game-tutorial/01-view-window)
  (:export #:start))
(in-package #:sdl2-game-tutorial)

(defun start (&key id)
  (case id
    (1 (sdl2-game-tutorial/01-view-window:main))
    (t (format t "Hello, SDL2~%"))))
