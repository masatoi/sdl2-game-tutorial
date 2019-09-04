(defpackage #:sdl2-game-tutorial/fps
  (:use #:cl)
  (:export #:fps
           #:fps-start-ticks
           #:fps-pause-ticks
           #:fps-started
           #:fps-paused
           #:timer-start
           #:timer-stop
           #:timer-pause
           #:timer-resume
           #:timer-get-ticks))
(in-package #:sdl2-game-tutorial/fps)

(defclass fps ()
  ((start-ticks
    :accessor fps-start-ticks
    :initform 0)
   (pause-ticks
    :accessor fps-pause-ticks
    :initform 0)
   (started
    :type     boolean
    :accessor fps-started
    :initform nil)
   (paused
    :type     boolean
    :accessor fps-paused
    :initform nil)))

(defmethod timer-start ((obj fps))
  (with-slots (start-ticks pause-ticks started paused) obj
    (setf start-ticks (sdl2:get-ticks))
    (setf pause-ticks 0)
    (setf started t)
    (setf paused nil)))

(defmethod timer-stop ((obj fps))
  (with-slots (start-ticks pause-ticks started paused) obj
    (setf start-ticks 0)
    (setf pause-ticks 0)
    (setf started nil)
    (setf paused nil)))

(defmethod timer-pause ((obj fps))
  (with-slots (start-ticks pause-ticks started paused) obj
    (when (and started (not paused))
      (setf start-ticks 0)
      (setf pause-ticks (- (sdl2:get-ticks) start-ticks))
      (setf paused t))))

(defmethod timer-resume ((obj fps))
  (with-slots (start-ticks pause-ticks started paused) obj
    (when (and started paused)
      (setf start-ticks (- (sdl2:get-ticks) pause-ticks))
      (setf pause-ticks 0)
      (setf paused nil))))

(defmethod timer-get-ticks ((obj fps))
  (with-slots (start-ticks pause-ticks started paused) obj
    (if started
        (if paused
            pause-ticks
            (- (sdl2:get-ticks) start-ticks))
        0)))
