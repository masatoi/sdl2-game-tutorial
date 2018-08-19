(defclass class-selectwin ()
  ((syswin-tex
    :initarg  :syswin-tex
    :initform (error "Must supply a syswin-tex"))
   (cursor-tex
    :initarg  :cursor-tex
    :initform (error "Must supply a cursor-tex"))
   (font
    :initarg  :font
    :initform (error "Must supply a font"))))

(defmethod create-menu-str (obj renderer values)
  (let ((menu-str (make-array 1 :initial-element nil :adjustable t)))
    (with-slots (font) obj
      (dotimes (n (length values))
        (setf (aref menu-str n) (tex-load-from-string renderer font (nth n values)))
        (adjust-array menu-str (+ n 2))))
    menu-str))

(defmethod max-str-length (values)
  (let ((max 0) (len 0))
    (dotimes (n (length values))
      (setf len (length (nth n values)))
      (when (> len max)
        (setf max len)))
    max))

(defmethod select-window (obj renderer menu-arr max-str-len count)
  (with-slots (syswin-tex cursor-tex font) obj
    (let* ((width    (* max-str-len 30))
           (height   (* count 40))
           (syswin-x (- (/ 640 2) (/ width  2)))
           (syswin-y (- (/ 480 2) (/ height 2)))
           (str-x    (+ syswin-x 10))
           (str-y    (+ syswin-y 10)))
      
      ;; ベースウィンドウ表示
      (system-window-render syswin-tex syswin-x syswin-y width height)

      ;; 文字列表示
      (dotimes (n count)
        (tex-render (aref menu-arr n) str-x str-y)
        (setf str-y (+ str-y 40)))

      ;; カーソル表示
      (system-window-render cursor-tex syswin-x syswin-y width 40 :alpha 100))))
