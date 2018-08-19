(defclass class-selectwin ()
  ((syswin-tex
    :initarg  :syswin-tex
    :initform (error "Must supply a syswin-tex"))
   (cursor-tex
    :initarg  :cursor-tex
    :initform (error "Must supply a cursor-tex"))
   (font
    :initarg  :font
    :initform (error "Must supply a font"))
   (menu
    :initarg  :menu
    :initform nil)
   (max-str-len
    :initarg  :max-str-len
    :initform 0)
   (menu-count
    :initarg  :menu-count
    :initform 0)
   (cursor-x
    :initarg  :cursor-x
    :initform 0)
   (cursor-y
    :initarg  :cursor-y
    :initform 0)))

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

(defmethod select-window (obj renderer)
  (with-slots (syswin-tex cursor-tex font menu max-str-len menu-count cursor-x cursor-y) obj
    (let* ((width    (* max-str-len 30))
           (height   (* menu-count 40))
           (syswin-x (- 320 (/ width  2)))
           (syswin-y (- 240 (/ height 2)))
           (str-x    (+ syswin-x 10))
           (str-y    (+ syswin-y 10))
           (cur-w    width)
           (cur-h    40)
           (cur-x    (+ syswin-x (* cursor-x cur-w)))
           (cur-y    (+ syswin-y (* cursor-y cur-h))))
      
      ;; ベースウィンドウ表示
      (system-window-render syswin-tex syswin-x syswin-y width height)

      ;; 文字列表示
      (dotimes (n menu-count)
        (tex-render (aref menu n) str-x str-y)
        (setf str-y (+ str-y 40)))

      ;; カーソル表示
      (system-window-render cursor-tex cur-x cur-y cur-w cur-h :alpha 100))))
