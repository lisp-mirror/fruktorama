;; mouse.lisp
;; Copyright Parasite Network 2018
;; GPL3

(defparameter *MOUSE-MOVEMENT-TRACKER* nil)
(defparameter *MOUSE-CLICK-VECTOR* nil)

(defconstant !MOUSE-LEAVE! 11)

; (INT, INT) -> NIL
(defun glas-initialize-mouse-defaults ()
  (setf *MOUSE-MOVEMENT-TRACKER* nil)
  (setf *MOUSE-CLICK-VECTOR* (make-array (list 5) :initial-element nil))
  nil)

(defun dispatch-mouse-leave-event (widget)
  (widget-event-mouse-leave widget))

(defun dispatch-mouse-enter-event (widget)
  (widget-event-mouse-enter widget))

(defun dispatch-mouse-movement-event (widget x y)
  (widget-event-mouse-movement widget x y))

; (INT,INT) -> NIL
(defun track-mouse-movement (x y)
  (let ((widget (search-stack-xy x y)))
    (when (and widget (not (widget-mouse-movement-awareness widget)))
      (setf widget nil))
    (if (eq *MOUSE-MOVEMENT-TRACKER* widget)
        (dispatch-mouse-movement-event widget x y)
        (progn
          (when *MOUSE-MOVEMENT-TRACKER*
            (dispatch-mouse-leave-event *MOUSE-MOVEMENT-TRACKER*))
          (setf *MOUSE-MOVEMENT-TRACKER* widget)
          (when *MOUSE-MOVEMENT-TRACKER*
            (dispatch-mouse-enter-event *MOUSE-MOVEMENT-TRACKER*)
            nil)))))

(defun track-mouse-button-down (button x y)
  (let ((widget (search-stack-xy x y)))
    (when (and widget (widget-mouse-click-awareness widget))
      (let ((which (truncate (log button 2))))
        (setf (aref *MOUSE-CLICK-VECTOR* which) widget)
        (dispatch-mouse-botton-down widget which x y)))))

(defun track-mouse-button-up (button x y)
  (let ((widget (search-stack-xy x y)))
    (when (and widget (not (widget-mouse-click-awareness widget)))
      (setf widget nil))
    (let ((which (truncate (log button 2))))
      (if (eq widget (aref *MOUSE-CLICK-VECTOR* which))
          (progn
            (setf (aref *MOUSE-CLICK-VECTOR* which) nil)
            (dispatch-mouse-botton-up widget which x y))
          (progn
            (dispatch-mouse-button-cancel (aref *MOUSE-CLICK-VECTOR* which) which x y)
            (setf (aref *MOUSE-CLICK-VECTOR* which) nil))))))

(defun dispatch-mouse-button-cancel (widget button x y)
  (ecase button
         (0
           (widget-event-mouse-left-click-cancel widget x y)
           (widget-event-mouse-click-cancel widget x y button))
         (1
           (widget-event-mouse-middle-click-cancel widget x y)
           (widget-event-mouse-click-cancel widget x y button))
         (2
           (widget-event-mouse-right-click-cancel widget x y)
           (widget-event-mouse-click-cancel widget x y button))
         (3
           (widget-event-mouse-click-cancel widget x y button))
         (4
           (widget-event-mouse-click-cancel widget x y button))))

(defun dispatch-mouse-botton-down (widget button x y)
  (ecase button
         (0
           (widget-event-mouse-left-click-down widget x y)
           (widget-event-mouse-click-down widget x y button)
           (widget-event-mouse-click widget x y button :down))
         (1
           (widget-event-mouse-middle-click-down widget x y)
           (widget-event-mouse-click-down widget x y button)
           (widget-event-mouse-click widget x y button :down))
         (2
           (widget-event-mouse-right-click-down widget x y)
           (widget-event-mouse-click-down widget x y button)
           (widget-event-mouse-click widget x y button :down))
         (3
           (widget-event-mouse-click-down widget x y button)
           (widget-event-mouse-click widget x y button :down))
         (4
           (widget-event-mouse-click-down widget x y button)
           (widget-event-mouse-click widget x y button :down))))

(defun dispatch-mouse-botton-up (widget button x y)
  (ecase button
         (0
           (widget-event-mouse-left-click-up widget x y)
           (widget-event-mouse-click-up widget x y button)
           (widget-event-mouse-click widget x y button :up))
         (1
           (widget-event-mouse-middle-click-up widget x y)
           (widget-event-mouse-click-up widget x y button)
           (widget-event-mouse-click widget x y button :up))
         (2
           (widget-event-mouse-right-click-up widget x y)
           (widget-event-mouse-click-up widget x y button)
           (widget-event-mouse-click widget x y button :up))
         (3
           (widget-event-mouse-click-up widget x y button)
           (widget-event-mouse-click widget x y button :up))
         (4
           (widget-event-mouse-click-up widget x y button)
           (widget-event-mouse-click widget x y button :up))))