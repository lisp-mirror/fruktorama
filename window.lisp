;; window.lisp
;; Copyright Parasite Network 2018
;; GPL3

(defstruct window
  "The WINDOW is the root container for all widgets within a layer."
  widget
  active
  id
  internal-id
  internal-stack-pos ;; MAKE STACK ARRAY ADJUSTABLE AND PUSH AND SET INDEX
  visible
  group
  callback-open
  callback-close
  callback-input
  todo-paint-cache)

(defparameter *WINDOWS* nil
  "Defined windows.")

(defparameter *WINDOW-INPUT-STACK* nil
  "Windows sorted in input order.")

(defparameter *WINDOW-PAINT-STACK* nil
  "Windows sorted in paint order.")

(defparameter *NAMED-WINDOWS* nil
  "Maps identifiers to windows.")

(defparameter *WINDOW-GROUPS* nil
  "Maps identifiers to window groups.")

(defparameter *WINDOW-WIDTH* nil
  "Maximum window width.")

(defparameter *WINDOW-HEIGHT* nil
  "Maximum window height.")

(defparameter *DEBUG-WIDGET-BORDER* nil
  "When set will cause borders to be painted around widgets.")

; (INT, INT) -> NIL
(defun initialize-windows (width height)
  (setf *WINDOWS* (make-hash-table :test #'eq))
  (setf *WINDOW-PAINT-STACK* nil)
  (setf *WINDOW-INPUT-STACK* nil)
  (setf *NAMED-WINDOWS* (make-hash-table :test #'eq))
  (setf *WINDOW-GROUPS* (make-hash-table :test #'eq))
  (setf *DEBUG-WIDGET-BORDER* nil)
  (setf *WINDOW-WIDTH* width)
  (setf *WINDOW-HEIGHT* height)
  (setf *MOUSE-MOVEMENT-TRACKER* nil)
  (setf *MOUSE-CLICK-VECTOR* (make-array (list 5) :initial-element nil))
  nil)

; (INT, INT) -> INT
(defun calc-offset-within (container child)
  (floor (/ (- container child) 2.0)))

; WINDOW -> NIL
(defun center-window-widget (window)
  "Puts the window's widget in the center. Called because :PLACE :CENTER."
  (let ((widget (window-widget window)))
    (let ((x (calc-offset-within *WINDOW-WIDTH* (widget-width widget)))
          (y (calc-offset-within *WINDOW-HEIGHT* (widget-height widget))))
      (setf (widget-x widget) x)
      (setf (widget-y widget) y)
      nil)))

; WINDOW -> NIL
(defun activate (window)
  "Activates input events on the window."
  (setf (window-active window) t)
  nil)

; WINDOW -> NIL
(defun deactivate (window)
  "Deactivates input events on the window."
  (setf (window-active window) nil)
  nil)

; WINDOW -> NIL
(defun push-window (window)
  "Pushes a window onto the stacks and register its id for lookup."
  (push window *WINDOW-INPUT-STACK*)
  (setf *WINDOW-PAINT-STACK* (append *WINDOW-PAINT-STACK* (list window)))
  (when (window-id window)
    (setf (gethash (window-id window) *named-windows*) window))
  nil)

; (WINDOW-ID &KEY) -> WINDOW | NIL
(defun find-window (id &key on-failure-nil) 
  "Tries to find the window."
  (let ((window (gethash id *named-windows*)))
    (if window
        window
        (unless on-failure-nil
          (error "FIND-WINDOW failed to find the window: ~A." id)))))

; WINDOW-ID -> WINDOW
(defun open-window-by-id (id)
  "Opens a window by its identifier."
  (let ((window (find-window id)))
    (open-window window)))

; WINDOW -> WINDOW
(defun open-window (window)
  "Opens a window by its object."
  (format t "Opening window: ~A.~%" (window-id window))    
  (let ((callback (window-callback-open window)))
    (when callback
      (funcall callback window)))
  (setf (window-visible window) t)
  (let ((widget (window-widget window)))
    (when widget
      (widget-propagate-open widget)))
  window)

; WINDOW-ID -> WINDOW
(defun close-window-by-id (id)
  "Closes a window by its identifier."
  (let ((window (find-window id)))
    (close-window window)))

; WINDOW -> WINDOW
(defun close-window (window)
  "Closes a window by its object."
  (format t "Closing window: ~A.~%" (window-id window))
  (let ((callback (window-callback-close window)))
    (when callback
      (funcall callback window)))
  (setf (window-visible window) nil)
  (let ((widget (window-widget window)))
    (when widget
      (widget-event-close widget)))
  window)

; (GROUP-ID, WINDOW) -> WINDOW
(defun add-window-to-group (group window)
  (format t "Adding ~A to group ~A.~%" (window-id window) group)
  (let ((windows (gethash group *WINDOW-GROUPS*)))
    (push window windows)
    (setf (gethash group *WINDOW-GROUPS*) windows)))

; GROUP-ID -> NIL
(defun open-window-group (group) 
  (let ((windows (gethash group *WINDOW-GROUPS*)))
    (format t "Opening group ~A with ~A.~%" group
            (loop for w in windows collect (window-id w)))
    (dolist (window windows nil)
      (open-window window))))

; GROUP-ID -> NIL
(defun close-window-group (group)
  (let ((windows (gethash group *WINDOW-GROUPS*)))
    (format t "Closing group ~A with ~A.~%" group
            (loop for w in windows collect (window-id w)))
    (dolist (window windows nil)
      (close-window window))))

; (SDL2:RENDERER, INT) -> NIL
(defun paint-windows (renderer tick)
  "Iterates through the paint stack and paints each window."
  (dolist (window *WINDOW-PAINT-STACK* nil)
    (when (window-visible window)
      (let ((widget (window-widget window)))
        (when widget
          (widget-propagate-paint widget renderer tick))))))

; SDL2:RENDERER -> NIL
(defun paint-debug-borders (renderer) 
  "Paints the widgets' borders for debugging."
  (dolist (window *WINDOW-PAINT-STACK* nil)
    (when (window-visible window)
      (let ((widget (window-widget window)))
        (paint-widget-debug-border widget renderer)))))

; () -> NIL
(defun initialize-window-widgets ()
  (dolist (window *WINDOW-INPUT-STACK* nil)
    (format t "Initializing window: ~A.~%" (window-id window))
    (let ((widget (window-widget window)))
      (when widget
        (setf (widget-window widget) (window-internal-id window))
        (widget-propagate-init widget)))))

(defun get-window-widget-xy (window x y)
  (when (window-visible window)
    (when (window-active window)
      (get-widget-at (window-widget window) x y))))

(defun get-widget-xy-in-stack (stack x y)
  (some (lambda (window)
          (get-window-widget-xy window x y))
        stack))

(defun get-widget-xy (x y)
  (get-widget-xy-in-stack *WINDOW-INPUT-STACK* x y))

(defun get-widget-xy-below (window x y)
  (let ((pos (position window *WINDOW-INPUT-STACK*)))
    (if pos
        (get-widget-xy-in-stack (nthcdr (1+ pos) *WINDOW-INPUT-STACK*) x y)
        (get-widget-xy-in-stack *WINDOW-INPUT-STACK* x y))))

; (&KEY) -> NIL
;; FIXME SHOULD BE A MACRO
(defun defwindow% (id &key
                     (active nil) 
                     (visible nil) 
                     (widget nil) 
                     (place '(0 0)) 
                     (trap-input nil) 
                     (trap-open nil) 
                     (trap-close nil)
                     (group nil))
  "Creates a window, pushes it on the stack and register it name."
  (let ((window (make-window
             :id id
             :internal-id (gensym)
             :widget widget
             :active active
             :visible visible
             :group group
             :callback-input trap-input
             :callback-open trap-open
             :callback-close trap-close)))
    
    ; A window can be placed either with an offset or at the center.
    ; PLACE is either :CENTER or a list (X Y).
    (when widget
      (typecase place
                (keyword
                  (unless (eq place :center)
                    (error "The placement designator ~A is not recognized." place))
                  (center-window-widget window))
                (list
                  (destructuring-bind (x y) place
                                      (setf (widget-x widget) x)
                                      (setf (widget-y widget) y)))
                (otherwise
                  (error "Unknown placement designator ~A." place)))
      (calculate-absolute-coordinates-in-window window)
      (register-widgets widget))
    (when group
      (add-window-to-group group window))
    (push-window window)
    (when visible
      (open-window window))
    (setf (gethash (window-internal-id window) *WINDOWS*) window)
    nil))

(defmacro defwindow (id &key
                        (active nil) 
                        (visible nil) 
                        (widget nil) 
                        (place '(0 0)) 
                        (trap-input nil) 
                        (trap-open nil) 
                        (trap-close nil)
                        (group nil)
                        (disabled nil))
  (unless disabled
    `(progn
       (format t "Creating window: ~A.~%" ,id)
       (defwindow% ,id 
                   :active ,active
                   :visible ,visible
                   :widget ,widget
                   :place (quote ,place)
                   :trap-open ,trap-open
                   :trap-close ,trap-close
                   :group ,group)
       (format t "Done.~%~%"))))

;;------------------------------------------------------------------------------



(defun calculate-absolute-coordinates-in-window (window)
  "Traverses the widget tree and calculates all widgets absolute positions.
  ALSO SETS THE WINDOW PROPERTY ON EACH WIDGET" 
  (format t "Calculating absolute xy.~%") 
  (widget-propagate-calculate-xy (window-widget window)))

(defun register-widgets (widget)
  "Traverses the widget tree and calls REGISTER-WIDGET on each widget."
  (when widget
    (register-widget widget)
    (each-widget-child widget child
                       (register-widgets child))))

;; (KEY) -> T | NIL
(defun window-event-onkey-down (key)
  (let ((handled (bubble-key key)))
    (unless handled
      (format t "   KEY DISCARDED.~%"))
    handled))

;; (KEY) -> T | NIL
(defun bubble-key (key)
  (some (lambda (window)
          (when (window-visible window)
            (when (window-active window)
              (format t "Key given to window: ~A.~%" (window-id window))
              (bubble-into key window))))
        *WINDOW-INPUT-STACK*))

(defun bubble-into (key window)
  (if (bubble-callback key window)
      t
      (when (window-widget window)
        (widget-propagate-onkey-down (window-widget window) key))))

(defun bubble-callback (key window)
  (when (window-callback-input window)
    (when (funcall (window-callback-input window) window key)
      (format t "Key handled by window trap: ~A.~%" (window-id window))
      t)))