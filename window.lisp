;; window.lisp
;; Copyright Parasite Network 2018
;; GPL3

(defstruct window
  "The WINDOW is the root container for all widgets."
  widget
  active
  id
  internal-id
  stack-index
  visible
  group
  callback-open
  callback-close
  callback-input
  todo-paint-cache)

(defparameter *WINDOWS* nil
  "Defined windows.")

(defparameter *WINDOW-STACK* nil
  "All windows sorted in paint order.")

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

(defparameter *DEBUG-MOUSE-XY-PRINT-WIDGET-TREE* nil
  "When true it will print out the widget tree under the mouse cursor.")

; (INT, INT) -> NIL
(defun glas-initialize-window-defaults (width height)
  (setf *WINDOWS* (make-hash-table :test #'eq))
  (setf *WINDOW-PAINT-STACK* nil)
  (setf *WINDOW-STACK* (make-array '(20) :adjustable t :fill-pointer 0))
  (setf *WINDOW-INPUT-STACK* (make-array '(0)))
  (setf *NAMED-WINDOWS* (make-hash-table :test #'eq))
  (setf *WINDOW-GROUPS* (make-hash-table :test #'eq))
  (setf *DEBUG-WIDGET-BORDER* nil)
  (setf *DEBUG-MOUSE-XY-PRINT-WIDGET-TREE* nil)
  (setf *WINDOW-WIDTH* width)
  (setf *WINDOW-HEIGHT* height)
  (setf *MOUSE-MOVEMENT-TRACKER* nil)
  (setf *MOUSE-CLICK-VECTOR* (make-array (list 5) :initial-element nil))
  nil)

;; EVENT  4 = WINDOW MOVED
;; EVENT 10 = MOUSE ENTER WINDOW
;; EVENT 11 = MOUSE LEAVE WINDOW
;; EVENT 12 = GAIN FOCUS
;; EVENT 13 = LOST FOCUS
;; EVENT 14 = WINDOW CLOSE
;; EVENT 15 = OFFERED FOCUS

(defun translate-window-event (event)
  (case event
        (1 "WINDOW SHOWN")
        (2 "WINDOW HIDDEN")
        (3 "WINDOW EXPOSED")
        (4 "WINDOW MOVED")
        (10 "MOUSE ENTER")
        (11 "MOUSE LEAVE")
        (12 "GAIN KEYBOARD FOCUS")
        (13 "LOST KEYBOARD FOCUS")
        (14 "WINDOW CLOSE")
        (15 "TAKE FOCUS")
        (otherwise event)))

;;------------------------------------------------------------------------------

(defun debug-print-active-windows ()
  (debug-print-window-stack "Active windows" (lambda (window)
                                               (window-active window))))

(defun debug-print-visible-windows ()
  (debug-print-window-stack "Visible windows" (lambda (window)
                                                (window-visible window))))

(defun debug-print-all-windows ()
  (debug-print-window-stack "All windows" (lambda (window) 
                                            t)))

(defun debug-print-window-stack (title predicate)
  (format t "╔════════  ~A~%" title)
  (loop for window across *WINDOW-STACK* do
    (when (funcall predicate window)
      (format t "║   ~2@A: ~A~%" (window-stack-index window) (window-id window))))
  (format t "╚════════ ~%"))

;;------------------------------------------------------------------------------

; (INT, INT) -> INT
(defun calc-offset-within (container child)
  (floor (/ (- container child) 2.0)))

; WINDOW -> NIL
(defun center-window-widget (window)
  "Puts the window's widget in the center. Called when :PLACE is :CENTER."
  (let ((widget (window-widget window)))
    (let ((x (calc-offset-within *WINDOW-WIDTH* (widget-width widget)))
          (y (calc-offset-within *WINDOW-HEIGHT* (widget-height widget))))
      (setf (widget-x widget) x)
      (setf (widget-y widget) y)
      nil)))

;;------------------------------------------------------------------------------

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

(defun flipivate (window)
  (setf (window-active window) (not (window-active window))))

;;------------------------------------------------------------------------------

; WINDOW -> NIL
(defun stack-window (window)
  (let ((index (vector-push-extend window *WINDOW-STACK*)))
    (setf (window-stack-index window) index)
    nil))

;;------------------------------------------------------------------------------

(defun register-window (window)
  (when (window-id window)
    (format t "Registering window: ~A.~%" (window-id window))
    (setf (gethash (window-id window) *NAMED-WINDOWS*) window)
    nil))

; (WINDOW-ID &KEY) -> WINDOW | NIL
(defun find-window (id &key on-failure-nil) 
  "Tries to find the window."
  (let ((window (gethash id *named-windows*)))
    (if window
        window
        (unless on-failure-nil
          (error "FIND-WINDOW failed to find the window: ~A." id)))))

;;------------------------------------------------------------------------------

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

;;------------------------------------------------------------------------------

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

;;------------------------------------------------------------------------------
;; PAINTING

(defun paint-window (window renderer tick &key debug-borders)
  (let ((widget (window-widget window)))
    (when widget
      (widget-propagate-paint widget renderer tick debug-borders))))

; (SDL2:RENDERER, INT) -> NIL
(defun paint-all-windows (renderer tick &key debug-borders)
  (loop for i from 0 below (fill-pointer *WINDOW-STACK*) do
        (let ((window (aref *WINDOW-STACK* i)))
          (when (window-visible window)
            (paint-window window renderer tick :debug-borders debug-borders)))))

;;------------------------------------------------------------------------------

(defun initialize-windows ()
  (map nil (lambda (window)
             (format t "Initializing window: ~A.~%" (window-id window))
             (let ((widget (window-widget window)))
               (when widget
                 (widget-propagate-init widget window))))
       *WINDOW-STACK*))

;;------------------------------------------------------------------------------
;; Searching for a widget using the input stack order (reverse paint order).

(defun prettyprint-widget-tree (window tree)
  (format t "╳ ~A (~A)~%" (type-of window) (window-id window))
  (let ((indent 2))
    (dolist (widget tree)
      (format t "~{ ~*~}╰ ~A (~A)~%" (make-list indent) (type-of widget) (widget-id widget))
      (incf indent 2))))

(defun search-stack-xy (x y)
  "Searches for the widget at X and Y beginning at the top of the stack."
  (search-stack-xy-from (1- (fill-pointer *WINDOW-STACK*)) x y))

(defun search-stack-xy-from (index x y)
  "Searches for the widget at X and Y beginning at stack index INDEX."
  (loop for i from index downto 0
        do (let ((result (search-window-xy (aref *WINDOW-STACK* i) x y)))
             (when result
               (when *DEBUG-MOUSE-XY-PRINT-WIDGET-TREE*
                 (prettyprint-widget-tree (aref *WINDOW-STACK* i) result))
               (return-from search-stack-xy-from (car (last result)))))))

(defun search-window-xy (window x y)
  "Searches the WINDOW for the widget at X and Y."
  (when (window-visible window)
    (let ((widget (window-widget window)))
      (when widget
        (search-widget-xy widget x y)))))

;;------------------------------------------------------------------------------

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
  "Creates a window, pushes it on the stack and registers its name."
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
      (calculate-absolute-coordinates-in-window window))
    (when group
      (add-window-to-group group window))
    (stack-window window)
    (register-window window)
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
       (format t "~&Creating window: ~A.~%" ,id)
       (defwindow% ,id 
                   :active ,active
                   :visible ,visible
                   :widget ,widget
                   :place (quote ,place)
                   :trap-input ,trap-input
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
  (bubble-key key :DOWN))

(defun window-event-onkey-up (key)
  (bubble-key key :UP))

(defun bubble-key (key state)
  (let ((handled (bubble-key-down-stack key state)))
    (unless handled
      (format t "   KEY DISCARDED.~%"))
    handled))

;; (KEY) -> T | NIL
(defun bubble-key-down-stack (key state)
  (some (lambda (window)
          (when (window-visible window)
            (when (window-active window)
              (format t "Key given to window: ~A.~%" (window-id window))
              (bubble-key-window key window state))))
        (reverse *WINDOW-STACK*)))

(defun bubble-key-window (key window state)
  (if (bubble-key-callback key window state)
      t
      (when (window-widget window)
        (case state
          (:DOWN
            (widget-propagate-onkey-down (window-widget window) key))
          (:UP
            (widget-propagate-onkey-up (window-widget window) key))))))

(defun bubble-key-callback (key window state)
  (when (eq state :DOWN)
    (when (window-callback-input window)
      (when (funcall (window-callback-input window) window key)
        (format t "Key handled by window trap: ~A.~%" (window-id window))
        t))))