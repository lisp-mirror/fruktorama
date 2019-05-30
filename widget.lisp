;; widget.lisp
;; Copyright Parasite Network 2018
;; GPL3

;; TODO
;; * If width or height is not set then calculate it from the first child, or 0,0.

#|
    This file contains an assortment of different widgets used to build up windows.
      
      IMAGE-WIDGET
      CANVAS-WIDGET
      BAG-WIDGET
      BOX-WIDGET
      
      MENU-WIDGET
      FLIPPER-WIDGET
      TAG-WIDGET
      SCOREBOARD-WIDGET
      HIGHSCOREBOARD-WIDGET
      SCOREENTRY-WIDGET
      FLIPBOOK-WIDGET
      PARTICLE-FIELD-WIDGET
      STAR-RAIN-WIDGET
    
    All widgets derive from WIDGET which provides the default behavior.
|#

;-------------------------------------------------------------------------
;; Auxiliary 

(defstruct color
  (r 255);(random 256))
  (g 255);(random 256))
  (b 255);(random 256))
  (a 0))

(defparameter +WHITE+ (make-color :r 255 :g 255 :b 255))
(defparameter +RED+ (make-color :r 255 :g 0 :b 0))
(defparameter +BLACK+ (make-color :r 0 :g 0 :b 0))
(defparameter +ALPHA+ (make-color :r 0 :g 0 :b 0 :a 255))
(defparameter +GREENISH+ (make-color :r 113 :g 223 :b 95))

; (SDL2:RENDERER, COLOR) -> ?
(defun set-render-coloring (renderer color)
  "Set rendering color using a COLOR object."
  (sdl2:set-render-draw-color renderer
                              (color-r color)
                              (color-g color)
                              (color-b color)
                              (color-a color)))

; WIDGET -> SDL2:RECT
(defun self-rectangle (widget &key (dx 0) (dy 0) (dw 0) (dh 0) (grow 0))
  "Creates a rectangle that matches the widget's shape."
  (sdl2:make-rect
    (+ (widget-x widget) dx (- grow))
    (+ (widget-y widget) dy (- grow))
    (+ (widget-width widget) dw (* 2 grow))
    (+ (widget-height widget) dh (* 2 grow))))


(defmacro each-widget-child (widget child &rest body)
  (let ((children (gensym)))
    `(let ((,children (get-widget-children ,widget)))
       (dolist (,child ,children)
         ,@body))))

; (WIDGET,TYPESPEC) -> WIDGET | NIL
(defun search-widget-by-type (widget type)
  "Searches for a widget of the given type. Starting with WIDGET and then
  traversing down the tree of children."
  (if (typep widget type)
      widget
      (each-widget-child widget child
                         (let ((result (search-widget-by-type child type)))
                           (when result
                             (return-from search-widget-by-type result))))))

; (WIDGET,KEYWORD) -> WIDGET | NIL
(defun search-widget-by-local-id (widget id)
  "Searches for a widget with the given local id. Starting with WIDGET and then
  traversing down the tree of children. Unlike regular id's which are visible
  globally through FIND-WIDGET, local id's must be searched for."
  (if (eq (widget-local-id widget) id)
      widget
      (each-widget-child widget child
                         (let ((result (search-widget-by-local-id child id)))
                           (when result
                             (return-from search-widget-by-local-id result))))))

(defun key= (sdlkey symkey)
  (sdl2:scancode= (sdl2:scancode-value sdlkey) symkey))

(defmacro keycase (key &body body)
  `(cond
     ,@(let ((statements))
         (dolist (clause body statements)
           (setf statements 
                 (append statements
                         (if (eq (car clause) t)
                             `((t ,@(cdr clause)))
                             `(((key= ,key ,(car clause)) ,@(cdr clause) t)))))))))

(defun initialize-struct (thestruct &rest args &key &allow-other-keys)
  (let ((prefix (symbol-name (type-of thestruct))))
    (loop for (key value) on args by #'cddr
          do (let ((postfix (symbol-name key)))
               (let ((accessor (intern (concatenate 'string prefix "-" postfix))))
                 (handler-case
                   (let ((writer (fdefinition (list 'setf accessor))))
                     (funcall writer value thestruct))
                   (undefined-function (e)
                                       (format t "When initializing ~A~% ~A~%" thestruct e)
                                       (error e))))))
    thestruct))

;-------------------------------------------------------------------------

(defstruct widget
  children
  (x 0)
  (y 0)
  ;(absolute-right 0)
  ;(absolute-bottom 0)
  (offset-x 0)
  (offset-y 0)
  id
  local-id
  (width 0)
  (height 0)
  (opaque nil)
  (opaque-draw-exception t)
  (mouse-movement-awareness nil)
  (mouse-click-awareness nil)
  (debug-border-color (make-color))
  debug-border-rect
  style
  window)

;; (WIDGET) -> LIST<WIDGET>
(defgeneric get-widget-children (widget))
(defmethod get-widget-children (widget)
  "Returns the widget's children. Overriding it is ONLY necessary
  if the children are not stored in the CHILDREN slot."
  (widget-children widget))

(defun widget-absolute-right (widget)
  (+ (widget-x widget) (widget-width widget)))

(defun widget-absolute-bottom (widget)
  (+ (widget-y widget) (widget-height widget)))

;-------------------------------------------------------------------------

(defun within (widget x y)
  (and (>= x (widget-x widget))
       (>= y (widget-y widget))
       (< x (widget-absolute-right widget))
       (< y (widget-absolute-bottom widget))))

(defun search-widget-xy (widget x y)
  (when (within widget x y)
    (format t "SEARCH-WIDGET-XY (~A;~A) ▶ ~A (~A).~%" x y (type-of widget) (widget-id widget))
    (format t "    XYWH (~A;~A;~A;~A) " (widget-x widget) (widget-y widget) (widget-width widget) (widget-height widget))
    (format t "RB (~A;~A)~%" (widget-absolute-right widget) (widget-absolute-bottom widget))
    (if (widget-opaque widget)
        widget
        (search-into-widget-xy widget x y))))

(defun search-into-widget-xy (widget x y)
  (let ((narrow (some (lambda (child)
                        (search-widget-xy child x y))
                      (get-widget-children widget))))
    (if narrow
        narrow
        widget)))

;-------------------------------------------------------------------------

(defun get-widget-at (widget x y)
  (let ((wx (widget-x widget))(wy (widget-y widget))
        (ww (widget-absolute-right widget))(wh (widget-absolute-bottom widget)))
    (if (and (>= x wx) (>= y wy) (< x ww) (< y wh))
        (progn
          (format t "GET XY (~A;~A) ENTER (~A) XYWH (~A;~A;~A;~A) RB (~A;~A)~%" x y (type-of widget) wx wy (widget-width widget) (widget-height widget) ww wh)
          (if (widget-opaque widget)
              widget
              (let ((subwidget (some (lambda (child)
                                       (get-widget-at child x y))
                                     (get-widget-children widget))))
                (if subwidget
                    subwidget
                    widget)))))))

(defun paint-debug-border (widget renderer)
  (let ((color (widget-debug-border-color widget)))
      (sdl2:set-render-draw-color renderer
                                  (color-r color)
                                  (color-g color)
                                  (color-b color)
                                  (color-a color))
      (let ((rect (widget-debug-border-rect widget)))
        (unless rect
          (setf rect (sdl2:make-rect
                       (widget-x widget)
                       (widget-y widget)
                       (widget-width widget)
                       (widget-height widget)))
          (setf (widget-debug-border-rect widget) rect))
        (sdl2:render-draw-rect renderer rect))))

(defun add-adjust-accordingly (widget child)
  "Adds the child to the widget and resizes widget to match the child."
  (setf (widget-children widget) (list child))
  (setf (widget-width widget) (widget-width child))
  (setf (widget-height widget) (widget-height child)))

;-------------------------------------------------------------------------
;; Event vector

(defparameter *EVENTS* (make-hash-table))

(defun attach-event-handler (widget event)
  (format t "The widget (~A): ~A accepts event: ~A~%" (type-of widget) (widget-id widget) event)
  (push widget (gethash event *EVENTS*)))

(defun dispatch-event (event payload)
  (let ((handlers (gethash event *EVENTS*)))
    (when handlers
      (dolist (handler handlers)
        (widget-event-handle handler event payload)))))

(defgeneric widget-event-handle (widget event payload))
(defmethod widget-event-handle (widget event payload))

;-------------------------------------------------------------------------
;; Widget events

(defgeneric widget-event-prepaint (widget renderer tick)
  (:documentation
    "Called before painting children."))
(defmethod widget-event-prepaint (widget renderer tick))

(defgeneric widget-event-paint (widget renderer tick)
  (:documentation
    "This method is only necessary to override if the widget needs
    to do unorthodox painting."))
(defmethod widget-event-paint (widget renderer tick))

(defgeneric widget-event-absolute-xy (widget)
  (:documentation
    "This event tells the widget that it has its absolute position set."))
(defmethod widget-event-absolute-xy (widget))

(defgeneric widget-event-onkey-down (widget key)
  (:documentation
    "This method must to be overridden if a widget needs to handle input.
    If the widget is opaque it must call WIDGET-PROPAGATE-ONKEY-DOWN
    on its own children if it wants to give them a chance to handle the input."))
(defmethod widget-event-onkey-down (widget key))

(defgeneric widget-event-open (widget)
  (:documentation
    "Widget event issued when the window is opened. Does not pass through opaque widgets."))
(defmethod widget-event-open (widget))

(defgeneric widget-event-close (widget)
  (:documentation
    "Widget event issued when the window is closed. Does not pass through opaque widgets."))
(defmethod widget-event-close (widget))

(defgeneric widget-event-init (widget window)
  (:documentation
    "Widget event issued after all windows are defined and all widgets are registered.
    Provides means to do post initialization.
    This event will pass through opaque widgets."))
(defmethod widget-event-init (widget window))

(defgeneric widget-event-mouse-enter (widget)
  (:documentation
    "Issued event when mouse enter the widget."))
(defmethod widget-event-mouse-enter (widget))

(defgeneric widget-event-mouse-leave (widget)
  (:documentation
    "Issued event when mouse leaves the widget."))
(defmethod widget-event-mouse-leave (widget))

(defgeneric widget-event-mouse-movement (widget x y)
  (:documentation
    "Issued event when mouse moves around in the widget."))
(defmethod widget-event-mouse-movement (widget x y))

;; Mouse clicks

;; Left
(defgeneric widget-event-mouse-left-click-down (widget x y))
(defmethod widget-event-mouse-left-click-down (widget x y))

(defgeneric widget-event-mouse-left-click-up (widget x y))
(defmethod widget-event-mouse-left-click-up (widget x y))

(defgeneric widget-event-mouse-left-click-cancel (widget x y))
(defmethod widget-event-mouse-left-click-cancel (widget x y))

;; Middle
(defgeneric widget-event-mouse-middle-click-down (widget x y))
(defmethod widget-event-mouse-middle-click-down (widget x y))

(defgeneric widget-event-mouse-middle-click-up (widget x y))
(defmethod widget-event-mouse-middle-click-up (widget x y))

(defgeneric widget-event-mouse-middle-click-cancel (widget x y))
(defmethod widget-event-mouse-middle-click-cancel (widget x y))

;; Right
(defgeneric widget-event-mouse-right-click-down (widget x y))
(defmethod widget-event-mouse-right-click-down (widget x y))

(defgeneric widget-event-mouse-right-click-up (widget x y))
(defmethod widget-event-mouse-right-click-up (widget x y))

(defgeneric widget-event-mouse-right-click-cancel (widget x y))
(defmethod widget-event-mouse-right-click-cancel (widget x y))

;; Any
(defgeneric widget-event-mouse-click-down (widget x y button))
(defmethod widget-event-mouse-click-down (widget x y button))

(defgeneric widget-event-mouse-click-up (widget x y button))
(defmethod widget-event-mouse-click-up (widget x y button))

(defgeneric widget-event-mouse-click-cancel (widget x y button))
(defmethod widget-event-mouse-click-cancel (widget x y button))

;; Raw
(defgeneric widget-event-mouse-click (widget x y button state))
(defmethod widget-event-mouse-click (widget x y button state))

;-------------------------------------------------------------------------
; These functions are used to start propagating events.

(defun widget-propagate-onkey-down (widget key)
  (if (widget-event-onkey-down widget key)
      (progn
        (format t "  Handled by widget (~A): ~A.~%" (type-of widget) (widget-id widget))
        t)
      (unless (widget-opaque widget)
        (let ((children (get-widget-children widget)))
          (some (lambda (child)
                  (widget-propagate-onkey-down child key))
                children)))))

(defun widget-propagate-calculate-xy (widget)
  (widget-propagate-calculate-xy-children widget (get-widget-children widget))
  (widget-event-absolute-xy widget)
  (register-widget widget))
    
(defun widget-propagate-calculate-xy-children (widget children)
  (dolist (child children)
    (setf (widget-x child)
          (+ (widget-offset-x child)
             (widget-x widget)))
    (setf (widget-y child)
          (+ (widget-offset-y child)
             (widget-y widget)))
    (widget-propagate-calculate-xy child)))        
                       
(defun widget-propagate-open (widget)
  (widget-event-open widget)
  (unless (widget-opaque widget)
    (each-widget-child widget child
                       (widget-propagate-open child))))

(defun widget-propagate-close (widget)
  (widget-event-close widget)
  (unless (widget-opaque widget)
    (each-widget-child widget child
                       (widget-propagate-close child))))

(defun widget-propagate-init (widget window)
  (format t "  Initializing widget (~A): ~A.~%"
          (symbol-name (type-of widget))
          (widget-id widget))
  (widget-event-init widget window)
  (each-widget-child widget child
                     (widget-propagate-init child window)))

;; OPAQUE SHOULD BE OBEYED, BUT ARE THEY DESIGNED TO
(defun widget-propagate-paint (widget renderer tick debug-borders)
  (widget-event-prepaint widget renderer tick)
  (when (or (not (widget-opaque widget)) (widget-opaque-draw-exception widget))
    (each-widget-child widget child
                       (widget-propagate-paint child renderer tick debug-borders)))
  (widget-event-paint widget renderer tick)
  (when debug-borders
    (paint-debug-border widget renderer)))

(defun paint-children (children renderer tick)
  (dolist (child children)
    (widget-propagate-paint child renderer tick)))
;-------------------------------------------------------------------------

(defparameter *REGISTERED-WIDGETS* nil)

(defun glas-initialize-widget-defaults ()
  (setf *REGISTERED-WIDGETS* (make-hash-table :test #'eq)))

; (WIDGET) -> NIL
(defun register-widget (widget)
  (let ((id (widget-id widget)))
    (when id
      (format t "Register widget: ~A.~%" id)
      (setf (gethash id *REGISTERED-WIDGETS*) widget)
      nil)))

; (WIDGET) -> NIL
(defun unregister-widget (widget)
  (let ((id (widget-id widget)))
    (when id
      (format t "Unregister widget: ~A.~%" id)
      (remhash id *REGISTERED-WIDGETS*)
      nil)))

; (ID,&KEY) -> WIDGET | NIL
(defun find-widget (id &key on-failure-nil)
  "Finds the named widget and fails horribly if it can't!"
  (let ((widget (gethash id *REGISTERED-WIDGETS*)))
    (if widget
        widget
        (if on-failure-nil
            (format t "Failed to find widget: ~A.~%" id)
            (error "Failed to find widget: ~A.~%" id)))))

;;------------------------------------------------------------------------------
;;------------------------------------------------------------------------------
;; Widgets
;;------------------------------------------------------------------------------
;;------------------------------------------------------------------------------

(defstruct (image-widget (:include widget))
  "The IMAGE-WIDGET paints a PIXMAP-DESCRIPTOR."
  pixmap
  (visible t))

(defun make-image (pid &key id (visible t))
  (let ((pixmap (get-pixmap pid)))
    (let ((widget (make-image-widget 
                    :pixmap pixmap
                    :width (pixmap-width pixmap)
                    :height (pixmap-height pixmap)
                    :visible visible
                    :id id)))
      (register-widget widget) ;; <<<<<< ?
      (format t " Created IMAGE-WIDGET: ~A.~%" id)
      widget)))

(defun switch-image-visibility (widget &optional state)
  (unless state
    (setf state (not (image-widget-visible widget))))
  (setf (image-widget-visible widget) state))

(defmethod widget-event-paint ((w image-widget) renderer tick)
  (when (image-widget-visible w)
    (let ((pixmap (image-widget-pixmap w)))
      (paint-descriptor pixmap renderer (widget-x w) (widget-y w) tick))))

;;------------------------------------------------------------------------------

(defstruct emboss-stylesheet
  (light (make-color :r 213 :g 213 :b 213))
  (dark (make-color :r 84 :g 84 :b 84))
  (plate (make-color :r 159 :g 159 :b 159)))

(defstruct (panel-widget (:include widget))
  "Draws an embossment around its child widget."
  relief
  ram
  top-rect
  bottom-rect
  plate-rect)

(defun toggle-panel (widget)
  "TODO: Use ROTATEF."
  (let ((style (widget-style widget)))
    (let ((toggled (make-emboss-stylesheet
                     :light (emboss-stylesheet-dark style)
                     :dark (emboss-stylesheet-light style)
                     :plate (emboss-stylesheet-plate style))))
      (setf (widget-style widget) toggled))))

(defun make-panel (&key widget id (relief 2) (ram 2) style fixed-width fixed-height)
  (unless (and (numberp relief) (plusp relief))
    (error "PANEL-WIDGET(~A): RELIEF must be a positive integer, not ~A.~%" id relief))
  (setf relief (max 1 (truncate relief)))
  (unless (and (numberp ram) (plusp ram))
    (error "PANEL-WIDGET(~A): RAM must be a positive integer, not ~A.~%" id ram))
  (setf ram (max 1 (truncate ram)))
  (unless style
    (setf style (make-emboss-stylesheet)))
  ;; If either FIXED-WIDTH or FIXED-HEIGHT (or both) is given we wrap the widget in a bag
  ;; with the specified dimensions. There is no overflow detection!
  (when (or fixed-width fixed-height)
    (let (bagwidth bagheight)
      (when fixed-width
        (setf bagwidth (- fixed-width relief relief ram ram)))
      (when fixed-height
        (setf bagheight (- fixed-height relief relief ram ram)))
      (setf widget (make-bag 
                     :align :cross
                     :fixed-width bagwidth
                     :fixed-height bagheight 
                     :widgets (list widget)))))
  
  (let ((self (make-panel-widget
                  :children (list widget)
                  :id id
                  :style style)))
    
    (setf (widget-width self) (+ (widget-width widget) relief relief ram ram))
    (setf (widget-height self) (+ (widget-height widget) relief relief ram ram))
    (setf (widget-offset-x widget) (+ relief ram))
    (setf (widget-offset-y widget) (+ relief ram))
    (setf (panel-widget-relief self) relief)
    (setf (panel-widget-ram self) ram)
    (format t " Created PANEL-WIDGET: ~A.~%" id)
    self))

(defmethod widget-event-init ((w panel-widget) window)
  (let ((relief (panel-widget-relief w)))
    (setf (panel-widget-plate-rect w) (self-rectangle w :grow (- relief)))
    (setf (panel-widget-top-rect w) (self-rectangle w))
    (setf (panel-widget-bottom-rect w) (self-rectangle w :dx relief :dy relief :dw (- relief) :dh (- relief)))))

(defmethod widget-event-prepaint ((w panel-widget) renderer tick)
  (let ((style (widget-style w)))
    (let ((light-color (emboss-stylesheet-light style))
          (dark-color (emboss-stylesheet-dark style))
          (plate-color (emboss-stylesheet-plate style)))
      ;; Light side
      (set-render-coloring renderer light-color)
      (sdl2:render-fill-rect renderer (panel-widget-top-rect w))
      ;; Dark side
      (set-render-coloring renderer dark-color)
      (sdl2:render-fill-rect renderer (panel-widget-bottom-rect w))
      ;; Dark corner pixels
      (loop for x from (+ (widget-x w) 1) 
            with y = (- (widget-absolute-bottom w) 1)
            for p from 1 
            repeat (1- (panel-widget-relief w)) do
            (loop for py from 0 below p do
                  (sdl2:render-draw-point renderer x (- y py))))
      ;; Dark corner pixels
      (loop with x = (- (widget-absolute-right w) 1)
            for y from (+ (widget-y w) 1)
            for p from 1
            repeat (1- (panel-widget-relief w)) do
            (loop for px from 0 below p do
                  (sdl2:render-draw-point renderer (- x px) y)))
      ;; Plate
      (set-render-coloring renderer plate-color)
      (sdl2:render-fill-rect renderer (panel-widget-plate-rect w)))))

;;------------------------------------------------------------------------------
;; Button

(defstruct (button-widget (:include widget))
  "A button."
  active
  panel
  click-callback)

(defun make-button (&key widget id trap (active t) style fixed-width fixed-height)
  (let ((panel (make-panel 
                 :widget widget 
                 :relief 3 
                 :style style
                 :fixed-width fixed-width
                 :fixed-height fixed-height)))
    (let ((self (make-button-widget
                  :id id
                  :panel panel
                  :active active
                  :click-callback trap
                  :children (list panel)
                  :width (widget-width panel)
                  :height (widget-height panel)
                  :mouse-click-awareness t
                  :opaque t)))
      (format t " Created BUTTON-WIDGET: ~A.~%" id)
      self)))

(defun set-button-active (widget status)
  (setf (button-widget-active widget) status))

(defmethod widget-event-mouse-left-click-down ((w button-widget) x y)
  (when (button-widget-active w)
    (toggle-panel (button-widget-panel w))))

(defmethod widget-event-mouse-left-click-up ((w button-widget) x y)
  (when (button-widget-active w)
    (toggle-panel (button-widget-panel w))
    (when (button-widget-click-callback w)
      (funcall (button-widget-click-callback w) w x y))))

(defmethod widget-event-mouse-left-click-cancel ((w button-widget) x y)
  (when (button-widget-active w)
    (toggle-panel (button-widget-panel w))))

(defmethod widget-event-paint ((w button-widget) renderer tick)
  (unless (button-widget-active w)
    (set-render-coloring renderer +RED+)
    (sdl2:render-draw-line renderer 
                           (widget-x w) 
                           (widget-y w) 
                           (1- (widget-absolute-right w)) 
                           (1- (widget-absolute-bottom w)))
    (sdl2:render-draw-line renderer
                           (widget-x w)
                           (1- (widget-absolute-bottom w))
                           (1- (widget-absolute-right w))
                           (widget-y w))))

;;------------------------------------------------------------------------------

(defstruct (canvas-widget (:include widget))
  "The CANVAS-WIDGET is used to paint a background color behind a widget."
  color
  rect)

(defun make-canvas (&key (color +BLACK+) id widget)
  (let ((canvas (make-canvas-widget
                  :color color
                  :id id
                  :width (widget-width widget)
                  :height (widget-height widget)
                  :children (list widget))))
    (format t " Created CANVAS-WIDGET: ~A.~%" id)
    canvas))

(defmethod widget-event-init ((w canvas-widget) window)
  (setf (canvas-widget-rect w) (self-rectangle w)))

(defmethod widget-event-prepaint ((w canvas-widget) renderer tick)
  (when (canvas-widget-color w)
    (set-render-coloring renderer (canvas-widget-color w))
    (sdl2:render-fill-rect renderer (canvas-widget-rect w))))

;;------------------------------------------------------------------------------

(defstruct (placeholder-widget (:include widget))
  )

(defun make-placeholder (&key id widget)
  (let ((self (make-placeholder-widget
                 :id id
                 :width (widget-width widget)
                 :height (widget-height widget)
                 :children (list widget))))
    (format t " Created PLACEHOLDER-WIDGET: ~A.~%" id)
    self))

(defun update-placeholder (placeholder widget &key init)
  (setf (widget-children placeholder) (list widget))
  (when init
    (calculate-absolute-coordinates (widget-window placeholder) placeholder)))

;;-------------------------------------------------------------------------

(defstruct (bag-widget (:include widget))
  "The BAG-WIDGET is a multi purpose layout container. The following layouts
  are supported:
  LEFT             Vertically left aligned.
  CENTER           Vertically centered.
  RIGHT            Vertically right aligned.
  TOP              Horisontally top aligned.
  MIDDLE           Horisontally middle aligned.
  BOTTOM           Horisontally bottom aligned.
  STACK            Stacked aligned to left top corner.
  CROSS            Stacked aligned to center point.
  Width and height is set to the largest value from the children, but can
  be overriden with FIXED-WIDTH and FIXED-HEIGHT.
  Extra space between the children can applied with SPACING in all
  layouts except STACK and CROSS.
  TODO: Implement FLOW to support column/row cutoff."
  fixed-width
  fixed-height
  spacing
  align
  todo-flow)

(defun make-bag (&key (align :left) widgets fixed-height fixed-width (spacing 0) id)
  ;; We remove any NIL in the widget list.
  (setf widgets (remove nil widgets))
  ;; Make sure fixed dimensions are positive.
  (when fixed-width
    (unless (and (numberp fixed-width) (plusp fixed-width))
      (error "BAG-WIDGET(~A): FIXED-WIDTH must be a positive integer, not ~A.~%" id fixed-width))
    (setf fixed-width (truncate fixed-width)))
  (when fixed-height
    (unless (and (numberp fixed-height) (plusp fixed-height))
      (error "BAG-WIDGET(~A): FIXED-HEIGHT must be a positive integer, not ~A.~%" id fixed-height))
    (setf fixed-height (truncate fixed-height)))
  ;; Also check the spacing.
  (unless (and (numberp spacing) (>= spacing 0))
    (error "BAG-WIDGET(~A): SPACING must be a non negative integer, not ~A.~%" id spacing))
  (setf spacing (truncate spacing))
  
  (let ((bag (make-bag-widget 
               :children widgets
               :fixed-width fixed-width
               :fixed-height fixed-height
               :align align
               :spacing spacing
               :id id)))
    (ecase align
           (:left (bag-align-left bag))
           (:center (bag-align-center bag))
           (:right (bag-align-right bag))
           (:top (bag-align-top bag))
           (:middle (bag-align-middle bag))
           (:bottom (bag-align-bottom bag))
           (:stack (bag-stack bag))
           (:cross (bag-cross bag)))
    (format t " Created BAG-WIDGET: ~A.~%" id)
    bag))
    
(defun bag-stack (bag)
  (let ((width 0)(height 0))
    (dolist (widget (widget-children bag))
      (setf width (max width (widget-width widget)))
      (setf height (max height (widget-height widget))))
    (setf (widget-width bag) width)
    (setf (widget-height bag) height)))

(defun bag-cross (bag)
  (let ((width (calc-maximal-width bag))
        (height (calc-maximal-height bag)))
    (setf (widget-width bag) width)
    (setf (widget-height bag) height)
    (dolist (widget (widget-children bag))
      (setf (widget-offset-x widget) (calc-offset-within width (widget-width widget)))
      (setf (widget-offset-y widget) (calc-offset-within height (widget-height widget))))))

(defun calc-maximal-width (bag)
  (let ((width (bag-widget-fixed-width bag)))
    (if width
        width        
        (let ((maxwidth 0))
          (dolist (w (widget-children bag) maxwidth)
            (setf maxwidth (floor (max maxwidth (widget-width w)))))))))

(defun calc-maximal-height (bag)
  (let ((height (bag-widget-fixed-height bag)))
    (if height
        height
        (let ((maxheight 0))
          (dolist (w (widget-children bag) maxheight)
            (setf maxheight (floor (max maxheight (widget-height w)))))))))

(defun bag-align-center (bag)
  (let ((maxwidth (calc-maximal-width bag))(y 0))
    (let ((spacing (bag-widget-spacing bag)))
      (setf (widget-width bag) maxwidth)
      (dolist (w (widget-children bag))
        (setf (widget-offset-x w) (calc-offset-within maxwidth (widget-width w)))
        (setf (widget-offset-y w) y)
        (setf y (+ y (widget-height w) spacing)))
      (setf y (max (- y spacing) 0))
      (setf (widget-height bag) y)
      (setf (widget-width bag) maxwidth))))

(defun bag-align-left (bag)
  (let ((maxwidth (calc-maximal-width bag))(y 0))
    (let ((spacing (bag-widget-spacing bag)))
      (setf (widget-width bag) maxwidth)
      (dolist (w (widget-children bag))
        (setf (widget-offset-x w) 0)
        (setf (widget-offset-y w) y)
        (setf y (+ y (widget-height w) spacing)))
      (setf y (max (- y spacing) 0))
      (setf (widget-height bag) y)
      (setf (widget-width bag) maxwidth))))

(defun bag-align-right (bag)
  (let ((maxwidth (calc-maximal-width bag))(y 0))
    (let ((spacing (bag-widget-spacing bag)))
      (setf (widget-width bag) maxwidth)
      (dolist (w (widget-children bag))
        (setf (widget-offset-x w) (- maxwidth (widget-width w)))
        (setf (widget-offset-y w) y)
        (setf y (+ y (widget-height w) spacing)))
      (setf y (max (- y spacing) 0))
      (setf (widget-height bag) y)
      (setf (widget-width bag) maxwidth))))

(defun bag-align-middle (bag)
  (let ((maxheight (calc-maximal-height bag))(x 0))
    (let ((spacing (bag-widget-spacing bag)))
      (setf (widget-height bag) maxheight)
      (dolist (w (widget-children bag))
        (setf (widget-offset-y w) (calc-offset-within maxheight (widget-height w)))
        (setf (widget-offset-x w) x)
        (setf x (+ x (widget-width w) spacing)))
      (setf x (max (- x spacing) 0))
      (setf (widget-width bag) x)
      (setf (widget-height bag) maxheight))))

(defun bag-align-top (bag)
  (let ((maxheight (calc-maximal-height bag))(x 0))
    (let ((spacing (bag-widget-spacing bag)))
      (setf (widget-height bag) maxheight)
      (dolist (w (widget-children bag))
        (setf (widget-offset-y w) 0)
        (setf (widget-offset-x w) x)
        (setf x (+ x (widget-width w) spacing)))
      (setf x (max (- x spacing) 0))
      (setf (widget-width bag) x)
      (setf (widget-height bag) maxheight))))

(defun bag-align-bottom (bag)
  (let ((maxheight (calc-maximal-height bag))(x 0))
    (let ((spacing (bag-widget-spacing bag)))
      (setf (widget-height bag) maxheight)
      (dolist (w (widget-children bag))
        (setf (widget-offset-y w) (- maxheight (widget-height w)))
        (setf (widget-offset-x w) x)
        (setf x (+ x (widget-width w) spacing)))
      (setf x (max (- x spacing) 0))
      (setf (widget-width bag) x)
      (setf (widget-height bag) maxheight))))

;;------------------------------------------------------------------------------

(defstruct (box-widget (:include widget))
  "The BOX-WIDGET can add padding around a widget."
  (top 0)
  (bottom 0)
  (left 0)
  (right 0))

(defun make-box (&key padding (top 0) (bottom 0) (left 0) (right 0) widget id)
  (when padding
    (etypecase padding
               (list
                 (ecase (length padding)
                        (2
                          (setf top (car padding) bottom (car padding) 
                                left (cadr padding) right (cadr padding)))
                        (4
                          (setf top (car padding) bottom (cadr padding)
                                left (caddr padding) right (cadddr padding)))))
               (integer
                 (setf top padding bottom padding left padding right padding))
               (t                 
                 (error "Unknown padding format ~A." padding))))
    
  (let ((box (make-box-widget
               :top top
               :bottom bottom
               :left left
               :right right
               :children (list widget)
               :id id)))
      
    (incf (widget-offset-x widget) left)
    (incf (widget-offset-y widget) top)
    
    (setf (widget-width box) (+ left (widget-width widget) right))
    (setf (widget-height box) (+ top (widget-height widget) bottom))
    (format t " Created BOX-WIDGET: ~A.~%" id)
    box))

;;------------------------------------------------------------------------------

(defstruct (debug-looking-glass-widget (:include widget))
  (outline-color +WHITE+)
  selected
  outline
  index)

(defun make-debug-looking-glass (width height &key id)
  (let ((self (make-debug-looking-glass-widget
                :id id
                :width width
                :height height
                :mouse-click-awareness t
                :mouse-movement-awareness t
                :opaque nil)))
    (format t " Created DEBUG-LOOKING-GLASS-WIDGET: ~A.~%" id)
    self))

(defmethod widget-event-init ((w debug-looking-glass-widget) window)
  (format t "----------debug-looking-glass-widget: window-stack-index: ~A~%" (window-stack-index window))
  (setf (debug-looking-glass-widget-index w) (1- (window-stack-index window)))
  (format t "----------debug-looking-glass-widget: my stack index: ~A~%" (debug-looking-glass-widget-index w)))

(defmethod widget-event-mouse-movement ((w debug-looking-glass-widget) x y)
  (format t "-----------widget-event-mouse-movement:debug-looking-glass-widget~%")
  (let ((widget (search-stack-xy-from (debug-looking-glass-widget-index w) x y)))
    (unless (eq widget (debug-looking-glass-widget-selected w))
      (setf (debug-looking-glass-widget-selected w) widget)
      (when widget
        (format t "Looking glass (~A) sees: ~A (~A)~%" (widget-id widget) (type-of widget) (widget-id widget))))))

(defmethod widget-event-mouse-leave ((w debug-looking-glass-widget)) ;; aölkdjfaösjfdö DISPATCHED!!!
  (format t "----------widget-event-mouse-leave:debug-looking-glass-widget~%")
  (setf (debug-looking-glass-widget-selected w) nil)
  (setf (debug-looking-glass-widget-outline w) nil))

(defmethod widget-event-mouse-left-click-up ((w debug-looking-glass-widget) x y)
  (let ((widget (debug-looking-glass-widget-selected w)))
    (when widget
      (setf (debug-looking-glass-widget-outline w) (self-rectangle widget)))))

(defmethod widget-event-paint ((w debug-looking-glass-widget) renderer tick)
  (let ((outline (debug-looking-glass-widget-outline w)))
    (when outline
      (set-render-coloring renderer (debug-looking-glass-widget-outline-color w))
      (sdl2:render-fill-rect renderer outline))))

;;------------------------------------------------------------------------------
;;------------------------------------------------------------------------------
;;------------------------------------------------------------------------------

(defstruct (menu-widget (:include widget))
  "The menu consist of two components.
   POINTER       ENTRIES
  ---------------------------------- - - - - - - - -
  |            |                   |
  | ---------- | ----------------- | - - - - - - - - 
  | |POINTER | | |ENTRY 1        | | MIRROR PONTER
  | ---------- | ----------------- | - - - - - - - -
  |            | ----------------- |
  |            | |ENTRY 2        | |
  |------------|-------------------- - - - - - - - - 
  The pointer will always line up with THE-ENTRY
  on each option in the menu. This way each option
  can have external layout."
  pointer  ; the pointer image
  options  ; the targets within each entry child which to align the pointer to 
  selected    ; current target index
  size)


(defstruct (menu-option-widget (:include widget))
  action)

(defun make-menu-option (&key action widget local-id)
  (make-menu-option-widget
    :children (list widget)
    :action action
    :local-id local-id
    :width (widget-width widget)
    :height (widget-height widget)))

(defun make-menu (&key id pointer widgets (start 0))
  (let ((options (remove nil
                         (mapcar
                           (lambda (widget)
                             (search-widget-by-type widget 'menu-option-widget))
                           widgets))))
    (let ((bag (make-bag
                 :align :left
                 :widgets widgets)))
      (setf (widget-offset-x bag) (widget-width pointer))
      (let ((size (length options)))
        (unless (> size 0)
          (error "MENU-WIDGET needs at least one MENU-OPTION-WIDGET."))
        (let ((menu (make-menu-widget
                      :id id
                      :width (+ (widget-width pointer) (widget-width bag))
                      :height (widget-height bag)
                      :pointer pointer
                      :children (list pointer bag)
                      :options (make-array size :initial-contents options)
                      :size size
                      :selected start)))
          (format t " Created MENU-WIDGET: ~A. ~%" id)
          menu)))))

(defun change-menu-selection (menu offset)
  (with-slots ((selected% selected)
               (size% size)) 
              menu
              (setf selected%
                    (mod (+ selected% offset) size%))))

(defun reposition-menu-pointer (menu)
  (with-slots ((pointer% pointer)
               (selected% selected)
               (options% options))
              menu
              (setf (widget-y pointer%) (widget-y (aref options% selected%)))))

(defun activate-current-menu-option (menu)
  (with-slots ((selected% selected)
               (options% options))
              menu
              (let ((option (aref options% selected%)))
                (let ((action (menu-option-widget-action option)))
                  (when action
                    (funcall action menu (widget-local-id option)))))))  

(defmethod widget-event-onkey-down ((menu menu-widget) key)
  (keycase key
    (:scancode-down
      (change-menu-selection menu 1)
      (reposition-menu-pointer menu)
      t)
    (:scancode-up
      (change-menu-selection menu -1)
      (reposition-menu-pointer menu)
      t)
    (:scancode-return
      (activate-current-menu-option menu)
      t)))

;;------------------------------------------------------------------------------
;;------------------------------------------------------------------------------
;;------------------------------------------------------------------------------


  




