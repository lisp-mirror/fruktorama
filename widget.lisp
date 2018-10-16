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
(defun self-rectangle (widget)
  "Creates a rectangle that matches the widget's shape."
  (sdl2:make-rect
    (widget-x widget)
    (widget-y widget)
    (widget-width widget)
    (widget-height widget)))


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
  (offset-x 0)
  (offset-y 0)
  id
  local-id
  (width 0)
  (height 0)
  (opaque nil)
  (debug-border-color (make-color))
  debug-border-rect)

(defun widget-absolute-right (widget)
  (+ (widget-x widget) (widget-width widget)))

(defun widget-absolute-bottom (widget)
  (+ (widget-y widget) (widget-height widget)))

(defgeneric get-widget-children (widget))
(defmethod get-widget-children (widget)
  "Returns the widget's children. Overriding it is only necessary
  if the children is not stored as a list in the CHILDREN slot."
  (widget-children widget))

(defun paint-widget-debug-border (widget renderer)
  "Debug function that paints a border around all widgets."
  (when widget
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
        (sdl2:render-draw-rect renderer rect)))
    (each-widget-child widget child
                       (paint-widget-debug-border child renderer))))

(defun frame (widget child)
  "Adds the child to the widget and resizes widget to match the child."
  (setf (widget-children widget) (list child))
  (setf (widget-width widget) (widget-width child))
  (setf (widget-height widget) (widget-height child)))

;-------------------------------------------------------------------------
;; Widget events

(defgeneric widget-event-paint (widget renderer tick)
  (:documentation
    "This method is only need to be overriden if the widget need
    to do unorthodox painting."))
(defmethod widget-event-paint (widget renderer tick))

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

(defgeneric widget-event-init (widget)
  (:documentation
    "Widget event issued after all windows are defined and all widgets are registered.
    Provides means to do post initialization.
    This event will pass through opaque widgets."))
(defmethod widget-event-init (widget))

;-------------------------------------------------------------------------
; These functions are used to start propagating events.

(defun widget-propagate-onkey-down (widget key)
  (when (widget-event-onkey-down widget key)
    (format t "  Handled by widget (~A): ~A.~%" (type-of widget) (widget-id widget))
    (throw 'onkey-event-success t))
  (when (widget-opaque widget)
    (format t "  Stopped at opaque widget (~A): ~A.~%" (type-of widget) (widget-id widget))
    (throw 'stopped-opaque nil))
  (each-widget-child widget child
                     (catch 'stopped-opaque
                       (widget-propagate-onkey-down child key))))

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

(defun widget-propagate-init (widget)
  (format t "  Initializing widget (~A): ~A.~%"
          (symbol-name (type-of widget))
          (widget-id widget))
  (widget-event-init widget)
  (each-widget-child widget child
                     (widget-propagate-init child)))

(defun widget-propagate-paint (widget renderer tick)
  (widget-event-paint widget renderer tick)
  (each-widget-child widget child
                     (widget-propagate-paint child renderer tick)))

;-------------------------------------------------------------------------

(defparameter *REGISTERED-WIDGETS* nil)

(defun initialize-widgets ()
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
      (register-widget widget)
      (format t "Created IMAGE-WIDGET: ~A.~%" id)
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

(defstruct (canvas-widget (:include widget))
  "The CANVAS-WIDGET is used to paint a background color behind a widget."
  color
  rect)

(defun make-canvas (&key color id widget)
  (let ((canvas (make-canvas-widget
                  :color color
                  :id id
                  :width (widget-width widget)
                  :height (widget-height widget)
                  :children (list widget))))
    (format t "Created CANVAS-WIDGET: ~A.~%" id)
    canvas))

(defmethod widget-event-init ((w canvas-widget))
  (setf (canvas-widget-rect w) (self-rectangle w)))

(defmethod widget-event-paint ((w canvas-widget) renderer tick)
  (when (canvas-widget-color w)
    (set-render-coloring renderer (canvas-widget-color w))
    (sdl2:render-fill-rect renderer (canvas-widget-rect w)))
  (call-next-method))

;;------------------------------------------------------------------------------

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
  (setf widgets (remove nil widgets))
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
    (format t "Created BAG-WIDGET: ~A.~%" id)
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
    (format t "Created BOX-WIDGET: ~A.~%" id)
    box))

;;------------------------------------------------------------------------------
;;------------------------------------------------------------------------------
;;------------------------------------------------------------------------------

(defstruct (menu-widget (:include widget))
  "The menu consist of two components.
   POINTER       ENTRIES
  ----------------------------------
  |            |                   |
  | ---------- | ----------------- |
  | |POINTER | | |ENTRY 1        | |
  | ---------- | ----------------- |
  |            | ----------------- |
  |            | |ENTRY 2        | |
  |------------|--------------------
  The pointer will always line up with THE-ENTRY
  on each option in the menu. This way each option
  can have external layout. TODO: THE-POINTER."
  pointer  ; the pointer image
  targets  ; the targets within each entry child which to align the pointer to
  size     ; number of targets
  index    ; current target index
  actions) ; callbacks

(defstruct (menu-entry-widget (:include widget))
  action)

(defun make-menu-entry (&key id widget action)
  (make-menu-entry-widget
    :id id
    :opaque t
    :children (list widget)
    :action action
    :width (widget-width widget)
    :height (widget-height widget)))

(defstruct (menu-target-widget (:include widget))
  target-id)

(defun the-target (target-id widget)
  (make-menu-target-widget
    :children (list widget)
    :width (widget-width widget)
    :height (widget-height widget)
    :target-id target-id))

(defun make-menu (&key id pointer entries (start 0))
  (let ((widgets (mapcar 
                   (lambda (entry)
                     (car (widget-children entry)))
                   entries)))
    (let ((bag (make-bag
                 :align :left
                 :widgets widgets)))
      (setf (widget-offset-x bag) (widget-width pointer))
      (let ((callbacks (mapcar
                         (lambda (entry)
                           (menu-entry-widget-action entry))
                       entries)))
        (let ((targets (mapcar
                         (lambda (widget)
                           (let ((target (search-widget-by-type widget 'menu-target-widget)))
                             (if target
                                 target
                                 (error "Unable to find (the-entry) in ~A." widget))))
                         widgets)))
          (let ((menu (make-menu-widget
                        :id id
                        :width (+ (widget-width pointer) (widget-width bag))
                        :height (widget-height bag)
                        :pointer pointer
                        :children (list pointer bag)
                        :targets targets
                        :index start
                        :size (length widgets)
                        :actions callbacks)))
            (format t "Created MENU-WIDGET: ~A.~%" id)
            menu))))))

(defmethod widget-event-onkey-down ((widget menu-widget) key)
  (cond
    ((sdl2:scancode= (sdl2:scancode-value key) :scancode-down)
     (let ((index (menu-widget-index widget)))
       (setf (menu-widget-index widget)
             (mod (1+ index) (menu-widget-size widget))))
     (update-menu-pointer widget)
     t)
    ((sdl2:scancode= (sdl2:scancode-value key) :scancode-up)
     (let ((index (menu-widget-index widget)))
       (setf (menu-widget-index widget)
             (mod (1- index) (menu-widget-size widget)))
       (update-menu-pointer widget))
     t)
    ((sdl2:scancode= (sdl2:scancode-value key) :scancode-return)
     (let ((callback (nth (menu-widget-index widget)
                          (menu-widget-actions widget))))
       (when callback
         (let ((target-id (menu-target-widget-target-id (nth (menu-widget-index widget)
                                                             (menu-widget-targets widget)))))
           (funcall callback widget target-id)))
       t))
    (t
      (let ((target (nth (menu-widget-index widget) (menu-widget-targets widget))))
        (widget-propagate-onkey-down target key)))))

(defun update-menu-pointer (widget)
  (let ((index (menu-widget-index widget)))
    (let ((targets (menu-widget-targets widget)))
      (let ((target (nth index targets)))
        (let ((pointer (menu-widget-pointer widget)))
          (setf (widget-y pointer) (widget-y target)))))))

;;------------------------------------------------------------------------------

(defstruct (flipper-widget (:include widget))
  alphabet
  pixmap
  (index 0)
  total)

(defun make-flipper (alphabet &key (initial 0) id)
  (let ((widget (initialize-flipper (make-flipper-widget)
                                     alphabet
                                     initial
                                     id)))
    (format t "Created FLIPPER-WIDGET: ~A.~%" id)
    widget))
  
(defun initialize-flipper (widget alphabet initial id)
  (let ((alphabet-descriptor (get-charmap alphabet :on-failure-nil t)))
    (unless alphabet-descriptor
      (error "(initialize-flipper) FLIPPER-WIDGET (~A) Invalid charmap id: ~A."
             id alphabet))
    (unless (or (numberp initial) (plusp initial))
      (error "(initialize-flipper) FLIPPER-WIDGET (~A) Invalid INITIAL index: ~A."
             id initial))
    (let ((widget (initialize-struct widget
                    :alphabet alphabet-descriptor
                    :index initial
                    :id id
                    :opaque t
                    :total (charmap-descriptor-total alphabet-descriptor)
                    :width (charmap-descriptor-width alphabet-descriptor)
                    :height (charmap-descriptor-height alphabet-descriptor))))
      (set-flipper-index widget initial)
      widget)))

(defun set-flipper-index (widget index)
  (setf index 
        (mod index 
             (flipper-widget-total widget)))
  (setf (flipper-widget-index widget) index)
  (update-flipper-letter widget))

(defun set-flipper-Δindex (widget offset)
  (let ((index (flipper-widget-index widget)))
    (incf index offset)
    (set-flipper-index widget index)))

(defun update-flipper-letter (widget)
  (setf (flipper-widget-pixmap widget)
        (get-charmap-pixmap
          (flipper-widget-alphabet widget)
          (flipper-widget-index widget))))

(defmethod widget-event-paint ((w flipper-widget) renderer tick)
  (paint-descriptor (flipper-widget-pixmap w) renderer (widget-x w) (widget-y w) tick))

(defmethod widget-event-onkey-down ((widget flipper-widget) key)
  (cond
    ((key= key :scancode-down)
     (set-flipper-Δindex widget -1)
     t)
    ((key= key :scancode-up)
     (set-flipper-Δindex widget 1)
     t)))

;;------------------------------------------------------------------------------

(defstruct (tag-widget (:include widget))
  flippers
  total
  selected
  chevron)

(defun make-tag (alphabet &key id initial chevron (letters 3))
  (let ((widget (initialize-tag alphabet id initial chevron letters)))
    (format t "Created TAG-WIDGET: ~A.~%" id)
    widget))

(defun initialize-tag (alphabet id initial chevron letters)
  (unless letters
    (setf letters (length initial)))
  (unless (or letters (typep letters 'number) (> letters 0))
    (error "(make-tag) TAG-WIDGET (~A) Invalid number of letters: ~A~%" id letters))
  (let ((flippers
          (loop for i below letters collect
                (make-box
                  :widget (make-bag
                            :align :cross
                            :widgets (list
                                       (when chevron
                                           (let ((image (make-image chevron :visible nil)))
                                             (setf (widget-local-id image) :local-chevron)
                                             image)) 
                                       (make-flipper alphabet
                                                     :initial (nth i initial))))))))
    (let ((bag (make-bag
                 :align :middle
                 :spacing 0
                 :widgets flippers)))
      
      (let ((tag (make-tag-widget
                   :id id
                   :width (widget-width bag)
                   :height (widget-height bag)
                   :selected 0
                   :flippers (make-array (list letters) :initial-contents flippers)
                   :total letters
                   :opaque t
                   :children (list bag))))
        (show-tag-chevron tag t)
        tag))))

(defun show-tag-chevron (tag visible)
  (with-slots ((selected% selected)
               (flippers% flippers)) tag
              (let ((chevron (search-widget-by-local-id
                               (aref flippers% selected%)
                               :local-chevron)))
                (when chevron
                  (switch-image-visibility chevron visible)))))

(defun flip-the-tag (widget offset)
  (show-tag-chevron widget nil)
  (let ((selected (tag-widget-selected widget)))
    (incf selected offset)
    (setf selected (mod selected (tag-widget-total widget)))
    (setf (tag-widget-selected widget) selected)
    (show-tag-chevron widget t)))

(defun set-tag-name (widget &optional name)
  (unless name
    (setf name (loop repeat (tag-widget-total widget) collect 0)))
  (let ((flippers (tag-widget-flippers widget)))
    (let ((size (min (length name) (length flippers))))
      (dotimes (index size)
        (set-flipper-index (search-widget-by-type (aref flippers index) 'flipper-widget)
                           (nth index name))))))

(defun get-tag-name (widget)
  (loop for flipper across (tag-widget-flippers widget)
        collect (flipper-widget-index (search-widget-by-type flipper 'flipper-widget))))

(defmethod widget-event-onkey-down ((w tag-widget) key)
  (cond
    ((key= key :scancode-left)
     (flip-the-tag w -1)
     t)
    ((key= key :scancode-right)
     (flip-the-tag w 1)
     t)
    (t
      (widget-propagate-onkey-down
        (aref (tag-widget-flippers w) 
              (tag-widget-selected w))
        key))))

;;------------------------------------------------------------------------------

(defstruct (scoreboard-widget (:include widget))
  points
  digits
  composition
  pix-width
  pixmaps)

(defun make-scoreboard (&key id (digits 5) pixmaps (initial 0))
  (let ((widget (initialize-scoreboard (make-scoreboard-widget) 
                                       id 
                                       digits 
                                       pixmaps 
                                       initial)))
    (format t "Created SCOREBOARD-WIDGET: ~A.~%" id)
    widget))

(defun initialize-scoreboard (widget id digits pixmaps initial)
  (unless (or (numberp digits) (> digits 0))
    (error "SCOREBOARD-WIDGET (~A) Number of digits must be positive: ~A~%" id digits))
  (unless (or pixmaps (listp pixmaps))
    (error "SCOREBOARD-WIDGET (~A) PIXMAPS is either NIL or not a list: ~A~%" id pixmaps))
  
  (let ((pixmaps-array (get-pixmaps-array pixmaps)))
    (let ((height (pixmap-height (aref pixmaps-array 0)))
          (width (pixmap-width (aref pixmaps-array 0))))
      (let ((board (initialize-struct widget
                     :id id
                     :pix-width width
                     :digits digits
                     :pixmaps pixmaps-array
                     :width (* width digits)
                     :height height
                     :points initial
                     :composition (make-array (list digits)))))
        (compute-composition board)
        board))))

(defgeneric reset-scoreboard (widget))
(defmethod reset-scoreboard ((widget scoreboard-widget))
  (set-score widget 0))

(defmethod widget-event-paint ((w scoreboard-widget) renderer tick)
  (let ((pixw (scoreboard-widget-pix-width w)))
    (loop for pixmap across (scoreboard-widget-composition w) for index from 0 do
          (paint-descriptor pixmap 
                            renderer 
                            (+ (widget-x w) (* pixw index)) 
                            (widget-y w)
                            tick))))

; (WIDGET, INT) -> NIL
(defgeneric set-score (widget points))
(defmethod set-score ((widget scoreboard-widget) points)
  (setf (scoreboard-widget-points widget) (abs points))
  (compute-composition widget))

(defgeneric get-score (widget))
(defmethod get-score ((widget scoreboard-widget))
  (scoreboard-widget-points widget))

; (INT, INT) -> STRING
(defun points-to-text (points digits)
  (let ((text (write-to-string points)))
    (let ((diff (- digits (length text))))
      (cond
        ((< diff 0) ;(subseq text (abs diff)))
         (make-string digits :initial-element #\9))
        ((> diff 0) (concatenate 'string
                                 (make-string diff :initial-element #\0)
                                 text))
        (t text)))))

; STRING -> LIST
(defun text-to-digits (text)
  (mapcar
    (lambda (c)
      (- (char-code c) (char-code #\0)))
    (coerce
      text
      'list)))

; SCOREBOARD-WIDGET -> NIL
(defun compute-composition (board)
  (let ((points (scoreboard-widget-points board)))
    (let ((text (points-to-text points (scoreboard-widget-digits board))))
      (let ((digits (text-to-digits text)))
        (loop for digit in digits for index from 0  do
              (setf (aref (scoreboard-widget-composition board) index)
                    (aref (scoreboard-widget-pixmaps board) digit)))))))

;;------------------------------------------------------------------------------

(defstruct (highscoreboard-widget (:include scoreboard-widget))
  red-pixmaps
  green-pixmaps
  in-the-green)

(defun make-highscoreboard (&key id (digits 5) red green (initial 0))
  (unless (> digits 0)
    (error "HIGHSCOREBOARD-WIDGET (~A) Number of digits must be positive: ~A~%" id digits))
  (unless (or red (listp red))
    (error "HIGHSCOREBOARD-WIDGET (~A) RED is either NIL or not a list: ~A~%" id red))
  (unless (or green (listp green))
    (error "HIGHSCOREBOARD-WIDGET (~A) GREEN is either NIL or not a list: ~A~%" id green))
  (let ((board (initialize-highscoreboard 
                 (make-highscoreboard-widget) id digits red green initial)))
    (format t "Created HIGHSCOREBOARD-WIDGET: ~A.~%" id)
    board))

(defun initialize-highscoreboard (widget id digits red green initial)
  (initialize-scoreboard widget id digits red initial)
  (let ((green-pixmaps (get-pixmaps-array green)))
    (let ((board (initialize-struct widget     
                                    :green-pixmaps green-pixmaps
                                    :red-pixmaps (scoreboard-widget-pixmaps widget))))
      board)))

(defmethod reset-scoreboard ((widget highscoreboard-widget))
  (setf (scoreboard-widget-pixmaps widget) (highscoreboard-widget-red-pixmaps widget))
  (setf (highscoreboard-widget-in-the-green widget) nil)
  (call-next-method))

(defmethod set-score ((widget highscoreboard-widget) points)
  (unless (highscoreboard-widget-in-the-green widget)
    (when (highscore:worthy? points)
      (setf (highscoreboard-widget-in-the-green widget) t)
      (setf (scoreboard-widget-pixmaps widget) 
            (highscoreboard-widget-green-pixmaps widget))))
  (call-next-method))

;;------------------------------------------------------------------------------

(defstruct (scoreentry-widget (:include widget))
  tag-link
  scoreboard-link)

(defun make-scoreentry (letter-charmap number-ids &key chevron editable (letters 3) (digits 5) emblem (initial-points 0) (initial-name '(0 0 0)) id)
  (let ((bag (make-bag
               :align :center
               :widgets (list 
                          (make-tag letter-charmap 
                                    :initial initial-name 
                                    :letters letters
                                    :chevron chevron)
                          (make-scoreboard :pixmaps number-ids 
                                           :digits digits 
                                           :initial initial-points)))))
    (when emblem
      (setf bag (make-bag
                  :align :middle
                  :widgets (list
                             (make-box
                               :right 10
                               :widget (etypecase emblem
                                                  (keyword
                                                    (make-image emblem))
                                                  (widget
                                                    emblem)
                                                  (t
                                                    (error "(make-scoreentry) SCOREENTRY-WIDGET (~A): Unknown emblem type ~A." id (type-of emblem)))))
                             bag))))
      (make-scoreentry-widget
        :children (list bag)
        :id id
        :opaque (not editable)
        :width (widget-width bag)
        :height (widget-height bag)
        :tag-link (search-widget-by-type bag 'tag-widget)
        :scoreboard-link (search-widget-by-type bag 'scoreboard-widget))))

(defun set-entry-score (widget points)
  (set-score (scoreentry-widget-scoreboard-link widget) points))

(defun set-entry-name (widget name)
  (set-tag-name (scoreentry-widget-tag-link widget) name))

(defun get-entry-name (widget)
  (get-tag-name (scoreentry-widget-tag-link widget)))

(defun get-entry-score (widget)
  (get-score (scoreentry-widget-scoreboard-link widget)))

;;------------------------------------------------------------------------------

(defstruct (flipbook-widget (:include widget))
  index
  total)

; (&KEY) -> FLIPBOOK-WIDGET
(defun make-flipbook (&key id images (initial 0))
  (let ((widget (initialize-flipbook (make-flipbook-widget) id images initial)))
    (format t "Created FLIPBOOK-WIDGET: ~A.~%" id)
    widget))

; (...) -> FLIPBOOK-WIDGET
(defun initialize-flipbook (widget id images initial)
  (unless (numberp initial)
    (error "(make-flipbook) FLIPBOOK-WIDGET (~A): Initial value is not a NUMBER: ~A."
           id initial))
  (unless (verify-pixmap-id-list images)
    (error "(make-flipbook) FLIPBOOK-WIDGET (~A): IMAGES contains invalid pixmap id: ~A."
           id images))
  (setf images (loop for x in images collect (make-image x :visible nil)))
  (initialize-struct widget
                     :id id
                     :children images
                     :total (length images)
                     :index initial
                     :width (widget-width (nth 0 images))
                     :height (widget-height (nth 0 images)))
  (flip-the-book-page widget initial)
  widget)

(defun flipbook-page-visible (widget state)
  (setf (image-widget-visible 
          (nth (flipbook-widget-index widget) (widget-children widget))) 
        state))

; () -> NIL
(defun flip-the-book-page (widget index)
  (when (minusp index)
    (format t "(flip-the-book) FLIPBOOK-WIDGET (~A): Warning, negative index: ~A.~%"
            (widget-id widget) index))
  
  (let ((total (flipbook-widget-total widget)))
    (if total
        (let ((index (mod index total)))
          (flipbook-page-visible widget nil)
          (setf (flipbook-widget-index widget) index)
          (flipbook-page-visible widget t))
        (format t "(flip-the-book) FLIPBOOK-WIDGET (~A): Trying to flip empty book to: ~A.~%"
                (widget-id widget) index))))
  
;;------------------------------------------------------------------------------

(defstruct (particle-field-widget (:include widget))
  (check-top t)
  (check-left t)
  (check-bottom t)
  (check-right t)
  particles)

(defun make-particle-field (&key (width *WINDOW-WIDTH*) 
                                 (height *WINDOW-HEIGHT*) 
                                 id
                                 (check-right t)
                                 (check-bottom t)
                                 (check-left t)
                                 (check-top t))
  (let ((widget (make-particle-field-widget
                  :height height
                  :width width
                  :id id
                  :check-right check-right
                  :check-bottom check-bottom
                  :check-left check-left
                  :check-top check-top)))
    (format t "Created PARTICLE-FIELD-WIDGET: ~A.~%" id)
    widget))

(defstruct (particle-sprite (:include sprite-descriptor))
  (death (lambda (particle field)
           (with-slots ((check-right% check-right)
                        (check-bottom% check-bottom)
                        (check-left% check-left)
                        (check-top% check-top)) field
                       (cond
                         ((and check-right% 
                               (> (sprite-x particle) (widget-absolute-right field)))
                          t)
                         ((and check-bottom% 
                               (> (sprite-y particle) (widget-absolute-bottom field)))
                          t)
                         ((and check-left% 
                               (< (sprite-absolute-right particle) (widget-x field)))
                          t)
                         ((and check-top%
                               (< (sprite-absolute-bottom particle) (widget-y field)))
                          t))))))

(defun particle-field-add (field particle)
  (push particle (particle-field-widget-particles field)))

(defun remove-all-particles (widget)
  (setf (particle-field-widget-particles widget) nil))

(defun strip-dead-particles (widget)
  (let ((particles))
    (dolist (particle (particle-field-widget-particles widget))
      (let ((death (particle-sprite-death particle)))
        (if death
            (unless (funcall death particle widget)
              (push particle particles))
            (push particle particles))))
    (setf (particle-field-widget-particles widget) (reverse particles))))

(defmethod widget-event-paint ((w particle-field-widget) renderer tick)
  (dolist (particle (particle-field-widget-particles w))
    (paint-sprite particle renderer tick))
  (strip-dead-particles w))

;;------------------------------------------------------------------------------

;; (0 0 0 0 1 2 4 6 9 11 12 12 11 9 6 4 2 1 0)

(defparameter *T3*
  '(3 4 5 6 7 8 4 5 6 7 8 9 5 6 7 8 9 10 6 7 8 9 10 11 7 8 9 10 11 12 8 9 10 11 12 13 4 5 6 7 8 9 5 6 7
      8 9 10 6 7 8 9 10 11 7 8 9 10 11 12 8 9 10 11 12 13 9 10 11 12 13 14 5 6 7 8 9 10 6 7 8 9 10 11 7 8
      9 10 11 12 8 9 10 11 12 13 9 10 11 12 13 14 10 11 12 13 14 15 6 7 8 9 10 11 7 8 9 10 11 12 8 9 10
      12 13 9 10 11 12 13 14 10 11 12 13 14 15 11 12 13 14 15 16 7 8 9 10 11 12 8 9 10 11 12 13 9 10
      11 12 13 14 10 11 12 13 14 15 11 12 13 14 15 16 12 13 14 15 16 17 8 9 10 11 12 13 9 10 11 12 13 14
      11 12 13 14 15 11 12 13 14 15 16 12 13 14 15 16 17 13 14 15 16 17 18))

(defstruct (star-rain-widget (:include widget))
  (dice (alexandria:shuffle (copy-list *T3*)))
  field
  startx
  starty
  pixmaps
  (tick 0))

(defun make-star-rain (pixmaps &key 
                               id 
                               (width *WINDOW-WIDTH*) 
                               (height *WINDOW-HEIGHT*)
                               (startx 0)
                               (starty 0))
  (let* ((field (make-particle-field :width width :height height :check-top nil))
         (widget (make-star-rain-widget
                  :id id
                  :width width
                  :height height
                  :children (list field)
                  :field field
                  :startx startx
                  :starty starty
                  :pixmaps (get-pixmaps-array pixmaps))))
    (format t "Created STAR-RAIN-WIDGET: ~A.~%" id)
    widget))

(defmethod widget-event-open ((w star-rain-widget))
  (remove-all-particles (star-rain-widget-field w)))

(defmethod widget-event-paint :after ((w star-rain-widget) renderer tick)
  (when (> (- tick (star-rain-widget-tick w)) 20)
    (setf (star-rain-widget-tick w) tick)
    (with-slots ((dice% dice)) w
                (let ((n (car dice%)))
                  (setf dice% (cdr dice%))
                  (when (>= n 14)
                    (incf (star-rain-widget-tick w) 250)
                    (add-star w))
                  (setf dice% (alexandria:shuffle (copy-list *T3*)))))))

(defstruct (star-particle (:include particle-sprite))
  (rotation (nth (random 2) '(-1 1))))

(defun add-star (widget)
  (particle-field-add (star-rain-widget-field widget)
                      (make-star-particle
                        :pixmap (aref (star-rain-widget-pixmaps widget) 
                                      (random (length (star-rain-widget-pixmaps widget))))
                        :velocity (list 0 70)
                        :acceleration (list 0 0)
                        :pos (list (star-rain-widget-startx widget) 
                                   (star-rain-widget-starty widget))
                        :transform-x (lambda (sprite tick)
                                       (+ (sprite-descriptor-x sprite)
                                          (floor (* (* 50 (star-particle-rotation sprite))
                                                    (sin (/ tick 1000.0)))))))))

