;; sprite.lisp
;; Copyright Parasite Network 2018
;; GPL3

(defstruct texture-descriptor 
  texture
  width
  height)

(defun create-texture-from-file (path renderer)
  (let ((surface (sdl2-image:load-image path)))
    (unless surface
      (error "Failed to create surface for file: ~A." path))
    (let ((width (sdl2:surface-width surface))
          (height (sdl2:surface-height surface)))
      (let ((texture (sdl2:create-texture-from-surface renderer surface)))
        (unless texture
          (error "Failed to create texture for surface: ~A" path))
        (sdl2:free-surface surface)
        (make-texture-descriptor
          :texture texture
          :width width
          :height height)))))

(defparameter *TEXTURE-DESCRIPTORS* nil)
(defparameter *PIXMAP-DESCRIPTORS* nil)

(defun initialize-sprites ()
  (setf *TEXTURE-DESCRIPTORS* (make-hash-table :test #'equal))
  (setf *PIXMAP-DESCRIPTORS* (make-hash-table :test 'eq)))

(defun get-texture (path renderer)
  (let ((texture (gethash path *TEXTURE-DESCRIPTORS*)))
    (if texture
        texture
        (let ((descriptor (create-texture-from-file path renderer)))
          (format t "Loaded texture (~Ax~A): ~A.~%" 
                  (texture-descriptor-width descriptor)
                  (texture-descriptor-height descriptor)
                  path)
          (setf (gethash path *texture-descriptors*) descriptor)
          descriptor))))

(defun destroy-textures ()
  (loop for key being each hash-key of *TEXTURE-DESCRIPTORS*
        using (hash-value value)
        do (progn
             (format t "Destroying texture: ~A.~%" key)
             (sdl2:destroy-texture (texture-descriptor-texture value)))))        

;;------------------------------------------------------------------------------

(defstruct pixmap-descriptor
  "A pixmap descriptor describes a region in a texture."
  id 			; self id
  texture		; the texture-descriptor
  width 		; grid width
  height 		; grid height
  x 			; x index, ie column index
  y 			; y index, ie row index
  absolute-x  ; absolute x within the texture
  absolute-y  ; absolute y within the texture
  rectangle)	; precomputed sdl rectangle

(defgeneric pixmap-width (pixmap))
(defgeneric pixmap-height (pixmap))

(defmethod pixmap-width ((pixmap pixmap-descriptor))
  (pixmap-descriptor-width pixmap))

(defmethod pixmap-height ((pixmap pixmap-descriptor))
  (pixmap-descriptor-height pixmap))

(defun create-pixmap-descriptor (texture id &optional width height x y)
  "By default the pixmap will cover the entire texture.
  We can use WIDTH HEIGHT X Y to restrict it to the grid cell (X;Y) where
  each grid cell has the shape (WIDTH;HEIGHT)."
  (unless width
    (setf width (texture-descriptor-width texture)))
  (unless height 
    (setf height (texture-descriptor-height texture)))
  (unless x
    (setf x 0))
  (unless y
    (setf y 0))
  (let ((absx (* width x))(absy (* height y)))
    (make-pixmap-descriptor
      :id id
      :texture (texture-descriptor-texture texture)
      :width (the fixnum width)
      :height (the fixnum height)
      :x x
      :y y
      :absolute-x absx
      :absolute-y absy
      :rectangle 
      (sdl2:make-rect
        absx
        absy
        width
        height))))

; (TEXTURE-DESCRIPTOR,KEYWORD,&KEY) -> PIXMAP-DESCRIPTOR
(defun register-new-pixmap-descriptor (texture id &optional width height x y)
  (setf (gethash id *pixmap-descriptors*) 
        (create-pixmap-descriptor texture id width height x y)))

(defun defresource (path renderer &rest formats)
  "The DEFRESOURCE function creates pixmaps from a texture.
  
  formats ::= symbol | (width height cell*)
  width ::= a number
  height ::= a number
  cell ::= (id cellx celly)
  id ::= symbol
  cellx ::= a number
  celly ::= a number"
  (let ((texture (get-texture path renderer)))
    (unless formats
      (error "Resource ~S needs at least one identifier!~%" path))
    (format t "Defining resources for: ~A.~%" path)
    (dolist (fmt formats nil)
      (typecase fmt
                (list 
                  (destructuring-bind (width height &rest cells) fmt
                                      (format t "  Shape (~Ax~A):~%" width height)
                                      (dolist (cell cells)
                                        (destructuring-bind (id x y) cell
                                                            (format t "    Id: ~A. @X: ~A. @Y: ~A.~%"
                                                                    id
                                                                    x
                                                                    y)
                                                            (register-new-pixmap-descriptor 
                                                              texture 
                                                              id 
                                                              width 
                                                              height 
                                                              x
                                                              y)))))
                (keyword
                  (let ((id fmt))
                    (format t "  Shape (~Ax~A):~%"
                            (texture-descriptor-width texture)
                            (texture-descriptor-height texture))
                    (format t "    Id: ~A.~%" id )
                    (register-new-pixmap-descriptor texture id)))
                (otherwise
                  (error "Pixmap id needs to be a keyword, not ~A~%" fmt))))))

(defun get-pixmap (id &key on-failure-nil)
  (let ((pixmap (gethash id *pixmap-descriptors*)))
    (if pixmap
        (copy-pixmap pixmap)
        (unless on-failure-nil
          (error "Can't find requested pixmap ~A~%" id)))))

(defgeneric copy-pixmap (pixmap))
(defmethod copy-pixmap ((pixmap pixmap-descriptor))
  (copy-pixmap-descriptor pixmap))

(defun verify-pixmap-id (id)
  (gethash id *pixmap-descriptors*))

(defun verify-pixmap-id-list (lst)
  ;TODO
  t)

(defun get-pixmaps-array (ids)
  (let ((pixmaps (make-array (list (length ids)))))
    (loop for id in ids for i from 0 do
          (setf (aref pixmaps i) (get-pixmap id)))
    pixmaps))       

;;------------------------------------------------------------------------------

(defstruct animated-pixmap-descriptor
  timeline
  pixmaps
  base
  index
  total)

(defmethod pixmap-width ((anixmap animated-pixmap-descriptor))
  (with-slots ((pixmaps% pixmaps)
               (index% index)) anixmap
              (pixmap-width (aref pixmaps% index%))))

(defmethod pixmap-height ((anixmap animated-pixmap-descriptor))
  (with-slots ((pixmaps% pixmaps)
               (index% index)) anixmap
              (pixmap-height (aref pixmaps% index%))))

(defmethod copy-pixmap ((anixmap animated-pixmap-descriptor))
  (copy-animated-pixmap-descriptor anixmap))

; TODO Accept charmap autoindex
(defun defanimation (timeline pixmaps id)
  (let ((pixmap (make-animated-pixmap-descriptor
                  :timeline (make-array (list (length timeline)) :initial-contents timeline)
                  :pixmaps (get-pixmaps-array pixmaps)
                  :base 0
                  :index 0
                  :total (length timeline))))
    (setf (gethash id *pixmap-descriptors*) pixmap))) 

(defun get-animation-pixmap (anixmap tick)
  (with-slots ((timeline% timeline)
               (pixmaps% pixmaps)
               (base% base)
               (index% index)
               (total% total)) anixmap
              (when (= base% 0)
                (setf base% tick))
              (let ((target (+ base% (aref timeline% index%))))
                (when (>= tick target)
                  (setf base% tick)
                  (setf index% (mod (1+ index%) total%)))
                (aref pixmaps% index%))))

(defun restart-animation (anixmap &key index tick)
  (when index
    (setf index (mod index (animation-descriptor-total anixmap)))
    (setf (animation-descriptor-index anixmap) index))
  (when tick
    (setf (animation-descriptor-tick anixmap) tick)))



;;------------------------------------------------------------------------------

;;------------------------------------------------------------------------------

(defstruct charmap-descriptor
  "A CHARMAP-DESCRIPTOR overlays a texture with a grid. Sprites can then
  be extracted through indexing."
  id 			; self id
  texture 	; the texture-descriptor
  width 		; grid width
  height 		; grid height
  row			; number of chars on a row
  total		; total number of chars UNUSED!
  cache)		; computed sprite-descripors

(defparameter *charmaps* (make-hash-table :test 'eq))

(defun get-charmap (id &key on-failure-nil)
  (let ((charmap (gethash id *charmaps*)))
    (if charmap
        charmap
        (unless on-failure-nil
          (error "Unable to get charmap ~A~%" id)))))

(defun create-charmap-descriptor (texture id width height row total)
  (make-charmap-descriptor
    :id id
    :texture texture
    :width width
    :height height
    :row row
    :total (if total
               total
               (* width height))
    :cache (make-hash-table :test #'eq)))

(defun defcharmap (path renderer id width height row &optional total)
  "Creates a charmap descriptor over the specified image FILE, and
	 names it ID. WIDTH and HEIGHT are the dimensions of the grid cells,
	 and ROW is the number of cells per row, and TOTAL is the number
	 of valid indexes."
  (let ((texture (get-texture path renderer)))
    (let ((charmap (create-charmap-descriptor texture id width height row total)))
      (setf (gethash id *charmaps*) charmap))))

(defun extract-pixmap (charmap index)
  "Creates a sprite descriptor over the charmap at the specified index."
  (unless (or (>= index 0) (< index (charmap-descriptor-total charmap)))
    (error "charmap->sprite: Index ~A is out of bounds for ~A~%" index charmap))
  (let ((descriptor
          (create-pixmap-descriptor
            (charmap-descriptor-texture charmap)
            index
            (charmap-descriptor-width charmap)
            (charmap-descriptor-height charmap)
            (rem index (charmap-descriptor-row charmap)) ; x 
            (floor (/ index (charmap-descriptor-row charmap)))))) ; y
    (format t "Created charmap (~A) pixmap index: ~A.~%"
            (charmap-descriptor-id charmap) index)
    (update-cached-descriptor charmap index descriptor)
    descriptor))

(defun get-charmap-pixmap (charmap index)
  "Retrieves a pixmap descriptor for the CHARMAP descriptor at INDEX.
	 It first checks if there already is one in the cache, otherwise it
	 creates a new one."
  (let ((descriptor (gethash index (charmap-descriptor-cache charmap))))
    (if descriptor
        descriptor
        (extract-pixmap charmap index))))

(defun export-charmap-pixmap (charmap index id)
	"Names the sprite descriptor over CHARMAP at INDEX to ID and makes
	 it available through FIND-SPRITE."
	(setf (gethash id *pixmap-descriptors*) (get-charmap-pixmap charmap index)))

(defun update-cached-descriptor (charmap index descriptor)
	(setf (gethash index (charmap-descriptor-cache charmap)) descriptor))

;;------------------------------------------------------------------------------

(defconstant SPRITEX 0)
(defconstant SPRITEY 2)
(defconstant TICKX 1)
(defconstant TICKY 3)

(defstruct sprite-descriptor
  id
  init
  pixmap
  transform-x
  transform-y
  (pos (make-array '(4) :initial-contents '(0 0 0 0)))
  (velocity (make-array '(4) :initial-contents '(0 0 0 0)))
  (acceleration (make-array '(4) :initial-contents '(0 0 0 0)))
  (jerk (make-array '(4) :initial-contents '(0 0 0 0))))

(defmethod print-object ((sprite sprite-descriptor) stream)
  (format stream "[sprite-descriptor: ~A. pixmap-id: ~A. "
          (sprite-descriptor-id sprite)
          (pixmap-descriptor-id (sprite-descriptor-pixmap sprite)))
  (format stream "(~A;~A) + (~A;~A) px/s¹ + (~A;~A) px/s² + (~A;~A) px/s³]~%"
          (aref (sprite-descriptor-pos sprite) SPRITEX)
          (aref (sprite-descriptor-pos sprite) SPRITEY)
          (aref (sprite-descriptor-velocity sprite) SPRITEX)
          (aref (sprite-descriptor-velocity sprite) SPRITEY)
          (aref (sprite-descriptor-acceleration sprite) SPRITEX)
          (aref (sprite-descriptor-acceleration sprite) SPRITEY)
          (aref (sprite-descriptor-jerk sprite) SPRITEX)
          (aref (sprite-descriptor-jerk sprite) SPRITEY)))

(defun sprite-x (sprite)
  (sprite-descriptor-x sprite))

(defun sprite-descriptor-x (sprite)
  (aref (sprite-descriptor-pos sprite) SPRITEX))

(defun sprite-y (sprite)
  (sprite-descriptor-y sprite))

(defun sprite-descriptor-y (sprite)
  (aref (sprite-descriptor-pos sprite) SPRITEY))

(defun sprite-absolute-right (sprite)
  (+ (sprite-x sprite) (pixmap-width (sprite-descriptor-pixmap sprite))))


(defun sprite-absolute-bottom (sprite)
  (+ (sprite-y sprite) (pixmap-height (sprite-descriptor-pixmap sprite))))

(defun make-sprite (&key id pixmap pos velocity acceleration jerk transform-x transform-y)
  (let ((sprite (make-sprite-descriptor
                  :id id
                  :pixmap pixmap
                  :pos pos
                  :velocity velocity
                  :acceleration acceleration
                  :jerk jerk
                  :transform-x transform-x
                  :transform-y transform-y)))
    sprite))

(defun change-sprite-position (sprite px py)
  (let ((pos (sprite-descriptor-pos sprite)))
    (setf (sprite-descriptor-pos sprite)
          (make-array '(4) :initial-contents
                      (list px
                            (aref pos 1)
                            py
                            (aref pos 3))))))

(defun change-sprite-velocity (sprite vx vy)
  (let ((vel (sprite-descriptor-velocity sprite)))
    (setf (sprite-descriptor-velocity sprite) 
          (make-array '(4) :initial-contents
                      (list vx
                            (aref vel 1)
                            vy
                            (aref vel 3))))))

(defun change-sprite-acceleration (sprite ax ay)
  (let ((acc (sprite-descriptor-acceleration sprite)))
    (setf (sprite-descriptor-acceleration sprite)
          (make-array '(4) :initial-contents
                      (list ax
                            (aref acc 1)
                            ay
                            (aref acc 3))))))


(defun change-sprite-jerk (sprite jx jy)
  (let ((jerk (sprite-descriptor-jerk sprite)))
    (setf (sprite-descriptor-jerk sprite)
          (make-array '(4) :initial-contents
                      (list jx
                            (aref jerk 1)
                            jy
                            (aref jerk 3))))))

(defun stop-sprite (sprite)
  (change-velocity sprite 0 0)
  (change-sprite-acceleration sprite 0 0)
  (change-sprite-jerk sprite 0 0))

(defun update-movement (sprite tick)
  (dolist (s (list SPRITEX SPRITEY))
    (update-dependency
      (sprite-descriptor-acceleration sprite)
      (sprite-descriptor-jerk sprite)
      s
      tick)
    (update-dependency
      (sprite-descriptor-velocity sprite)
      (sprite-descriptor-acceleration sprite)
      s
      tick)
    (update-dependency
      (sprite-descriptor-pos sprite)
      (sprite-descriptor-velocity sprite)
      s
      tick)))

(defun update-dependency (dependant depender s tick)
  (let ((xtick (+ 1 s)))
    (if (zerop (aref depender xtick))
        (setf (aref depender xtick) tick)
        (let ((v (aref depender s)))
          (when v
            (let* ((diff (- tick (aref depender xtick)))
                   (frac (/ diff (float INTERNAL-TIME-UNITS-PER-SECOND)))
                   (Δv (truncate (* frac v))))
              (when (not (= Δv 0))
                (incf (aref dependant s) Δv)
                (setf (aref depender xtick) tick))))))))

(defun initialize-sprite (sprite tick)
  (unless (sprite-descriptor-init sprite)
    (with-slots ((pos% pos)
                 (vel% velocity)
                 (acc% acceleration)
                 (jerk% jerk)) sprite
                (setf pos% (make-array '(4) :initial-contents
                                       (list (elt pos% 0)
                                             tick
                                             (elt pos% 1)
                                             tick)))
                (setf vel% (make-array '(4) :initial-contents
                                       (list (elt vel% 0)
                                             0
                                             (elt vel% 1)
                                             0)))
                (setf acc% (make-array '(4) :initial-contents
                                       (list (elt acc% 0)
                                             0
                                             (elt acc% 1)
                                             0)))
                (setf jerk% (make-array '(4) :initial-contents
                                       (list (elt jerk% 0)
                                             0
                                             (elt jerk% 1)
                                             0)))
                (setf (sprite-descriptor-init sprite) t))))

(defun paint-sprite (sprite renderer tick)
  (initialize-sprite sprite tick)
  (update-movement sprite tick)
  (let ((pos (sprite-descriptor-pos sprite)))
    (let ((x (aref pos SPRITEX))
          (y (aref pos SPRITEY)))
      (let ((tx (sprite-descriptor-transform-x sprite)))
        (when tx
          (setf x (funcall tx sprite (- tick (aref pos TICKX))))))
      (let ((ty (sprite-descriptor-transform-y sprite)))
        (when ty
          (setf y (funcall ty sprite (- tick (aref pos TICKY))))))
      (paint-descriptor (sprite-descriptor-pixmap sprite)
                        renderer
                        x
                        y
                        tick))))

;;------------------------------------------------------------------------------

;;------------------------------------------------------------------------------

(defgeneric paint-descriptor (descriptor renderer x y tick))
(defmethod paint-descriptor ((descriptor pixmap-descriptor) renderer x y tick)
  (let ((source-rect (pixmap-descriptor-rectangle descriptor)))
    (unless source-rect
      (error "Failed to get rect for ~A~%" descriptor))
    (paint-descriptor-with-rect descriptor renderer x y source-rect)))

(defmethod paint-descriptor ((descriptor animated-pixmap-descriptor) renderer x y tick)
  (let ((pixmap (get-animation-pixmap descriptor tick)))
    (paint-descriptor pixmap renderer x y tick)))

(defun paint-descriptor-with-rect (descriptor renderer x y source-rect)
  (let ((dest-rect (sdl2:make-rect
                     x
                     y
                     (sdl2:rect-width source-rect)
                     (sdl2:rect-height source-rect))))
    (sdl2:render-copy
      renderer
      (pixmap-descriptor-texture descriptor)
      :source-rect source-rect
      :dest-rect dest-rect)
    (sdl2:free-rect dest-rect)))
