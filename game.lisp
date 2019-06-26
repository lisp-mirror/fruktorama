;; game.lisp
;; Copyright Parasite Network 2019
;; GPL3

(in-package :f3)

(defstruct grid
  array
  dropx
  blokks
  width
  height)

(defun create-grid (width height dropx)
  (make-grid
    :array (make-array (list height width) :initial-element nil)
    :width width
    :height height
    :dropx dropx))

;;------------------------------------------------------------------------------
;; Printing 

(defmethod print-object ((grid grid) stream)
  (print-grid grid stream))

(defun print-grid-matches (grid)
  (print-grid grid *standard-output* (grid-blokks grid)))

(defun print-grid (grid stream &optional matches)
  (format t "~&")
  (loop for y from 0 below (grid-height grid) do
        (print-grid-row grid stream y matches))
  (print-grid-bottom-columns grid stream))

(defun print-grid-row (grid stream y matches)
  (format stream "~2D▌" y)
  (loop for x from 0 below (grid-width grid) do
        (print-grid-row-cell grid stream x y matches))
  (format stream "▐~2D~%" y))

(defun print-grid-row-cell (grid stream x y matches)
  (if (empty? grid x y)
      (format stream "   ")
      (let ((element (at grid x y)))
        (if matches
            (print-grid-row-cell-matches element stream x y matches)
            (print-grid-row-cell-normal element stream)))))

(defun print-grid-row-cell-normal (element stream)
  (format stream "[~D]" element))

(defun print-grid-row-cell-matches (element stream x y matches)
  (if (member (make-blokk :x x :y y :element element) 
              matches 
              :test #'comp-blokks-xy)
      (print-grid-row-cell-normal element stream)
      (format stream "███")))

(defun comp-blokks-xy (a b)
  (and (equal (blokk-x a) (blokk-x b))
       (equal (blokk-y a) (blokk-y b))))

(defun print-grid-bottom-columns (grid stream)
  (format stream "  ▙")
  (loop for x from 0 below (grid-width grid) do
        (format stream "▄█▄"))
  (format stream "▟~%")
  (format stream "   ")
  (loop for x from 0 below (grid-width grid) do
        (format stream " ~D " x))
  (format stream "~%"))

;;------------------------------------------------------------------------------

(defstruct blokk
  x
  y
  element)

;;------------------------------------------------------------------------------
;; Printing

(defun print-blokks (blokks)
  (format t "~&")
  (loop for b in blokks
        maximize (blokk-y b) into ymax
        minimize (blokk-y b) into ymin
        maximize (blokk-x b) into xmax
        minimize (blokk-x b) into xmin
        finally (return (print-blokks-layout blokks ymax ymin xmax xmin))))

(defun print-blokks-layout (blokks ymax ymin xmax xmin)
  (loop for y from ymin upto ymax do
        (loop for x from xmin upto xmax do
              (let ((blokk (find-if (lambda (b)
                                  (and
                                    (eql (blokk-y b) y)
                                    (eql (blokk-x b) x))) blokks)))
                (if blokk
                    (format t "[~D]" (blokk-element blokk))
                    (format t "   ")))
              finally (format t "~%"))))

;;------------------------------------------------------------------------------

; (GRID,INT) -> T | NIL
(defun valid-x (grid x)
  (and
    (>= x 0)
    (< x (grid-width grid))))

; (GRID,INT) -> T | NIL
(defun valid-y (grid y)
  (and
    (>= y 0)
    (< y (grid-height grid))))


; (GRID,INT,INT) -> ? | NIL
(defun at (grid x y)
  (when (valid-x grid x)
    (when (valid-y grid y)
      (aref (grid-array grid) y x)))) ; ROW COLUMN

; (GRID,INT,INT,INT:value) -> INT:value
(defun update-grid-at (grid x y value)
  "Update function for SETF."
  (unless (valid-x grid x)
    (error "Cannot set x: ~A in grid with width: ~A." x (grid-width grid)))
  (unless (valid-y grid y)
    (error "Cannot set y: ~A in grid with height: ~A." y (grid-height grid)))
  (setf (aref (grid-array grid) y x) value)) ; ROW COLUMN

(defsetf at update-grid-at)

; (GRID,INT,INT) -> T | NIL
(defun empty? (grid x y)
  (null (at grid x y)))


; (GRID,BLOKK) -> T | NIL
(defun placeable-blokk? (grid blokk)
  (let ((x (blokk-x blokk))
        (y (blokk-y blokk)))
    (and
      (valid-x grid x)
      (valid-y grid y)
      (empty? grid (blokk-x blokk) (blokk-y blokk)))))


; (GRID, LIST<BLOKK>) -> T | NIL
(defun placeable-blokks? (grid blokks)
  (every (lambda (blokk)
           (placeable-blokk? grid blokk))
         blokks))

; (GRID,INT,INT) -> NIL
(defun clear-grid-at (grid x y)
  (setf (at grid x y) nil))

; (GRID,BLOKK) -> NIL
(defun place-on-grid (grid blokk)
  (setf (at grid (blokk-x blokk) (blokk-y blokk)) (blokk-element blokk))
  nil)

; (GRID) -> NIL
(defun clear-blokks (grid)
  (setf (grid-blokks grid) nil))

;;------------------------------------------------------------------------------

; (GRID,LIST<INT>) -> T | NIL
(defun spawn-3-blokks (grid elements)
  "Creates three new blocks and puts them in GRID:BLOKKS.
  It returns whether or not they are placeable on the grid."
  (let ((blokks
          (loop for y from 0 below 3 collect
                (make-blokk 
                  :x (grid-dropx grid)
                  :y y 
                  :element (nth y elements)))))
    (setf (grid-blokks grid) blokks)
    (when (placeable-blokks? grid blokks)
      t)))


; (LIST<BLOKK>,INT,INT) -> LIST<BLOKK>
(defun make-delta-blokks (blokks Δx Δy)
  (loop for blokk in blokks
        collect (make-blokk
                  :x (+ (blokk-x blokk) Δx)
                  :y (+ (blokk-y blokk) Δy)
                  :element (blokk-element blokk))))


; (GRID,INT,INT) -> T:LIST<BLOKK> | NIL
(defun move-blokks (grid Δx Δy)
  "Tries to move GRID:BLOKKS by Δx and Δy."
  (let ((Δblokks (make-delta-blokks (grid-blokks grid) Δx Δy)))
    (if (placeable-blokks? grid Δblokks)
        (setf (grid-blokks grid) Δblokks)
        nil)))


; (GRID) -> NIL
(defun place-blokks (grid)
  "Will copy GRID:BLOKKS onto the grid."
  (dolist (blokk (grid-blokks grid) nil)
    (setf (at grid (blokk-x blokk) (blokk-y blokk)) (blokk-element blokk))))


; (GRID) -> NIL
(defun unplace-blokks (grid)
  "It will clear all grids cells in named in GRID:BLOKKS."
  (dolist (blokk (grid-blokks grid) nil)
    (clear-grid-at grid (blokk-x blokk) (blokk-y blokk))))

; (GRID) -> INT
(defun drop-3-blokks (grid)
  "Will only drop the three playable blocks."
  (when (= (length (grid-blokks grid)) 3)
    (loop while (move-blokks grid 0 1) for drops from 0 count drops)))

; (GRID) -> NIL
(defun rotate-3-blokks (grid)
  "Will only rotate the three playable blocks."
  (when (= (length (grid-blokks grid)) 3)
    (let ((blokks (grid-blokks grid)))
      (rotatef (blokk-element (nth 0 blokks))
               (blokk-element (nth 1 blokks))
               (blokk-element (nth 2 blokks))))))

; (GRID,(INT:x,INT:y,INT:element) -> ?) -> NIL
(defun foreach-grid-element (grid callback)
  (dotimes (y (grid-height grid) nil)
    (dotimes (x (grid-width grid) nil)
      (unless (empty? grid x y)
        (funcall callback x y (at grid x y))))))

; (LIST<BLOKK>,(INT:x,INT:y,INT:element) -> ?) -> NIL
(defun foreach-blokk (blokks callback)
  (dolist (blokk blokks nil)
    (funcall callback (blokk-x blokk) (blokk-y blokk) (blokk-element blokk))))

;---------------------------------------------------------------------------
;; Matches

; (GRID) -> LIST<BLOKK> | NIL
(defun find-matches (grid)
  "FIND-MATCHES is used to find matches using the blocks
  in ORIGINS as origins. All match sets are combined
  and into a single match set. This set is then sorted
  in -X(-Y..+Y)..+X order. Since we have combined sets
  from several potentially overlapping matches we need
  to remove all duplicate blocks. This final set is
  then returned."
  (let ((matches))
    (dolist (blokk (grid-blokks grid))
      (setf matches (append matches (match-from-origin grid blokk))))
    (when matches
      (let ((sorted (sort-matches matches)))
        (let ((final (remove-duplicates sorted :test #'compare-blokks-xy)))
          (setf (grid-blokks grid) final))))))

; (LIST<BLOKK>) -> LIST<BLOKK>
(defun sort-matches (blokks)
  "Sorts the blocks -X(-Y..+Y)..+X."
  (sort blokks (lambda (lhs rhs)
                 (if (= (blokk-x lhs) (blokk-x rhs))
                     (< (blokk-y lhs) (blokk-y rhs))
                     (< (blokk-x lhs) (blokk-x rhs))))))

; (BLOKK,BLOKK) -> T | NIL
(defun compare-blokks-xy (lhs rhs)
  (when (= (blokk-y lhs) (blokk-y rhs))
    (= (blokk-x lhs) (blokk-x rhs))))

(defvar +Y 1)
(defvar -Y -1)
(defvar +X 1)
(defvar -X -1)

; (GRID,LIST<BLOKK>) -> LIST<BLOKK> | NIL
(defun match-from-origin (grid origin)
  "Using ORIGIN as the starting point we try to match
  diagonally and orthogonally using straight lines.
  Bent lines is possible, but not used in this game.
  All matches are combined and returned.
  
           (-Y)
          1  2  3
           \ | /
            \|/
      (-X)4--X--4(+X)
            /|\
           / | \
          3  2  1 
           (+Y)
  "
  (append
    ;; 1
    (match-line grid origin -X -Y +X +Y)
    ;; 2
    (match-line grid origin 0 +Y 0 -Y)
    ;; 3
    (match-line grid origin +X -Y -X +Y)
    ;; 4
    (match-line grid origin +X 0 -X 0)))

; (GRID,LIST<BLOKK>,INT,INT,INT,INT) -> LIST<BLOKK> | NIL
(defun match-line (grid origin Δxa Δya Δxb Δyb)
  "The line is described by the two vectors (Δxa;Δya)
  and (Δxb;Δyb) from the origin. Using the vectors
  we cast two 'rays' returning any consecutive matches
  along the lines. 
  We combine the ray matches and the origin into the
  final set and if there are least three matches we
  return the set, otherwise nil."
  (let ((match (append
                (list origin)
                (match-ray grid origin Δxa Δya)
                (match-ray grid origin Δxb Δyb))))
    (when (>= (length match) 3)
      match)))

; (GRID,LIST<BLOKK>,INT,INT) -> LIST<BLOKK> | NIL
(defun match-ray (grid origin Δx Δy)
  "We match the origin's element along the vector (Δx;Δy)
  and return the matches exluding the origin."
  (loop
    for x = (+ (blokk-x origin) Δx) then (+ x Δx)
    for y = (+ (blokk-y origin) Δy) then (+ y Δy)
    with element = (blokk-element origin)
    while (equal (at grid x y) element)
    collect (make-blokk :x x :y y :element element)))

;---------------------------------------------------------------------------

; (GRID) -> LIST<BLOKK> | NIL
(defun reap (grid)
  "Removes the current GRID:BLOKKS from the grid and moves down everything
  hanging above. What is moved down is set to GRID:BLOKKS and returned."
  (unplace-blokks grid)
  (let ((ridge (get-bottom-ridge (grid-blokks grid))))
    (let ((dropped (drop-ridge grid ridge)))
      (setf (grid-blokks grid) dropped))))

; (LIST<BLOKK>) -> LIST<BLOKK>
(defun get-bottom-ridge (blokks)
  "Since BLOKKS is sorted according to -X(-Y..+Y)..+X we can simply
  remove all duplicate X in the columns leaving us with the bottom +Y.
  We want the bottom ridge instead of the top ridge since matches
  can cut through the block columns leaving blocks inbetween, for example:
  [2]      
     [2]   
        [2]
     [2]   
  [2]"
  (remove-duplicates blokks :key #'blokk-x))

; (GRID,LIST<BLOKK>:ridge) -> LIST<BLOKK>:dropped
(defun drop-ridge (grid ridge)
  "For each X we move up the column (-Y) and drop each fruit as
  far down as it goes. We return the set of all dropped fruits."
  (let ((dropped))
    (dolist (blokk ridge dropped)
      (let ((x (blokk-x blokk)))
        (loop for y from (blokk-y blokk) downto 0 do           
              (let ((drop (drop-xy grid x y)))
                (when drop
                  (place-on-grid grid drop)
                  (push drop dropped))))))))

; (GRID,INT,INT) -> BLOKK
(defun drop-xy (grid x y)
  (let ((element (at grid x y)))
    (when element
      (clear-grid-at grid x y)
      (let ((yy (drop-y grid x y)))
        (make-blokk
          :x x
          :y yy
          :element element)))))
        
; (GRID,INT,INT) -> INT
(defun drop-y (grid x y)
  (let ((yy y))
    (loop while (and
                  (< (1+ yy) (grid-height grid))
                  (empty? grid x (1+ yy)))
          do (incf yy))
    yy))

;;------------------------------------------------------------------------------

(defconstant +OFFSET-X+ 5)
(defconstant +OFFSET-Y+ 4)
(defconstant +SPACING-X+ 1)
(defconstant +CELL-WIDTH+ 16)
(defconstant +CELL-HEIGHT+ 16)
(defconstant +GAME-WIDTH+ 128)
(defconstant +GAME-HEIGHT+ 329)

(defparameter *fruits* '((0 :apple :black-apple)
                         (0 :pear :black-pear)
                         (0 :banana :black-banana)
                         (1000 :blueberry :black-blueberry)
                         (1200 :orange :black-orange)
                         (1400 :cherry :black-cherry)
                         (1600 :melon :black-melon)
                         (2000 :lemon :black-lemon)
                         (3000 :pineapple :black-pineapple)
                         (3000 :grapes :black-grapes))
  "A sorted list of fruit pixmap identifiers and the associated
  minimum points needed to unlock them within the game.")

(defun unlocked (points)
  (loop for fruit in *fruits* with i = 0
        while (>= points (car fruit))
        do (incf i 1)
        finally (return i)))

(defstruct (game-widget (:include glas:widget))
  background-pixmap
  grid
  status
  tick
  paused
  highlight
  points
  fruit-pixmaps
  black-pixmaps)

; (&KEY) -> GAME-WIDGET
(defun make-game (&key id background bare)
  (let ((widget (make-game-widget 
                  :id id
                  :width +GAME-WIDTH+
                  :height +GAME-HEIGHT+
                  :background-pixmap (unless bare 
                                       (get-pixmap background :on-failure-nil t)))))
    (unless bare
      (setf (game-widget-fruit-pixmaps widget) 
            (get-pixmaps-array (mapcar #'cadr *fruits*)))
      (setf (game-widget-black-pixmaps widget)
            (get-pixmaps-array (mapcar #'caddr *fruits*))))
    (initialize-game widget)
    (format t " Created GAME-WIDGET: ~A.~%" id)
    widget))

; (GAME-WIDGET) -> GAME-WIDGET
(defun reset-game (widget)
  (initialize-game widget)
  (dispatch-event :RESET-SCORE nil))

; (GAME-WIDGET) -> GAME-WIDGET
(defun initialize-game (widget)
  (initialize-struct widget
                     :grid (create-grid 7 20 3)
                     :status :new
                     :tick 0
                     :paused nil
                     :highlight nil
                     :points 0))

; (INT,INT) -> INT
(defun grid-effective-x (cellx x)
  "Calculates the absolute x coordinate for CELLX."
  (+ 
    (* cellx +CELL-WIDTH+) 
    (* cellx +SPACING-X+)
    +OFFSET-X+
    x))

; (INT,INT) -> INT
(defun grid-effective-y (celly y)
  "Calculates the absolute y coordinate for CELLY."
  (+
    (* celly +CELL-HEIGHT+)
    +OFFSET-Y+
    y))

(defmethod widget-event-paint ((widget game-widget) renderer tick)
  (update-game widget tick)
  (let ((x (widget-x widget))
        (y (widget-y widget)))
    (paint-descriptor (game-widget-background-pixmap widget) renderer x y tick)
    (paint-grid-fruits widget x y renderer tick)
    (paint-falling-fruits widget x y renderer tick)
    (paint-highlighted-fruits widget x y renderer)))

(defun paint-fruit (game x y renderer fruit gridx gridy tick)
  (let ((pixmap (aref (game-widget-fruit-pixmaps game) fruit)))
    (paint-descriptor pixmap renderer
                      (grid-effective-x gridx x)
                      (grid-effective-y gridy y)
                      tick)))

(defun paint-falling-fruits (game x y renderer tick)
  (let ((grid (game-widget-grid game)))
    (foreach-blokk (grid-blokks grid)
                   (lambda (gridx gridy fruit)
                     (paint-fruit game x y renderer fruit gridx gridy tick)))))

(defun paint-grid-fruits (game x y renderer tick)
  (foreach-grid-element (game-widget-grid game)
                   (lambda (gridx gridy fruit)
                     (paint-fruit game x y renderer fruit gridx gridy tick))))
  
(defun paint-highlighted-blokks (game x y renderer blokks blend r b g a)
  (when blokks
      (sdl2:set-render-draw-color renderer r b g a)
      (sdl2:set-render-draw-blend-mode renderer blend)
      (foreach-blokk blokks
                     (lambda (gridx gridy element)
                       (let ((rect (sdl2:make-rect
                                     (grid-effective-x gridx x)
                                     (grid-effective-y gridy y)
                                     16
                                     16)))
                         (sdl2:render-fill-rect renderer rect)
                         (sdl2:free-rect rect))))
      (sdl2:set-render-draw-blend-mode renderer :none)))
  
(defun paint-highlighted-fruits (game x y renderer)
  (paint-highlighted-blokks
    game x y renderer
    (game-widget-highlight game)
    :mod
    255 0 60 0))

(defmacro delay ((game interval tick) &rest body)
  (let ((now (gensym))(old (gensym)))
    `(let ((,now ,tick))
       (let ((,old (game-widget-tick ,game)))
         (when (< ,interval (- ,now ,old))
           (setf (game-widget-tick ,game) ,now)
           ,@body)))))

; (GAME-WIDGET,INT) -> GAME-WIDGET:POINTS
(defun add-game-score (game Δpoints)
  (with-slots ((points% points)
               (scoreboard-id% scoreboard-id)) game
               (incf points% Δpoints)
               (dispatch-event :SET-SCORE points%)))
               ;(when scoreboard-id%
                 ;(let ((scoreboard (find-widget scoreboard-id%)))
                   ;(set-score scoreboard points%)))))

; (INT) -> LIST<INT>
(defun spawn-three-fruits (points)
  (let ((limit (unlocked points)))
    (loop repeat 3 collect (random limit))))

; (GAME-WIDGET) -> LIST<BLOKK>
(defun spawn-new-fruits (game)
  (let ((fruits (spawn-three-fruits (game-widget-points game))))
    (spawn-3-blokks (game-widget-grid game) fruits)))

(defmacro set-state (state newstate)
  `(progn
     (setf ,state ,newstate)
     (format t "Game state changed: ~A.~%" ,newstate)))

(defun random-particle-velocity ()
  (let ((direction (nth (random 2) (list -1 1))))
    (list
      (* direction (+ 20 (random 60)))
      (* direction (+ 5 (random 15))))))

(defun random-particle-acceleration ()
  (list
    0
    (+ 60 (random 60))))

(defun random-particle-jerk ()
  (list
    0
    (+ 0 (random 60))))

(defun create-fruit-particles (game blokks)
  (foreach-blokk blokks
                 (lambda (x y element)
                   (format t "Dispatching (~A;~A)~%" x y)))
  (foreach-blokk blokks
                 (lambda (x y element)
                   (dispatch-event :ADD-PARTICLE
                                   (make-sprite
                                     (aref (game-widget-black-pixmaps game) element)
                                     :velocity (random-particle-velocity)
                                     :acceleration (random-particle-acceleration)
                                     :jerk (random-particle-jerk)
                                     :pos (list
                                            (grid-effective-x x (widget-x game))
                                            (grid-effective-y y (widget-y game))))))))

(defun update-game (game tick)
  "new      -> play | gameover
   play     -> place
   drop     -> delay
   delay    -> place
   place    -> harvest
   harvest  -> new | reap
   reap     -> harvest
  
  NEW is the starting state. It will end up in GAMEOVER after several
  visits through the PLAY path.
  The DROP state is the only abnormal. It will be set when the player
  presses the arrow down button."
  (unless (game-widget-paused game)
    (with-slots ((status% status)
                 (crops% crops)
                 (points% points)
                 (highlight% highlight)
                 (grid% grid)
                 (tick% tick)) game
                (ecase (game-widget-status game)
                       (:new
                         (delay (game 500 tick)
                                (format t "~%~A~%" grid%)
                                (set-state status% :play)
                                (if (spawn-new-fruits game)
                                    (set-state status% :play)
                                    (progn
                                      (setf highlight% (grid-blokks grid%))
                                      (set-state status% :gameover)))))
                       (:play
                         (delay (game 1000 tick)
                                (if (move-blokks grid% 0 1)
                                    (add-game-score game 1)
                                    (set-state status% :place))))
                       (:drop
                         (setf tick% tick) 
                         (let ((dropped (drop-3-blokks grid%)))
                           (add-game-score game dropped))
                         (set-state status% :delay))
                       (:place
                         (place-blokks grid%)
                         (set-state status% :harvest))
                       (:harvest
                         (setf highlight% (find-matches grid%))
                         (if highlight%
                             (progn
                               (print-grid-matches grid%)
                               (set-state status% :reap))
                             (set-state status% :new)))
                       (:reap
                         (delay (game 500 tick)
                                (add-game-score game (* (length highlight%) 10))
                                (create-fruit-particles game highlight%)
                                (setf highlight% nil)
                                (reap grid%)
                                (set-state status% :harvest))) ;; does not place!
                       (:delay
                         (delay (game 200 tick)
                                (set-state status% :place)))
                       (:gameover
                         (set-state status% :dead)
                         (let ((pos (highscore:worthy? points%)))
                           (if pos
                               (progn
                                 (flip-the-book-page (find-widget :emblem-book) pos)
                                 (set-entry-score (find-widget :winnerbox) points%)
                                 (open-window-by-id :gameover-success))
                               (open-window-by-id :gameover-failure))))
                       (:dead)))))

(defmethod widget-event-onkey-down ((w game-widget) key)
  (keycase key
           (:scancode-escape
             (format t "GAME-WIDGET (~A): Escape caught!~%" (widget-id w))
             (set-state (game-widget-status w) :gameover)
             (close-window-by-id :paused)
             (setf (game-widget-paused w) nil)))
  (when (eq (game-widget-status w) :play)
    (let ((grid (game-widget-grid w)))
      (keycase key
               (:scancode-left
                 (move-blokks grid -1 0))
               (:scancode-right
                 (move-blokks grid +1 0))
               (:scancode-up
                 (rotate-3-blokks grid))
               (:scancode-down
                 (set-state (game-widget-status w) :drop))
               (:scancode-space
                 (if (setf (game-widget-paused w) (not (game-widget-paused w)))
                     (open-window-by-id :paused)
                     (close-window-by-id :paused)))))))