;; gadgets.lisp
;; Copyright Parasite Network 2018
;; GPL3

(in-package :f3)

;;------------------------------------------------------------------------------
;; FLIP-WIDIGT
(defstruct (flipper-widget (:include widget))
  iterator)

(defun make-flipper (charmap &key id initial)
  (let ((widget (initialize-flipper (make-flipper-widget)
                                    charmap
                                    id
                                    initial)))
    (format t " Created FLIPPER-WIDGET: ~A.~%" id)
    widget))

(defun initialize-flipper (widget charmap-id id initial)
  (let* ((charmap (get-charmap charmap-id))
         (flipper (initialize-struct widget
                                     :iterator (make-translator-iterator charmap :initial initial)
                                     :id id
                                     :opaque t
                                     :opaque-draw-exception nil
                                     :width (pixmap-width charmap) ;;(charmap-descriptor-width charmap)
                                     :height (pixmap-height charmap)))) ;;(charmap-descriptor-height charmap))))
    flipper))

(defun set-flipper-key (widget key)
  (set-current-iterator-key (flipper-widget-iterator widget) key))

(defun get-flipper-key (widget)
  (get-current-iterator-key (flipper-widget-iterator widget)))

(defmethod widget-event-paint ((widget flipper-widget) renderer tick)
  (paint-descriptor (get-iterator-pixmap (flipper-widget-iterator widget))
                    renderer
                    (widget-x widget) 
                    (widget-y widget) 
                    tick))

(defmethod widget-event-onkey-down ((widget flipper-widget) key)
  (let ((iterator (flipper-widget-iterator widget)))
    (keycase key
             (:scancode-down
               (next iterator)
               t)
             (:scancode-up
               (prev iterator)
               t))))

;;------------------------------------------------------------------------------

(defstruct (tag-widget (:include widget))
  flippers
  total
  selected
  chevron)

(defun make-tag (alphabet-id &key id initial chevron (letters 3))
  (let ((widget (initialize-tag alphabet-id id initial chevron letters)))
    (format t " Created TAG-WIDGET: ~A.~%" id)
    widget))

(defun initialize-tag (alphabet-id id initial chevron letters)
  (unless (or letters (typep letters 'number) (> letters 0))
    (error "(make-tag) TAG-WIDGET (~A) Invalid number of letters: ~A~%" id letters))
  (when initial
    (unless (equal (length initial) letters)
      (error "TAG-WIDGET (~A): The length of INITIAL (~A) must be equal to LETTERS (~A)." id initial letters)))
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
                                       (let ((flipper (make-flipper alphabet-id
                                                                    :initial (if initial
                                                                                 (elt initial i)
                                                                                 nil))))
                                         (setf (widget-local-id flipper) :local-flipper)
                                         flipper)))))))
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
              (let ((chevron (search-widget-by-local-id (aref flippers% selected%) :local-chevron)))
                (when chevron
                  (switch-image-visibility chevron visible)))))

(defun switch-tag-by-offset (widget offset)
  (let ((current (tag-widget-selected widget)))
    (incf current offset)
    (let ((next (mod current (tag-widget-total widget))))
      (switch-tag widget next))))

(defun switch-tag (widget select)
  (unless (and (>= select 0)
               (< select (tag-widget-total widget)))
    (error "TAG-WIDGET (~A): Cannot switch to invalid select: ~A." (widget-id widget) select))
  (show-tag-chevron widget nil)
  (setf (tag-widget-selected widget) select)
  (show-tag-chevron widget t))

(defun set-tag-name (widget name)
  (loop for flipper across (tag-widget-flippers widget) for bokstav across name do
        (set-flipper-key (search-widget-by-local-id flipper :local-flipper) bokstav)))

(defun get-tag-name (widget)
  (coerce (loop for flipper across (tag-widget-flippers widget)
                collect (get-flipper-key (search-widget-by-local-id flipper :local-flipper)))
          'string))

(defmethod widget-event-onkey-down ((widget tag-widget) key)
  (keycase key
           (:scancode-left
             (switch-tag-by-offset widget -1)
             t)
           (:scancode-right
             (switch-tag-by-offset widget +1)
             t)
           (t
             (widget-propagate-onkey-down
               (aref (tag-widget-flippers widget) 
                     (tag-widget-selected widget))
               key))))

;;------------------------------------------------------------------------------
;; RENAME PICTUREBOOK-WIDGET

(defstruct (flipbook-widget (:include widget))
  index
  total)

; (&KEY) -> FLIPBOOK-WIDGET
(defun make-flipbook (&key id images (initial 0))
  (format t "Make FLIPBOOK-WIDGET (~A).~%" id)
  (let ((widget (initialize-flipbook (make-flipbook-widget) id images initial)))
    (format t " Created FLIPBOOK-WIDGET: ~A.~%" id)
    widget))

; (...) -> FLIPBOOK-WIDGET
(defun initialize-flipbook (widget id images initial)
  (format t "Initialize FLIPBOOK-WIDGET (~A).~%" id)
  (unless (numberp initial)
    (error "FLIPBOOK-WIDGET (~A): Initial value is not a NUMBER: ~A." id initial))
  (unless (verify-images-existence images)
    (error "FLIPBOOK-WIDGET (~A): IMAGES contain invalid images: ~A.~%" id images))
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

(defstruct (display-widget (:include widget))
  digits
  links)

(defun make-display (images &key id (digits 5))
  (format t "Make DISPLAY-WIDGET (~A).~%" id)
  (unless (verify-images-existence images)
    (error "DISPLAY-WIDGET (~A): IMAGES contain invalid images: ~A.~%" id images))
  (unless (verify-images-same-dimensions images)
    (error "DISPLAY-WIDGET (~A): All images in IMAGES needs to be of the same dimensions: ~A.~%" id images))
  (unless (equal (length images) 10)
    (error "DISPLAY-WIDGET (~A): There needs to be 10 images in IMAGES counting from 0 to 9: ~A.~%" id images))
  (let ((links (make-array (list digits))))
    (let ((bag (make-bag 
                 :align :middle
                 :widgets (loop for i from 0 below digits 
                                collect (setf (aref links i) (make-flipbook :images images :initial 0))))))
      (let ((display (make-display-widget
                       :id id
                       :digits digits
                       :links links
                       :children (list bag)
                       :width (widget-width bag)
                       :height (widget-height bag)
                       :opaque t
                       :opaque-draw-exception t)))
        (format t " Created DISPLAY-WIDGET: ~A.~%" id)
        display))))

(defun display-select (widget pos digit)
  (flip-the-book-page (aref (display-widget-links widget) pos) digit))

;;------------------------------------------------------------------------------

(defstruct (scoreboard-widget (:include widget))
  points
  digits)

(defun make-scoreboard (images &key id (digits 5) (initial 0) (events nil))
  (format t " Make SCOREBOARD-WIDGET (~A).~%" id)
  (let ((widget (initialize-scoreboard (make-scoreboard-widget) 
                                       images
                                       id 
                                       digits  
                                       initial
                                       events)))
    (format t " Created SCOREBOARD-WIDGET: ~A.~%" id)
    widget))

(defun initialize-scoreboard (widget images id digits initial events)
  (format t "Initialize SCOREBOARD-WIDGET (~A).~%" id)
  (unless (or (numberp digits) (> digits 0))
    (error "SCOREBOARD-WIDGET (~A) (&key DIGITS): Must be a positive number: ~A~%" id digits))
  (unless (or (numberp initial) (>= initial 0))
    (error "SCOREBOARD-WIDGET (~A) (&key INITIAL): Must be a positive number: ~A~%." id initial))
  (unless (verify-images-existence images)
    (error "SCOREBOARD-WIDGET (~A): Unknown image identifier in: ~A~%." id images))
  (unless (verify-images-same-dimensions images)
    (error "SCOREBOARD-WIDGET (~A): All images in IMAGES must be of the same dimensions: ~A.~%" id images))
  (let* ((display (make-display images :digits digits))
         (board (initialize-struct widget
                                   :id id
                                   :width (widget-width display)
                                   :height (widget-height display)
                                   :digits digits
                                   :children (list display)
                                   :opaque t
                                   :opaque-draw-exception t)))
    (set-score board initial)
    (when events
      (attach-event-handler board :SET-SCORE)
      (attach-event-handler board :RESET-SCORE))
    board))

(defun compute-score-display (widget)
  (let* ((textpoints (write-to-string (scoreboard-widget-points widget)))
         (listpoints (coerce textpoints 'list))
         (digits (scoreboard-widget-digits widget))
         (points (last (append (make-list digits :initial-element #\0) 
                               listpoints) 
                       digits)))
  (let ((display (car (widget-children widget))))
    (loop for i from 0 below digits do
          (display-select display i (- (char-code (nth i points)) (char-code #\0)))))))
 
(defgeneric set-score (widget points))
(defmethod set-score ((widget scoreboard-widget) points)
  (setf (scoreboard-widget-points widget) (abs points))
  (compute-score-display widget))

(defgeneric get-score (widget))
(defmethod get-score ((widget scoreboard-widget))
  (scoreboard-widget-points widget))

(defgeneric reset-scoreboard (widget))
(defmethod reset-scoreboard ((widget scoreboard-widget))
  (set-score widget 0))
  
(defmethod widget-event-handle ((widget scoreboard-widget) event payload)
  (cond
    ((eq event :SET-SCORE) (set-score widget payload))
    ((eq event :RESET-SCORE) (reset-scoreboard widget))))

;;------------------------------------------------------------------------------

(defstruct (highscore-widget (:include scoreboard-widget))
  onhigh
  secondary-display)

(defun make-highscore (low-images high-images &key id (digits 5) (initial 0) (events nil))
  (format t "Make HIGHSCORE-WIDGET.~%")
  (let ((board (initialize-highscore (make-highscore-widget)
                                     low-images
                                     high-images
                                     id
                                     digits
                                     initial
                                     events)))
    (format t "Created HIGHSCORE-WIDGET: ~A.~%" id)
    board))

(defun initialize-highscore (widget low-images high-images id digits initial events)
  (format t "Initialize HIGHSCORE-WIDGET (~A).~%" id)
  (unless (verify-images-same-dimensions low-images high-images)
    (error "HIGHSCORE-WIDGET (~A): Images for low score and high score must be of the same dimensions: ~A ~A~%" id low-images high-images))
  (let ((board (initialize-struct (initialize-scoreboard widget low-images id digits initial events)
                                  :onhigh nil
                                  :secondary-display (list (make-display high-images :digits digits)))))
    board))

(defmethod reset-scoreboard ((widget highscore-widget))
  (when (highscore-widget-onhigh widget)
    (setf (highscore-widget-onhigh widget) nil)
    (rotatef (highscore-widget-secondary-display widget)
             (widget-children widget)))
  (call-next-method))

(defmethod set-score ((widget highscore-widget) points)
  (unless (highscore-widget-onhigh widget)
    (when (highscore:worthy? points)
      (setf (highscore-widget-onhigh widget) t)
      (rotatef (highscore-widget-secondary-display widget) (widget-children widget))))
  (call-next-method))

(defmethod widget-event-absolute-xy ((widget highscore-widget))
  (widget-propagate-calculate-xy-children widget
                                          (highscore-widget-secondary-display widget)))

;;------------------------------------------------------------------------------

(defstruct (scoreentry-widget (:include widget))
  tag-link
  scoreboard-link)

(defun make-scoreentry (letter-charmap number-ids &key chevron editable (letters 3) (digits 5) emblem (initial-points 0) initial-name id)
  (let ((bag (make-bag
               :align :center
               :widgets (list 
                          (make-tag letter-charmap 
                                    :initial initial-name 
                                    :letters letters
                                    :chevron chevron)
                          (make-scoreboard number-ids 
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
    (let ((widget (make-scoreentry-widget
                    :children (list bag)
                    :id id
                    :opaque (not editable)
                    :width (widget-width bag)
                    :height (widget-height bag)
                    :tag-link (search-widget-by-type bag 'tag-widget)
                    :scoreboard-link (search-widget-by-type bag 'scoreboard-widget))))
      (format t " Created SCOREENTRY-WIDGET: ~A.~%" id)
      widget)))

(defun set-entry-score (widget points)
  (set-score (scoreentry-widget-scoreboard-link widget) points))

(defun set-entry-name (widget name)
  (set-tag-name (scoreentry-widget-tag-link widget) name))

(defun get-entry-name (widget)
  (get-tag-name (scoreentry-widget-tag-link widget)))

(defun get-entry-score (widget)
  (get-score (scoreentry-widget-scoreboard-link widget)))
  
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
                                 (check-top t)
                                 listen-events)
  (let ((widget (make-particle-field-widget
                  :height height
                  :width width
                  :id id
                  :check-right check-right
                  :check-bottom check-bottom
                  :check-left check-left
                  :check-top check-top
                  :opaque t
                  :opaque-draw-exception nil)))
    (when listen-events
      (attach-event-handler widget :ADD-PARTICLE))
    (format t " Created PARTICLE-FIELD-WIDGET: ~A.~%" id)
    widget))
           
(defun is-particle-dead (field particle)
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
                 t))))

(defun particle-field-add (field particle)
  (push particle (particle-field-widget-particles field)))

(defun remove-all-particles (widget)
  (setf (particle-field-widget-particles widget) nil))

(defun strip-dead-particles (widget)
  (let ((particles))
    (dolist (particle (particle-field-widget-particles widget))
      (unless (is-particle-dead widget particle)
        (push particle particles)))
    (setf (particle-field-widget-particles widget) (reverse particles))))

(defmethod widget-event-handle ((widget particle-field-widget) event payload)
  (format t "Event ~A with payload:~%~A~%" event payload)
  (when (eq event :ADD-PARTICLE)
    (when (eq (type-of payload) 'SPRITE-DESCRIPTOR)
      (particle-field-add widget payload))))

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
                  :pixmaps (get-pixmaps-array pixmaps)
                  :opaque t
                  :opaque-draw-exception t)))
    (format t " Created STAR-RAIN-WIDGET: ~A.~%" id)
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

(defstruct (star-particle (:include sprite-descriptor) (:constructor %new-star-particle))
  (|PARENT-STAR-PARTICLE| 'sprite-descriptor)
  (rotation (nth (random 2) '(-1 1))))

(defun make-star-particle (widget)
  (let ((particle (%new-star-particle)))
    (initialize-star-particle widget particle)
    particle))

(defun initialize-star-particle (widget particle)
  (let ((particle2 (initialize-sprite particle
                                     (aref (star-rain-widget-pixmaps widget) 
                                           (random (length (star-rain-widget-pixmaps widget))))
                                     :type :STAR
                                     :velocity #(0 70)
                                     :pos (list (star-rain-widget-startx widget) 
                                                (star-rain-widget-starty widget))
                                     ;; (LAMBDA (SPRITE X TICKSTART TICKNOW TICKDIFF))
                                     :transform-x (lambda (sprite x tickstart ticknow tickdiff)
                                                    (+ (sprite-x sprite)
                                                       (floor (* (* 50 (star-particle-rotation sprite))
                                                                 (sin (/ ticknow 1000.0)))))))))
    particle2))

(defun add-star (widget)
  (particle-field-add (star-rain-widget-field widget) (make-star-particle widget)))
