;; f3.lisp
;; Copyright Parasite Network 2018
;; GPL3

(defparameter *HIGHSCORE-PATH* (merge-pathnames "poäng.txt"))  

(defun define-resources (renderer)
  (format t "Loading resources.~%")
  
  (defresource "graphics/background.png" renderer :background)
  (defresource "graphics/title.png" renderer :title)
  (defresource "graphics/play.png" renderer :play)
  (defresource "graphics/highscore.png" renderer :highscore)
  (defresource "graphics/exit.png" renderer :exit)
  (defresource "graphics/grid.png" renderer :gamegrid)
  (defresource "graphics/gameover-overlay.png" renderer :gameover-overlay)
  (defresource "graphics/dimmer-overlay.png" renderer :dimmer-overlay)
  (defresource "graphics/paused-overlay.png" renderer :paused-overlay)
  (defresource "graphics/highscore-overlay.png" renderer :highscore-overlay)
  (defresource "graphics/chevron.png" renderer :chevron)
  (defresource "graphics/sky-overlay.png" renderer :sky)
  (defresource "graphics/help.png" renderer :help)
  (defresource "graphics/help-overlay.png" renderer :helptext)
  
  (defresource "graphics/star1.png" renderer
               '(50 50
                 (:ystar1 0 0)
                 (:ystar2 1 0)
                 (:ystar3 2 0)
                 (:ystar4 3 0)))
  (defanimation (list 50 50 50 50) (list :ystar1 :ystar2 :ystar3 :ystar4) :yellow-star)
  
  (defresource "graphics/star2.png" renderer
               '(50 50
                 (:rstar1 0 0)
                 (:rstar2 1 0)
                 (:rstar3 2 0)
                 (:rstar4 3 0)))
  (defanimation (list 50 50 50 50) (list :rstar1 :rstar2 :rstar3 :rstar4) :red-star)
  
  (defcharmap "graphics/black-large-alfabet.png" renderer :black-large-alfabet 27 40 10 42)
  (defcharmap "graphics/black-small-alfabet.png" renderer :black-small-alfabet 22 30 10 42)
  
  (defresource "graphics/pokaler.png" renderer
               '(53 80
                 (:1st-pokal 0 0)
                 (:2nd-pokal 1 0)
                 (:3rd-pokal 2 0)
                 (:4th-pokal 3 0)
                 (:5th-pokal 4 0)))
  
  (defresource "graphics/emblem.png" renderer
               '(30 30
                 (:1st-emblem 0 0)
                 (:2nd-emblem 1 0)
                 (:3rd-emblem 2 0)
                 (:4th-emblem 3 0)
                 (:5th-emblem 4 0)))
  
  (defresource "graphics/småsiffror.png" renderer
               '(13 20
                 (:s0 0 0)
                 (:s1 1 0)
                 (:s2 2 0)
                 (:s3 3 0)
                 (:s4 4 0)
                 (:s5 5 0)
                 (:s6 6 0)
                 (:s7 7 0)
                 (:s8 8 0)
                 (:s9 9 0)))
  
  (defresource "graphics/siffror.png" renderer
               '(25 40
                 (:g0 0 0)
                 (:g1 1 0)
                 (:g2 2 0)
                 (:g3 3 0)
                 (:g4 4 0)
                 (:g5 0 1)
                 (:g6 1 1)
                 (:g7 2 1)
                 (:g8 3 1)
                 (:g9 4 1)
                 (:r0 0 2)
                 (:r1 1 2)
                 (:r2 2 2)
                 (:r3 3 2)
                 (:r4 4 2)
                 (:r5 0 3)
                 (:r6 1 3)
                 (:r7 2 3)
                 (:r8 3 3)
                 (:r9 4 3)))
  (defresource "graphics/frukter.png" renderer
               '(16 16
                 (:apple 0 0)
                 (:banana 1 0)
                 (:pineapple 2 0)
                 (:pear 3 0)
                 (:orange 4 0)
                 (:melon 5 0)
                 (:lemon 6 0)
                 (:grapes 7 0)
                 (:cherry 8 0)
                 (:blueberry 9 0)
                 (:black-apple 0 1)
                 (:black-banana 1 1)
                 (:black-pineapple 2 1)
                 (:black-pear 3 1)
                 (:black-orange 4 1)
                 (:black-melon 5 1)
                 (:black-lemon 6 1)
                 (:black-grapes 7 1)
                 (:black-cherry 8 1)
                 (:black-blueberry 9 1))))

(defun define-windows (renderer)
  (format t "Defining windows.~%")    
  (let ((red-large-numbers (list :r0 :r1 :r2 :r3 :r4 :r5 :r6 :r7 :r8 :r9))
        (green-large-numbers (list :g0 :g1 :g2 :g3 :g4 :g5 :g6 :g7 :g8 :g9))
        (green-small-numbers (list :s0 :s1 :s2 :s3 :s4 :s5 :s6 :s7 :s8 :s9)))
    
    (defwindow nil
               :visible t
               :active nil
               :widget (make-image :background))
    
    (defwindow :title
               :visible t
               :active nil
               :group :title-screen
               :widget (make-box 
                         :padding '(5 0 0 0)
                         :widget (make-bag
                                   :align :center
                                   :fixed-width *WINDOW-WIDTH*
                                   :widgets (list 
                                              (make-image :title)))))
    
    (defwindow :gamegrid
               :visible nil
               :active t
               :place :center
               :widget (make-bag
                         :align :cross
                         :widgets (list
                                    (make-bag
                                      :align :center
                                      :widgets (list
                                                 (make-highscoreboard
                                                   :id :thescoreboard
                                                   :digits 5
                                                   :green green-large-numbers
                                                   :red red-large-numbers)
                                                 (make-game
                                                   :id :thegame
                                                   :background :gamegrid
                                                   :scoreboard-id :thescoreboard
                                                   :particle-id :poppy)))
                                    (make-particle-field :id :poppy)))
               :trap-open (lambda (window)
                            (reset-scoreboard (find-widget :thescoreboard))
                            (reset-game (find-widget :thegame))))
    
    (defwindow :gameover-failure 
               :visible nil
               :active t
               :place :center
               :widget (make-bag
                         :align :stack
                         :widgets (list
                                    (make-image :dimmer-overlay)
                                    (make-image :gameover-overlay)))
               :trap-input (lambda (window key)
                             (close-window-by-id :gameover-failure)
                             (close-window-by-id :gamegrid)
                             (open-window-group :title-screen)
                             t))
    
    (defwindow :gameover-success 
               :visible nil
               :active t
               :place :center
               :widget (make-bag
                         :align :stack
                         :widgets (list
                                    (make-image :dimmer-overlay)
                                    (make-image :highscore-overlay)))
               :trap-input (lambda (window key)
                             (close-window-by-id :gameover-success)
                             (close-window-by-id :gamegrid)
                             (open-window-by-id :title)
                             (open-window-by-id :enter-highscore)
                             t))
    
    (defwindow :paused
               :visible nil
               :active nil
               :widget (make-bag
                         :align :stack
                         :widgets (list
                                    (make-image :dimmer-overlay)
                                    (make-image :paused-overlay))))
    
    (defwindow :highscore 
               :visible nil
               :active t
               :place :center
               :widget (make-box
                         :top 60
                         :widget  
                         (make-bag
                           :align :center
                           :spacing 10
                           :widgets (list
                                      (make-scoreentry :black-small-alfabet green-small-numbers 
                                                       :emblem :1st-emblem
                                                       :id :1st)
                                      (make-scoreentry :black-small-alfabet green-small-numbers 
                                                       :emblem :2nd-emblem 
                                                       :id :2nd)
                                      (make-scoreentry :black-small-alfabet green-small-numbers
                                                       :emblem :3rd-emblem
                                                       :id :3rd)
                                      (make-scoreentry :black-small-alfabet green-small-numbers
                                                       :emblem :4th-emblem 
                                                       :id :4th)
                                      (make-scoreentry :black-small-alfabet green-small-numbers
                                                       :emblem :5th-emblem 
                                                       :id :5th))))
               :trap-open (lambda (window)
                            (let ((widget (window-widget window)))
                              (load-higschore-entry 0 (find-widget :1st))
                              (load-higschore-entry 1 (find-widget :2nd))
                              (load-higschore-entry 2 (find-widget :3rd))
                              (load-higschore-entry 3 (find-widget :4th))
                              (load-higschore-entry 4 (find-widget :5th))
                              nil))
               :trap-input (lambda (window key)
                             (cond
                               ((key= key :scancode-c)
                                (highscore:reset)
                                (highscore:save-to *HIGHSCORE-PATH*)
                                (open-window-by-id :highscore)
                                t)
                               ((key= key :scancode-escape)
                                 (close-window-by-id :highscore)
                                 (open-window-by-id :start-menu)
                                 t))))
    
    (defwindow :enter-highscore
               :visible nil
               :active t
               :place :center
               :widget (make-bag
                         :align :cross
                         :widgets (list
                                    (make-star-rain 
                                      (list :yellow-star :red-star)
                                      :startx (- (/ *WINDOW-WIDTH* 2) 25) 
                                      :starty -50)
                                    (make-image :sky)
                                    (make-bag
                                      :align :center
                                      :spacing 20
                                      :widgets (list
                                                 (make-flipbook
                                                   :id :emblem-book
                                                   :images (list
                                                             :1st-pokal
                                                             :2nd-pokal
                                                             :3rd-pokal
                                                             :4th-pokal
                                                             :5th-pokal))
                                                 (make-scoreentry :black-large-alfabet 
                                                                  green-large-numbers
                                                                  :id :winnerbox
                                                                  :editable t
                                                                  :chevron :chevron)))))
               :trap-input (lambda (window key)
                             (when (key= key :scancode-return)
                               (let ((entry (find-widget :winnerbox)))
                                 (highscore:add
                                   (get-entry-score entry)
                                   (get-entry-name entry))
                                 (highscore:save-to *HIGHSCORE-PATH*)
                                 (close-window-by-id :enter-highscore)
                                 (open-window-by-id :highscore)))))
    
    (defwindow :help
               :active t
               :visible nil
               :widget (make-image :helptext)
               :trap-input (lambda (window key)
                             (close-window-by-id :help)
                             (open-window-by-id :start-menu)))
    
    (defwindow :start-menu
               :active t
               :visible t
               :disabled nil
               :group :title-screen
               :place :center
               :widget (make-box 
                         :padding '(0 60 0 0)
                         :widget (make-menu
                                   :pointer (make-image :apple)
                                   :entries (list
                                              (make-menu-entry
                                                :widget (the-target :play
                                                                    (make-image :play))
                                                :action (lambda (menu selection)
                                                          (close-window-by-id :start-menu)
                                                          (close-window-by-id :title)
                                                          (open-window-by-id :gamegrid)))
                                              (make-menu-entry
                                                :widget (the-target :scores
                                                                    (make-image :highscore))
                                                :action (lambda (menu selection)
                                                          (close-window-by-id :start-menu)
                                                          (open-window-by-id :highscore)))
                                              (make-menu-entry
                                                :widget (the-target :help
                                                                    (make-image :help))
                                                :action (lambda (menu selection)
                                                          (close-window-by-id :start-menu)
                                                          (open-window-by-id :help)))
                                              (make-menu-entry
                                                :widget (the-target :exit
                                                                    (make-image :exit))
                                                :action (lambda (menu selection)
                                                          (sdl2:push-event :quit)))))))
    
    (defwindow :starfall
               :visible t
               :active t
               :disabled t
               :widget (make-bag
                         :align :stack
                         :widgets (list
                                    (make-star-rain
                                      (list :yellow-star :red-star)
                                      :startx (- (/ *WINDOW-WIDTH* 2) 25) 
                                      :starty -50)
                                    (make-image :sky))))
    
    (defwindow :global-hotkeys
               :visible t
               :active t
               :trap-input (lambda (window key)
                             (cond
                               ((key= key :scancode-d)
                                (setf *DEBUG-WIDGET-BORDER* (not *DEBUG-WIDGET-BORDER*))
                                t))))
    
    (initialize-window-widgets)))
  
(defun gameloop (renderer)        
  (format t "Entering game loop.~%")
  (sdl2:with-event-loop (:method :poll)
                        (:quit () t)
                        (:keydown (:keysym keysym)
                                  (format t "Key input: (~A) ~A.~%"
                                          (sdl2:scancode-value keysym)
                                          (sdl2:scancode keysym))                                  
                                  (unless (window-event-onkey-down keysym)
                                    (cond 
                                      ((sdl2:scancode= (sdl2:scancode-value keysym) :scancode-escape)
                                       (sdl2:push-event :quit)))))
                        (:keyup
                          )
                        (:idle ()
                               (let ((tick (get-internal-real-time)))
                                 (sdl2:set-render-draw-color renderer 255 0 60 0)
                                 (sdl2:render-clear renderer)
                                 (paint-windows renderer tick)
                                 
                                 (when *debug-widget-border* (paint-debug-borders renderer))
                                 
                                 (sdl2:render-present renderer)))))  
    
(defun start ()
  (format t "Starting F3.~%")
  (unless (probe-file "f3.asd")
    (format t "You need to be in the Fruktorama 3 directory for all assets to load properly.~%")
    (return-from start nil))
  (highscore:set-defaults '((1966 . (20 26 8))
                            (1752 . (24 28 24))
                            (1304 . (22 31 25))
                            (714 . (0 15 0))
                            (57 . (10 21 10))))
  (highscore:load-from *HIGHSCORE-PATH*)
  (format t "Highscore loaded.~%")
  (initialize-sprites)
  (format t "Sprites initialized.~%")
  (initialize-widgets)
  (format t "Widgets initialized.~%")
  (initialize-windows 190 419)
  (format t "Windows initialized.~%")
  (unwind-protect 
    (sdl2:with-init (:everything)
                    (format t "SDL2 initialized.~%")
                    (sdl2-image:init '(:png))
                    (format t "SDL2-Image initialized.~%")
                    (setf *window-width* 190)
                    (setf *window-height* 419)
                    (sdl2:with-window (win :w *window-width* :h *window-height* :title "F3" :flags '(:shown))
                                      (format t "SDL2 Window created.~%")
                                      (sdl2:with-renderer (renderer win)
                                                          (handler-case
                                                            (progn
                                                              (define-resources renderer)
                                                              (define-windows renderer)
                                                              (gameloop renderer))
                                                            (error (e)
                                                                   (format t "Caught error: ~A~%" e)
                                                                   (format t "ABORT!~%"))))))
    (destroy-textures)
    (format t "Tjingeling.~%")))

(defun x ()
  (ql:quickload "f3"))

;;------------------------------------------------------------------------------
    
(defun load-higschore-entry (entry widget)
  (set-entry-score widget 0)
  (set-entry-name widget nil)
  (when (< entry (highscore:number-of-entries))
    (let ((entry (highscore:get-entry entry)))
      (when entry
        (let ((points (car entry))
              (name (cdr entry)))
          (set-entry-score widget points)
          (set-entry-name widget name))))))