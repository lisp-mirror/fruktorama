# Fruktorama 3 Lisp Edition

## Quick overview

Fruktorama is a Tetrislike game where the player
tries to match at least three of a kind either diagonally
or orthogonally.

![Start screen](https://bitbucket.org/ParasiteNetwork/fruktorama3-lisp-edition/raw/0dea7ab842fe0f8ae03bbced5bf1b7cf358249a9/screenshots/screenshot-1.png)

Fruits are dropped from the above and can be rotated to make matches easier.
Each time a block of fruits moves down the player gets one point.

![Fruits dropping](https://bitbucket.org/ParasiteNetwork/fruktorama3-lisp-edition/raw/0dea7ab842fe0f8ae03bbced5bf1b7cf358249a9/screenshots/screenshot-2.png)

Each diagonally or orthogonally row of at least three fruits of the same kind
will be removed. Points are scored as ten times the number of fruits removed.

![Reaping fruits](https://bitbucket.org/ParasiteNetwork/fruktorama3-lisp-edition/raw/0dea7ab842fe0f8ae03bbced5bf1b7cf358249a9/screenshots/screenshot-3.png)

If the player gains enough points they'll be admitted to the highscore list.

![Enter name](https://bitbucket.org/ParasiteNetwork/fruktorama3-lisp-edition/raw/0dea7ab842fe0f8ae03bbced5bf1b7cf358249a9/screenshots/screenshot-4.png)

The highscore is saved locally as "poäng.txt".

![Highscore](https://bitbucket.org/ParasiteNetwork/fruktorama3-lisp-edition/raw/a43507a6520b443c7701a4be28c6e18b0d21bdda/screenshots/screenshot-5.png)

## Some implementation notes

The game is structured around widgets and windows. These provide sufficient abstraction
above the SDL framework.

### Windows

A window is defined with `DEFWINDOW` which puts all the windows, in order of
definition, on the window stack. It's a bit static at the moment. They are then
drawn from the bottom up. Input is handled in the reverse order.

Each window has two important properties: `VISIBLE` and `ACTIVE`. A window
needs to be visible in order to be drawn and receive input. Hidden windows are
hidden from it all. Also, to receive input the window needs to be active. For
example, the background window is defined as:

````
(defwindow nil
           :visible t
           :active nil
           :widget (make-image :background))
````

It is visible, but not active. It contains one widget, the `IMAGE-WIDGET`
returned from `(MAKE-IAMGE)`. It also has no name, hence the `NIL`.

This simple window can be contrasted with the `:HIGHSCORE` window:

````
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
                            (reinitialize-highscore-data)
                            (save-highscore-data)
                            (open-window-by-id :highscore)
                            t)
                           ((key= key :scancode-escape)
                            (close-window-by-id :highscore)
                            (open-window-by-id :start-menu)
                            t))))
````

As said, it is named `:HIGHSCORE`. It also uses three types of widget to
construct the layout: `BOX-WIDGET`, `BAG-WIDGET` and `SCOREENTRY-WIDGET`.
The `SCOREENTRY-WIDGET` is a widget for displaying entries in the highscore
list. The other two are simple layout containers. The `BAG-WIDGET` sorts all
its children according to its `:ALIGN`, in this case they will be centered
vertically, from the top to the bottom, in the order of appearance in the list
provided to `:WIDGETS`. The `BOX-WIDGET` adds padding around its child.

`TRAP-OPEN` is a callback which is called when the window is opened. And
`TRAP-INPUT` is called when the window receives input. Both these operate on
the window level. Widgets themselves are also able to react to these event.

Any input to a window will be forwarded to its children, unless there is a
`TRAP-INPUT` in which case it gets first dibs. If the trap or any of its
children decides to consume the input it returns `T` and the input propagation
ends. If the final result is `NIL` the framework moves on to the next window.

### Widgets

All widgets derive from `WIDGET` which contain the default behavior. During
definition all widget use relative offsets to build up the layout. Once done,
absolute coordinates are calculated, and remain absolute throughout.

The most basic widget is the `IMAGE-WIDGET` which paints an image.



















