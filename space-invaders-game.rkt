;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname |space-invaders-starter (1)|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/universe)
(require 2htdp/image)

;; Space Invaders


;; Constants:

(define WIDTH  300)
(define HEIGHT 500)

(define INVADER-X-SPEED 1.5)  ;speeds (not velocities) in pixels per tick
(define INVADER-Y-SPEED 1.5)
(define TANK-SPEED 3)
(define MISSILE-SPEED 10)

(define HIT-RANGE 10)

(define INVADE-RATE 200)

(define BACKGROUND (empty-scene WIDTH HEIGHT))

(define INVADER
  (overlay/xy (ellipse 10 15 "outline" "blue")              ;cockpit cover
              -5 6
              (ellipse 20 10 "solid"   "blue")))            ;saucer

(define TANK
  (overlay/xy (overlay (ellipse 28 8 "solid" "black")       ;tread center
                       (ellipse 30 10 "solid" "green"))     ;tread outline
              5 -14
              (above (rectangle 5 10 "solid" "black")       ;gun
                     (rectangle 20 10 "solid" "black"))))   ;main body

(define TANK-HEIGHT/2 (/ (image-height TANK) 2))

(define MISSILE (ellipse 5 15 "solid" "red"))



;; Data Definitions:

(define-struct game (invaders missiles tank))
;; Game is (make-game  (listof Invader) (listof Missile) Tank)
;; interp. the current state of a space invaders game
;;         with the current invaders, missiles and tank position

;; Game constants defined below Missile data definition

#;
(define (fn-for-game s)
  (... (fn-for-loinvader (game-invaders s))
       (fn-for-lom (game-missiles s))
       (fn-for-tank (game-tank s))))



(define-struct tank (x dir))
;; Tank is (make-tank Number Integer[-1, 1])
;; interp. the tank location is x, HEIGHT - TANK-HEIGHT/2 in screen coordinates
;;         the tank moves TANK-SPEED pixels per clock tick left if dir -1, right if dir 1

(define T0 (make-tank (/ WIDTH 2) 1))   ;center going right
(define T1 (make-tank 50 1))            ;going right
(define T2 (make-tank 50 -1))           ;going left

#;
(define (fn-for-tank t)
  (... (tank-x t) (tank-dir t)))



(define-struct invader (x y dx))
;; Invader is (make-invader Number Number Number)
;; interp. the invader is at (x, y) in screen coordinates
;;         the invader along x by dx pixels per clock tick

(define I1 (make-invader 150 100 12))           ;not landed, moving right
(define I2 (make-invader 150 HEIGHT -10))       ;exactly landed, moving left
(define I3 (make-invader 150 (+ HEIGHT 10) 10)) ;> landed, moving right


#;
(define (fn-for-invader invader)
  (... (invader-x invader) (invader-y invader) (invader-dx invader)))

#;
(define (fn-for-invaders loi)
  (cond [(empty? loi) (...)]
        [else
         (... (fn-for-invader  (first loi))
              (fn-for-invaders (rest  loi)))]))

(define-struct missile (x y))
;; Missile is (make-missile Number Number)
;; interp. the missile's location is x y in screen coordinates

(define M1 (make-missile 150 300))                       ;not hit U1
(define M2 (make-missile (invader-x I1) (+ (invader-y I1) 10)))  ;exactly hit U1
(define M3 (make-missile (invader-x I1) (+ (invader-y I1)  5)))  ;> hit U1

#;
(define (fn-for-missile m)
  (... (missile-x m) (missile-y m)))

#;
(define (fn-for-missiles lom)
  (cond [(empty? lom) (...)]
        [else
         (... (fn-for-missile  (first lom))
              (fn-for-missiles (rest  lom)))]))


(define G0 (make-game empty empty T0))
(define G1 (make-game empty empty T1))
(define G2 (make-game (list I1) (list M1) T1))
(define G3 (make-game (list I1 I2) (list M1 M2) T1))



;; =================
;; Functions:

;; game -> game
;; start the world with (main G0)          ; (main (make-game empty empty T0))
;; 
(define (main g)
  (big-bang g               ; game
    (on-tick   next-game)   ; game -> game
    (to-draw   render-game) ; game -> Image
    (on-key    handle-key)  ; game KeyEvent -> game
    (stop-when end-game?))) ; game -> Boolean




;; game -> game
;; produce the next game state:
;;    add new invader to ListOf invaders
;;        update the position and direction of every invader
;;        remove invaders that are hit with missile
;;    update the position of every missile by MISSILE-SPEED
;;        remove missiles that hit an Invader
;;        remove missiles that off the screen
;;    update the position of the tank along x-axis
#;
(check-expect (next-game (make-game
                          (list (make-invader 100 100 1))
                          (list (make-missile 100 110))
                          (make-tank 50 1)))
              (make-game empty empty (make-tank (+ 50 TANK-SPEED) 1)))
              

;(define (next-game g) g)

(define (next-game g)
  (make-game (add-invader (update-invaders (filter-invaders (game-invaders g) (game-missiles g))))
             (update-missiles (filter-missiles (game-missiles g) (game-invaders g)))
             (update-tank (game-tank g))))


;; ListOfInvaders -> ListOfInvaders
;; add new invader at a random x position top of the screen
(check-random (add-invader empty) (cond [(> INVADE-RATE (random 5000))
                                         (cons (make-invader (random WIDTH) 10 1)
                                               empty)]
                                        [else  empty]))
(check-random (add-invader (cons I1 empty)) (cond [(> INVADE-RATE (random 5000))
                                                   (cons (make-invader (random WIDTH) 10 1)
                                                         (cons I1 empty))]
                                                  [else  (cons I1 empty)]))

;(define (add-invader loi) ...)

(define (add-invader loi)
  (cond [(> INVADE-RATE (random 5000))
         (cons (make-invader (random WIDTH) 10 1) loi)]
        [else loi]))


;; ListOfInvaders -> ListOfInvaders
;; move the invaders along x and y axes by INVADER-X-SPEED * dx and INVADER-Y-SPEED * dx
;;      change dx to +ve if x = 0
;;      change dx to -ve if x = WIDTH
(check-expect (update-invaders empty) empty)
(check-expect (update-invaders (list (make-invader 100 0 12)))
              (list (make-invader (+ 100 (* 12 INVADER-X-SPEED)) (+ 0 (* (abs 12) INVADER-Y-SPEED)) 12)))
(check-expect (update-invaders (list (make-invader 100 0 1) (make-invader 100 100 -12)))
              (list
               (make-invader (+ 100 (* 1 INVADER-X-SPEED)) (+ 0 (* (abs 1) INVADER-Y-SPEED)) 1)
               (make-invader (+ 100 (* -12 INVADER-X-SPEED)) (+ 100 (* (abs -12) INVADER-Y-SPEED)) -12)))

;(define (update-invaders loi) loi)

(define (update-invaders loi)
  (cond [(empty? loi) empty]
        [else
         (cons (update-invader  (first loi))
               (update-invaders (rest  loi)))]))

;; invader -> invader
;; update invader s and y speed by INVADER-X-SPEED * dx and INVADER-Y-SPEED * dx, if hit a wall inverse dx
(check-expect (update-invader (make-invader 100 0 -12))
              (make-invader (+ 100 (* -12 INVADER-X-SPEED)) (+ 0 (* (abs -12) INVADER-Y-SPEED)) -12))
(check-expect (update-invader (make-invader 100 0 12))
              (make-invader (+ 100 (* 12 INVADER-X-SPEED)) (+ 0 (* (abs 12) INVADER-Y-SPEED)) 12))

(check-expect (update-invader (make-invader (- 0 10) 0 -12))
              (make-invader 0 (+ 0 (* (abs -12) INVADER-Y-SPEED)) 12))
(check-expect (update-invader (make-invader 0 0 12))
              (make-invader (+ 0 (* 12 INVADER-X-SPEED)) (+ 0 (* (abs 12) INVADER-Y-SPEED)) 12))

(check-expect (update-invader (make-invader (+ 10 WIDTH) 0 12))
              (make-invader WIDTH (+ 0 (* (abs 12) INVADER-Y-SPEED)) -12))
(check-expect (update-invader (make-invader WIDTH 0 -12))
              (make-invader (+ WIDTH (* -12 INVADER-X-SPEED)) (+ 0 (* (abs -12) INVADER-Y-SPEED)) -12))

;(define (update-invader invader) invader)

(define (update-invader invader)
  (cond [(and (> 0 (invader-x invader)) (< (invader-dx invader) 0))
         (make-invader 0
                       (+ (invader-y invader) (* (abs (invader-dx invader)) INVADER-Y-SPEED))
                       (* -1 (invader-dx invader)))]
        
        [(and (< WIDTH (invader-x invader)) (> (invader-dx invader) 0))
         (make-invader WIDTH
                       (+ (invader-y invader) (* (abs (invader-dx invader)) INVADER-Y-SPEED))
                       (* -1 (invader-dx invader)))]
        
        [else
         (make-invader (+ (invader-x invader) (* (invader-dx invader) INVADER-Y-SPEED))
                       (+ (invader-y invader) (* (abs (invader-dx invader)) INVADER-Y-SPEED))
                       (invader-dx invader))]))

;; ListOfInvaders ListOfMissiles -> ListOfInvaders
;; remove invader hit by missile (both are the same x and (missile-y lom) - (invader-y loi) <= HIT-RANGE)
(check-expect (filter-invaders (list (make-invader 100 100 1))
                               (list (make-missile 100 20)))
              (list (make-invader 100 100 1)))

(check-expect (filter-invaders (list (make-invader 100 100 1))
                               (list (make-missile 60 60) (make-missile 90 110) (make-missile 100 110)))
              empty)

(check-expect (filter-invaders (list (make-invader 100 100 1) (make-invader 60 60 10) (make-invader 200 50 10))
                               (list (make-missile 200 55)))
              (list (make-invader 100 100 1) (make-invader 60 60 10)))

(check-expect (filter-invaders (list (make-invader 100 100 1) (make-invader 60 60 10) (make-invader 200 50 10))
                               (list (make-missile 60 60)))
              (list (make-invader 100 100 1) (make-invader 200 50 10)))

(check-expect (filter-invaders (list (make-invader 100 100 1) (make-invader 60 60 10) (make-invader 200 50 10))
                               (list (make-missile 50 50) (make-missile 90 110) (make-missile 100 110)))
              (list (make-invader 200 50 10)))

;(define (filter-invaders loi lom) loi)

(define (filter-invaders loi lom)
  (cond [(empty? loi) empty]
        [else
         (if (is-hit-by-missiles? (first loi) lom)
             (filter-invaders (rest loi) lom)
             (cons (first loi) (filter-invaders (rest loi) lom)))]))

;; invader ListOfMissiles -> Boolean
;; produce true if the invader hit missile, false if not
(check-expect (is-hit-by-missiles? (make-invader 100 100 1)
                                   empty)
              false)
(check-expect (is-hit-by-missiles? (make-invader 100 100 1)
                                   (cons (make-missile 100 100) (cons (make-missile 50 50) empty)))
              true)
(check-expect (is-hit-by-missiles? (make-invader 50 50 1)
                                   (cons (make-missile 100 100) (cons (make-missile 50 60) empty)))
              true)
(check-expect (is-hit-by-missiles? (make-invader 50 50 1)
                                   (cons (make-missile 100 100) (cons (make-missile 50 55) empty)))
              true)
(check-expect (is-hit-by-missiles? (make-invader 50 50 1)
                                   (cons (make-missile 100 100) (cons (make-missile 50 70) empty)))
              false)
(check-expect (is-hit-by-missiles? (make-invader 60 60 1)
                                   (cons (make-missile 100 100) (cons (make-missile 50 50) empty)))
              true)

;(define (is-hit-by-missiles? invader lom) false)

(define (is-hit-by-missiles? invader lom)
  (cond [(empty? lom) false]
        [else
         (if (hit? invader (first lom))
             true
             (is-hit-by-missiles? invader (rest lom)))]))


;; invader missile -> Boolean
;; produce true  if the invader and missile has same x and y2 - y1 <= HIT-RANGE
;;         false if else.
;; !!!

;(define (is-hit? invadere missile) false)

(define (hit? invader missile)
  (and (<= (abs (- (invader-x invader)  (missile-x missile))) HIT-RANGE)
       (<= (abs (- (missile-y missile) (invader-y invader))) HIT-RANGE)))

;; ===============================================

;; ListOfMissiles -> ListOfMissiles
;; move missiles by MISSILES-SPEED in y-axis
(check-expect (update-missiles empty) empty)

(check-expect (update-missiles (cons (make-missile 100 100)
                                     (cons (make-missile 50 60) empty)))
              (cons (make-missile 100 (- 100 MISSILE-SPEED))
                    (cons (make-missile 50 (- 60 MISSILE-SPEED)) empty)))

;(define (update-missiles lom) lom)

(define (update-missiles lom)
  (cond [(empty? lom) empty]
        [else
         (cons (update-missile (first lom))
               (update-missiles (rest  lom)))]))

;; missile -> missile
;; move missiles by MISSILES-SPEED in y-axis
(check-expect (update-missile (make-missile 100 100)) (make-missile 100 (- 100 MISSILE-SPEED)))

(define (update-missile m)
  (make-missile (missile-x m) (- (missile-y m) MISSILE-SPEED)))


;; ListOfMissiles ListOfInvaders -> ListOfMissiles
;; remove missile hit invader
;;        missile off the screen (y <= 0)
(check-expect (filter-missiles empty empty) empty)
(check-expect (filter-missiles (cons (make-missile 100 100) empty)
                               (cons (make-invader 100 100 1) empty))
              empty)
(check-expect (filter-missiles (list (make-missile 100 100) (make-missile 50 50) (make-missile 200 100))
                               (list (make-invader 100 100 1)))
              (list (make-missile 50 50) (make-missile 200 100)))
(check-expect (filter-missiles (list (make-missile 100 100) (make-missile 50 50) (make-missile 200 100))
                               (list (make-invader 100 100 1)(make-invader 60 100 1)(make-invader 200 100 1)))
              (list (make-missile 50 50)))
(check-expect (filter-missiles (list (make-missile 100 100) (make-missile 50 50) (make-missile 200 100))
                               (list (make-invader 50 45 1)))
              (list (make-missile 100 100) (make-missile 200 100)))
(check-expect (filter-missiles (list (make-missile 100 100) (make-missile 50 0) (make-missile 200 100))
                               (list (make-invader 100 100 1)))
              (list (make-missile 200 100)))
(check-expect (filter-missiles (list (make-missile 100 100) (make-missile 50 0) (make-missile 200 -10))
                               (list (make-invader 100 100 1)))
              empty)

;(define (filter-missiles lom loi) lom)

(define (filter-missiles lom loi)
  (cond [(empty? lom) empty]
        [else
         (if (or (is-hit-by-invaders? (first lom) loi)
                 (is-off-screen? (first lom)))
             (filter-missiles (rest lom) loi)
             (cons (first lom) (filter-missiles (rest lom) loi)))]))

;; missile ListOfInvaders -> Boolean
;; produce true  if missile hit any of invaders
;;         false if else.
(check-expect (is-hit-by-invaders? (make-missile 100 100)
                                   empty)
              false)
(check-expect (is-hit-by-invaders? (make-missile 100 100)
                                   (list (make-invader 50 50 1)
                                         (make-invader 100 100 1)
                                         (make-invader 60 50 1)))
              true)
(check-expect (is-hit-by-invaders? (make-missile 70 100)
                                   (list (make-invader 50 50 1)
                                         (make-invader 100 100 1)
                                         (make-invader 60 50 1)))
              false)

;(define (is-hit-by-invaders? missile loi) false)

(define (is-hit-by-invaders? missile loi)
  (cond [(empty? loi) false]
        [else
         (if (hit? (first loi) missile)
             true
             (is-hit-by-invaders? missile (rest loi)))]))


;; missile -> Boolean
;; produce true if the missile is off the screen (y <= 0)
(check-expect (is-off-screen? (make-missile 100  100)) false)
(check-expect (is-off-screen? (make-missile 100  0)) true)
(check-expect (is-off-screen? (make-missile 100 -10)) true)

;(define (is-off-screen? m) false)

(define (is-off-screen? m)
  (<= (missile-y m) 0))

;; =====================================================

;; Tank -> Tank
;; change speed by TANK-SPEED
;;    to right if dir = 1
;;    to left  if dir = -1
;;    stop if x = 0 or x = width (put dir = 0)
(check-expect (update-tank (make-tank 100  1)) (make-tank (+ 100 (*  1 TANK-SPEED))  1)) ; to right
(check-expect (update-tank (make-tank 100 -1)) (make-tank (+ 100 (* -1 TANK-SPEED)) -1)) ; to left

(check-expect (update-tank (make-tank -10 -1)) (make-tank (/ (image-width TANK) 2) -1))
(check-expect (update-tank (make-tank -10  1)) (make-tank (/ (image-width TANK) 2)  1))

(check-expect (update-tank (make-tank (+ 10 WIDTH) -1)) (make-tank (- WIDTH (/ (image-width TANK) 2)) -1))
(check-expect (update-tank (make-tank (+ 10 WIDTH)  1)) (make-tank (- WIDTH (/ (image-width TANK) 2))  1))

;(define (update-tank t) t)

(define (update-tank t)
  (cond [(> (/ (image-width TANK) 2) (+ (tank-x t) (* TANK-SPEED (tank-dir t))))
         (make-tank (/ (image-width TANK) 2) (tank-dir t))]
        [(< (- WIDTH (/ (image-width TANK) 2)) (+ (tank-x t) (* TANK-SPEED (tank-dir t))))
         (make-tank (- WIDTH (/ (image-width TANK) 2)) (tank-dir t))]
        [else
         (make-tank (+ (tank-x t) (*  (tank-dir t) TANK-SPEED))  (tank-dir t))]))

;;=========================================================

;; game -> Image
;; render game images
(check-expect (render-game (make-game (list (make-invader 150 100 12))
                                      (list (make-missile 150 300))
                                      (make-tank 50 1)))
              (place-image TANK 50 (- HEIGHT TANK-HEIGHT/2)
                           (place-image INVADER 150 100
                                        (place-image MISSILE 150 300 BACKGROUND))))

(check-expect (render-game (make-game (list (make-invader 150 100 12)
                                            (make-invader 150 HEIGHT -10))
                                      (list (make-missile 150 300)
                                            (make-missile (invader-x I1)
                                                          (+ (invader-y I1) 10)))
                                      (make-tank 50 1)))
              (place-image TANK 50 (- HEIGHT TANK-HEIGHT/2)
                           (place-image INVADER 150 100
                                        (place-image INVADER 150 HEIGHT
                                                     (place-image MISSILE 150 300
                                                                  (place-image MISSILE (invader-x I1)
                                                                               (+ (invader-y I1) 10) BACKGROUND))))))

;;(define (render-game g) ...)


(define (render-game g)
  (render-tank (game-tank g) (render-invader (game-invaders g) (render-missile (game-missiles g)))))
  
;; tank image -> image
;; place the tank image at the approporiate place

(define (render-tank t img)
  (place-image TANK
               (tank-x t)
               (- HEIGHT TANK-HEIGHT/2)
               img))

;; ListOfInvader image -> image
;; place the invader image at the approporiate place

(define (render-invader loi img)
  (cond [(empty? loi) img]
        [else
         (place-image INVADER
                      (invader-x (first loi))
                      (invader-y (first loi))
                      (render-invader (rest loi) img))]))


;; ListofMissile -> image
;; place the missile image at the approporiate place

(define (render-missile lom)
  (cond [(empty? lom) BACKGROUND]
        [else
         (place-image MISSILE
                      (missile-x (first lom))
                      (missile-y (first lom))
                      (render-missile (rest lom)))]))

;; ======================================================

;; game KeyEvent -> game
;; moves the tank by (TANK-SPEED) pixels per clock tick: left if dir -1, right if dir 1
;;   fires missiles straight up from tank's current position when the space bar is pressed.
(check-expect (handle-key (make-game empty empty T0) "left")
              (make-game empty empty (make-tank (tank-x T0) -1))) ; move left

(check-expect (handle-key (make-game (list I1) (list M1) T1) "right")
              (make-game (list I1) (list M1) (make-tank (tank-x T1) 1))) ; move right

(check-expect (handle-key (make-game empty empty T0) " ")
              (make-game empty (list (make-missile (tank-x T0) (- HEIGHT TANK-HEIGHT/2))) T0)) ; fire missile

(check-expect (handle-key (make-game empty empty T0) "up")
              (make-game empty empty T0)) ; no change

(define (handle-key g ke)
  (cond [(key=? ke "left")  (move-left g)]
        [(key=? ke "right") (move-right g)]
        [(key=? ke " ") (fire-missile g)]
        [else           g]))


;; game -> game
;; make the tank move left with TANK-SPEED

(define (move-left g)
  (make-game (game-invaders g)
             (game-missiles g)
             (make-tank
              (tank-x (game-tank g)) -1)))


;; game -> game
;; make the tank move right with TANK-SPEED

(define (move-right g)
  (make-game (game-invaders g)
             (game-missiles g)
             (make-tank
              (tank-x (game-tank g)) 1)))


;; game -> game
;; add a missile to the ListOf missiles in the game

(define (fire-missile g)
  (make-game (game-invaders g)
             (cons (make-missile
                    (tank-x (game-tank g))
                    (- HEIGHT TANK-HEIGHT/2))
                   (game-missiles g))
             (game-tank g)))
;; ========================================

;; game -> Boolean
;; check if the invader landed
(check-expect (end-game? G0) false)
(check-expect (end-game? (make-game (list (make-invader 100 HEIGHT 1))
                                empty
                                T0))
              true)
(check-expect (end-game? (make-game (list (make-invader 100 100 1)
                                      (make-invader 100 HEIGHT 1))
                                empty
                                T0))
              true)

;(define (end-game? g) false)

(define (end-game? g)
  (check-invaders (game-invaders g)))

;; ListOfInvader -> Boolean
;; produce true  if invader lands (y = HEIGHT)
;;         false if else.
;; !!!

(define (check-invaders loi)
  (cond [(empty? loi) false]
        [else
         (if (landed?  (first loi))
             true
             (check-invaders (rest  loi)))]))


;; invader -> Boolean
;; produce true  if invader lands (y = HEIGHT)
;;         false if else.
;; !!!

(define (landed? invader)
  (>= (invader-y invader) HEIGHT))