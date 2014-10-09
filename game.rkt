(require 2htdp/image)
(require 2htdp/universe)

;; -----------------------------------------------------------------------------------------

;;; CONSTANTS
(define BOARD-WIDTH 500)
(define BOARD-HEIGHT 500)
(define BACKGROUND (place-image (rectangle 20 30 "solid" "green") 250 490
                                (empty-scene BOARD-WIDTH BOARD-HEIGHT)))

(define BULLET-RADIUS 5)
(define BULLET-IMAGE (circle BULLET-RADIUS "solid" "green"))
(define BULLET-SPEED 5)

(define MISSILE-RADIUS 9)
(define MISSILE-IMAGE (circle MISSILE-RADIUS "solid" "red"))
(define MISSILE-SPEED 1)

;; -----------------------------------------------------------------------------------------

(define-struct sprite (loc vel size color))
;;; A Sprite is a (make-sprite Posn Posn Number String)
;;; interp. (make-sprite p1 p2 r c) is a sprite with p1
;;; as its location, p2 is its velocity, r is the size
;;; of the radius of the sprite and c is its color. Location,
;;; velocity and size are in computer-graphic/pixel coordinates.
;;; A sprite represents either an attacker's missile or a defender's
;;; anti-missile bullet.

;;; A LOS (list of sprites) is one of:
;;; - empty
;;; - (cons Sprite LOS)

(define-struct world (missiles bullets health))
;;; A World structure is a (make-world LOS LOS Number)
;;; - interp. (make-world m b h) is a world with m missiles
;;; (the missiles attacking the player), b bullets
;;; (bullets launched by the player) and h health (current
;;; health of the player -- game-over if health <= 0)

;; -----------------------------------------------------------------------------------------
;;; EXAMPLES

;; bullet positions
(define bulpos1 (make-posn 200 200))
(define bulpos2 (make-posn 200 200))
(define bulpos3 (make-posn 50 50))
(define bulpos4 (make-posn 70 70))
;; missile positions
(define mispos1 (make-posn 100 10))
(define mispos2 (make-posn 300 400))
(define misbulcontact (make-posn 62 86))
;; positions that will leave the screen
(define edgeposL (make-posn 5 300))
(define edgeposT (make-posn 5 495))
(define edgeposR (make-posn 495 300))
(define groundpos1 (make-posn 100 5))
;; positions outside the screen
(define out1 (make-posn -5 10))
(define out2 (make-posn 510 10))
(define out3 (make-posn 200 510))
(define out4 (make-posn 200 -10))
;; velocity examples
(define exvel1 (make-posn 3 4)) ;;5
(define exvel2 (make-posn -5 12)) ;;13
(define exvel3 (make-posn 3 -4))
(define exvel4 (make-posn -5 -12))
;; velocities that leave the screen
(define outvelL (make-posn -1000 10))
(define outvelT (make-posn 30 1000))
(define outvelR (make-posn 1000 10))
(define outvelB (make-posn 10 -1000))

;; Sprite examples
;; bullets
(define bullet1 (make-sprite bulpos1 exvel1 BULLET-RADIUS "green"))
(define bullet2 (make-sprite bulpos2 exvel2 BULLET-RADIUS "green"))
(define bullet3 (make-sprite bulpos3 exvel1 BULLET-RADIUS "green"))
(define bullet4 (make-sprite bulpos4 exvel2 BULLET-RADIUS "green"))
(define bulletL (make-sprite edgeposL outvelL BULLET-RADIUS "green"))
(define bulletT (make-sprite edgeposT outvelT BULLET-RADIUS "green"))
(define bulletR (make-sprite edgeposR outvelR BULLET-RADIUS "green"))
(define bullet1moved (make-sprite (make-posn 203 204) (make-posn 3 4) BULLET-RADIUS "green"))
(define bullet2moved (make-sprite (make-posn 195 212) (make-posn -5 12) BULLET-RADIUS "green"))
;; missiles
(define missile1 (make-sprite mispos1 exvel3 MISSILE-RADIUS "red"))
(define missile2 (make-sprite mispos1 exvel4 MISSILE-RADIUS "red"))
(define missile3 (make-sprite mispos2 exvel3 MISSILE-RADIUS "red"))
(define missile4 (make-sprite misbulcontact exvel3 MISSILE-RADIUS "red"))
(define missile5 (make-sprite edgeposL exvel3 MISSILE-RADIUS "red"))
(define missile6 (make-sprite edgeposR exvel4 MISSILE-RADIUS "red"))
(define missileB (make-sprite groundpos1 outvelB MISSILE-RADIUS "red"))
(define hit (make-sprite out4 outvelB MISSILE-RADIUS "red"))
(define missile8mk (make-sprite (make-posn 10 530) exvel3 MISSILE-RADIUS "red"))
(define missile10mk (make-sprite (make-posn 250 520) exvel3 MISSILE-RADIUS "red"))
(define missile13mk (make-sprite (make-posn 300 510) exvel3 MISSILE-RADIUS "red"))

;; LOS examples
;; bullet lists
(define los-b (cons bullet1 (cons bullet2 empty)))
(define bulletstorm1 (list bullet1 bullet2 bullet3 bullet4 bulletL bulletT bulletR))
(define bulletstorm2 (list bulletT bulletR bulletL bullet1 bullet2 bullet3 bullet4))
(define bulletstorm3 (list bulletT bullet1 bulletR bullet2 bulletL  bullet3 bullet4))
(define bulletstorm1moved (list
                           (make-sprite (make-posn 203 204) (make-posn 3 4) BULLET-RADIUS "green")
                           (make-sprite (make-posn 195 212) (make-posn -5 12) BULLET-RADIUS "green")
                           (make-sprite (make-posn 53 54) (make-posn 3 4) BULLET-RADIUS "green")
                           (make-sprite (make-posn 65 82) (make-posn -5 12) BULLET-RADIUS "green")))
;; missile lists
(define los-m (cons missile1 (cons missile2 empty)))
(define MIRV (list missile1 missile2 missile3 missile4 missile5 missile6 missileB))

;; World examples
(define world1 (make-world los-m los-b 3))
(define DEFCON1 (make-world MIRV bulletstorm1 100))
(define STARTWORLD (make-world empty empty 5))
(define nosurvivors (make-world (list missile8mk missile10mk missile13mk) empty 10))

;; -----------------------------------------------------------------------------------------

;;; TEMPLATES
;; sprite-temp: Sprite -> ?
#; (define (sprite-temp sp)
     ...(sprite-loc sp)...(sprite-vel sp)...
     ...(sprite-size sp)...(sprite-color sp)...)

;; los-temp: LOS -> ?
#;(define (los-temp los)
    (cond [(empty? los) ...]
          [(cons? los)...(sprite-temp (first los))...
                      ...(los-temp (rest los))...]))

;; world-temp: World -> ?
#;(define (world-temp w)
    ...(los-temp (world-missiles w))...
    ...(los-temp (world-bullets w))...(world-health w)...)

;; -----------------------------------------------------------------------------------------

;; HELPER FUNCTIONS


;; distance: Posn Posn -> Number
;; computes the distance between two positions
(define (distance a b)
  (sqrt (+ (sqr (- (posn-x a) (posn-x b)))
           (sqr (- (posn-y a) (posn-y b))))))

(check-expect (distance (make-posn 3 0) (make-posn 8 0)) 5)
(check-expect (distance (make-posn 3 0) (make-posn 6 4)) 5)
(check-expect (distance (make-posn 5 16) (make-posn 10 28)) 13)

;; move-LOS: LOS -> LOS
;; move a list of sprites according to velocity
(define (move-LOS b)
  (local [;; outside-frame?: Sprite -> Boolean
          ;; Is a posn in a given sprite outside of 500x500 frame?
          (define (outside-frame? p)
            (or (> (abs (+ (posn-x (sprite-loc p)) (sprite-size p))) BOARD-WIDTH )
                (> (+ (posn-y (sprite-loc p)) (sprite-size p)) BOARD-HEIGHT )
                (< (+ (posn-y (sprite-loc p)) (sprite-size p)) -10 )))
          ;; sprite-mover: Sprite -> Sprite
          ;; move a sprite one time step according to its velocity
          (define (sprite-mover s)
            (local
              ;; apply-delt: Posn Posn -> Posn
              ;; creates a new position according to the given velocity
              [(define(apply-delt p d)
                      (make-posn (+ (posn-x p) (posn-x d)) (+ (posn-y p) (posn-y d))))]
              (make-sprite (apply-delt (sprite-loc s) (sprite-vel s))
                           (sprite-vel s) (sprite-size s) (sprite-color s))))]
    (filter (lambda (s) (not (outside-frame? s))) (map sprite-mover b))))

(check-expect (move-LOS bulletstorm1) bulletstorm1moved)
(check-expect (move-LOS empty) empty)                                    ;;;;;;;;;;;;;;more tests

;; bullet-on-missile: LOS LOS -> LOS
;; create a list of sprites that have not collided
(define (bullet-on-missile emptying emptior)
  (local[;; consult-list: Sprite LOS -> Boolean
         ;; Is given missile located next to the bullet in a list of sprites?
         (define (consult-list value lst)
           (ormap (lambda (s)
                    (<= (distance (sprite-loc value) (sprite-loc s))
                        (+ (sprite-size value) (sprite-size s)))) lst))]
    (filter (lambda (e) (not (consult-list e emptior))) emptying)))

(check-expect (bullet-on-missile (list missile1) (list bullet1)) (list missile1))
(check-expect (bullet-on-missile (move-LOS MIRV) (move-LOS bulletstorm1))
              (move-LOS (list missile1 missile2 missile3 missile5 missile6)))

;; launch-sprites: LOS Posn Posn Number Number String-> LOS
;; creates a list of sprites with a new missile at a random position
(define (launch-sprites m launch land n radius color)
  (local
    ;; create-delta: Posn Posn Number -> Posn
    ;; Create a velocity from a speed in pixels n traveled between 2 Posns
    [(define (create-delta launch land n)
            (make-posn (* (- (posn-x land) (posn-x launch))
                          (/ n (distance launch land)))
                       (* (- (posn-y land) (posn-y launch))
                          (/ n (distance launch land)))))]
    (cons (make-sprite launch (create-delta launch land n) radius color) m)))

(check-expect (launch-sprites los-m (make-posn 5 16) (make-posn 10 28) 1 MISSILE-RADIUS "red")
              (cons (make-sprite (make-posn 5 16)
                                 (make-posn (* 5 (/ 1 13)) (* 12 (/ 1 13)))
                                 MISSILE-RADIUS "red")
                    los-m))



;; -----------------------------------------------------------------------------------------

;;; UPDATE-WORLD HELPER FUNCTIONS

;;; move-bullets: World -> World
;;; Move the bullets one time step and remove any that have gone off screen.
(define (move-bullets w)
  (make-world (world-missiles w) (move-LOS (world-bullets w)) (world-health w)))

(check-expect (move-bullets world1)
              (make-world (cons missile1 (cons missile2 empty))
                          (list (make-sprite (make-posn 203 204) (make-posn 3 4)
                                             BULLET-RADIUS "green")
                                (make-sprite (make-posn 195 212) (make-posn -5 12)
                                             BULLET-RADIUS "green")) 3))

;;; move-missiles: World -> World
;;; Move the missiles one time step.
(define (move-missiles w)
  (make-world (move-LOS (world-missiles w)) (world-bullets w) (world-health w)))

(check-expect (move-missiles world1)
              (make-world (list (make-sprite (make-posn 103 6) (make-posn 3 -4)
                                             MISSILE-RADIUS "red")
                                (make-sprite (make-posn 95 -2) (make-posn -5 -12)
                                             MISSILE-RADIUS "red"))
                          (cons bullet1 (cons bullet2 empty)) 3))


;;; remove-dead-missiles-and-bullets: World -> World
;;; Remove every missile that is touching some bullet and vice-versa.
(define (remove-dead-missiles-and-bullets w)
  (make-world (bullet-on-missile (world-missiles w) (world-bullets w))
              (bullet-on-missile (world-bullets w) (world-missiles w))
              (world-health w)))

(check-expect (remove-dead-missiles-and-bullets world1)
              (make-world
               (list
                (make-sprite (make-posn 100 10) (make-posn 3 -4) 9 "red")
                (make-sprite (make-posn 100 10) (make-posn -5 -12) 9 "red"))
               (list
                (make-sprite (make-posn 200 200) (make-posn 3 4) 5 "green")
                (make-sprite (make-posn 200 200) (make-posn -5 12) 5 "green")) 3))

;;; detonate-missiles: World -> World
;;; Remove missiles that landed... and decrement the player's health if any did.
(define (detonate-missiles w)
  (make-world (world-missiles (move-missiles w))
              (world-bullets w)
              (- (world-health w) 
                 (- (length (world-missiles w))
                    (length (world-missiles (move-missiles w)))))))


(check-expect (detonate-missiles (make-world (list hit) empty 90))
              (make-world empty empty 89))
(check-expect (detonate-missiles nosurvivors) (make-world empty empty 7))

;;; maybe-fire-missile: World -> World
;;; If we haven't maxed out on missiles, launch another one.
(define (maybe-fire-missile w)
  (cond [(< (length (world-missiles w)) 5)
         (make-world (launch-sprites (world-missiles w)
                                     (make-posn (random 501) 0)
                                     (make-posn (random 501) 500) MISSILE-SPEED MISSILE-RADIUS "red")
                     (world-bullets w)
                     (world-health w))]
        [else w]))


(check-range (posn-x (sprite-loc (first (world-missiles (maybe-fire-missile world1))))) 0 500)
(check-expect (posn-y (sprite-loc (first (world-missiles (maybe-fire-missile world1))))) 0)
(check-range (abs (posn-x (sprite-vel (first (world-missiles (maybe-fire-missile world1))))))
             (/ 1(* 100(sqrt 50))) 1)
(check-range (abs (posn-y (sprite-vel (first (world-missiles (maybe-fire-missile world1))))))
             (/ 1(* 100(sqrt 50))) 1)
(check-expect (world-bullets (maybe-fire-missile world1)) los-b)
(check-expect (world-health (maybe-fire-missile world1)) 3)
(check-expect (rest (world-missiles (maybe-fire-missile world1))) los-m)
(check-expect (maybe-fire-missile DEFCON1) DEFCON1)

;;; -------------------------------------------------------------------------------------------------
;;; ON-TICK MAIN FUNCTION

;;; update-world: World -> World
;;; Step the world one tick.
(define (update-world w)
  (maybe-fire-missile (detonate-missiles
                       (remove-dead-missiles-and-bullets
                        (move-missiles (move-bullets w))))))


(check-expect (update-world DEFCON1)
              (make-world
               (list
                (make-sprite (make-posn 106 2) (make-posn 3 -4) 9 "red")
                (make-sprite (make-posn 90 -14) (make-posn -5 -12) 9 "red")
                (make-sprite (make-posn 306 392) (make-posn 3 -4) 9 "red")
                (make-sprite (make-posn 11 292) (make-posn 3 -4) 9 "red")
                (make-sprite (make-posn 485 276) (make-posn -5 -12) 9 "red"))
               (list
                (make-sprite (make-posn 203 204) (make-posn 3 4) 5 "green")
                (make-sprite (make-posn 195 212) (make-posn -5 12) 5 "green")
                (make-sprite (make-posn 53 54) (make-posn 3 4) 5 "green"))
               100))

;; --------------------------------------------------------------------------------------------------

;; draw-LOS: LOS -> Image
;; draw a list of sprites on the scene
(define (draw-LOS lst img)
  (local (;; draw-sprites: Sprite -> Image
          ;; create an image of a given sprite
          (define (draw-sprite s)
            (circle (sprite-size s) "solid" (sprite-color s))))
  (foldr (lambda (s back) (place-image (draw-sprite s) (posn-x (sprite-loc s)) (posn-y (sprite-loc s)) back)) img lst)))

(check-expect (draw-LOS los-m BACKGROUND)
              (place-image MISSILE-IMAGE 100 10
                           (place-image MISSILE-IMAGE 100 10 BACKGROUND)))
(check-expect (draw-LOS los-b BACKGROUND)
              (place-image BULLET-IMAGE 200 200
                           (place-image BULLET-IMAGE 200 200 BACKGROUND)))

;;; ----------------------------------------------------------------------------------------------
;;; TO-DRAW MAIN FUNCTION

;; world->image: World -> Image
;; draw the current world state
(define (world->image w)
  (draw-LOS (world-bullets w)
            (draw-LOS (world-missiles w) BACKGROUND)))

(check-expect (world->image world1)
              (place-image BULLET-IMAGE 200 200
                           (place-image BULLET-IMAGE 200 200
                                        (place-image MISSILE-IMAGE 100 10
                                                     (place-image MISSILE-IMAGE 100 10 BACKGROUND)))))

;; -------------------------------------------------------------------------------------------
;;; ON-MOUSE MAIN FUNCTION

;; create-bullet: World Int Int MouseEvent -> World
;; creates a world with bullets after clicking a mouse
(define (create-bullet w x y z)
  (cond [(string=? "button-down" z)
         (make-world (world-missiles w)
                     (launch-sprites (world-bullets w) (make-posn 250 490)
                                     (make-posn x y) BULLET-SPEED BULLET-RADIUS "green")
                     (world-health w))]
        [else w]))

(check-expect (create-bullet world1 245 490 "button-down")
              (make-world los-m
                          (list
                           (make-sprite (make-posn 250 490) (make-posn -5 0) 5 "green")
                           (make-sprite (make-posn 200 200) (make-posn 3 4) 5 "green")
                           (make-sprite (make-posn 200 200) (make-posn -5 12) 5 "green")) 3))

(check-expect (create-bullet world1 100 100 "k") world1)

;; --------------------------------------------------------------------------------------------
;;; STOP-WHEN MAIN FUNCTION

;; health-0 World -> Boolean
;; stop the game when health is zero
(define (health-0 w)
  (<= (world-health w) 0))

(check-expect (health-0 (make-world los-m los-b 0)) true)
(check-expect (health-0 (make-world los-m los-b 1)) false)

;; --------------------------------------------------------------------------------------------

;; main: Number -> World
;; creates a missile defense game where missiles are fired from above
;; and bullets are fired from the bottom with starting health x of the player
(define (main x)
  (big-bang
   (make-world empty empty x)
   (on-tick update-world)
   (to-draw world->image)
   (on-mouse create-bullet)
   (stop-when health-0)))
(main 10)
