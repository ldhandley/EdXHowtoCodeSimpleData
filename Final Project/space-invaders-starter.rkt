;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname space-invaders-starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/universe)
(require 2htdp/image)

;; Space Invaders
;;==========================
;; Constants:

(define WIDTH  300)
(define HEIGHT 500)

(define INVADER-X-SPEED 1.5)  ;speeds (not velocities) in pixels per t
(define INVADER-Y-SPEED 1.5)
(define TANK-SPEED 2)
(define MISSILE-SPEED 10)

(define HIT-RANGE 10)

(define INVADE-RATE 100)

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


;;==========================
;; Data Definitions:

(define-struct game (invaders missiles tank))
;; Game is (make-game  (listof Invader) (listof Missile) Tank)
;; interp. the current state of a space invaders game
;;         with the current invaders, missiles and tank position

;; Game constants defined below Missile data definition

#;
(define (fn-for-game g)
  (... (fn-for-loinvader (game-invaders g))
       (fn-for-lom (game-missiles g))
       (fn-for-tank (game-tank g))))



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


(define-struct missile (x y))
;; Missile is (make-missile Number Number)
;; interp. the missile's location is x y in screen coordinates

(define M0 (make-missile 150 -1)) ;;off screen, should get cleaned up
(define M1 (make-missile 150 300))                       ;not hit U1
(define M2 (make-missile (invader-x I1) (+ (invader-y I1) 10)))  ;exactly hit U1
(define M3 (make-missile (invader-x I1) (+ (invader-y I1)  5)))  ;> hit U1
#;
(define (fn-for-missile m)
  (... (missile-x m) (missile-y m)))

;;ListofMissiles is either:
;; - empty
;; - (cons (make-missile Number Number) empty)
(define LOM0 empty)
(define LOM1 (list (make-missile 10 10)))
(define LOM2 (list (make-missile 10 10) (make-missile 100 50)))
#;
(define (fn-for-lom lom)
      (cond
        [(empty? lom) empty]
        [else (... (missile-x (first lom))
                   (missile-y (first lom))
                   (fn-for-missile (rest lom)))]))


(define G0 (make-game empty empty T0))
(define G1 (make-game empty empty T1))
(define G2 (make-game (list I1) (list M1) T1))
(define G3 (make-game (list I1 I2) (list M1 M2) T1))
;;==========================
;; Functions:

;; Game -> Game
;; start the world with (main (make-game empty empty T0))
;; 
(define (main g)
  (big-bang g                   ; Game
            (on-tick   tock)     ; Game -> Game
            (to-draw   render)   ; Game -> Image
            (stop-when stopping-condition)      ; Game -> Boolean
            (on-key    handle-key)))    ; Game KeyEvent -> Game

;; Game -> Game
;; produce the next game state
;; Only tank & missiles right now - need to add invaders
;; !!!
;;(define (tock game) (make-game empty empty T0));stub
(check-expect (tock (make-game empty empty (make-tank 50 1)))
              (make-game empty empty (move-tank (make-tank 50 1))))
(check-expect (tock (make-game empty
                               (list (make-missile 40
                                                   (/ HEIGHT 2))
                                     (make-missile 50
                                                   2))
                               (make-tank 50 1)))
              (make-game empty
                         (list (make-missile 40
                                             (- (/ HEIGHT 2) MISSILE-SPEED)))
                         (move-tank (make-tank 50 1))))

(define (tock g)
  (make-game
   (game-invaders g)
   (clean-missiles (move-missiles (game-missiles g)))
   (move-tank (game-tank g))))

;; ListOfMissiles -> ListOfMissiles
;; gets rid of missiles that have flown off screen (y coordinate < 0)
;;(define (clean-missiles lom) empty);stub
(check-expect (clean-missiles empty) empty)
(check-expect (clean-missiles (list M1 M0 M2)) (list M1 M2))
(check-expect (clean-missiles (list M0)) empty)
(define (clean-missiles lom)
      (cond
        [(empty? lom) empty]
        [else (if (< (missile-y (first lom)) 0)
                   (clean-missiles (rest lom))
                   (cons (first lom) (clean-missiles (rest lom))))]))

;; ListOfMissiles -> ListOfMissiles
;; moves all the missiles in the game up the screen by MISSILE-SPEED
;;(define (move-missiles lom) empty)
;;(define LOM2 (list (make-missile 10 10) (make-missile 100 50)))
(check-expect (move-missiles LOM0)
              empty)
(check-expect (move-missiles LOM2)
              (list (make-missile 10
                                  (- 10 MISSILE-SPEED))
                    (make-missile 100
                                  (- 50 MISSILE-SPEED))))
(define (move-missiles lom)
      (cond
        [(empty? lom) empty]
        [else (cons (move-missile (first lom))
                    (move-missiles (rest lom)))]))

;; Missile -> Missile
;; moves a single missile up the screen by MISSILE-SPEED
;;(define (move-missile m) (make-missile 50 50));stub
(check-expect (move-missile (make-missile 50 50)) (make-missile 50 (- 50 MISSILE-SPEED)))
(define (move-missile m)
  (make-missile (missile-x m) (- (missile-y m) MISSILE-SPEED)))

;; Tank -> Tank
;; moves tank by tank speed in specified direction (left or right)
;; cool feature: tank bounces off edge!
;;(define (move-tank t) T1);stub
(check-expect (move-tank (make-tank 50 1)) (make-tank (+ (* 1 TANK-SPEED) 50) 1))
(check-expect (move-tank (make-tank 50 -1)) (make-tank (+ (* -1 TANK-SPEED) 50) -1))
(check-expect (move-tank (make-tank 50 0)) (make-tank (+ (* 0 TANK-SPEED) 50) 0))
(check-expect (move-tank (make-tank WIDTH 1)) (make-tank (- WIDTH TANK-SPEED) -1))
(check-expect (move-tank (make-tank 0 -1)) (make-tank (+ 0 TANK-SPEED) 1))

(define (move-tank t)
  (cond [(>= (tank-x t) WIDTH)
         (make-tank (- WIDTH TANK-SPEED)
                    -1)]
        [(<= (tank-x t) 0)
         (make-tank (+ 0 TANK-SPEED)
                    1)]
        [else (make-tank (+ (* (tank-dir t) TANK-SPEED) (tank-x t))
                         (tank-dir t))]))

;; Game -> Image
;; renders the game state (only tank & missiles right now! need to add missiles, invaders)
;; !!!
;;(define (render game) BACKGROUND);stub
(check-expect (render (make-game empty empty T0))
              (place-image TANK
                           (tank-x T0)
                           (- HEIGHT TANK-HEIGHT/2)
                           BACKGROUND))
(check-expect (render (make-game empty empty (make-tank (/ WIDTH 4) -1)))
              (place-image TANK
                           (/ WIDTH 4)
                           (- HEIGHT TANK-HEIGHT/2)
                           BACKGROUND))

(define (render g) 
   (render-missiles g (render-tank g BACKGROUND)))

;; Game Image -> Image
(define (render-tank g bg)
  (place-image TANK
               (extract-tank-x (game-tank g))
               (- HEIGHT TANK-HEIGHT/2)
               bg))

;; Game Image -> Image
;; grabs missiles from the game and places missiles in background at appropriate x,y coordinates
;; !!!
;;(define (render-missiles g img)
;;  (place-image TANK
;;               (extract-tank-x (game-tank g))
;;               (- HEIGHT TANK-HEIGHT/2)
;;               img));stub
(check-expect (render-missiles G2 BACKGROUND)
              (place-image MISSILE
                           150
                           300
                           BACKGROUND))

(define (render-missiles g img)
  (place-missiles (extract-missiles g) img))

;;Game -> ListOfMissiles
;;Extracts missiles from game
(define (extract-missiles g)
  (game-missiles g))

;;ListOfMissiles Img -> Img
;; Places missiles on image
(define (place-missiles lom img)
      (cond
        [(empty? lom) (place-image (square 0 "solid" "white")
                                   0
                                   0
                                   img)]
        [else (place-missile (first lom)
               (place-missiles (rest lom) img))]))

;; Missile Img -> Img
;; Places a single missile within larger image
;; !!!
;;(define (place-missile m img) BACKGROUND);STUB

(check-expect (place-missile (make-missile 50 50) BACKGROUND)
              (place-image MISSILE
                           50
                           50
                           BACKGROUND))
(define (place-missile m img)
  (place-image MISSILE
               (missile-x m)
               (missile-y m)
               img))

;; Tank -> Integer
;; extracts the x position of the tank
(check-expect (extract-tank-x (make-tank 50 1)) 50)
(check-expect (extract-tank-x (make-tank 150 -1)) 150)
(define (extract-tank-x t)
  (tank-x t))

;; Game KeyEvent -> Game
;; changes tank direction with left/right
;; TODO:fires missile (adds to missile list) with space bar
;; !!!
;;(make-game empty empty T0)
;;(define (handle-key g ke) G0);stub
(check-expect (handle-key G0 "left")
              (make-game empty empty (make-tank (/ WIDTH 2) -1)))
(check-expect (handle-key G0 "right")
              (make-game empty empty (make-tank (/ WIDTH 2) 1)))
(check-expect (handle-key G0 " ")
              (make-game empty
                         (cons (make-missile (extract-tank-x (game-tank G0)) (- HEIGHT TANK-HEIGHT/2))
                               empty)
                         (make-tank (/ WIDTH 2) 1)))

(define (handle-key g ke)
  (cond [(key=? ke "left")
         (make-game (game-invaders g)
                    (game-missiles g)
                    (make-tank (extract-tank-x (game-tank g)) -1))] 
        [(key=? ke "right")
         (make-game (game-invaders g)
                    (game-missiles g)
                    (make-tank (extract-tank-x (game-tank g)) 1))]
        [(key=? ke " ")
         (make-game (game-invaders g)
                    (add-missile g)
                    (game-tank g))]
        [else g]))

;; Game -> ListOfMissiles
;; add missile to missile list
;; !!!
;;(define (add-missile g) empty);stub
(check-expect (add-missile G0)
              (cons (make-missile (extract-tank-x (game-tank G0)) (- HEIGHT TANK-HEIGHT/2)) empty))
(check-expect (add-missile G3)
              (list (make-missile (extract-tank-x (game-tank G3)) (- HEIGHT TANK-HEIGHT/2)) M1 M2))
(define (add-missile g)
  (cons (make-missile
         (extract-tank-x (game-tank g))
         (- HEIGHT TANK-HEIGHT/2))
        (game-missiles g)))

;; Game -> Boolean
;; stops game when a space invader reaches the bottom of the screen
;; !!!
(define (stopping-condition g) false);stub