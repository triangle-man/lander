#lang racket
(require 2htdp/image
         2htdp/universe
         lang/posn)

(struct world (player-x player-y antimuffin-angle) #:transparent)

(define *height* 200)
(define *width* 300)


(define *antimuffin-orbit* 30)
(define *world-start*
  (world (/ *width* 2) (/ *height* 2) (/ pi 4)))

(define *player-size* 5)
(define player-image
  (circle *player-size* "solid" "green"))

(define *antimuffin-size* 5)
(define antimuffin-image
  (circle *antimuffin-size* "solid" "red"))

(define (move-right wrld delta)
  (struct-copy world wrld
               [player-x (+ delta (world-player-x wrld))]))

(define (move-up wrld delta)
  (struct-copy world wrld
               [player-y (+ delta (world-player-y wrld))]))


(define (rotate-antimuffins wrld)
  (struct-copy world wrld
               [antimuffin-angle (+ (world-antimuffin-angle wrld) 0.1)]))

(define (antimuffin-locations wrld)
  (let ([amx (* *antimuffin-orbit* (sin (world-antimuffin-angle wrld)))]
        [amy (* *antimuffin-orbit* (cos (world-antimuffin-angle wrld)))])
   (list (make-posn (- (/ *width* 2) amx) (+ (/ *height* 2) amy))
         (make-posn (+ (/ *width* 2) amx) (- (/ *height* 2) amy)))))
 
(define (draw-world wrld)
  (place-images
   (list player-image
         antimuffin-image
         antimuffin-image)
   (cons (make-posn (world-player-x wrld) (world-player-y wrld))
         (antimuffin-locations wrld))
   (empty-scene *width* *height*)))



(define (respond-to-key-event wrld key)
  (cond
    [(key=? key "left") (move-right wrld -1)]
    [(key=? key "right") (move-right wrld 1)]
    [(key=? key "up") (move-up wrld -1)]
    [(key=? key "down") (move-up wrld 1)]
    [else wrld]))

(define (sq x)
  (* x x))

(define (d2 x1 y1 x2 y2)
  (+ (sq (- x2 x1))
     (sq (- y2 y1))))

(define (collision? wrld)
  (let ([ams (antimuffin-locations wrld)])
    (let ([am1x (posn-x (car ams))]
          [am1y (posn-y (car ams))]
          [am2x (posn-x (cadr ams))]
          [am2y (posn-y (cadr ams))]
          [x (world-player-x wrld)]
          [y (world-player-y wrld)])
      (or (< (d2 x y am1x am1y)
             (sq (+ *player-size* *antimuffin-size*)))
          (< (d2 x y am2x am2y)
             (sq (+ *player-size* *antimuffin-size*))
             )))))


(define (game-over-image wrld)
  (place-image
   (text "Cwumbs" 24 "olive")
   (/ *width* 2) (/ *height* 2)
   (draw-world wrld)))

(big-bang *world-start*
  (to-draw draw-world)
  (on-key respond-to-key-event)
  (on-tick rotate-antimuffins)
  (stop-when collision? game-over-image))           

