#lang racket
(require 2htdp/image
         2htdp/universe)

(struct world (player-x player-y) #:transparent)

(define *world-start* (world 50 50))

(define (move-right wrld delta)
  (world (+ delta (world-player-x wrld))
         (world-player-y wrld)))

(define (move-up wrld delta)
  (world (world-player-x wrld)
         (+ delta (world-player-y wrld))))


(define (draw-world wrld)
  (place-image (circle 5 "solid" "green")
               (world-player-x wrld) (world-player-y wrld)
               (empty-scene 100 100)))

(define (respond-to-key-event wrld key)
  (cond
    [(key=? key "left") (move-right wrld -1)]
    [(key=? key "right") (move-right wrld 1)]
    [(key=? key "up") (move-up wrld -1)]
    [(key=? key "down") (move-up wrld 1)]
    [else wrld]))