#lang racket/base
(require "board.rkt"
         "unique.rkt"
         racket/stream
         racket/list
         "monad.rkt")

;; Detour to another representation: continuation-passing style,
;; instead of representing the continuation as a list
;; to reverse.

(provide all-next-setups
         all-next-moves
         winner?)

;; ----------------------------------------

;; A `List` instance is a ((-> list) -> list)

(define (extract-list m)
  (m (lambda () empty)))

(define-monad List
  #:return (lambda (v)
             (lambda (next)
               (cons v (next))))
  #:bind (lambda (st k)
           (lambda (next)
             (let loop ([st st])
               (cond
                 [(stream-empty? st) (next)]
                 [else (let ([v (stream-first st)]
                             [st (stream-rest st)])
                         ((k v)
                          (lambda ()
                            (loop st))))])))))

(define (when... v)
  (if v '(dummy) '()))
        
(define (unless... v)
  (if v '() '(dummy)))

;; ----------------------------------------

(define (all-next-setups players)
  (unique-elements
   (extract-list
    (do-it
     List
     (define* i1 (in-list all-dims))
     (define* j1 (in-list all-dims))
     (define p1 (posn i1 j1))
     (define* i2 (in-list all-dims))
     (define* j2 (in-list all-dims))
     (define p2 (posn i2 j2))
     (unless... (or (posn=? p1 p2)
                    (for*/or ([pl (in-list players)]
                              [t (in-list (player-tokens pl))])
                      (or (posn=? p1 t)
                          (posn=? p2 t)))))
     (return (append players (list (player p1 p2))))))))

(define (all-next-moves b)
  (define pl (board-current-player b))
  (extract-list
   (do-it
    List
    (define* t (in-list (player-tokens pl)))
    (define level (board-ref b t))
    (define* d (in-list all-deltas))
    (define t-to (posn+ t d))
    (when... t-to)
    ;; We have a valid position, but it is occupied?
    (unless... (board-ref-player b t-to))
    ;; Not a capped space or illegal jump?
    (define level-to (board-ref b t-to))
    (unless... (or (= level-to MAX-LEVEL)
                   (level-to . > . (add1 level))))
    ;; After moving, we have to either have won
    ;; or be able to build
    (if (= level-to WIN-LEVEL)
        ;; won
        (return (board-player-move b (player-move pl t t-to)))
        (all-builds b pl t t-to)))))

(define (all-builds b pl t t-to)
  (do-it
   List
   (define* d2 (in-list all-deltas))
   (define c-build (posn+ t-to d2))
   (when... c-build)
   ;; Can't build on an occupied spot
   (unless... (and (not (posn=? c-build t))
                   (board-ref-player b c-build)))
   ;; Can't build if the tower is maxed out
   (define level3 (board-ref b c-build))
   (unless... (= level3 MAX-LEVEL))
   (define move-b (board-player-move b (player-move pl t t-to)))
   (return (board-set move-b c-build (add1 level3)))))

(define (winner? b)
  (for/or ([t (in-list (player-tokens (board-previous-player b)))])
    (eqv? WIN-LEVEL (board-ref b t))))

;; ----------------------------------------

(module+ test
  (require racket/port)
  
  (define-syntax-rule (check a b)
    (unless (equal? a b) (error 'test "failed\n ~s\n ~s" 'a 'b)))
  
  (check (length (all-next-setups '()))
         ;; N choose 2
         (let ([n (* NUM-DIMS NUM-DIMS)])
           (/ (* n (sub1 n)) 2)))

  (define (json->board bstr) (with-input-from-bytes bstr read-board))

  (define b (json->board
             (bytes-append #"{\"turn\":18,\"players\":[[[2,3],[4,4]],[[3,5],[2,5]]],"
                           #"\"spaces\":[[0,0,0,0,2],[1,1,2,0,0],[1,0,0,3,0],[0,0,3,0,0],[0,0,0,1,4]]}")))
  (define b2 (json->board
              (bytes-append #"{\"turn\":19,\"players\":[[[3,5],[2,5]],[[3,3],[4,4]]],"
                            #"\"spaces\":[[0,0,0,0,2],[1,1,2,0,0],[1,0,0,3,0],[0,0,4,0,0],[0,0,0,1,4]]}")))
  (define b3 (json->board
              (bytes-append #"{\"turn\":20,\"players\":[[[3,3],[4,4]],[[3,5],[2,4]]],"
                            #"\"spaces\":[[0,0,0,0,2],[1,1,2,0,1],[1,0,0,3,0],[0,0,4,0,0],[0,0,0,1,4]]}")))

  (define b2-alt (json->board
                  (bytes-append #"{\"turn\":19,\"players\":[[[3,5],[2,5]],[[3,4],[4,4]]],"
                                #"\"spaces\":[[0,0,0,0,2],[1,1,2,0,0],[1,0,0,3,0],[0,0,3,0,0],[0,0,0,1,4]]}")))

  ;; No jumping too high:
  (define b3-no (json->board
                 (bytes-append #"{\"turn\":20,\"players\":[[[3,3],[4,4]],[[3,5],[1,5]]],"
                               #"\"spaces\":[[0,0,0,0,2],[1,1,2,0,1],[1,0,0,3,0],[0,0,4,0,0],[0,0,0,1,4]]}")))
  (define b3-no2 (json->board
                  (bytes-append #"{\"turn\":20,\"players\":[[[3,3],[4,4]],[[3,5],[3,4]]],"
                                #"\"spaces\":[[0,0,0,0,2],[1,1,2,0,1],[1,0,0,3,0],[0,0,4,0,0],[0,0,0,1,4]]}")))
  ;; No building over token:
  (define b3-no3 (json->board
                  (bytes-append #"{\"turn\":20,\"players\":[[[3,3],[4,4]],[[3,5],[2,4]]],"
                                #"\"spaces\":[[0,0,0,0,2],[1,1,2,0,0],[1,0,0,3,1],[0,0,4,0,0],[0,0,0,1,4]]}")))

  (check (and (member b2 (all-next-moves b)) #t) #t)
  (check (and (member b3 (all-next-moves b2)) #t) #t)
  (check (and (member b2-alt (all-next-moves b)) #t) #t)

  (check (member b3-no (all-next-moves b2)) #f)
  (check (member b3-no2 (all-next-moves b2)) #f)
  (check (member b3-no3 (all-next-moves b2)) #f)
  
  (void))
