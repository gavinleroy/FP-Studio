#lang racket/base
(require racket/list
         "board.rkt"
         "unique.rkt"
         racket/stream)

;; Partial hand expansion of v1 to demonstrate the kind of code
;; that `for*/list` generates.

(provide all-next-setups
         all-next-moves
         winner?)

(define (all-next-setups players)
  (define (i1-loop i1s)
    (cond
      [(stream-empty? i1s) empty]
      [else
       (define i1 (stream-first i1s))
       (define (j1-loop j1s)
         (cond
           [(stream-empty? j1s) empty]
           [else
            (define j1 (stream-first j1s))
            (define p1 (posn i1 j1))
            (define (i2-loop i2s)
              (cond
                [(stream-empty? i2s) empty]
                [else
                 (define i2 (stream-first i2s))
                 (define (j2-loop j2s)
                   (cond
                     [(stream-empty? j2s) empty]
                     [else
                      (define j2 (stream-first j2s))
                      (define p2 (posn i2 j2))
                      (cond
                        [(posn=? p1 p2) (j2-loop (stream-rest j2s))]
                        [(for*/or ([pl (in-list players)]
                                   [t (in-list (player-tokens pl))])
                           (or (posn=? p1 t)
                               (posn=? p2 t)))
                         (j2-loop (stream-rest j2s))]
                        [else
                         (cons (append players (list (player p1 p2)))
                               (j2-loop (stream-rest j2s)))])]))
                 (append (j2-loop all-dims)
                         (i2-loop (stream-rest i2s)))]))
            (append (i2-loop all-dims)
                    (j1-loop (stream-rest j1s)))]))
       (append (j1-loop all-dims)
               (i1-loop (stream-rest i1s)))]))
  (unique-elements
   (i1-loop all-dims)))

(define (all-next-moves b)
  (define pl (board-current-player b))
  ;; Clockwise makes an attractive-looking search:
  (let t-loop ([ts (player-tokens pl)])
    (cond
      [(stream-empty? ts) empty]
      [else
       (define t (stream-first ts))
       (define level (board-ref b t))
       (let d-loop ([ds all-deltas])
         (cond
           [(stream-empty? ds) (t-loop (stream-rest ts))]
           [else
            (define d (stream-first ds))
            (define t-to (posn+ t d))
            (cond
              [t-to
               ;; We have a valid position, but it is occupied?
               (cond
                 [(board-ref-player b t-to)
                  (d-loop (stream-rest ds))]
                 [else
                  ;; Not a capped space or illegal jump?
                  (define level-to (board-ref b t-to))
                  (cond
                    [(or (= level-to MAX-LEVEL)
                         (level-to . > . (add1 level)))
                     (d-loop (stream-rest ds))]
                    [else
                     ;; After moving, we have to either have won
                     ;; or be able to build
                     (define won? (= level-to WIN-LEVEL))
                     (let d2-loop ([d2s (if won?
                                            '((0 . 0)) ; dummy delta stays in bounds
                                            all-deltas)])
                       (cond
                         [(stream-empty? d2s) (d-loop (stream-rest ds))]
                         [else
                          (define d2 (stream-first d2s))
                          (define c-build (posn+ t-to d2))
                          (cond
                            [c-build
                             ;; Can't build on an occupied spot
                             (cond
                               [(and (not won?)
                                     (not (posn=? c-build t))
                                     (board-ref-player b c-build))
                                (d2-loop (stream-rest d2s))]
                               [else
                                ;; Can't build if the tower is maxed out
                                (define level3 (board-ref b c-build))
                                (cond
                                  [(and (not won?)
                                        (= level3 MAX-LEVEL))
                                   (d2-loop (stream-rest d2s))]
                                  [else
                                   (define move-b (board-player-move b (player-move pl t t-to)))
                                   (cons (if won?
                                             move-b
                                             (board-set move-b c-build (add1 level3)))
                                         (d2-loop (stream-rest d2s)))])])]
                            [else (d2-loop (stream-rest d2s))])]))])])]
              [else (d-loop (stream-rest ds))])]))])))

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
