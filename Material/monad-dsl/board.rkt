#lang racket/base
(require racket/list
         racket/sequence
         json)

(provide MIN-LEVEL MAX-LEVEL NUM-LEVELS WIN-LEVEL
         MIN-DIM MAX-DIM NUM-DIMS all-dims

         posn posn-i posn-j posn=? posn+ all-deltas

         player player-tokens player-move

         empty-board board-ref board-ref-player board-set board-empty?
         board-current-player board-previous-player board-player-move
         board-turn board-player-count
         board-token-add board-token-remove
         board-swap-player board-unswap-player

         write-board read-board
         write-players read-players

         board-add-style styled-board? styled-board-style)

;; a level is a number: 0 to 4
(define MIN-LEVEL 0)
(define MAX-LEVEL 4)
(define NUM-LEVELS (add1 (- MAX-LEVEL MIN-LEVEL))) 
(define WIN-LEVEL (sub1 MAX-LEVEL))

;; board spaces are numbered 1 to 5 in each dimension
(define MIN-DIM 1)
(define MAX-DIM 5)
(define NUM-DIMS (add1 (- MAX-DIM MIN-DIM)))

;; list of valid dimensions:
(define all-dims (sequence->list (in-range MIN-DIM (add1 MAX-DIM))))

;; posn is a pair of dimensions
(define posn cons)
(define posn-i car)
(define posn-j cdr)

(define (posn=? p1 p2)
  (and (= (posn-i p1) (posn-i p2))
       (= (posn-j p1) (posn-j p2))))

;; deltas; clockwise makes an attractive-looking search
(define all-deltas '((-1 . -1) (-1 . 0) (-1 . 1)
                               (0 . 1)
                               (1 . 1) (1 . 0) (1 . -1)
                               (0 . -1)))

;; add a posn and delta, but return #f if it doesn't stay
;; on the board
(define (posn+ p delta)
  (define i (+ (posn-i p) (car delta)))
  (define j (+ (posn-j p) (cdr delta)))
  (and (<= MIN-DIM i MAX-DIM)
       (<= MIN-DIM j MAX-DIM)
       (posn i j)))

;; a player is a set of 2 posns, which is represented as a hash table
;; of two keys mapped to #t
(define (player t1 t2) (hash t1 #t t2 #t))
(define (player-tokens p) (hash-keys p))
(define (player-move p from to)
  (hash-set (hash-remove p from) to #t))

;; turn: the turn number counting from 0
;; players: a list (length 2 or 3) of players, first is current player, second is next player
;; spaces: a mapping from posn to levels, where a mapping to `MIN-LEVEL` is omitted
(struct board (turn players spaces) #:prefab)
(struct styled-board board (style) #:prefab)

(define (board-current-player b)
  (car (board-players b)))

(define (board-previous-player b)
  (last (board-players b)))

(define (board-player-count b)
  (length (board-players b)))

(define (empty-board #:turn [turn 0] players)
  (board turn players #hash()))

(define (board-ref b posn)
  (hash-ref (board-spaces b) posn MIN-LEVEL))

(define (board-empty? b)
  (zero? (hash-count (board-spaces b))))

;; Returns #f, 0, 1, or 2 --- and 2 only for a 3-player game
(define (board-ref-player b posn)
  (for/or ([p (in-list (board-players b))]
           [k (in-naturals)])
    (and (hash-ref p posn #f) k)))

(define (board-set b posn level)
  (board (board-turn b)
         (board-players b)
         (if (eqv? level MIN-LEVEL)
             (hash-remove (board-spaces b) posn)
             (hash-set (board-spaces b) posn level))))

(define (board-player-move b player)
  (board (add1 (board-turn b))
         (append (cdr (board-players b)) (list player))
         (board-spaces b)))

(define (board-token-add b token)
  (define players (board-players b))
  (struct-copy board b
               [players (cons (hash-set (first players) token #t)
                              (rest players))]))

(define (board-token-remove b token)
  (define players (board-players b))
  (struct-copy board b
               [players (cons (hash-remove (first players) token)
                              (rest players))]))

(define (board-swap-player b)
  (struct-copy board b
               [turn (add1 (board-turn b))]
               [players (reverse (board-players b))]))

(define (board-unswap-player b)
  (struct-copy board b
               [turn (sub1 (board-turn b))]
               [players (reverse (board-players b))]))

(define (board-add-style b style)
  (styled-board (board-turn b) (board-players b) (board-spaces b) style))

;; ----------------------------------------

;; Convert a board to a JSON byte string
(define (write-board b [out (current-output-port)])
  (define spaces (board-spaces b))
  (define ht
    (hasheq 'turn
            (board-turn b)
            'players
            (players->list (board-players b))
            'spaces
            (for/list ([i (in-range MIN-DIM (add1 MAX-DIM))])
              (for/list ([j (in-range MIN-DIM (add1 MAX-DIM))])
                (hash-ref spaces (posn i j) MIN-LEVEL)))))
  (write-json (if (styled-board? b)
                  (hash-set ht 'style (styled-board-style b))
                  ht)
              out)
  (newline out)
  (flush-output out))

;; Convert a list of players to a JSON byte string
(define (write-players players [out (current-output-port)])
  (write-json (players->list players) out)
  (newline out)
  (flush-output out))

(define (players->list players)
  (define (posn->hash p) (list (posn-i p) (posn-j p)))
  (for/list ([p (in-list players)])
    (map posn->hash (player-tokens p))))

;; Parse JSON input as a board, rejecting anything that is
;; not a valid board
(define (read-board [in (current-input-port)]
                    #:allow-init? [allow-init? #f])
  (define (wrong msg . args)
    (apply error 'parse-board msg args))
  (define ht
    (with-handlers ([exn:fail:contract? (lambda (exn)
                                          (wrong "not valid JSON"))])
      (read-json in)))
  (cond
    [(eof-object? ht) eof]
    [(and allow-init? (list? ht))
     (define players (parse-players ht wrong #:min-players 0))
     (empty-board players)]
    [else
     (unless (hash? ht) (wrong "JSON data is not a dictionary"))
     (define turn (hash-ref ht 'turn (lambda () (wrong "dictionary has no \"turn\" entry"))))
     (unless (exact-nonnegative-integer? turn) (wrong "invalid turn number"))
     (define players-l (hash-ref ht 'players (lambda () (wrong "dictionary has no \"players\" entry"))))
     (define players (parse-players players-l wrong #:min-players 2))
     (define spaces-l (hash-ref ht 'spaces (lambda () (wrong "dictionary has no \"spaces\" entry"))))
     (unless (list? spaces-l) (wrong "\"spaces\" entry is not a array"))
     (unless (= (length spaces-l) NUM-DIMS) (wrong "\"spaces\" entry is not a array of the right length"))
     (for ([row (in-list spaces-l)])
       (unless (list? row) (wrong "row within \"spaces\" array is not itself a array"))
       (unless (= (length row) NUM-DIMS) (wrong "row array within \"spaces\" array is not the right length"))
       (for ([level (in-list row)])
         (unless (and (exact-integer? level) (<= MIN-LEVEL level MAX-LEVEL))
           (wrong "space value in \"spaces\" is not a valid level: ~v" level))))
     (define spaces (for/hash ([row (in-list spaces-l)]
                              [i (in-naturals MIN-DIM)]
                              #:when #t
                              [level (in-list row)]
                              [j (in-naturals MIN-DIM)]
                              #:when (not (eqv? level MIN-LEVEL)))
                      (values (posn i j) level)))
     (define has-style? (hash-has-key? ht 'style))
     (unless (= (hash-count ht) (if has-style? 4 3)) (wrong "dictionary has extra keys"))
     (define b (board turn players spaces))
     (if has-style?
         (board-add-style b (hash-ref ht 'style))
         b)]))

;; Parse a JSON byte string as a list of players, as used for setting
;; up a board
(define (read-players [in (current-input-port)])
  (define (wrong msg) (error 'parse-players "~a" msg))
  (define players-l
    (with-handlers ([exn:fail:contract? (lambda (exn)
                                          (wrong "not valid JSON"))])
      (read-json in)))
  (cond
    [(eof-object? players-l) eof]
    [else (parse-players players-l wrong #:min-players 0)]))

(define (parse-players players-l wrong
                       #:min-players min-players
                       #:max-players [max-players 2])
  (unless (list? players-l) (wrong "\"players\" entry is not a array"))
  (unless (<= min-players (length players-l) max-players) (wrong "\"players\" array has invalid length"))
  (define players (for/list ([l (in-list players-l)])
                    (unless (list? l) (wrong "token in \"players\" array is not itself a array"))
                    (unless (= (length l)) (wrong "token in \"players\" array is not a array of length 2"))
                    (define (json->posn t)
                      (unless (list? t) (wrong "token is not an array"))
                      (unless (= (length t) 2) (wrong "token is not an array of length 2"))
                      (define i (car t))
                      (define j (cadr t))
                      (unless (and (exact-integer? i) (<= MIN-DIM i MAX-DIM)) (wrong "token's row is not valid"))
                      (unless (and (exact-integer? j) (<= MIN-DIM j MAX-DIM)) (wrong "token's column is not valid"))
                      (posn i j))
                    (define t1 (json->posn (car l)))
                    (define t2 (json->posn (cadr l)))
                    (when (equal? t1 t2) (wrong "player's tokens cannot be at the same position"))
                    (player t1 t2)))
  (for*/fold ([occupied #hash()]) ([p (in-list players)]
                                   [t (in-list (player-tokens p))])
    (when (hash-ref occupied t #f) (wrong "different players' tokens are at the same position"))
    (hash-set occupied t #t))
  players)

;; ================================================================================

(module+ test
  (require racket/port)
  
  (define (board->json b) (with-output-to-bytes (lambda () (write-board b))))
  (define (players->json players) (with-output-to-bytes (lambda () (write-players players))))

  (define (json->board bstr) (with-input-from-bytes bstr read-board))
  (define (json->players bstr) (with-input-from-bytes bstr read-players))
  
  (define-syntax-rule (check a b)
    (unless (equal? a b) (error 'test "failed\n ~s\n ~s" 'a 'b)))
  (define-syntax-rule (check-bad msg bstr ...)
    (with-handlers ([exn:fail? (lambda (exn)
                                 (unless (regexp-match? msg (exn-message exn))
                                   (error 'bad "failed: ~s ~s" (syntax-line #'msg)
                                          (exn-message exn))))])
      (json->board (bytes-append bstr ...))
      (error "bad should have failed")))

  (define p1 (player (posn 2 3) (posn 4 5)))
  (define p2 (player (posn 1 1) (posn 5 5)))
  (define p3 (player (posn 4 3) (posn 3 2)))

  (define b2 (empty-board (list p1 p2)))

  (check (board-ref b2 (posn 3 4))
         0)
  (check (board-ref b2 (posn 4 5))
         0)
  (check (board-ref-player b2 (posn 3 4))
         #f)
  (check (board-ref-player b2 (posn 4 5))
         0)
  (check (board-ref-player b2 (posn 5 5))
         1)
  
  (define b2x (let* ([b (board-set b2 (posn 1 1) 1)]
                     [b (board-set b (posn 4 5) 4)]
                     [b (board-set b (posn 3 2) 2)]
                     [b (board-set b (posn 3 2) 3)])
                b))

  (check (board-ref b2x (posn 2 2))
         0)
  (check (board-ref b2x (posn 3 2))
         3)
  
  (check (board-set (board-set b2 (posn 2 2) 3) (posn 2 2) 0)
         b2)
  
  (check (board-current-player b2)
         (player (posn 2 3) (posn 4 5)))  
  (check (board-player-move b2
                            (player-move p1 (posn 2 3) (posn 3 3)))
         (empty-board #:turn 1 (list p2 (player (posn 3 3) (posn 4 5)))))
  (check (board-player-move b2
                            (player-move p1 (posn 4 5) (posn 3 3)))
         (empty-board #:turn 1 (list p2 (player (posn 2 3) (posn 3 3)))))
  
  (check b2 (json->board (board->json b2)))
  (check b2x (json->board (board->json b2x)))

  (define ok-spaces-bstr
    (bytes-append
     #"{\"spaces\":[[0,0,0,0,0],[0,0,0,0,0],[0,0,0,0,0],[0,0,0,0,0],[0,0,0,0,0]],"
     #" \"turn\":1,"))

  (check-bad
   "players"
   #"{\"turn\":1,\"spaces\":[[0,0,0,0,0],[0,0,0,0,0],[0,0,0,0,0],[0,0,0,0,0],[0,0,0,0,0]]}")

  (check-bad
   "spaces"
   #"{\"turn\":1,\"players\":[[[2,3],[4,5]],[[1,1],[5,5]]]}")

  (check-bad
   "turn"
   #"{\"spaces\":[[0,0,0,0,0],[0,0,0,0,0],[0,0,0,0,0],[0,0,0,0,0],[0,0,0,0,0]]}"
   #"\"players\":[[[2,3],[4,5]],[[1,1],[5,5]]]}")

  (check-bad
   "turn"
   #"{\"turn\":\"x\",\"spaces\":[[0,0,0,0,0],[0,0,0,0,0],[0,0,0,0,0],[0,0,0,0,0],[0,0,0,0,0]]}"
   #"\"players\":[[[2,3],[4,5]],[[1,1],[5,5]]]}")

  (check-bad
   "turn"
   #"{\"turn\":-10,\"spaces\":[[0,0,0,0,0],[0,0,0,0,0],[0,0,0,0,0],[0,0,0,0,0],[0,0,0,0,0]]}"
   #"\"players\":[[[2,3],[4,5]],[[1,1],[5,5]]]}")

  (check-bad
   "extra"
   ok-spaces-bstr
   #"\"other\":5,"
   #"\"players\":[[[2,3],[4,5]],[[1,1],[5,5]]]}")

  (check-bad
   "spaces"
   #"{\"turn\":3,\"spaces\":7,"
   #"\"players\":[[[2,3],[4,5]],[[1,1],[5,5]]]}")

  (check-bad
   "players"
   ok-spaces-bstr
   #"\"players\":7}")

  (check-bad
   "players"
   ok-spaces-bstr
   #"\"players\":[[[1,2],[3,4]]]}")

  (check-bad
   "players"
   ok-spaces-bstr
   #"\"players\":[[[1,2],[3,4]],[[1,1],[5,5]],"
   #"[[2,1],[2,2]],[{\"i\":4,\"j\":3},{\"i\":3,\"j\":3}]]}")

  (check-bad
   "spaces"
   #"{\"turn\":1,\"spaces\":[[0,0,0,0,0],[0,0,0,0,0],[0,0,0,0,0],[0,0,0,0,0],[0,0,0,0,0,0]],"
   #"\"players\":[[[2,3],[4,5]],[[1,1],[5,5]]]}")

  (check-bad
   "valid level"
   #"{\"turn\":1,\"spaces\":[[0,0,0,0,0],[0,0,0,0,0],[0,0,0,0,\"0\"],[0,0,0,0,0],[0,0,0,0,0]],"
   #"\"players\":[[[2,3],[4,5]],[[1,1],[5,5]]]}")

  (check-bad
   "valid level"
   #"{\"turn\":1,\"spaces\":[[0,0,0,0,0],[0,0,0,0,0],[0,0,0,8,0],[0,0,0,0,0],[0,0,0,0,0]],"
   #"\"players\":[[[2,3],[4,5]],[[1,1],[5,5]]]}")

  (check-bad
   "valid level"
   #"{\"turn\":1,\"spaces\":[[0,0,0,0,0],[0,0,-1,0,0],[0,0,0,0,0],[0,0,0,0,0],[0,0,0,0,0]],"
   #"\"players\":[[[2,3],[4,5]],[[1,1],[5,5]]]}")

  (check-bad
   "token"
   ok-spaces-bstr
   #"\"players\":[[[1,2],10],[[1,1],[5,5]]]}")

  (check-bad
   "token"
   ok-spaces-bstr
   #"\"players\":[[[1,2],[3]],[[1,1],[5,5]]]}")

  (check-bad
   "token"
   ok-spaces-bstr
   #"\"players\":[[[1,2],[3,\"no\"]],[[1,1],[5,5]]]}")

  (check-bad
   "token"
   ok-spaces-bstr
   #"\"players\":[[[1,2],[3,6]],[[1,1],[5,5]]]}")

  (check-bad
   "token"
   ok-spaces-bstr
   #"\"players\":[[[2,3],[-3,5]],[[1,1],[5,5]]]}")

  (check-bad
   "same position"
   ok-spaces-bstr
   #"\"players\":[[[2,3],[2,3]],[[1,1],[5,5]]]}")

  (check-bad
   "same position"
   ok-spaces-bstr
   #"\"players\":[[[2,3],[4,5]],[[1,1],[2,3]]]}")

  (void))
