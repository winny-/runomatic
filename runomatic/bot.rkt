#lang racket

(provide (all-defined-out)
         (all-from-out "types.rkt"))

(require "client.rkt"
         "types.rkt"
         gregor)

(define (my-player state)
  (match state
    [(struct* GameState ([players players]))
     (for/first ([p players]
                 #:when (Player-id p))
       p)]))

(define (my-hand state)
  (Player-hand (my-player state)))

(define (admin-player state)
  (match state
    [(struct* GameState ([players players]))
     (for/first ([p players]
                 #:when (Player-admin p))
       p)]))

(define (my-turn? state)
  (and (GameState-active state)
       (let ([me (my-player state)])
         (Player-active me))))

(define (playable-card? state card)
  (and (my-turn? state)
       (let ([last-discard (GameState-last-discard state)])
         (match card
           [(struct Card (_ _ (or "WILD" "WILD_DRAW_FOUR"))) #t]
           [(struct Card (_ (? (curry equal? (Card-color last-discard))) _)) #t]
           [(struct Card (_ _ (? (curry equal? (Card-value last-discard))))) #t]
           [_ #f]))))

(define (playable-cards state)
  (if (my-turn? state)
      (filter (curry playable-card? state) (Player-hand (my-player state)))
      empty))

(define (actions state)
  (and (my-turn? state)
       (append
        (playable-cards state)
        (list 'draw))))

(define (hand-by-color hand)
  (for/hash ([color '("RED" "GREEN" "BLUE" "YELLOW")])
    (values color (filter (λ (c) (equal? color (Card-color c))) hand))))

(define (select-wildcard-color state)
  (let ([h (hash->list (hand-by-color (my-hand state)))])
    (if (empty? h)
        "BLUE"
        (car (argmax (match-lambda [(cons color cards) (length cards)]) h)))))

(define (select-action state)
  (define available (actions state))
  (define-values (last-resort colored)
    (partition (match-lambda
                 [(struct* Card ([value (or "WILD" "WILD_DRAW_FOUR")])) #t]
                 [_ #f])
               (filter Card? available)))
  (cond
    [(not (empty? colored))
     (or
      (findf (λ (c) (member (Card-value c) '("DRAW_TWO" "SKIP" "REVERSE"))) colored)
      (car colored))]
    [(not (empty? last-resort)) (struct-copy Card (car last-resort)
                                             [color (select-wildcard-color state)])]
    [else (car available)]))

(define (play-turn g [state (get-state g)])
  (and (my-turn? state)
       (match (select-action state)
         ['draw (draw-card g)]
         [(struct* Card ([id id] [color color])) (play-card g id color)])))

(define (bot-loop game-descriptor start-game-at-capacity)
  (display "." (current-error-port))
  (flush-output (current-error-port))
  (define st (get-state game-descriptor))
  (when st
    (cond
      [(and (not (GameState-active st)) start-game-at-capacity (>= (length (GameState-players st)) start-game-at-capacity))
       (start-game game-descriptor)]
      [(my-turn? st)
       (play-turn game-descriptor st)]))
  (sleep .5)
  (bot-loop game-descriptor start-game-at-capacity))

(module+ main
  (define *bot-name* (make-parameter "racket"))
  (define *game-id* (make-parameter #f))
  (define *game-name* (make-parameter #f))
  (define *game-descriptor* (make-parameter #f))
  (define *new-game* (make-parameter #f))

  (command-line
   #:once-each
   [("-n" "--name") n "The bot's name"
                    (*bot-name* n)]
   [("-l" "--list") "List open games"
                    (displayln "ID                                               Name       Players Created")
                    (for ([g (open-games)])
                      (match-define (struct OpenGame [id name created players]) g)
                      (printf "~a ~a ~a ~a\n"
                              (~a id #:width 48)
                              (~a name #:width 10)
                              (~a players #:width 7 #:align 'right)
                              (~t created "YYYY-MM-dd HH:mm:ss")))
                    (exit 0)]
   #:once-any
   [("--game-id") i "Join by game ID"
                  (*game-id* i)]
   [("--game-name") n "Join by game Name"
                    (*game-name* n)]
   [("--game-descriptor") d "Resume an existing game"
                          (match-define (list game-id player-id) (string-split d ":"))
                          (*game-descriptor* (GameDescriptor game-id player-id))]
   [("--new-game") num-players "Start a new game, and start when num-players join"
                   (define n (string->number num-players))
                   (unless n
                     (fprintf (current-error-port) "Could not parse --new-game ~a\n" num-players))
                   (*new-game* n)]
   #:args ()
   (cond
     [(*game-name*)
      (define game (findf (λ (g) (equal? (*game-name*) (OpenGame-name g))) (open-games)))
      (unless game
        (fprintf (current-error-port) "Could not find game \"~a\"\n")
        (exit 1))
      (*game-id* (OpenGame-id game))]
     [(*game-descriptor*)
      (*game-id* (GameDescriptor-game-id (*game-descriptor*)))]
     [(*new-game*)
      (*game-descriptor* (new-game (*bot-name*)))
      (*game-id* (GameDescriptor-game-id (*game-descriptor*)))])
   (unless (*game-id*)
     (fprintf (current-error-port) "Please specify either --game-id, --game-name, --game-descriptor, --new-game.\n")
     (exit 1))
   (*game-descriptor*
    (or (*game-descriptor*)
        (join-game (*game-id*) (*bot-name*))))
   (unless (*game-descriptor*)
     (fprintf (current-error-port) "Could not join game with ID ~a\n" (*game-id*))
     (exit 1))
   (printf "Game name: ~a Game ID: ~a\n"
           (GameState-name (get-state (*game-descriptor*)))
           (GameDescriptor-game-id (*game-descriptor*)))
   (printf "To resume session, use --game-descriptor ~a:~a\n"
           (GameDescriptor-game-id (*game-descriptor*))
           (GameDescriptor-player-id (*game-descriptor*)))
   (printf "Open this URL to watch the game: ~a\n" (GameDescriptor->url (*game-descriptor*)))
   (bot-loop (*game-descriptor*) (*new-game*))))
