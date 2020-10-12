#|

It's funny... I hate how weird and nasty Racket is to package and deploy.  But
little sessions like this help remind me why I like it so much.  It's very
expressive and has clean simple constructs to build up APIs.  If I wish to
refactor this to be even less typing, I can easily use a macro to wrap up most
of the (get ...) repetition.  Blissful.  And no weird syntax, it's just
lists. --- winny


This is just an "API client" for https://runogame.com/ .  It is quick and
dirty.  To come:

1. A higher level client to streamline controlling the game,
2. and a bot to play the game.


There are some things to fix up in this file:

TODO: better error handling

|#

#lang racket

(provide (all-defined-out))

(require html-parsing
         gregor
         json
         request
         sxml
         net/uri-codec)

(struct GameDescriptor (game-id player-id) #:transparent)
(struct Card (id color value) #:transparent)
(struct Message (type data) #:transparent)
(struct Player (
                id name admin active
                hand hand-size draw-required points
                round-won game-winner ux-id) #:transparent)
(struct GameState (
                   id name active
                   draw-pile-size discard-pile-size last-discard reverse point-to-win
                   messages created-at started-at ended-at
                   players max-players min-players) #:transparent)
(struct OpenGame (id name created players) #:transparent)

(define (GameDescriptor->url game-descriptor)
  (format "https://runogame.com/play/~a/~a" (GameDescriptor-game-id game-descriptor) (GameDescriptor-player-id game-descriptor)))

(define (coerce-json-null ht default-value [special-case (hash)])
  (for/hasheq ([(k v) (in-hash ht)])
    (values k
            (if (eq? (json-null) v)
                (if (hash-has-key? special-case k)
                    (hash-ref special-case k)
                    default-value)
                v))))

(define (jsexpr->Card jsexpr)
  (match jsexpr
    [(hash-table ('color color)
                 ('id id)
                 ('value value))
     (Card id (if (eq? color (json-null)) #f color) value)]))

(define (jsexpr->Message jsexpr)
  (match jsexpr
    [(hash-table ('data data)
                 ('type type))
     (Message type data)]))

(define (jsexpr->Player jsexpr)
  (match (coerce-json-null jsexpr #f (hash 'hand empty))
    [(hash-table ('active active)
                 ('admin admin)
                 ('hand_size hand-size)
                 ('game_winner game-winner)
                 ('id id)
                 ('name name)
                 ('points points)
                 ('rounds_won rounds-won)
                 ('ux_id ux-id))
     (Player id name admin active
             (map jsexpr->Card (hash-ref jsexpr 'hand empty))
             hand-size (hash-ref jsexpr 'draw_required #f) points
             rounds-won game-winner ux-id)]))



;; Turns out request is a bit basic.  And the web service is a bit buggy,
;; timing out on responses only when the PaaS times them out.  So instead let's
;; just wrap each API call in a timeout.
(define-syntax-rule (define/timeout (binding args ...)
                      body ...)
  (define (binding args ...)
    (define ch (make-channel))
    (define th
      (thread
       (thunk
        (channel-put ch
                     (let ()
                       body ...)))))
    (begin0
        (sync/timeout 5 ch)
      (kill-thread th))))

(define (game-descriptor->parameters game-descriptor)
  (alist->form-urlencoded `((game_id . ,(GameDescriptor-game-id game-descriptor))
                            (player_id . ,(GameDescriptor-player-id game-descriptor)))))

(define (redirect-response->game-info response)
  (match (hash-ref (http-response-headers response) "Location")
    [(regexp #rx"/play/([A-Za-z0-9]+)/([A-Za-z0-9]+)" (list _ game-id player-id))
     (GameDescriptor game-id player-id)]))

(define runo-requester
  (make-domain-requester "runogame.com" (make-https-requester http-requester)))

(define/timeout (new-game player-name)
  (redirect-response->game-info (get runo-requester (format "/newgame?~a" (alist->form-urlencoded `((player_name . ,player-name)))))))

(define/timeout (join-game game-id player-name)
  (redirect-response->game-info (get runo-requester (format "/join?~a" (alist->form-urlencoded `((name . ,player-name)
                                                                                                 (game_id . ,game-id)))))))

(define/timeout (start-game game-descriptor)
  (match-define (struct http-response [code headers body])
    (get runo-requester (format "/start?~a" (game-descriptor->parameters game-descriptor))))
  (hash-ref (string->jsexpr body) 'result))

(define/timeout (play-card game-descriptor card-id [selected-color ""])
  (match-define (struct http-response [code headers body])
    (get runo-requester (format "/playcard?~a&~a"
                                (game-descriptor->parameters game-descriptor)
                                (alist->form-urlencoded `((card_id . ,card-id)
                                                          (selected_color . ,selected-color))))))
  (hash-ref (string->jsexpr body) 'result))

(define/timeout (draw-card game-descriptor)
  (match-define (struct http-response [code headers body])
    (get runo-requester (format "/draw?~a" (game-descriptor->parameters game-descriptor))))
  (hash-ref (string->jsexpr body) 'result))

(define/timeout (open-games)
  (match-define (struct http-response [code headers body])
    (get runo-requester "/"))
  (for/list ([tr ((sxpath '(// (table (@ id (equal? "gamelist"))) tbody tr))
                  (html->xexp body))])
    (match
        (for/hash ([td ((sxpath '(td)) tr)]
                   [column-definition `((game-id (input @ value *text*) ,identity)
                                        (game-name (label *text*) ,identity)
                                        (players (*text*) ,string->number)
                                        (created (*text*) ,iso8601->datetime))])
          (match-define (list name location mapping) column-definition)
          (values name
                  (mapping (car ((sxpath (list* '// location)) td)))))
      [(hash-table ('game-id id)
                   ('game-name name)
                   ('players players)
                   ('created created))
       (OpenGame id name created players)])))

(define/timeout (get-state game-descriptor)
  (match-define (struct http-response [code headers body])
    (get runo-requester (format "/getstate?~a" (game-descriptor->parameters game-descriptor))))
  (match (coerce-json-null (string->jsexpr body) #f)
    [(hash-table ['active active]
                 ['created_at created-at]
                 ['discard_pile_size discard-pile-size]
                 ['id id]
                 ['ended_at ended-at]
                 ['draw_pile_size draw-pile-size]
                 ['last_discard last-discard]
                 ['messages messages]
                 ['min_players min-players]
                 ['max_players max-players]
                 ['name name]
                 ['players players]
                 ['points_to_win points-to-win]
                 ['reverse reverse]
                 ['started_at started-at])
     (GameState id name active
                draw-pile-size discard-pile-size
                (if (hash? last-discard) (jsexpr->Card last-discard) #f) reverse points-to-win
                (map jsexpr->Message messages)
                (and created-at (iso8601->datetime created-at))
                (and started-at (iso8601->datetime started-at))
                (and ended-at (iso8601->datetime ended-at))
                (map jsexpr->Player players) max-players min-players)]))

(define/timeout (quit-game game-descriptor)
  (match-define (struct http-response [code headers body])
    (get runo-requester (format "/quit/~a/~a"
                                (GameDescriptor-game-id game-descriptor)
                                (GameDescriptor-player-id game-descriptor))))
  ;; No way to tell if this was successful without doing extra requests.
  (void))
