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

(provide (all-defined-out)
         (all-from-out "types.rkt"))

(require html-parsing
         gregor
         json
         request
         sxml
         net/uri-codec
         "types.rkt")

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
  (jsexpr->GameState (string->jsexpr body)))

(define/timeout (quit-game game-descriptor)
  (match-define (struct http-response [code headers body])
    (get runo-requester (format "/quit/~a/~a"
                                (GameDescriptor-game-id game-descriptor)
                                (GameDescriptor-player-id game-descriptor))))
  ;; No way to tell if this was successful without doing extra requests.
  (void))
