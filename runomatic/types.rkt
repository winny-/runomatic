#lang racket

(require gregor
         json)

(provide
 ;; Structures with JSON parsing logic.
 (struct-out Card)
 jsexpr->Card
 (struct-out Message)
 jsexpr->Message
 (struct-out Player)
 jsexpr->Player
 (struct-out GameState)
 jsexpr->GameState
 ;; Structures that aren't parsed from JSON.
 (struct-out OpenGame)
 (struct-out GameDescriptor)
 ;; Helper procedures.
 GameDescriptor->url)

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

(define (jsexpr->GameState jsexpr)
  (match (coerce-json-null jsexpr #f)
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
