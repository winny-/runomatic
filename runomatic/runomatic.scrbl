#lang scribble/manual

@(require (for-label runomatic racket json) scribble/core)

@; Via https://github.com/greghendershott/frog/blob/master/frog/frog.scrbl#L25
@(define (grey . contents)
   (elem #:style (style #f (list (color-property (list 127 127 127))))
         contents))
@(define (pre #:title [title #f] . contents)
   (let* ([title    (cond [title (list (grey (italic (tt title))) "\n")]
                          [else  (list)])]
          [contents (append title contents)]
          [contents (apply verbatim contents)])
     (nested #:style 'code-inset contents)))
@title{Runomatic --- Runo Client and Bot}
@author[@hyperlink["https://winny.tech/"]{Winston "Winny" Weinert}]

@defmodule[runomatic]

@section{About}

While hanging out with some friends on a Saturday evening, we wanted to play
Uno, and found a @hyperlink["https://runogame.com/"]{FOSS Uno webapp}
(@hyperlink["https://github.com/richgieg/runo"]{source code}), we wanted to add
additional players to make the game more interesting.  Plus it would be fun to
play bots against each other. As a result I wrote this quick ‘n dirty “API
Client” to the Runo API, and a simple rule-based bot to play the game.

@image{../demo.gif}

The source code for this project
@hyperlink["https://github.com/winny-/runomatic"]{is hosted on GitHub}.  This
project is licensed
@hyperlink["https://github.com/winny-/runomatic/blob/master/LICENSE"]{is
licensed under the terms of Unlicense.}

@section{Bot}

The bot is available as an API and as a Command Line application.

@subsection{Command Line Bot}

Note: the dots outputted by the CLI's bot loop correspond to a @italic{game tick}.

Usage:

@pre|{
$ runomatic --help
runomatic [ <option> ... ]
 where <option> is one of
  -n <n>, --name <n> : The bot's name
  -l, --list : List open games
/ --game-id <i> : Join by game ID
| --game-name <n> : Join by game Name
| --game-descriptor <d> : Resume an existing game
\ --new-game <num-players> : Start a new game, and start when num-players join
  --help, -h : Show this help
  -- : Do not treat any remaining argument as a switch (at this level)
 /|\ Brackets indicate mutually exclusive options.
 Multiple single-letter switches can be combined after one `-'; for
  example: `-h-' is the same as `-h --'
}|

List open games:

@pre|{
$ runomatic -l
ID                                               Name       Players Created
wspY17d6GkLGPVXU2dmfbDO7q4bZubEIjdxOBPb9bExfAvLx Game30517        1 2020-10-19 01:16:
44                                                                                  
}|

Start a new game with three players:

@pre|{
$ runomatic --name billybob --new-game 3
Game name: Game89758 Game ID: cTNVmUBBfPU3s1NyFLalO5JG4azE97Awabln25PCYvFG6TDL
To resume session, use --game-descriptor cTNVmUBBfPU3s1NyFLalO5JG4azE97Awabln25PCYvFG
6TDL:uSkcPgYN8oWKtnaAJgbAYF4xbKq5QO2t17jQWYpyzhup72cZ                               
Open this URL to watch the game: https://runogame.com/play/cTNVmUBBfPU3s1NyFLalO5JG4a
zE97Awabln25PCYvFG6TDL/uSkcPgYN8oWKtnaAJgbAYF4xbKq5QO2t17jQWYpyzhup72cZ             
....
}|

Join an existing game:

@pre|{
$ runomatic --game-id cTNVmUBBfPU3s1NyFLalO5JG4azE97Awabln25PCYv
FG6TDL                                                                              
Game name: Game89758 Game ID: cTNVmUBBfPU3s1NyFLalO5JG4azE97Awabln25PCYvFG6TDL
To resume session, use --game-descriptor cTNVmUBBfPU3s1NyFLalO5JG4azE97Awabln25PCYvFG
6TDL:ERvXVQ6gXoE2ZXlTz00vtymLlEs5NC50Wlkzu5zm6niKrbW6                               
Open this URL to watch the game: https://runogame.com/play/cTNVmUBBfPU3s1NyFLalO5JG4a
zE97Awabln25PCYvFG6TDL/ERvXVQ6gXoE2ZXlTz00vtymLlEs5NC50Wlkzu5zm6niKrbW6             
....
}|

Resume an existing session:

@pre|{
$ runomatic --game-descriptor cTNVmUBBfPU3s1NyFLalO5JG4azE97Awab
ln25PCYvFG6TDL:ERvXVQ6gXoE2ZXlTz00vtymLlEs5NC50Wlkzu5zm6niKrbW6                      
                                                                                    
Game name: Game89758 Game ID: cTNVmUBBfPU3s1NyFLalO5JG4azE97Awabln25PCYvFG6TDL
To resume session, use --game-descriptor cTNVmUBBfPU3s1NyFLalO5JG4azE97Awabln25PCYvFG
6TDL:ERvXVQ6gXoE2ZXlTz00vtymLlEs5NC50Wlkzu5zm6niKrbW6                               
Open this URL to watch the game: https://runogame.com/play/cTNVmUBBfPU3s1NyFLalO5JG4a
zE97Awabln25PCYvFG6TDL/ERvXVQ6gXoE2ZXlTz00vtymLlEs5NC50Wlkzu5zm6niKrbW6             
....
}|

@subsection{Bot API}

@defmodule[runomatic/bot]

@defproc[(my-player [state GameState?]) Player?]{
Get your player.
}

@defproc[(my-hand [state GameState?]) (listof Card?)]{
Get your player's hand.
}

@defproc[(admin-player [state GameState?]) Player?]{
Get the player that created the game.
}

@defproc[(my-turn? [state GameState?]) boolean?]{
Is it my player's turn?
}

@defproc[(playable-card? [state GameState?] [card Card?]) boolean?]{
Can the card be played?
}

@defproc[(playable-cards [state GameState?]) (listof Card?)]{
Cards that are immediately playable.
}

@defproc[(actions [state GameState?]) (listof (or/c Card? 'draw))]{
Actions immediately available to your player.
}

@defproc[(hand-by-color [hand (listof Card?)]) (hash? string? (listof Card?))]{
The hand organized by color.
}

@defproc[(select-wildcard-color [state GameState?]) string?]{
Select an ideal color for the player to play a wildcard as.
}

@defproc[(select-action [state GameState?]) (or/c Card? 'draw)]{
Select an action to play.
}

@defproc[(play-turn [g GameDescriptor?] [state GameState? (get-state g)]) boolean?]{
Play the turn.  Result indicates if the turn was played.
}

@defproc[(bot-loop [game-descriptor GameDescriptor?] [start-game-at-capacity exact-nonnegative-integer?]) void?]{
Run the bot.  If the game hasn't been started, the bot will attempt to play
the game when at least @racketid[start-game-at-capacity] players are have
joined (including the bot itself).
}

@section{API Client}

@defmodule[runomatic/client]

@defproc[(new-game [player-name string?]) GameDescriptor?]{
Create a new game.
}

@defproc[(join-game [game-id string?] [player-name string?]) GameDescriptor?]{
Join an existing game.
}

@defproc[(start-game [game-descriptor GameDescriptor?]) boolean?]{
Start the game.  Only works if the player referenced by the
@racketid[game-descriptor] is an admin.
}

@defproc[(play-card [game-descriptor GameDescriptor?]
                    [card-id string?]
                    [selected-color string? ""]) boolean?]{
Play a card.
}

@defproc[(draw-card [game-descriptor GameDescriptor?]) boolean?]{
Draw a card.
}

@defproc[(open-games) (listof OpenGame?)]{
Get the open games.
}

@defproc[(get-state [game-descriptor GameDescriptor?]) GameState?]{
Get the game state.
}

@defproc[(quit-game [game-descriptor GameDescriptor?]) void?]{
Quit the game.
}

@section{Types}

@defmodule[runomatic/types]

@subsection{Structure Definitions}

@defstruct[GameDescriptor
    ([game-id string?]
     [player-id string?])
    #:transparent]{
A token that represents a player in a particular game session.  The
@racketid[game-id] and @racketid[player-id] are both chosen by the Runo backend
server.
}

@defstruct[Card
    ([id string?]
     [color (or/c string? #f)]
     [value string?])
    #:transparent]{
A card as given back from the server.  Wildcards will have a @racketid[color]
of @racket[#f].
}

@defstruct[Message
    ([type string?]
     [data string?])
    #:transparent]{
A game related message sent from the server, such as the reversal of play order
or when a player wins the round.
}

@defstruct[Player
    ([id string?]
     [name string?]
     [admin boolean?]
     [active boolean?]
     [hand (or/c (listof Card?) #f)]
     [hand-size exact-nonnegative-integer?]
     [draw-required boolean?]
     [points exact-nonnegative-integer?]
     [rounds-won exact-nonnegative-integer?]
     [game-winner boolean?]
     [ux-id string?])
    #:transparent]{
A player as represented by the server.  The fields are:
@itemlist[
    @item{@racketid[id] corresponds to a @racketid[GameDescriptor-player-id].}
    @item{@racketid[name] is the human-readable name for the player.}
    @item{@racketid[admin] is set to @racket[#t] when the player can start a
          game.}
    @item{@racketid[active] is @racket[#t] when the player can play a card.}
    @item{@racketid[hand] is a list of the cards the current player has in
          their hand.  Other players @racketid[hand] is @racket[#f].}
    @item{@racketid[hand-size] is the number of cards in this player's
          hand.  This field is always available to other players.}
    @item{@racketid[draw-required] }
    @item{@racketid[points] is the total number of points this player has
          earned.}
    @item{@racketid[rounds-won] is the number of rounds won by this player.}
    @item{@racketid[game-winner] is set to @racket[#t] when the player won the
          entire game.}
    @item{@racketid[ux-id] is unused by this Racket package, but is available
          for consumers of the API client.}]}

@defstruct[GameState
    ([id string?]
     [name string?]
     [active boolean?]
     [draw-pile-size exact-nonnegative-integer?]
     [discard-pile-size exact-nonnegative-integer?]
     [last-discard (or/c Card? #f)]
     [reverse boolean?]
     [point-to-win exact-nonnegative-integer?]
     [messages (listof Message?)]
     [created-at date?]
     [started-at (or/c date? #f)]
     [ended-at (or/c date? #f)]
     [players (listof Player?)]
     [max-players exact-nonnegative-integer?]
     [min-players exact-nonnegative-integer?])
    #:transparent]{
A game state represented by the server.  The fields are:
@itemlist[
    @item{@racketid[id] corresponds to a @racketid[GameDescriptor-game-id]}
    @item{@racketid[name] is the human readable name of the game.}
    @item{@racketid[active] is @racket[#t] when the game is in session.}
    @item{@racketid[draw-pile-size] is the size of the draw pile.}
    @item{@racketid[discard-pile-size] is the size of the discard pile.}
    @item{@racketid[last-discard] is the last played card.}
    @item{@racketid[reverse] is @racket[#t] when the turn order is reversed.}
    @item{@racketid[point-to-win] is the number of points necessary to win the
          entire game.}
    @item{@racketid[messages] is the game messages such as when a player wins a
          round.}
    @item{@racketid[created-at] is the time the game was created.}
    @item{@racketid[started-at] is the time the game was started.  If the game
          has yet to be started, it is @racket[#f].}
    @item{@racketid[ended-at] is the time the game finished.  If the game has
          yet to finish, it is @racket[#f].}
    @item{@racketid[players] is a list of the game players, including your own
          player.}
    @item{@racketid[max-players] is the maximum number of players that can join
          this game.}
    @item{@racketid[min-players] is the minimum number of players necessary to
          start the game.}
]}

@defstruct[OpenGame
    ([id string?]
     [name string?]
     [created date?]
     [players exact-nonnegative-integer?])
    #:transparent]{
An open game available to join.
}

@subsection{Converting from JSON}

@defproc[(jsexpr->GameState [jsexpr jsexpr?]) GameState?]
@defproc[(jsexpr->Card [jsexpr jsexpr?]) Card?]
@defproc[(jsexpr->Player [jsexpr jsexpr?]) Player?]
@defproc[(jsexpr->Message [jsexpr jsexpr?]) Message?]
