#lang info

(define name "Runomatic")

(define deps
  (list "base"
        "html-parsing"
        "gregor"
        "request"
        "sxml"))

(define racket-launcher-libraries '("bot.rkt"))
(define racket-launcher-names '("runomatic"))
