#lang racket
;;  Copyright (C) 2017  Zaoqi

;;  This program is free software: you can redistribute it and/or modify
;;  it under the terms of the GNU Affero General Public License as published
;;  by the Free Software Foundation, either version 3 of the License, or
;;  (at your option) any later version.

;;  This program is distributed in the hope that it will be useful,
;;  but WITHOUT ANY WARRANTY; without even the implied warranty of
;;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;  GNU Affero General Public License for more details.

;;  You should have received a copy of the GNU Affero General Public License
;;  along with this program.  If not, see <http://www.gnu.org/licenses/>.
(provide map)
(define chars-num (set #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9 #\0))
(define chars-abc
  (set #\q #\w #\e #\r #\t #\y #\u #\i #\o #\p #\a #\s #\d #\f #\g #\h #\j #\k #\l #\z #\x #\c #\v #\b #\n #\m
       #\Q #\W #\E #\R #\T #\Y #\U #\I #\O #\P #\A #\S #\D #\F #\G #\H #\J #\K #\L #\Z #\X #\C #\V #\B #\N #\M))
(define chars (set-union chars-num chars-abc))
(define chars_ (set-add chars #\_))
(define (id sym)
  (let* ([s (symbol->string sym)] [xs (string->list s)])
    (if (and (andmap (λ (c) (set-member? chars_ c)) xs) (set-member? chars (car xs)))
        s
        (apply string-append
               (append
                (list "z")
                (map (λ (c)
                       (if (set-member? chars c)
                           (string c)
                           (string-append "_" (number->string (char->integer c)) "C"))) xs)
                (list "Z"))))))
(define (++ . xs)
  (cond
    [(null? xs) ""]
    [(list? (car xs)) (apply ++ (append (car xs) (cdr xs)))]
    [else (string-append (car xs) (apply ++ (cdr xs)))]))
(define call/gensym
  (let ([c 0])
    (λ (f)
      (set! c (+ c 1))
      (f (string->symbol (string-append "g" (number->string c)))))))
(define gensym! gensym)
(define (float->string x) (number->string (exact->inexact x)))
(define map ((λ () (include "map.scm") map)))