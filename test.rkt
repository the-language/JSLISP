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
(require rackunit)
(require "map.rkt")
(define-syntax-rule (test f [src dist] ...)
  (begin
    (check-equal? (f (quote src)) dist) ...))
(test
 map
 [((λ xs xs)) '((λ xs xs))]
 [((begin (define (x) (x))) (x)) '((define x (λ () (x))) (x))]
 [((λ () (if #t 0 1))) '((λ () (define g1) (if/void #t ((set! g1 0)) ((set! g1 1))) g1))]
 )