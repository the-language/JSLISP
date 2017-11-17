#lang racket
;;  Map:Map
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
(provide (all-defined-out))

(define (id x)
  (apply string-append
         (map
          (λ (x) (if (or (char-alphabetic? x) (char-numeric? x))
                     (string x)
                     (string-append
                      "_"
                      (number->string (char->integer x)))))
          (string->list (symbol->string x)))))
(define ++
  (case-lambda
    [() ""]
    [(x . xs) (if (list? x)
                  (apply ++ (cons (apply ++ x) xs))
                  (string-append x (apply ++ xs)))]))
(define genvar!
  (let ([c 0])
    (λ ()
      (set! c (+ c 1))
      (string-append "T" (number->string c)))))
(define (EVALxs EVAL xs f)
  (if (null? xs)
      (f '())
      (EVAL (car xs) (λ (a) (EVALxs EVAL (cdr xs) (λ (d)
                                                    (f (cons a d))))))))
(define (+-*/ EVAL o xs f)
  (EVALxs EVAL
          xs
          (λ (xss)
            (f (++ "(" (add-between xss o) ")")))))
(define (<>= EVAL o xs f)
  (match xs
    [`(,x ,y)
     (EVAL x (λ (xx) (EVAL y (λ (yy)
                              (f (++ "(" xx o yy ")"))))))]))
