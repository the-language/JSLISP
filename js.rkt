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
(define ++ string-append)
(define (*var x) (symbol->string x)) ; Bug
(define (*return x) (++ "return "x))
(define (*lambda args body)
  (++ "(function("(%*lam-arg args)"){"
      (%*lam body)
      "})"))
(define (%*lam xs)
  (if (null? (cdr xs))
      (++ (car xs)";")
      (++ (car xs)";"(%*lam (cdr xs)))))
(define (%*lam-arg args)
  (cond
    [(symbol? args) (++ (*var args)"...")]
    [(null? args) ""]
    [(null? (cdr args)) (*var (car args))]
    [else (++ (*var (car args))","(%*lam-arg (cdr args)))]))
(define (*apply f xs) (++ f"("(foldr string-append "" (add-between xs ","))))
(define (**js-typeof x) (++ "(typeof "x")"))
(define (**procedure? x) (++ "(typeof "x"=='function')"))
(define (**string? x) (++ "(typeof "x"=='string')"))
(define (**object/vector? x) (++ "(typeof "x"=='object')"))
(define (**is-a? x t) (++ "("x" instanceof "t")"))
(define (**vector? x) (**is-a? x "Array"))
