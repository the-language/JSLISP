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
(require "common.rkt")
(define ++
  (case-lambda
    [() ""]
    [(x . xs) (if (list? x)
                  (apply ++ (cons (apply ++ x) xs))
                  (string-append x (apply ++ xs)))]))

(define (EVAL x)
  (match x
    [(? symbol? x) (id x)]
    [(? number? x) (number->string x)]
    [(? string? x) (format "~s" x)]
    [#t "true"]
    [#f "false"]
    [`(define ,i) (++ "local " (id i) "\n")]
    [`(define ,i ,v) (++ "local " (id i) "=" (EVAL v) "\n")]
    [`(set! ,x ,v) (++ (EVAL x) "=" (EVAL v) "\n")]
    [`(lambda (,a ...) ,s ...)
     (++ "(function(" (add-between a ",") ")"
                                (map EVAL s)
                                "end)")]
    [`(return ,x) (++ "return" (EVAL x))]
    [`(! ,@v)
     (++ "{" (add-between
      (map (match-lambda [`[,i ,v] (++ (id i) "=" (EVAL v))])
          v)
      ",")
         "}")]
    [`(ref ,x ,k) (++ (EVAL x) "[" (EVAL k) "]")]
    [`(@ ,x ,i) (++ (EVAL x) "." (id i))]
    [`(if/begin ,b [,@t] [,@f])
     (++ "if " (EVAL b) " then\n"
         (map EVAL t)
         "else\n"
         (map EVAL f)
         "end\n")]
    [`(begin ,@c) (EVAL `((lambda () ,@c)))]
    [`(vector ,@x) (++ "{" (add-between (map EVAL x) ",") "}")]
    [`(apply ,f ,xs) (++ (EVAL f) "(unpack(" (EVAL xs) "))")]
    [`(+ ,@x) (++ "(" (add-between (map EVAL x) "+") ")")]
    [`(- ,@x) (++ "(" (add-between (map EVAL x) "-") ")")]
    [`(* ,@x) (++ "(" (add-between (map EVAL x) "*") ")")]
    [`(/ ,@x) (++ "(" (add-between (map EVAL x) "/") ")")]
    [`(< ,@x) (++ "(" (add-between (map EVAL x) "<") ")")]
    [`(> ,@x) (++ "(" (add-between (map EVAL x) ">") ")")]
    [`(= ,@x) (++ "(" (add-between (map EVAL x) "==") ")")]
    [`(<= ,@x) (++ "(" (add-between (map EVAL x) "<=") ")")]
    [`(>= ,@x) (++ "(" (add-between (map EVAL x) ">=") ")")]
    [`(and ,@x) (++ "(" (add-between (map EVAL x) " and ") ")")]
    [`(or ,@x) (++ "(" (add-between (map EVAL x) " or ") ")")]
    [`(not ,x) (++ "(not " (EVAL x) ")")]
    [`(eq? ,x ,y) (++ "(" (EVAL x) "==" (EVAL y) ")")]
    [`(,f ,@x) (++ (EVAL f) "(" (add-between (map EVAL x) ",") ")")]))
