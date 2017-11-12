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
(provide EVAL)

(define (EVAL x)
  (match x
    [(? symbol? x) (id x)]
    [(? number? x) (number->string x)]
    [(? string? x) (format "~s" x)]
    [#t "true"]
    [#f "false"]
    [`(define ,i) (++ "var " (id i) "\n")]
    [`(define ,i ,v) (++ "var " (id i) "=" (EVAL v) "\n")]
    [`(set! ,x ,v) (++ (EVAL x) "=" (EVAL v) "\n")]
    [`(lambda (,a ...) ,s ...)
     (++ "(function(" (add-between a ",") "){"
         (map EVAL s)
         "})")]
    [`(return ,x) (++ "return " (EVAL x) "\n")]
    [`(! ,@v)
     (++ "({" (add-between
              (map (match-lambda [`[,i ,v] (++ (id i) ":" (EVAL v))])
                   v)
              ",")
         "})")]
    [`(ref ,x ,k) (++ (EVAL x) "[" (EVAL k) "]")]
    [`(@ ,x ,i) (++ (EVAL x) "." (id i))]
    [`(if/begin ,b [,@t] [,@f])
     (++ "if(" (EVAL b) "){\n"
         (map EVAL t)
         "}else{\n"
         (map EVAL f)
         "}\n")]
    [`(block ,@c) (EVAL `((lambda () ,@c)))]
    [`(if ,b ,x ,y) (++ "(" (EVAL b) "?" (EVAL x) ":" (EVAL y) ")")]
    [`(vector ,@x) (++ "[" (add-between (cons "null" (map EVAL x)) ",") "]")]
    [`(apply ,f ,xs) (++ (EVAL f) ".apply(null," (EVAL xs) ")")] ;BUG this错误
    [`(+ ,@x) (++ "(" (add-between (map EVAL x) "+") ")")]
    [`(- ,@x) (++ "(" (add-between (map EVAL x) "-") ")")]
    [`(* ,@x) (++ "(" (add-between (map EVAL x) "*") ")")]
    [`(/ ,@x) (++ "(" (add-between (map EVAL x) "/") ")")]
    [`(< ,@x) (++ "(" (add-between (map EVAL x) "<") ")")]
    [`(> ,@x) (++ "(" (add-between (map EVAL x) ">") ")")]
    [`(= ,@x) (++ "(" (add-between (map EVAL x) "==") ")")]
    [`(<= ,@x) (++ "(" (add-between (map EVAL x) "<=") ")")]
    [`(>= ,@x) (++ "(" (add-between (map EVAL x) ">=") ")")]
    [`(and ,@x) (++ "(" (add-between (map EVAL x) "&&") ")")]
    [`(or ,@x) (++ "(" (add-between (map EVAL x) "||") ")")]
    [`(not ,x) (++ "(!" (EVAL x) ")")]
    [`(eq? ,x ,y) (++ "(" (EVAL x) "==" (EVAL y) ")")]
    [`(noteq? ,x ,y) (++ "(" (EVAL x) "!=" (EVAL y) ")")]
    [`(vector-for ,i ,x ,xs ,@c)
     (let ([i (id i)] [x (id x)])
       (++ "var e_=" (EVAL xs) "\n" ; BUG 可能对GC支持不好
           "for(var i_=1;i_<e_.length;i_++){\n"
           "var " i "=i_\n"
           "var " x "=e_[i_]\n"
           (map EVAL c)
           "}\n"))]
    [`(for ,i ,x ,t ,@c)
     (let ([i (id i)] [x (id x)])
       (++ "var e_=" (EVAL t) "\n" ; BUG 可能对GC支持不好
           "for(var " i " in e_){\n"
           "var " x "=e_[" i "]\n"
           "if(" x "!=null){\n"
           (map EVAL c)
           "}\n"
           "}\n"))]
    [`(number? ,x) (++ "(typeof " (EVAL x) "=='number')")]
    [`(boolean? ,x) (++ "(typeof " (EVAL x) "=='boolean')")]
    [`(procedure? ,x) (++ "(typeof " (EVAL x) "=='function')")]
    [`(!/vectror? ,x) (++ "(typeof " (EVAL x) "=='object')")]
    [`(,f ,@x) (++ (EVAL f) "(" (add-between (map EVAL x) ",") ")")]))