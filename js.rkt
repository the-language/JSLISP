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
(provide js)
(define (js x) (EVAL x ig))

(define undefined "undefined")
(define (ig x)
  (if (eq? x undefined)
      ""
      (++ x "\n")))
(define (with-genvar! f)
  (let ([v (genvar!)])
    (++ "var " v "\n"
        (f v))))
(define (store! x f)
  (let ([v (genvar!)])
    (++ "var " v "=" x "\n"
        (f v))))

(define (EVAL x f)
  (match x
    [(? symbol? x) (f (id x))]
    [(? number? x) (f (number->string x))]
    [(? string? x) (f (format "~s" x))]
    [#t (f "true")]
    [#f (f "false")]
    [`(define ,i) (++ "var " (id i) "\n" (f undefined))]
    [`(define ,(cons k xs) ,@v) (EVAL `(define ,k (lambda ,xs ,@v)) f)]
    [`(define ,i ,v) (EVAL v (λ (vv)
                               (++ "var " (id i) "=" vv "\n" (f undefined))))]
    [`(set! ,x ,v) (EVAL x (λ (xx) (EVAL v (λ (vv)
                                             (++ xx "=" vv "\n" (f undefined))))))]
    [(cons 'λ x) (EVAL (cons 'lambda x) f)]
    [`(lambda (,a ...) ,@s)
     (f (++ "(function(" (add-between (map id a) ",") "){"
            (EVAL `(begin ,@s) (λ (x) (++ "return " x "\n")))
            "})"))]
    [`(lambda ,(list-rest a ... r) ,@s)
     (f (let ([r (id r)])
          (++ "(function(" (add-between (map id a) ",") "){\n"
              "var " r "=[]\n"
              "for(var i_=" (number->string (length a)) ";i_<arguments.length;i++){\n"
              r "[" r ".length]=arguments[i_]\n"
              "}\n"
              (EVAL `(begin ,@s) (λ (x) (++ "return " x "\n")))
              "})")))]
    [`(return ,x) (EVAL x (λ (xx)
                            (++ "return " x "\n")))]
    [`(! ,@v)
     (let loop ([xs '()] [rs v])
       (match rs
         ['()
          (f (++ "({"
                 (add-between (map (match-lambda [`(,i . ,v) (++ i ":" v)]) xs) ",")
                 "})"))]
         [`([,i ,v] . ,rs)
          (EVAL v (λ (vv)
                    (loop (cons (cons (id i) vv) xs) rs)))]))]
    [`(ref ,x ,k) (EVAL x (λ (xx) (EVAL k (λ (kk)
                                            (f (++ xx "[" kk "]"))))))]
    [`(vector-ref ,v ,k) (EVAL x (λ (xx) (EVAL k (λ (kk)
                                                   (f (++ xx "[" kk "]"))))))]
    [`(@ ,x ,@k) (EVAL x (λ (xx)
                          (f (add-between (cons xx (map id k)) "."))))]
    [`(if/begin ,b [,@t] [,@fa])
     (EVAL b (λ (bb)
               (++ "if(" bb "!==false){\n"
                   (EVAL `(begin ,@t) ig)
                   "}else{\n"
                   (EVAL `(begin ,@fa) ig)
                   "}\n"
                   (f undefined))))]
    [`(begin ,x) (EVAL x f)]
    [`(begin ,x ,@xs) (EVAL x (λ (xx)
                                (++
                                 (ig xx)
                                 (EVAL `(begin ,@xs) f))))]
    [`(if ,b ,x ,y)
     (EVAL b (λ (bb) (EVAL x (λ (xx) (EVAL y (λ (yy)
                                               (++ "(" bb "!==false?" xx ":" yy ")")))))))]
    [`(vector ,@xs) (EVALxs EVAL xs (λ (xss) (f (++ "[" (add-between xss ",") "]"))))]
    [`(vector-length ,v) (EVAL v (λ (vv)
                                   (++ vv ".length")))]
    [`(apply ,f ,xs) (EVAL f (λ (ff) (EVAL xs (λ (xss)
                                                (f (++ ff ".apply(null," xss ")"))))))] ;BUG 'this'不正确
    [`(+ ,@x) (+-*/ EVAL "+" x f)]
    [`(- ,@x) (+-*/ EVAL "-" x f)]
    [`(* ,@x) (+-*/ EVAL "*" x f)]
    [`(/ ,@x) (+-*/ EVAL "/" x f)]
    [`(< ,@x) (<>= EVAL "<" x f)]
    [`(> ,@x) (<>= EVAL ">" x f)]
    [`(= ,@x) (<>= EVAL "===" x f)]
    [`(<= ,@x) (<>= EVAL "<=" x f)]
    [`(>= ,@x) (<>= EVAL ">=" x f)]
    [`(and ,@x) (+-*/ EVAL "&&" x f)]
    [`(or ,@x) (+-*/ EVAL "||" x f)]
    [`(not ,x)
     (EVAL x (λ (xx) (f (++ "(! " xx ")"))))]
    [`(eq? ,x ,y) (EVAL `(= ,x ,y) f)]
    [`(noteq? ,x ,y) (EVAL x (λ (xx) (EVAL y (λ (yy) (f (++ "(" xx "!==" yy ")"))))))]
    [`(vector-for ,i ,x ,xs ,@c)
     (EVAL
      xs
      (λ (xss)
        (store!
         xss
         (λ (t)
           (let ([i (id i)] [x (id x)])
             (++ "for(var i_=0;i_<" t ".length;i_++){\n"
                 "var " i "=i_\n"
                 "var " x "=" t "[i_]\n"
                 (EVAL `(begin ,@c) ig)
                 "}\n"
                 (f undefined)))))))]
    [`(for ,i ,x ,t ,@c)
     (EVAL
      t
      (λ (tt)
        (store!
         tt
         (λ (t)
           (let ([i (id i)] [x (id x)])
             (++ "for(var " i " in " t "){\n"
                 "var " x "=" t "[" i "]\n"
                 "if(" x "!=null&&typeof " i "!=='number'){\n"
                 (EVAL `(begin ,@c) ig)
                 "}\n"
                 "}\n"
                 (f undefined)))))))]
    [`(number? ,x) (EVAL x (λ (xx) (++ "(typeof " xx "=='number')")))]
    [`(boolean? ,x) (EVAL x (λ (xx) (++ "(typeof " xx "=='boolean')")))]
    [`(procedure? ,x) (EVAL x (λ (xx) (++ "(typeof " xx "=='function')")))]
    [`(string? ,x) (EVAL x (λ (xx) (++ "(typeof " xx "=='string')")))]
    [`(!/vectror? ,x) (EVAL x (λ (xx) (++ "(typeof " xx "=='object')")))]
    [`(vector? ,x) (EVAL x (λ (xx) (++ "(" xx " instanceof Array")))]
    [`(host  ,_ ... [js ,v] ,_ ...) (f v)]
    [`(,k ,@x)
     (EVAL k (λ (kk) (EVALxs EVAL x (λ (xss)
                                      (f (++ kk "(" (add-between xss ",") ")"))))))]))
