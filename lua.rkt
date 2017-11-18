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
(provide lua)
(define (lua x) (EVAL x ig))

(define undefined "nil")
(define (ig x)
  (if (eq? x undefined)
      ""
      (++ "type(" x ")\n")))
(define (with-genvar! f)
  (let ([v (genvar!)])
    (++ "local " v "\n"
        (f v))))
(define (store! x f)
  (let ([v (genvar!)])
    (++ "local " v "=" x "\n"
        (f v))))

(define (EVAL x f)
  (match x
    [(? symbol? x) (f (id x))]
    [(? number? x) (f (number->string x))]
    [(? string? x) (f (format "~s" x))]
    [#t (f "true")]
    [#f (f "false")]
    [`(define ,i) (++ "local " (id i) "\n" (f undefined))]
    [`(define ,(cons k xs) ,@v) (EVAL `(define ,k (lambda ,xs ,@v)) f)]
    [`(define ,i ,v) (EVAL v (λ (vv)
                               (++ "local " (id i) "=" vv "\n" (f undefined))))]
    [`(set! ,x ,v) (EVAL x (λ (xx) (EVAL v (λ (vv)
                                             (++ xx "=" vv "\n" (f undefined))))))]
    [(cons 'λ x) (EVAL (cons 'lambda x) f)]
    [`(lambda (,a ...) ,@s)
     (f (++ "(function(" (add-between (map id a) ",") ")\n"
            (EVAL `(begin ,@s) (λ (x) (++ "return " x "\n")))
            "end)"))]
    [`(lambda ,(list-rest a ... r) ,@s)
     (f (++ "(function(" (add-between (append (map id a) (list "...")) ",") ")\n"
            "local " (id r) "={...}\n"
            (EVAL `(begin ,@s) (λ (x) (++ "return " x "\n")))
            "end)"))]
    [`(return ,x) (EVAL x (λ (xx)
                            (++ "return " x "\n")))]
    [`(! ,@v)
     (let loop ([xs '()] [rs v])
       (match rs
         ['()
          (f (++ "{"
                 (add-between (map (match-lambda [`(,i . ,v) (++ i "=" v)]) xs) ",")
                 "}"))]
         [`([,i ,v] . ,rs)
          (EVAL v (λ (vv)
                    (loop (cons (cons (id i) vv) xs) rs)))]))]
    [`(ref ,x ,k) (EVAL x (λ (xx) (EVAL k (λ (kk)
                                            (f (++ xx "[" kk "]"))))))]
    [`(vector-ref ,x ,k) (EVAL x (λ (xx) (EVAL k (λ (kk)
                                                   (f (++ xx "[" kk "+1]"))))))]
    [`(@ ,x ,@k) (EVAL x (λ (xx)
                           (f (add-between (cons xx (map id k)) "."))))]
    [`(if/begin ,b [,@t] [,@fa])
     (EVAL b (λ (bb)
               (++ "if " bb " then\n"
                   (EVAL `(begin ,@t) ig)
                   "else\n"
                   (EVAL `(begin ,@fa) ig)
                   "end\n"
                   (f undefined))))]
    [`(begin ,x) (EVAL x f)]
    [`(begin ,x ,@xs) (EVAL x (λ (xx)
                                (++
                                 (ig xx)
                                 (EVAL `(begin ,@xs) f))))]
    [`(if ,b ,x ,y)
     (with-genvar!
         (λ (v)
           (EVAL
            `(if/begin ,b [(set! ,v ,x)] [(set! ,v ,y)])
            (λ (u) (f v)))))]
    [`(vector ,@xs)
     (EVALxs EVAL xs (λ (xss) (f (++ "{" (add-between xss ",") "}"))))]
    [`(vector-length ,v) (EVAL v (λ (vv) (f (++ "(#" vv ")"))))]
    [`(apply ,f ,xs) (EVAL f (λ (ff) (EVAL xs (λ (xss)
                                                (f (++ ff "(unpack(" xss "))"))))))]
    [`(+ ,@x) (+-*/ EVAL "+" x f)]
    [`(- ,@x) (+-*/ EVAL "-" x f)]
    [`(* ,@x) (+-*/ EVAL "*" x f)]
    [`(/ ,@x) (+-*/ EVAL "/" x f)]
    [`(< ,@x) (<>= EVAL "<" x f)]
    [`(> ,@x) (<>= EVAL ">" x f)]
    [`(= ,@x) (<>= EVAL "==" x f)]
    [`(<= ,@x) (<>= EVAL "<=" x f)]
    [`(>= ,@x) (<>= EVAL ">=" x f)]
    [`(and ,@x) (+-*/ EVAL " and " x f)]
    [`(or ,@x) (+-*/ EVAL " or " x f)]
    [`(not ,x)
     (EVAL x (λ (xx) (f (++ "(not " xx ")"))))]
    [`(eq? ,x ,y) (EVAL `(= ,x ,y) f)]
    [`(noteq? ,x ,y) (EVAL x (λ (xx) (EVAL y (λ (yy) (f (++ "(" xx "~=" yy ")"))))))]
    [`(vector-for ,i ,x ,xs ,@c)
     (EVAL xs (λ (xss)
                (++ "for _i," (id x) " in ipairs(" xss ") do\n"
                    "local " (id i) "=_i-1\n"
                    (EVAL `(begin ,@c) ig)
                    "end\n"
                    (f undefined))))]
    [`(for ,i ,x ,t ,@c)
     (EVAL t (λ (tt)
               (++ "for " (id i) "," (id x) " in pairs(" tt ") do\n"
                   "if " (id x) "~=nil and type(" (id i) ")~=\"number\" then\n"
                   (EVAL `(begin ,@c) ig)
                   "end \n"
                   "end \n"
                   (f undefined))))]
    [`(for-from-to ,x ,i ,a ,@c)
     (EVAL i (λ (ii) (EVAL a (λ (aa)
                               (++ "for " (id x) "=" ii "," aa " do\n"
                                   (EVAL `(begin ,@c) ig)
                                   "end \n"
                                   (f undefined))))))]
    [`(number? ,x) (EVAL `(eq? (type ,x) "number") f)]
    [`(boolean? ,x) (EVAL `(eq? (type ,x) "boolean") f)]
    [`(procedure? ,x) (EVAL `(eq? (type ,x) "function") f)]
    [`(string? ,x) (EVAL `(eq? (type ,x) "string") f)]
    [`(!/vectror? ,x) (EVAL `(eq? (type ,x) "table") f)]
    [`(vector? ,x)
     (EVAL x (λ (xx)
               (store! xx (λ (v) (f (++ "(" v "[1] or next(" v ")==nil)"))))))]
    [`(host ,@c) (match c [`(,_ ... [lua ,v] ,_ ...) (f v)])]
    [`(raise ,e) (EVAL e (λ (ee)
                           (++ "E_="ee"\n"
                               "error(tostring(E_)..\" MapErr\")\n")))]
    [`(with-exception-handler ,handler ,thunk)
     (EVAL
      handler
      (λ (h)
        (EVAL
         thunk
         (λ (t)
           (let ([s (genvar!)] [x (genvar!)])
             (++ "local "s","x"=pcall("t")\n"
                 "if "s"==false and string.sub("x",-6)==\"MapErr\" then\n"
                 x"="h"(E_)\n"
                 "end\n"
                 (f x)))))))]
    [`(,k ,@x)
     (EVAL k (λ (kk) (EVALxs EVAL x (λ (xss)
                                      (f (++ kk "(" (add-between xss ",") ")"))))))]))
