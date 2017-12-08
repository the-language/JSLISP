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
(define N ";")
(define undefined "undefined")
(define (ig x)
  (if (eq? x undefined)
      ""
      (++ x N)))
(define (EVAL x f)
  (cond
    [(pair? x) (APPLY (car x) (cdr x) f)]
    [(symbol? x)
     (f (if (eq? x 'undefined)
            undefined
            (id x)))]
    [(number? x) (f (float->string x))]
    [(eq? x #t) (f "true")]
    [(eq? x #f) (f "false")]
    [else (error "js: undefined" x f)]))
(define (EVALxs xs k)
  (if (null? xs)
      (k '())
      (EVAL (car xs)
            (λ (x)
              (EVALxs (cdr xs)
                      (λ (d)
                        (k (cons x d))))))))
(define (! xs k)
  (if (null? xs)
      (k '())
      (let ([x (car xs)] [xs (cdr xs)])
        (EVAL (second x)
              (λ (v)
                (! xs
                   (λ (xs)
                     (k (cons (cons (id (first x)) v) xs)))))))))
(define (DEFINE xs k)
  (cond
    [(null? (cdr xs)) (++ "var "(id (car xs))N (k undefined))]
    [(pair? (car xs))
     (let ([a (car xs)])
       (DEFINE (list (car a) (cons 'λ (cons (cdr a) (cdr xs)))) k))]
    [else (EVAL (second xs)
                (λ (x)
                  (++ "var "(id (first xs))"="x N
                      (k undefined))))]))
(define genvar! gensym!)
(define (store! x f)
  (let ([v (genvar!)])
    (++ "var "v"="x N
        (f v))))
(define (with-genvar! f)
  (let ([v (genvar!)])
    (++ "var " v N
        (f v))))
(define (APPLY f xs k)
  (cond
    [(eq? f 'define) (DEFINE xs k)]
    [(eq? f 'set!)
     (EVAL (first xs)
           (λ (x)
             (EVAL (second xs)
                   (λ (v)
                     (++ x"="v N
                         (k undefined))))))]
    [(eq? f 'λ) (LAMBDA xs k)]
    [(eq? f 'return)
     (EVAL (first xs)
           (λ (x)
             (++ "return "x N)))]
    [(eq? f '!) (! xs (λ (xs)
                        (k (++
                            "({"
                            (add-between (map (λ (x) (++ (car x)":"(cdr x))) xs) ",") "})"))))]
    [(or (eq? f 'vector-ref) (eq? f 'ref))
     (EVAL (first xs)
           (λ (o)
             (EVAL (second xs)
                   (λ (key)
                     (k (++ o"["key"]"))))))]
    [(eq? f '/)
     (EVAL (first xs)
           (λ (o)
             (k (++ o"."(id (second xs))))))]
    [(eq? f ':)
     (EVAL (car xs)
           (λ (o)
             (let ([xs (cdr xs)])
               (EVALxs (cdr xs)
                       (λ (d)
                         (k (++ o"."(id (car xs))"("(add-between d ",")")")))))))]
    [(eq? f 'begin) (BEGIN xs k)]
    [(eq? f 'apply)
     (EVAL (first xs)
           (λ (f)
             (EVAL (second xs)
                   (λ (xs)
                     (k (++ f"(..."xs")"))))))]
    [(eq? f 'if) ; BUG
     (EVAL (first xs)
           (λ (b)
             (EVAL (second xs)
                   (λ (x)
                     (EVAL (third xs)
                           (λ (y)
                             (k (++ "("b"?"x":"y")"))))))))]
    [(eq? f 'if/void)
     (EVAL (first xs)
           (λ (b)
             (++ "if("b"){"
                 (BEGIN (second xs) ig)
                 "}else{"
                 (BEGIN (third xs) ig)
                 "}"
                 (k undefined))))]
    [(eq? f 'vector)
     (EVALxs xs (λ (xs)
                  (k (++ "["(add-between xs ",")"]"))))]
    [(eq? f 'length)
     (EVAL (first xs) (λ (x)
                        (k (++ x".length"))))]
    [(eq? f '+) (+-*/ "+" xs k)]
    [(eq? f '-) (+-*/ "-" xs k)]
    [(eq? f '*) (+-*/ "*" xs k)]
    [(eq? f '/) (+-*/ "/" xs k)]
    [(eq? f '<) (<>= "<" xs k)]
    [(eq? f '>) (<>= ">" xs k)]
    [(or (eq? f 'eq?) (eq? f '=)) (<>= "===" xs k)]
    [(eq? f 'js-eq?) (<>= "==" xs k)]
    [(eq? f '!=) (<>= "!==" xs k)]
    [(eq? f 'js-not-eq?) (<>= "!=" xs k)]
    [(eq? f '<=) (<>= "<=" xs k)]
    [(eq? f '>=) (<>= ">=" xs k)]
    [(eq? f 'and) (<>= "&&" xs k)]
    [(eq? f 'or) (<>= "||" xs k)]
    [(eq? f 'not)
     (EVAL (first xs) (λ (x)
                        (k (++ "(!"x")"))))]
    [(eq? f 'vector-for)
     (let ([x (car xs)] [b (cdr xs)])
       (let ([i (id (first x))] [x (id (second x))] [xs (third x)])
         (EVAL
          xs
          (λ (xs)
            (store!
             xs
             (λ (xs)
               (++ "for(var i_=0;i_<"xs".length;i_++){"
                   "var "i"=i_"N
                   "var "x"="xs"[i_]"N
                   (BEGIN b ig)
                   "}"
                   (k undefined))))))))]
    [(eq? f 'object-for)
     (let ([x (car xs)] [b (cdr xs)])
       (let ([i (id (first x))] [x (id (second x))] [o (third x)])
         (EVAL
          o
          (λ (o)
            (store!
             o
             (λ (o)
               (++ "for(var "i" in "o"){"
                   "var "x"="o"["i"]"N
                   (BEGIN b ig)
                   "}"
                   (k undefined))))))))]
    [(eq? f 'c-for)
     (let ([x (car xs)] [b (cdr xs)])
       (let ([init (first x)] [c (second x)] [i (third x)])
         (++ "for("(EVAL init ig)(EVAL c ig)(EVAL i ig)"){"
             (BEGIN b ig)
             "}"
             (k undefined)
             )))]
    [(eq? f 'while)
     (let ([x (car xs)] [b (cdr xs)])
       (EVAL
        x
        (λ (x)
          (++ "if("x"){"
              "while(true){"
              (BEGIN b ig)
              (EVAL
               x
               (λ (x)
                 (++ "if(!"x"){break}")))
              "}}"
              (k undefined)))))]
    [(eq? f 'number?) (TYPE? "number" (first xs) k)]
    [(eq? f 'procedure?) (TYPE? "function" (first xs) k)]
    [(eq? f 'boolean?) (TYPE? "boolean" (first xs) k)]
    [(eq? f 'string?) (TYPE? "string" (first xs) k)]
    [(eq? f 'object/vector?) (TYPE? "object" (first xs) k)]
    [(eq? f 'vector?) (EVAL (first xs) (λ (x)
                                         (k (++ "("x" instanceof Array)"))))]
    [(eq? f 'undefined?) (EVAL (first xs)
                               (λ (x)
                                 (k (++ "("x"===null)"))))]
    [(or (eq? f '->string) (eq? f 'number->string))
     (EVAL (first xs) (λ (x)
                        (k (++ "String("x")"))))]
    [(or (eq? f '->number) (eq? f 'string->number))
     (EVAL (first xs) (λ (x)
                        (k (++ "Number("x")"))))]
    [(eq? f 'is-a?)
     (EVAL (first xs) (λ (x)
                        (EVAL (second xs) (λ (c)
                                            (k (++ "("x" instanceof "c")"))))))]
    [(eq? f 'typeof)
     (EVAL (first xs) (λ (x) (k (++ "(typeof "x")"))))]
    [(eq? f 'new)
     (let ([x (car xs)] [xs (cdr xs)])
       (EVAL x
             (λ (x)
               (EVALxs xs
                       (λ (xs)
                         (k (++ "(new "x"("(add-between xs ",")"))")))))))]
    [(eq? f 'newtype) (k "(function(){})")]
    [(eq? f 'raise) (EVAL (first xs)
                          (λ (x)
                            (++ "throw "x N)))]
    [(eq? f 'with-exception-handler)
     (EVAL
      (first xs)
      (λ (handler)
        (EVAL
         (second xs)
         (λ (thunk)
           (let ([e (genvar!)])
             (with-genvar!
                 (λ (x)
                   (++ "try{"
                       x"="thunk"()"
                       "}catch("e"){"
                       x"="handler"("e")}"
                       (k x)))))))))]
    [(eq? f 'assert)
     (EVAL
      (first xs)
      (λ (b)
        (++ "if("b"){throw 'assertion failed!'}"
            (k undefined))))]
    [(eq? f 'cond/begin) (COND/BEGIN xs k)]
    [(eq? f 'struct) (let ([s (second xs)])
                       (++
                        (STRUCT (first xs) (car s) (cdr s))
                        (k undefined)))]
    [else (EVAL f (λ (f)
                    (EVALxs xs (λ (xs)
                                 (k (++ f"("(add-between xs ",")")"))))))]))
(define (TYPE? t x k)
  (EVAL x (λ (x)
            (k (++ "(typeof "x"=='"t"')")))))
(define (LAMBDA xs k)
  (let ([args (car xs)] [body (cdr xs)])
    (k (++ "(function("(add-between (LAMBDA-ARGS args) ",")"){"
           (EVAL (cons 'begin body) (λ (x) (++ "return "x N)))"})"))))
(define (LAMBDA-ARGS args)
  (cond
    [(symbol? args) (list (++ "..."(id args)))]
    [(null? args) '()]
    [else (cons (id (car args)) (LAMBDA-ARGS (cdr args)))]))
(define (BEGIN xs k)
  (cond
    [(null? (cdr xs)) (EVAL (car xs) k)]
    [else (EVAL (car xs)
                (λ (x)
                  (++ (ig x) (BEGIN (cdr xs) k))))]))
(define (+-*/ f xs k)
  (EVALxs xs (λ (xs)
               (k (++ "("(add-between xs f)")")))))
(define (<>= f xs k)
  (EVAL (first xs)
        (λ (x)
          (EVAL (second xs)
                (λ (y)
                  (k (++ "("x f y")")))))))
(define (COND/BEGIN xs k)
  (if (null? xs)
      (k undefined)
      (let ([x (car xs)] [xs (cdr xs)])
        (let ([a (car x)] [d (cdr x)])
          (if (eq? a 'else)
              (++ (BEGIN d (λ (x) (k undefined))))
              (EVAL a (λ (a)
                        (++ "if("a"){"
                            (BEGIN d ig)
                            "}" (%COND/BEGIN xs k)))))))))
(define (%COND/BEGIN xs k)
  (if (null? xs)
      (k undefined)
      (let ([x (car xs)] [xs (cdr xs)])
        (let ([a (car x)] [d (cdr x)])
          (if (eq? a 'else)
              (++ "else{"
                  (BEGIN d ig)
                  "}"
                  (k undefined))
              (EVAL a (λ (a)
                        (++ "else if("a"){"
                            (BEGIN d ig)
                            "}" (%COND/BEGIN xs k)))))))))
(define (STRUCT pred constructor fields)
  (let ([t (id (string-append pred "T"))] [pred (id pred)] [constructor (id constructor)] [fields (map id fields)])
    (++ "var "t"=function(){}"N
        "var "pred"=function(x){return x instanceof "t N"}"N
        "var "constructor"="
        "function("(add-between fields ",")"){var v_=new "t"()"N (map (λ (f) (++ "v_."f"="f N)) fields) "return v_"N"}" N)))
(define (js x) (EVAL x ig))
