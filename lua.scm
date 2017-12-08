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
(define undefined "nil")
(define Tstatement 's)
(define Tpure 'p)
(define Texp #f)
(define (ig x t)
  (cond
    [(eq? t Tstatement) (++ x "\n")]
    [(eq? t Tpure) ""]
    [else (++ "type("x")\n")]))
(define genvar! gensym!)
(define (store! x f)
  (let ([v (genvar!)])
    (++ "local "v"="x"\n"
        (f v Tpure))))
(define (with-genvar! f)
  (let ([v (genvar!)])
    (++ "local "v"\n"
        (f v Tpure))))
(define (EVALxs xs k)
  (if (null? xs)
      (k '())
      (EVAL (car xs)
            (λ (x t)
              (EVALxs (cdr xs)
                      (λ (d)
                        (k (cons x d))))))))
(define (! xs k)
  (if (null? xs)
      (k '())
      (let ([x (car xs)] [xs (cdr xs)])
        (EVAL (second x)
              (λ (v t)
                (! xs
                   (λ (xs)
                     (k (cons (cons (id (first x)) v) xs)))))))))
(define (DEFINE xs k)
  (cond
    [(null? (cdr xs)) (++ "local "(id (car xs))"\n"(k undefined Tpure))]
    [(pair? (car xs))
     (let ([a (car xs)])
       (DEFINE (list (car a) (cons 'λ (cons (cdr a) (cdr xs)))) k))]
    [else (EVAL (second xs)
                (λ (x)
                  (++ "local "(id (first xs))"="x"\n"
                      (k undefined Tpure))))]))
(define (LAMBDA xs k)
  (let ([args (car xs)] [body (cdr xs)])
  (if (list? args)
(k (++ "(function("(add-between (map id args) ",")")\n"
       
      (LAMBDA... args body k)
      ;WIP
(define (EVAL x f)
  (cond
    [(pair? x) (APPLY (car x) (cdr x) f)]
    [(symbol? x)
     (f (if (eq? x 'undefined)
            undefined
            (id x)) Tpure)]
    [(number? x) (f (float->string x) Tpure)]
    [(eq? x #t) (f "true" Tpure)]
    [(eq? x #f) (f "false" Tpure)]
    [else (error "lua: undefined" x f)]))
(define (APPLY f xs k)
  (cond
    [(eq? f 'define) (DEFINE xs k)]
    [(eq? f 'set!)
     (EVAL (first xs)
           (λ (x t)
             (EVAL (second xs)
                   (λ (v t)
                     (++ x"="v"\n"(k undefined Tpure))))))]
    [(eq? f 'λ) (LAMBDA xs k)]
    [(eq? f 'return)
     (EVAL (first xs)
           (λ (x t)
             (++ "return "x"\n")))]
    [(eq? f '!) (! xs (λ (xs)
                        (k (++
                            "{"
                            (add-between (map (λ (x) (++ (car x)"="(cdr x))) xs) ",") "}"))) Texp)]
    [(eq? f 'vector-ref)
     (EVAL (first xs)
           (λ (v t)
             (EVAL (second xs)
                   (λ (key t)
                     (k (++ v"[1+"key"]") Texp)))))]
    [(eq? f 'ref)
     (EVAL (first xs)
           (λ (o t)
             (EVAL (second xs)
                   (λ (key t)
                     (k (++ v"["key"]") Texp)))))]
    [(eq? f '/)
     (EVAL (first xs)
           (λ (o t)
             (k (++ o"."(id (second xs))) Texp)))]
    [(eq? f ':)
     (EVAL (car xs)
           (λ (o t)
             (let ([xs (cdr xs)])
               (EVALxs (cdr xs)
                       (λ (d)
                         (k (++ o"."(id (car xs))"("(add-between d ",")")") Tstatement))))))]
    [(eq? f 'begin) (BEGIN xs k)]
    [(eq? f 'apply)
     (EVAL (first xs)
           (λ (f t)
             (EVAL (second xs)
                   (λ (xs t)
                     (k (++ f"(unpack("xs"))") Tstatement)))))]
    [(eq? f 'if)
     (EVAL (first xs)
           (λ (b t)
             (with-genvar!
                 (λ (r)
                   (++ "if "b" then\n"
                       (EVAL (second xs)
                             (λ (x t)
                               (++ r"="x"\n")))
                       "else\n"
                       (EVAL (third xs)
                             (λ (y t)
                               (++ r"="y"\n")))
                       "end\n"
                       (k r Tpure))))))]
    [(eq? f 'if/void)
     (EVAL (first xs)
           (λ (b t)
             (++ "if "b" then\n"
                 (BEGIN (second xs) ig)
                 "else\n"
                 (BEGIN (third xs) ig)
                 "end\n"
                 (k undefined Tpure))))]
    [(eq? f 'vector)
     (EVALxs xs (λ (xs)
                  (k (++ "{"(add-between xs ",")"}") Texp)))]
    [(eq? f 'length)
     (EVAL (first xs) (λ (x t)
                        (k (++ "(#"x")") Texp)))]
    
    [(eq? f '+) (+-*/ "+" xs k)]
    [(eq? f '-) (+-*/ "-" xs k)]
    [(eq? f '*) (+-*/ "*" xs k)]
    [(eq? f '/) (+-*/ "/" xs k)]
    [(eq? f '<) (<>= "<" xs k)]
    [(eq? f '>) (<>= ">" xs k)]
    [(or (eq? f 'eq?) (eq? f '=)) (<>= "==" xs k)]
    [(eq? f '!=) (<>= "~=" xs k)]
    [(eq? f '<=) (<>= "<=" xs k)]
    [(eq? f '>=) (<>= ">=" xs k)]
    [(eq? f 'and) (<>= "&&" xs k)]
    [(eq? f 'or) (<>= "||" xs k)]
    [(eq? f 'not)
     (EVAL (first xs) (λ (x)
                        (k (++ "(not "x")") Texp)))]))
(define (lua x) (EVAL x ig))
