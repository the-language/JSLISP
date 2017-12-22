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
(define (**var x) (symbol->string x)) ; Bug
(define (**return x) (++ "return "x))
(define (**lambda args body)
  (++ "(function("(%**lam-arg args)"){"
      (**top body)
      "})"))
(define (**top xs)
  (if (null? (cdr xs))
      (++ (car xs)";")
      (++ (car xs)";"(**top (cdr xs)))))
(define (%**lam-arg args)
  (cond
    [(symbol? args) (++ (**var args)"...")]
    [(null? args) ""]
    [(null? (cdr args)) (**var (car args))]
    [else (++ (**var (car args))","(%**lam-arg (cdr args)))]))
(define (%**app xs) (foldr string-append "" (add-between xs ",")))
(define (**apply f xs) (++ f"("(%**app xs)")"))
(define (*js-typeof x) (++ "(typeof "x")"))
(define (*procedure? x) (++ "(typeof "x"=='function')"))
(define (*string? x) (++ "(typeof "x"=='string')"))
(define (*object/vector? x) (++ "(typeof "x"=='object')"))
(define (*is-a? x t) (++ "("x" instanceof "t")"))
(define (*vector? x) (*is-a? x "Array"))
(define (**c-for s1 s2 s3 xs) (++ "for("s1";"s2";"s3"){"(**top xs)"}"))
(define (**for-vector i v xs)
  (let ([s (gensym)] [o (gensym)])
    (++ (**define o v)
        (**c-for (**define s 0) (*< (**var s) (*length (**var o))) (**++ (**var s))
                 (cons (**define i (**var s)) xs)))))
(define (**define s x) (++ "var "(**var s)"="x))
(define (**define-undefined s) (++ "var "(**var s)))
(define (*< x y) (++ "("x"<"y")"))
(define (*length v) (++ v".length"))
(define (**++ v) (++ v"++"))
(define *undefined "undefined")
(define (*undefined? x) (++ "("x"===null)"))
(define (*eq? x y) (++ "("x"==="y")"))
(define (*not-eq? x y) (++ "("x"!=="y")"))
(define (*js-eq? x y) (++ "("x"=="y")"))
(define (*js-not-eq? x y) (++ "("x"!="y")"))
(define (**js-for-in s o xs) (++ "for("s" in "o"){"(**top xs)"}"))
(define (**js-if b xb yb) (++ "if("b"){"(**top xb)"}else{"(**top yb)"}"))
(define (**if b xb yb) (**js-if (*not-eq? b *false) xb yb))
(define *false "false")
(define *true "true")
(define (*js-if b x y) (++ "("b"?"x":"y")"))
(define (*if b x y) (*js-if (*not-eq? b *false) x y))
(define (**set! v x) (++ (**var v)"="x))
(define (*object-set! o k x) (++ o"["k"]="x))
(define (*vector-set! v k x) (++ v"["k"]="x))
(define (**/ o k) (++ o"."(**var k)))
(define (**/= o k v) (++ o"."(**var k)"="v))
(define (*object-ref o k) (++ o"["k"]"))
(define (*vector-ref v k) (++ v"["k"]"))
(define (*: o k xs) (++ o"."(**var k)"("(%**app xs)")"))
(define (**number x) (number->string (exact->inexact x)))
(define (**string x) (format "~s" x))
(define (**new x xs) (++ "new "x"("(%**app xs)")"))
(define %**struct-t (gensym))
(define %**struct-t-v (**var %**struct-t))
(define (**struct pred new fs)
  (let ([k (**var (gensym))])
    (++ "var "k"=function(){};"
        (**define new
                  (**lambda fs
                            (cons
                             (**define %**struct-t (**new k '()))
                             (append
                              (map (λ (f) (**/= %**struct-t-v f (**var f))) fs)
                              (list (**return %**struct-t-v))))))
        ";"
        (**define pred
                  (**lambda (list %**struct-t)
                            (list (**return (*is-a? %**struct-t-v k))))))))
(define (**or xs) (++ "("(add-between xs "||")")"))
(define (**and xs) (++ "("(add-between xs "&&")")"))
