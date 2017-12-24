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

(define (%**lam-arg args)
  (cond
    [(symbol? args) (++ (**var args)"...")]
    [(null? args) ""]
    [(null? (cdr args)) (**var (car args))]
    [else (++ (**var (car args))","(%**lam-arg (cdr args)))]))
(define (ADDbetweenSTRING xs k) (foldr string-append "" (add-between xs k)))
(define ((*%* k) xs) (++ "("(ADDbetweenSTRING xs k)")"))
(define ((%*xfx f) x y) (++ "("x f y")"))

(define **var**cs (string->list "qwertyuiopasdfghjklzxcvbnmQWERTYUIOPASDFGHJKLZXCVBNM1234567890_$"))
(define (**var x)
  (let* ([x (symbol->string x)] [xs (string->list x)])
    (if (andmap (λ (x) (member x **var**cs)) xs)
        x
        (%**var xs (λ (xs) x) (λ (xs) (list->string (cons #\Z xs)))))))
(define (%**var xs k1 k2)
  (define (pack_ x) (cons #\_ (append (string->list (number->string (char->integer x))) '(#\C))))
  (if (null? xs)
      (k1 '())
      (let ([x (car xs)] [xs (cdr xs)])
        (if (member x **var**cs)
            (%**var xs (λ (xs) (k1 (cons x xs)))
                    (λ (xs)
                      (if (eq? x #\_)
                          (k2 (append (pack_ x) xs))
                          (k2 (cons x xs)))))
            (let ([xx (pack_ x)])
              (%**var xs (λ (xs) (k2 (append xx xs))) (λ (xs) (k2 (append xx xs)))))))))
(define (**top xs)
  (if (null? (cdr xs))
      (++ (car xs)";")
      (++ (car xs)";"(**top (cdr xs)))))
(define (**define s x) (++ "var "(**var s)"="x))
(define (**define-undefined s) (++ "var "(**var s)))
(define (**set! v x) (++ (**var v)"="x))
(define (**begin* xs) (ADDbetweenSTRING xs ";"))

(define (**return x) (++ "return "x))
(define (**lambda args body)
  (++ "(function("(%**lam-arg args)"){"
      (**top body)
      "})"))
(define (%**app xs) (ADDbetweenSTRING xs ","))
(define (**apply* f xs) (++ f"("(%**app xs)")"))
(define (*apply f xs) (++ f".apply(null,"xs")"))
(define (*procedure? x) (++ "(typeof "x"=='function')"))

(define (*raise x) (++ "throw "x))
(define (**try xs e ys)
  (++ "try{"(**top xs)"}catch("(**var e)"){"(**top ys)"}"))

(define (*boolean? x) (++ "(typeof "x"=='boolean')"))
(define *false "false")
(define *true "true")
(define *eq? (%*xfx "==="))
(define *not-eq? (%*xfx "!=="))
(define (*or* xs) (*%* "||"))
(define (*and* xs) (*%* "&&"))
(define (*not x) (++ "(!"x")"))
(define (**if-boolean b xb yb) (++ "if("b"){"(**top xb)"}else{"(**top yb)"}"))
(define (**if b xb yb) (**if-boolean (*not-eq? b *false) xb yb))
(define (*if-boolean b x y) (++ "("b"?"x":"y")"))
(define (*if b x y) (*if-boolean (*not-eq? b *false) x y))

(define (*string? x) (++ "(typeof "x"=='string')"))
(define *string-append* (*%* "+"))
(define (*string-ref s k) (++ s".charAt("k")"))
(define (*string-length s) (++ s".length"))
(define (*list->string xs) (++ xs".join('')"))
(define (*string->list s) (++ "Array.prototype.slice.call("s")"))

(define (*number? x) (++ "(typeof "x"=='number')"))
(define (*number->string x) (++ "String("x")"))
(define (*string->number x) (++ "Number("x")"))
(define *+* (*%* "+"))
(define *-* (*%* "-"))
(define *** (*%* "*"))
(define */* (*%* "/"))
(define *= *eq?)
(define *< (%*xfx "<"))
(define *> (%*xfx ">"))
(define *<= (%*xfx "<="))
(define *>= (%*xfx ">="))

(define %*r1-s (gensym))
(define %*r1 (**var %*r1-s))
(define %*r2 (**var (gensym)))
(define (**@ o k) (++ o"."(**var k)))
(define (**@= o k v) (++ o"."(**var k)"="v))
(define (*object-ref o k) (++ o"["k"]"))
(define (**object* xs) (++ "{"(ADDbetweenSTRING (map (λ (x) (++ (**var (first x))":"(second x))) xs) ",")"}"))
(define (**: o k xs) (++ o"."(**var k)"("(%**app xs)")"))
(define (*object/vector? x) (++ "(typeof "x"=='object')"))
(define (*object-set! o k x) (++ o"["k"]="x))
(define (**for-object i v xs)
  (++ "var "%*r1"="v";"
      "for(var "%*r2" in "v"){var "i"="%*r2";"(**top xs)"}"))
(define (*vector? x) (++ "("x" instanceof Array)"))
(define (*vector* xs) (++ "["(%**app xs)"]"))
(define (*vector-set! v k x) (++ v"["k"]="x))
(define (*vector-ref v k) (++ v"["k"]"))
(define (*vector-append* xs) (++ (car xs)".concat("(%**app (cdr xs))")"))
(define (*vector-length x) (++ x".length"))
(define (**for-vector i v xs)
  (++ "var "%*r1"="v";"
      "for(var "%*r2"=0;"%*r2"<"%*r1".length;"%*r2"++){var "i"="%*r2";"(**top xs)"}"
      ))
(define (*vector-head x) (++ x"[0]"))
(define (*vector-tail x) (++ x".slice(1)"))
(define (*vector-map f xs) (++ xs".map("f")"))

(define *undefined "undefined")
(define (*undefined? x) (++ "("x"===null)"))

(define (**number x) (number->string (exact->inexact x)))
(define (**string x) (format "~s" x))
(define (**struct pred new fs)
  (let ([k (**var (gensym))])
    (++ "var "k"=function(){};"
        (**define new
                  (**lambda fs
                            (cons
                             (**define %*r1-s (++ "new "k"()"))
                             (append
                              (map (λ (f) (**@= %*r1 f (**var f))) fs)
                              (list (**return %*r1))))))
        ";"
        (**define pred
                  (**lambda '(x)
                            (list (**return (++ "(x instanceof "k")"))))))))

(define %*exp*fs
  (hash
   'begin (λ xs (**begin* xs))
   'return **return
   'apply *apply
   'procedure? *procedure?

   'raise *raise

   'boolean? *boolean?
   'eq? *eq?
   'not-eq? *not-eq?
   'or (λ xs (*or* xs))
   'and (λ xs (*and* xs))
   'not *not
   'if-boolean *if-boolean
   'if *if

   'string? *string?
   'string-append (λ xs (*string-append* xs))
   'string-ref *string-ref
   'string-length *string-length
   'list->string *list->string
   'string->list *string->list

   'number? *number?
   'number->string *number->string
   'string->number *string->number
   '+ (λ xs (*+* xs))
   '- (λ xs (*-* xs))
   '* (λ xs (*** xs))
   '/ (λ xs (*/* xs))
   '= *=
   '< *<
   '> *>
   '<= *<=
   '>= *>=

   'object-ref *object-ref
   'object/vector? *object/vector?
   'object-set! *object-set!
   'vector? *vector?
   'vector (λ xs (*vector* xs))
   'vector-set! *vector-set!
   'vector-ref *vector-ref
   'vector-append (λ xs (*vector-append* xs))
   'vector-length *vector-length
   'vector-head *vector-head
   'vector-tail *vector-tail
   'vector-map *vector-map

   'undefined *undefined
   'undefined? *undefined?
   ))
   
(define (*exp x)
  (cond
    [(pair? x)
     (let ([f (car x)] [xs (cdr x)])
       (let ([r (hash-ref %*exp*fs f #f)])
         (if r
             (apply r (map *exp xs))
             (cond
               [(or (eq? f 'λ) (eq? f 'lambda)) (**lambda (car xs) (map *exp (cdr xs)))]
               [(eq? f 'define) (**define (first xs) (*exp (second xs)))]
               [(eq? f 'define-undefined) (**define-undefined (first xs))]
               [(eq? f 'set!) (**set! (first xs) (*exp (second xs)))]
               [(eq? f 'try) (**try (map *exp (first xs)) (second xs) (map *exp (third xs)))]

               [(eq? f 'if-boolean/do) (**if-boolean (*exp (first xs)) (map *exp (second xs)) (map *exp (third xs)))]
               [(eq? f 'if/do) (**if (*exp (first xs)) (map *exp (second xs)) (map *exp (third xs)))]

               [(eq? f '@) (**@ (*exp (first xs)) (second xs))]
               [(eq? f '@=) (**@= (*exp (first xs)) (second xs) (*exp (third xs)))]
               [(eq? f 'object) (**object* (map (λ (x) (list (first x) (*exp (second x)))) xs))]
               [(eq? f ':) (**: (*exp (car xs)) (cadr xs) (map *exp (cddr xs)))]

               [(eq? f 'for-object) (**for-object (car xs) (*exp (cadr xs)) (cddr xs))]
               [(eq? f 'for-vector) (**for-vector (car xs) (*exp (cadr xs)) (cddr xs))]

               [(eq? x 'undefined) *undefined]

               [(eq? f 'struct) (**struct (first xs) (second xs) (third xs))]
               [else (**apply* (*exp f) (map *exp xs))]))))]
    [(symbol? x) (**var x)]
    [(number? x) (**number x)]
    [(eq? x #t) *true]
    [(eq? x #f) *false]
    [(string? x) (**string x)]
    [else (error)]))
