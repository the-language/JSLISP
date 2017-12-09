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
(define ((<- v) x)
  (list (list 'set! v x)))
(define (must b) (if b 0 (error "_!_")))
(define undefined 'undefined)
(define (ig x)
  (if (or (number? x) (string? x) (symbol? x))
      '()
      (list x)))
(define (EVAL x b k)
  (cond
    [(pair? x)
     (let ([f (car x)] [xs (cdr x)])
       (cond
         [(eq? f 'if)
          (if b
              (call/gensym
               (λ (r)
                 (cons
                  (list 'define r)
                  (EVAL (first xs) #t
                        (λ (b)
                          (cons
                           (list 'if/void b
                                 (EVAL (second xs) #t (<- r))
                                 (EVAL (third xs) #t (<- r)))
                           (k r)))))))
              (EVAL (first xs) #f
                    (λ (b)
                      (EVAL (second xs) #f
                            (λ (x)
                              (EVAL (third xs) #f
                                    (λ (y)
                                      (k (list 'if b x y)))))))))]
         [(eq? f 'set!)
          (must b)
          (EVAL (second xs) #t
                (λ (v)
                  (cons
                   (list 'set! (first xs) v)
                   (k undefined))))]
         [(eq? f 'define)
          (must b)
          (let ([v (car xs)])
            (if (pair? v)
                (EVAL (list 'define (car v) (cons 'λ (cons (cdr v) (cdr xs)))) b k)
                (cons
                 (list 'define v (second xs))
                 (k undefined))))]
         [(eq? f 'return)
          (must b)
          (list (list 'return (first xs)))]
         [(or (eq? f 'and) (eq? f 'or))
          (EVALxs xs #f
                  (λ (xs) (k (cons f xs))))]
         [(eq? f 'begin)
          (must b)
          (if (null? (cdr xs))
              (EVAL (car xs) b k)
              (EVAL (car xs) b
                    (λ (a)
                      (append
                       (ig a)
                       (BEGIN (cdr xs) b k)))))]
         [(eq? f 'λ)
          (k (cons 'λ (cons (car xs)
                            (BEGIN (cdr xs) #t list))))]
         [(or (eq? f 'for-object) (eq? f 'for-vector))
          (must b)
          (let ([x (car xs)] [xs (cdr xs)])
            (let ([i (first x)] [v (second x)] [o (third x)])
              (EVAL o b
                    (λ (o)
                      (cons (cons f
                                  (cons (list i v o) (BEGIN xs #t ig)))
                            (k undefined))))))]
         [else (EVAL f b
                     (λ (f)
                       (EVALxs xs b
                               (λ (xs) (k (cons f xs))))))]))]
    [else (k x)]))
(define (EVALxs xs b k)
  (if (null? xs)
      (k '())
      (EVAL (car xs) b
            (λ (a)
              (EVALxs (cdr xs) b
                      (λ (d)
                        (k (cons a d))))))))
(define (BEGIN xs b k)
  (EVAL (cons 'begin xs) b k))
(define (map xs) (BEGIN xs #t (λ (x) (if (eq? x 'undefined) '() (list x)))))