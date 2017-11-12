例

Map                          | Lua                         |               Javascript
-----------------------------|-----------------------------|---------------------------
`(define map)`               |`local map`                  |`var map`
`(define (map f x) ...)`     |`local function map(f,x) ...`|`function map(f,x){ ... }`
`(set! x 1)`                 |`x=1`                        |`x=1;`
`(lambda (x) (return x))`    |`(function(x)return x end)`  |`(function(x){return x;})`
`(! [x 132] [y 343] [z 912])`|`{x=132, y=343, z=912}`      |`{"x":132, "y":343, "z":912}`
`(ref t "x")`                |`t["x"]`                     |`t["x"]`
`(@ t x)`                    |`t.x`                        |`t.x`
`(if/begin b [...] [...])`   |`if b then ... else ... end` |`if(b){ ... }else{ ... }`
`(vector x y z)`             |`{x,y,z}`                    |`[undefined,x,y,z]`
`(ref v 1)`                  |`v[1]`                       |`v[1]`
`(set! (ref v 1) 1)`         |`v[1]=1`                     |`v[1]=1;`
`(: t f 0)`                  |`t:f(0)`                     |`t.f(t,0)`
