例

Map                          | Lua                         |               Javascript
-----------------------------|-----------------------------|---------------------------
`(define map)`               |`local map`                  |`var map`
`(define a 0)`               |`local a=0`                  |`var a=0`
`(set! x 1)`                 |`x=1`                        |`x=1`
`(lambda (x) x)`             |`(function(x)return x end)`  |`(function(x){return x})`
`(! [x 132] [y 343] [z 912])`|`{x=132, y=343, z=912}`      |`{x:132, y:343, z:912}`
`(ref t "x")`                |`t["x"]`                     |`t["x"]`
`(@ t x)`                    |`t.x`                        |`t.x`
`(if/begin b [...] [...])`   |`if b then ... else ... end` |`if(b){ ... }else{ ... }`
`(vector x y z)`             |`{x,y,z}`                    |`[x,y,z]`
`(vector-ref v 1)`           |`v[1+1]`                     |`v[1]`
`(set! (vector-ref v 1) 1)`  |`v[1+1]=1`                   |`v[1]=1`
`(apply f t)`                |                             |

与Scheme的区别
============
+ 无char,pair,symbol,`vector-set!`,`vector->string`,`call/cc`等
+ 无`define-record-type`
+ `<` `>` `=` 等只能有2个参数
+ 所有primitive不是值
+ 没有宏
+ 数字只有float
+ define前不能使用这个值
+ ...
