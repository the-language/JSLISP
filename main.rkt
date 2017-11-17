#lang racket
(require racket/cmdline)
(require "lua.rkt")
(require "js.rkt")
(define to (make-parameter js))
(command-line
 #:program "Map"
 #:once-any
 [("-j" "--js") "Compile to js" (to js)]
 [("-l" "--lua") "Compile to lua" (to lua)]
 #:args fs
 (define v
   (if (null? fs)
       (port->list)
       (foldl append '() (map file->list fs))))
 (displayln ((to) (cons 'begin v))))
