#lang info

(define version "0.0.1")

(define deps '("aws"
               "base"
               "drracket-plugin-lib"
               "gui-lib"
               "comm-panel"))

(define drracket-tool-names (list "Magic Loader"))
(define drracket-tools (list (list "tool.rkt")))

