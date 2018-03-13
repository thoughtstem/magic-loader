#lang racket/base

(require drracket/tool
         racket/class
         racket/gui/base
         racket/unit
         mrlib/switchable-button
         setup/dirs
         racket/file
         racket/string
         racket/list)

(provide tool@)
 
 
(define tool@
  (unit
    (import drracket:tool^)
    (export drracket:tool-exports^)
 
    (define reverse-button-mixin
      (mixin (drracket:unit:frame<%>) ()
        (super-new)
        (inherit get-button-panel
                 get-definitions-text
                 get-current-tab
                 open-in-new-tab)
        (inherit register-toolbar-button)
 
        (let ((btn
               (new switchable-button%
                    (label "TS Magic Loader")
                    (callback (λ (button)
                                (load-file )
                                ))
                    (parent (get-button-panel))
                    (bitmap reverse-content-bitmap))))
          (register-toolbar-button btn #:number 11)
          (send (get-button-panel) change-children
                (λ (l)
                  (cons btn (remq btn l)))))
                  
                  
        (define (load-file)
          ; Make a frame by instantiating the frame% class
          (define frame (new frame% [label "TS Magic Loader"]))
 
          ; Make a static text message in the frame
          (define msg (new message% [parent frame]
                           [label "Type the name of a file to load it"]))


 
          (define panel (new horizontal-panel% [parent frame]))


          (define text-field (new text-field%
                                  (label "Text")
                                  (parent panel)
                                  (init-value "")))

          (new button% [parent panel]
               [label "Load"]
               [callback (lambda (button event)
                           (send msg set-label "Loading..")
                           (let ([f (locate-file (send text-field get-value))])
                             (if f
                                 (maybe-move-and-open f)
                                 (void)))
                           )])
 
          ; Show the frame by calling its show method
          (send frame show #t))

        (define (path->string* ps)
          (if (string? ps)
              ps
              (path->string ps)))

        (define (maybe-move-and-open src)
          (maybe-create-smw)
          (if (regexp-match #rx"SAVE_MY_WORK" src )
              (open-in-new-tab src)
              (move-and-open src)))

        (define (maybe-create-smw)
          (define smw (string-append (path->string (find-system-path 'home-dir)) "Desktop/SAVE_MY_WORK/"))
          (or (directory-exists? smw)
              (make-directory smw)))

        (define (move-and-open src)
          (define f-name    (last (string-split (path->string* src) "/")))
          (define dest-file (string-append (path->string (find-system-path 'home-dir)) "/Desktop/SAVE_MY_WORK/" f-name ))

          (copy-file src
                     dest-file
                     #t)

          (open-in-new-tab dest-file))

        (define (locate-file name)
          ;TODO: Search in online locations... 
          ;      Backend?  
          ;TODO: Search local network?? Get stuff from teachers???

          (define home    (string-append (path->string (find-system-path 'home-dir)) name ".rkt" ))
          (define desktop (string-append (path->string (find-system-path 'home-dir)) "/Desktop/" name ".rkt" ))
          (define smw     (string-append (path->string (find-system-path 'home-dir)) "/Desktop/SAVE_MY_WORK/" name ".rkt" ))

      
          ;Note: order these so that it will return the local file with higher priority
    
          (or 
              (cond 
                [(file-exists? smw)     smw]
                [(file-exists? home)    home]
                [(file-exists? desktop) desktop]
                [else #f])
              (search-pkgs name)
              ))

        (define (search-pkgs name) 
          (define pkgs_dir  
                   (path->string (find-user-pkgs-dir)))

          (define files
            (find-files	 
                  (λ(x) (string-contains? (path->string x)
                                          (string-append "/" name ".rkt")))	 
                  pkgs_dir))

          (if (empty? files)
              #f
              (first files)) )))
 
    (define reverse-content-bitmap
      (let* ((bmp (make-bitmap 16 16))
             (bdc (make-object bitmap-dc% bmp)))
        (send bdc erase)
        (send bdc set-smoothing 'smoothed)
        (send bdc set-pen "black" 1 'transparent)
        (send bdc set-brush "black" 'solid)
        (send bdc draw-ellipse 2 2 8 8)
        (send bdc set-brush "green" 'solid)
        (send bdc draw-ellipse 6 6 8 8)
        (send bdc set-bitmap #f)
        bmp))
 

 
    (define (phase1) (void))
    (define (phase2) (void))
    
    (drracket:get/extend:extend-unit-frame reverse-button-mixin)))


