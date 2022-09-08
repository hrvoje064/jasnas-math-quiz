#lang racket/gui

;;; Copyright (c) 2022, Capt. Hrvoje Blazevic. All rights reserved.

;;; Redistribution and use in source and binary forms,
;;; with or without modification, are permitted provided
;;; that the following conditions are met:

;;; Redistributions of source code must retain the above copyright notice,
;;; this list of conditions and the following disclaimer.
;;; Redistributions in binary form must reproduce the above copyright notice,
;;; this list of conditions and the following disclaimer in the documentation
;;; and/or other materials provided with the distribution.

;;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDER "AS IS" AND ANY
;;; EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO,
;;; THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
;;; PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER
;;; BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY,
;;; OR CONSEQUENTIAL DAMAGES.


;;; Scheme (racket) source

;;; Math Quiz, Version 2.8 Sequences, before/between/after, position-value added
;;; geometry odd/even adjusted, arithmetic/popup separated
;;; fractions and scrolling start-button panel added, fonts cleaned

;;; working with three digits unlimited
;;; when subtraction weighing left number with more 0s in the middle digit
;;; for comparison weighin = to 1/5
;;; multiplication table 12*12 (default 10*10)

(require racket/string)
(require 2htdp/universe)
(require pict)
(require graphics/value-turtles)

(define *speed-factor* 1) ; reduce or increase allotted time
(define *left-number* 700) ; Max size-1 of left number
(define *left-digit* 8) ; Max size-1 of left digit of left number
(define *exponent* 3) ; regulating division result precision (3 places)
(define *min-break-ex* 10) ; min number of exercises to enable suspend break button
(define *max-penalty-exercises* 10) ; a reasonable punishment for exceeding time
(define *max-slices* 10) ; default slices for fractions

(define *sequence-difficulty* 1) ; difficulty level for sequence missing number
(define *level+-* 1) ; (1 2d limited +) (2 2d limited +-) (3 3d unlimited +-)

(define get-problem #f) ; problem composing function
(define setup #f) ; problem setup function
(define do-math #f) ; which operation to run arithemetic or others
(define equal= #f) ; equal function (returns a number->string in (/) exercises)
(define get-sequence #f) ; type of sequence for IQ test

(define *time-start* #f)
(define *allowed-time* #f)
(define *time-factor* #f) ; minutes per problem
(define *wiwi-time* #f) ; pause time
(define *wiwi-start* #f)
(define *wiwi* #f) ; pause switch
(define *exec-button* #f) ; calc/comp button switch

(define *used-numbers* '()) ; avoiding repeating numbers in (+ - * /)
(define *max-used-pairs* #f) ; max allowed number of pairs in *used-numbers*

;;; Initializing fonts
;;; ==========================================================

;;; Font sizes (defaults if no j-m-q.init file
(define button-fnt-size (box #f))
(define msg-fnt-size (box #f))
(define msg-b-fnt-size (box #f))
(define doc-fnt-size (box #f))
(define about-fnt-size (box #f))
(define report-fnt-size (box #f))
(define *min-font* 7)
(define *max-font* 16)
(define all-fonts-delta #f)

(define all-font-list
  (list button-fnt-size msg-fnt-size msg-b-fnt-size doc-fnt-size
        about-fnt-size report-fnt-size))
(define all-font-defaults '(10 12 13 11 10 11)) ; fonts in points
  
(define *font-init-file* ".jmq-fontsrc")

;; At startup initializing fonts
(define (initialize-fonts)
  (if (file-exists? *font-init-file*)
      (begin
        (display (format "Loading font init file ~a~n" *font-init-file*))
        (call-with-input-file *font-init-file*
          (lambda (in)
            (with-handlers
                ([exn:fail?
                  (lambda (exn)
                    (display (format "Loading init file failed. File corrupted!~n"))
                    (displayln "Setting default fonts")
                    (set-default-fonts))])
              (for-each
               (lambda (f v) (set-box! f (legal-font (string->number v))))
               all-font-list
               (read-init-file in))))))
      (begin
        (display
         (format "Font init file ~a not found!~nSetting default fonts.~n"
                 *font-init-file*))
        (set-default-fonts))))

(define (read-init-file in)
  (define (mk-font-lst lst)
    (let ((v (read-line in 'any)))
      (if (eof-object? v)
          lst
          (cons v (mk-font-lst lst)))))
  (mk-font-lst '()))

(define (legal-font size)
  (if (and (number? size) (exact? size) (>= size *min-font*) (<= size *max-font*))
      size
      (error 'legal-font)))

(define (truncate-font-size size)
  (cond ((> size *max-font*) *max-font*)
        ((< size *min-font*) *min-font*)
        (else size)))

(define (set-default-fonts)
  (for-each (lambda (f v) (set-box! f v)) all-font-list all-font-defaults))
        
;; When user changes font size in preferences menu
(define (update-all-fonts delta)
  (with-handlers ([exn:fail? (lambda (exn) (set-default-fonts))])
    (call-with-output-file *font-init-file*
      (lambda (out)
        (for-each
         (lambda (f) (set-box! f (truncate-font-size (+ delta (unbox f))))
           (writeln (unbox f) out))
         all-font-list))
      #:mode 'text #:exists 'replace)))
  
;;; Initialize!
(initialize-fonts)

;;; built in operations, print form and exec form
(define plus (cons '+ +))
(define minus (cons '- -))
(define mult (cons '* *))
(define div (cons '/ /))
(define comp> (cons '> >))
(define comp< (cons '< <))
(define comp= (cons '= =))
(define is-odd (cons 'odd odd?))
(define is-even (cons 'even even?))
(define fract (cons 'red-parts/all-parts /))
(define comp<=> '(<=>)) ; only show for comparisons. User has to input operation
(define odd/even '(odd/even)) ; show only
(define IQ '(iq)) ; show only
(define B/B/A '(bba))
(define pvalue '(pvalue)) ; show only
(define clock '(:)) ; show only
(define run cdr)
(define show car)

(define *n* 20) ; default number of problems
(define *max*table* 10) ; max size of multiplication table

(struct state (question problems mistakes) #:mutable)
(define *state* (state 1 *n* 0)) ; initialize question to 1, problems to *n*, mistakes to 0
(struct problem (x op y) #:mutable)
(define *problem* (problem #f #f #f))

;;; GUI part
;;; =================================================================

(define start-button-width 150)
(define start-button-height 30)

;;; fonts colors & message strings
;;; =================================================================

(define op-start-label "            ")
(define prompt-msg-label"Click one of Exercise buttons to run the exercises      ")
(define prompt-msg-label-again (string-append (string-trim prompt-msg-label) " again"))
(define input-label " ") ; 1 space to minimize input field???

(define message-font (make-object font%
                       (unbox msg-fnt-size) 'modern 'normal 'semibold))

(define message-bold-font
  (send the-font-list find-or-create-font
        (unbox msg-b-fnt-size); Size
        "Modern" ; Font face
        'default ; Font family 
        'normal ; Font style (italic...)
        'semibold)) ; Font weight

(define button-font
  (send the-font-list find-or-create-font
        (unbox button-fnt-size); Size
        "DejaVu Sans Mono" ; Font face
        'default ; Font family 
        'normal ; Font style (italic...)
        'semibold)) ; Font weight

;;; Text displayed in report canvas, instructions & about pane
(define text-lines (new text% [auto-wrap #t]))
(define doc-instructions (new text% [auto-wrap #t]))
(define about-text (new text% [auto-wrap #t]))

(define style-delta-font-doc-size
  (make-object style-delta% 'change-size (unbox doc-fnt-size)))
(define style-delta-font-doc1-family
  (make-object style-delta% 'change-family 'modern))
(define style-delta-font-doc1-weight
  (make-object style-delta% 'change-weight 'semibold))
(send doc-instructions change-style style-delta-font-doc-size)

(define style-delta-font-doc2-family
  (make-object style-delta% 'change-family 'roman))
(define style-delta-font-doc2-weight
  (make-object style-delta% 'change-weight 'normal))

(define style-delta-font-about-size
  (make-object style-delta% 'change-size (unbox about-fnt-size)))
(define style-delta-font-about-family
  (make-object style-delta% 'change-family 'roman))
(send about-text change-style style-delta-font-about-size)

(define style-delta-font-report-size
  (make-object style-delta% 'change-size (unbox report-fnt-size)))
(define style-delta-font-report-family
  (make-object style-delta% 'change-family 'modern))
(send text-lines change-style style-delta-font-report-size)
(send text-lines change-style style-delta-font-report-family)

;; Text color
;; ======================================================

;; red
(define style-delta-red (make-object style-delta% 
                          'change-normal-color))
;; black
(define style-delta-black (make-object style-delta% 
                            'change-normal-color))
;; blue
(define style-delta-blue (make-object style-delta% 
                           'change-normal-color))
;; green
(define style-delta-green (make-object style-delta% 
                            'change-normal-color))

(define (silence-colors)
  (send style-delta-red set-delta-foreground "red")
  (send style-delta-black set-delta-foreground "black")
  (send style-delta-blue set-delta-foreground "blue")
  (send style-delta-green set-delta-foreground "Forest Green")
  (void))
;; silence the object notifications
(silence-colors)

(define white-color (make-object color% "White"))

;; ===============================================================

;;; Changing all fonts up to +3 or -3
;; single font  (set-box! font size))

(define (change-font-size delta)
  (for-each (lambda (fnt) (set-box! fnt (+ (unbox fnt) delta)))
            all-font-list)) ; write this to the .jmq-fontrc file

;;; Main Window & geometry
;;; ================================================================

(define main-window (new frame%
                         [label "Jasna's math quiz"]
                         [width 790]
                         [height 600]
                         [alignment '(left top)]
                         [stretchable-width #t]
                         [stretchable-height #t]))

(define main-pane (new horizontal-pane%
                       [parent main-window]
                       [alignment '(left top)]
                       [min-width 790]
                       [min-height 600]
                       [vert-margin 0]
                       [horiz-margin 0]                         
                       [stretchable-width #t]
                       [stretchable-height #t]))

;;; vertical main panes

(define v-pane-left (new vertical-pane%
                         [parent main-pane]
                         [vert-margin 0]
                         [horiz-margin 0]
                         [alignment '(left top)]
                         [min-width 590]
                         [min-height 600]
                         [stretchable-width #t]
                         [stretchable-height #t]))

(define v-pane-right (new vertical-pane%
                          [parent main-pane]
                          [vert-margin 0]
                          [horiz-margin 0]
                          [alignment '(center top)]
                          [min-width 200]
                          [min-height 600]
                          [stretchable-width #t]
                          [stretchable-height #t]))

;;; horizontal panes 

(define h-pane-input (new horizontal-pane%
                          [parent v-pane-left]
                          [alignment '(left top)]
                          [min-width 570]
                          [min-height 40]
                          [vert-margin 5]
                          [horiz-margin 10]                         
                          [stretchable-width #t]
                          [stretchable-height #f]))

(define h-pane-prompt (new horizontal-pane%
                           [parent h-pane-input]
                           [alignment '(right center)]
                           [min-width 245]
                           [min-height 40]
                           [vert-margin 0]
                           [horiz-margin 0]                         
                           [stretchable-width #t]
                           [stretchable-height #f]))

(define h-pane-result (new horizontal-pane%
                           [parent h-pane-input]
                           [alignment '(left center)]
                           [min-width 340]
                           [min-height 40]
                           [vert-margin 0]
                           [horiz-margin 0]                         
                           [stretchable-width #t]
                           [stretchable-height #f]))

(define h-pane-status (new horizontal-pane%
                           [parent v-pane-left]
                           [alignment '(center top)]
                           [min-width 580]
                           [min-height 20]
                           [vert-margin 0]
                           [horiz-margin 5]                         
                           [stretchable-width #t]
                           [stretchable-height #f]))

(define h-pane-report (new horizontal-pane%
                           [parent v-pane-left]
                           [alignment '(center top)]
                           [min-width 582]
                           [min-height 530]
                           [vert-margin 0]
                           [horiz-margin 4]                         
                           [stretchable-width #t]
                           [stretchable-height #t]))

;;; vertical inner panes

(define v-pane-stop (new vertical-pane%
                         [parent v-pane-right]
                         [alignment '(center center)]
                         [min-width 200]
                         [min-height 40]
                         [vert-margin 0]
                         [horiz-margin 0]                         
                         [stretchable-width #f]
                         [stretchable-height #t]))

(define v-pane-start (new vertical-pane%
                          [parent v-pane-right]
                          [vert-margin 0]
                          [horiz-margin 0]
                          [alignment '(center top)]
                          [min-width 200]
                          [min-height 560]
                          [stretchable-width #f]
                          [stretchable-height #t]))

(define v-pane-pause (new vertical-pane%
                          [parent v-pane-start]
                          [vert-margin 0]
                          [horiz-margin 0]
                          [alignment '(center center)]
                          [min-width 200]
                          [min-height 60]
                          [stretchable-width #f]
                          [stretchable-height #t]))

(define v-start-arithmetic (new vertical-pane%
                                [parent v-pane-start]
                                [vert-margin 5]
                                [horiz-margin 5]
                                [alignment '(center top)]
                                [min-width 190]
                                [min-height 240]
                                [stretchable-width #f]
                                [stretchable-height #t]))

;;; giving it scrolling space for future expansion
(define v-start-popup (new vertical-panel%
                           [parent v-pane-start]
                           [vert-margin 7]
                           [horiz-margin 5]
                           [alignment '(center bottom)]
                           [spacing 2]
                           [min-width 190]
                           [min-height 256]
                           [style '(border auto-vscroll)]
                           [stretchable-width #f]
                           [stretchable-height #t]))

;;; Menu
;;; =================================================================
(define main-menu-bar (new menu-bar%
                           [parent main-window]))

;;; Setup menu

(define setup-menu (new menu%
                        [parent main-menu-bar]
                        [label "Setup"]))

(define set-n-exercises (new menu-item%
                             [label "Set number of Exercises"]
                             [parent setup-menu]
                             [callback
                              (lambda (mi e)
                                (send slider-n-dialog show #t))]))

(define set-+-level (new menu-item%
                         [label "Set + - difficulty level"]
                         [parent setup-menu]
                         [callback
                          (lambda (mi e)
                            (send slider-+-dialog show #t))]))

(define set-max-table (new menu-item%
                           [label "Set max factor for */ table"]
                           [parent setup-menu]
                           [callback
                            (lambda (mi e)
                              (send slider-10*-dialog show #t))]))

(define set-left-number (new menu-item%
                             [label "Set max size of numbers"]
                             [parent setup-menu]
                             [callback
                              (lambda (mi e)
                                (send slider-left-dialog show #t))]))

(define set-speed-% (new menu-item%
                         [label "Set % of inc/dec allotted time"]
                         [parent setup-menu]
                         [callback
                          (lambda (mi e)
                            (send slider-speed-dialog show #t))]))

(define set-/-precision (new menu-item%
                             [label "Set division precision"]
                             [parent setup-menu]
                             [callback
                              (lambda (mi e)
                                (send slider-precision-dialog show #t))]))

(define set-sequence-level (new menu-item%
                                [label "Set sequence difficulty level"]
                                [parent setup-menu]
                                [callback
                                 (lambda (mi e)
                                   (send slider-sequence-dialog show #t))]))

(define set-fraction-slices (new menu-item%
                                 [label "Set number of fraction slices"]
                                 [parent setup-menu]
                                 [callback
                                  (lambda (mi e)
                                    (send slider-fraction-dialog show #t))]))
                                
(define slider-n-dialog (new dialog%
                             [label "Set"]
                             [parent main-window]
                             [width 250]
                             [height 80]
                             [style '(close-button)]
                             [alignment '(right top)]))

(define slider-+-dialog (new dialog%
                             [label "Set"]
                             [parent main-window]
                             [width 250]
                             [height 80]
                             [style '(close-button)]
                             [alignment '(right top)]))

(define slider-10*-dialog (new dialog%
                               [label "Set"]
                               [parent main-window]
                               [width 250]
                               [height 80]
                               [style '(close-button)]
                               [alignment '(right top)]))

(define slider-left-dialog (new dialog%
                                [label "Set"]
                                [parent main-window]
                                [width 300]
                                [height 80]
                                [style '(close-button)]
                                [alignment '(right top)]))

(define slider-speed-dialog (new dialog%
                                 [label "Set"]
                                 [parent main-window]
                                 [width 250]
                                 [height 80]
                                 [style '(close-button)]
                                 [alignment '(right top)]))

(define slider-precision-dialog (new dialog%
                                     [label "Set"]
                                     [parent main-window]
                                     [width 250]
                                     [height 80]
                                     [style '(close-button)]
                                     [alignment '(right top)]))

(define slider-sequence-dialog (new dialog%
                                    [label "Set"]
                                    [parent main-window]
                                    [width 250]
                                    [height 80]
                                    [style '(close-button)]
                                    [alignment '(right top)]))

(define slider-fraction-dialog (new dialog%
                                    [label "Set"]
                                    [parent main-window]
                                    [width 250]
                                    [height 80]
                                    [style '(close-button)]
                                    [alignment '(right top)]))

(define exercises-slider (new slider%
                              [label "number of exercises"]
                              [min-value 10]
                              [max-value 30]
                              [parent slider-n-dialog]
                              [init-value 20]
                              [callback
                               (lambda (s e)
                                 (set! *n* (send s get-value)))]
                              [style '(vertical-label horizontal)]))

(define level-+-slider (new slider%
                            [label "difficulty level of + - exercises"]
                            [min-value 1]
                            [max-value 3]
                            [parent slider-+-dialog]
                            [init-value *level+-*]
                            [callback
                             (lambda (s e)
                               (set! *level+-* (send s get-value)))]
                            [style '(vertical-label horizontal)]))

(define max-table-slider (new slider%
                              [label "max factor * / table"]
                              [min-value 5]
                              [max-value 12]
                              [parent slider-10*-dialog]
                              [init-value 10]
                              [callback
                               (lambda (s e)
                                 (set! *max*table* (send s get-value)))]
                              [style '(vertical-label horizontal)]))

(define left-slider (new slider%
                         [label "max size of numbers"]
                         [min-value 100]
                         [max-value 900]
                         [parent slider-left-dialog]
                         [init-value *left-number*]
                         [callback
                          (lambda (s e)
                            (let ((val (send s get-value)))
                              (set! *left-number* val)
                              (set! *left-digit* (quotient val 100))))] ; was add1
                         [style '(vertical-label horizontal)]))

(define speed-slider (new slider%
                          [label "% of dec/inc allotted time"]
                          [min-value 50]
                          [max-value 150]
                          [parent slider-speed-dialog]
                          [init-value (* 100 *speed-factor*)]
                          [callback
                           (lambda (s e)
                             (set! *speed-factor* (/ (send s get-value) 100)))]
                          [style '(vertical-label horizontal)]))

(define precision-slider (new slider%
                              [label "(/) precision - digits after (.)"]
                              [min-value 1]
                              [max-value 7]
                              [parent slider-precision-dialog]
                              [init-value *exponent*]
                              [callback
                               (lambda (s e)
                                 (set! *exponent* (send s get-value)))]
                              [style '(vertical-label horizontal)]))

(define sequence-slider (new slider%
                             [label "Sequence difficulty level"]
                             [min-value 1]
                             [max-value 3]
                             (parent slider-sequence-dialog)
                             [init-value *sequence-difficulty*]
                             [callback
                              (lambda (s e)
                                (set! *sequence-difficulty* (send s get-value)))]
                             [style '(vertical-label horizontal)]))

(define fraction-slider (new slider%
                             [label "Number of fraction slices"]
                             [min-value 5]
                             [max-value 12]
                             (parent slider-fraction-dialog)
                             [init-value *max-slices*]
                             [callback
                              (lambda (s e)
                                (set! *max-slices* (send s get-value)))]
                             [style '(vertical-label horizontal)]))

(define clear-reports (new menu-item%
                           [label "Clear all reports"]
                           [parent setup-menu]
                           [callback
                            (lambda (mi e)
                              (send text-lines erase)
                              (send text-lines change-style
                                    style-delta-font-report-size)
                              (send text-lines change-style
                                    style-delta-font-report-family))]))

;;; ==================================================================

;;; Preferences menu

(define preferences-menu (new menu%
                              [parent main-menu-bar]
                              [label "Preferences"]))

;;; Font submenues
;;; All fonts

(define set-all-fonts (new menu-item%
                           [label "Inc/Dec all fonts"]
                           [parent preferences-menu]
                           [callback
                            (lambda (mi e)
                              (send slider-all-dialog show #t)
                              (if (or (not (number? all-fonts-delta)) ; user didn't change slider
                                      (zero? all-fonts-delta))
                                  (set! all-fonts-delta #f)
                                  (begin
                                    (update-all-fonts all-fonts-delta)
                                    (set! all-fonts-delta 0)
                                    (send set-all-fonts enable #f))))]))

(define slider-all-dialog (new dialog%
                               [label "Set"]
                               [parent main-window]
                               [width 250]
                               [height 80]
                               [style '(close-button)]
                               [alignment '(right top)]))

(define all-slider (new slider%
                        [label "Inc / Dec all font sizes
Restart program immediately after"]
                        [min-value -3]
                        [max-value +3]
                        [parent slider-all-dialog]
                        [init-value 0]
                        [callback
                         (lambda (s e)
                           (set! all-fonts-delta (send s get-value)))]
                        [style '(vertical-label horizontal)]))

;;; Individual fonts

(define set-status-msg-font (new menu-item%
                                 [label "Set status line font"]
                                 [parent preferences-menu]
                                 [callback
                                  (lambda (mi e)
                                    (send slider-status-dialog show #t))]))

(define slider-status-dialog (new dialog%
                                  [label "Set"]
                                  [parent main-window]
                                  [width 250]
                                  [height 80]
                                  [style '(close-button)]
                                  [alignment '(right top)]))

(define status-slider (new slider%
                           [label "Status line font size"]
                           [min-value *min-font*]
                           [max-value *max-font*]
                           [parent slider-status-dialog]
                           [init-value (unbox msg-fnt-size)]
                           [callback
                            (lambda (s e)
                              (set-box! msg-fnt-size (send s get-value))
                              (update-all-fonts 0))]
                           [style '(vertical-label horizontal)]))

(define set-input-font (new menu-item%
                            [label "Set input line font"]
                            [parent preferences-menu]
                            [callback
                             (lambda (mi e)
                               (send slider-input-dialog show #t))]))

(define slider-input-dialog (new dialog%
                                 [label "Set"]
                                 [parent main-window]
                                 [width 250]
                                 [height 80]
                                 [style '(close-button)]
                                 [alignment '(right top)]))

(define input-slider (new slider%
                          [label "Input line font size"]
                          [min-value *min-font*]
                          [max-value *max-font*]
                          [parent slider-input-dialog]
                          [init-value (unbox msg-b-fnt-size)]
                          [callback
                           (lambda (s e)
                             (set-box! msg-b-fnt-size (send s get-value))
                             (update-all-fonts 0))]
                          [style '(vertical-label horizontal)]))

(define set-doc-font (new menu-item%
                          [label "Set documentation font"]
                          [parent preferences-menu]
                          [callback
                           (lambda (mi e)
                             (send slider-doc-dialog show #t))]))

(define slider-doc-dialog (new dialog%
                               [label "Set"]
                               [parent main-window]
                               [width 250]
                               [height 80]
                               [style '(close-button)]
                               [alignment '(right top)]))

(define doc-slider (new slider%
                        [label "Documentation font size"]
                        [min-value *min-font*]
                        [max-value *max-font*]
                        [parent slider-doc-dialog]
                        [init-value (unbox doc-fnt-size)]
                        [callback
                         (lambda (s e)
                           (set-box! doc-fnt-size (send s get-value))
                           (update-all-fonts 0))]
                        [style '(vertical-label horizontal)]))

(define set-about-font (new menu-item%
                            [label "Set about font"]
                            [parent preferences-menu]
                            [callback
                             (lambda (mi e)
                               (send slider-about-dialog show #t))]))

(define slider-about-dialog (new dialog%
                                 [label "Set"]
                                 [parent main-window]
                                 [width 250]
                                 [height 80]
                                 [style '(close-button)]
                                 [alignment '(right top)]))

(define about-slider (new slider%
                          [label "About font size"]
                          [min-value *min-font*]
                          [max-value *max-font*]
                          [parent slider-about-dialog]
                          [init-value (unbox about-fnt-size)]
                          [callback
                           (lambda (s e)
                             (set-box! about-fnt-size (send s get-value))
                             (update-all-fonts 0))]
                          [style '(vertical-label horizontal)]))

(define set-report-font (new menu-item%
                             [label "Set report canvas font"]
                             [parent preferences-menu]
                             [callback
                              (lambda (mi e)
                                (send slider-report-dialog show #t))]))

(define slider-report-dialog (new dialog%
                                  [label "Set"]
                                  [parent main-window]
                                  [width 250]
                                  [height 80]
                                  [style '(close-button)]
                                  [alignment '(right top)]))

(define report-slider (new slider%
                           [label "Report canvas font size"]
                           [min-value *min-font*]
                           [max-value *max-font*]
                           [parent slider-report-dialog]
                           [init-value (unbox report-fnt-size)]
                           [callback
                            (lambda (s e)
                              (set-box! report-fnt-size (send s get-value))
                              (update-all-fonts 0))]
                           [style '(vertical-label horizontal)]))

(define set-button-font (new menu-item%
                             [label "Set button font"]
                             [parent preferences-menu]
                             [callback
                              (lambda (mi e)
                                (send slider-button-dialog show #t))]))

(define slider-button-dialog (new dialog%
                                  [label "Set"]
                                  [parent main-window]
                                  [width 250]
                                  [height 80]
                                  [style '(close-button)]
                                  [alignment '(right top)]))

(define button-slider (new slider%
                           [label "Button font size"]
                           [min-value *min-font*]
                           [max-value *max-font*]
                           [parent slider-button-dialog]
                           [init-value (unbox button-fnt-size)]
                           [callback
                            (lambda (s e)
                              (set-box! button-fnt-size (send s get-value))
                              (update-all-fonts 0))]
                           [style '(vertical-label horizontal)]))

;;; ===================================================================

;;; Lost & found menu

(define retrieve-menu (new menu%
                           [parent main-menu-bar]
                           [label "Lost and Found"]))

(define show-compare-window-menu (new menu-item%
                                      [label "Show Comparison Window"]
                                      [parent retrieve-menu]
                                      [callback
                                       (lambda (mi e)
                                         (when (eq? *exec-button* compare-button)
                                           (send input-dialog show #t)))]))

(define show-odd-even-window-menu (new menu-item%
                                       [label "Show Odd/Even Window"]
                                       [parent retrieve-menu]
                                       [callback
                                        (lambda (mi e)
                                          (when (eq? *exec-button* odd-even-button)
                                            (send odd-even-dialog show #t)))]))

(define show-sequence-window-menu (new menu-item%
                                       [label "Show Sequence Window"]
                                       [parent retrieve-menu]
                                       [callback
                                        (lambda (mi e)
                                          (when (eq? *exec-button* sequence-button)
                                            (send sequence-dialog show #t)))]))

(define show-bba-window-menu (new menu-item%
                                  [label "Show before between after Window"]
                                  [parent retrieve-menu]
                                  [callback
                                   (lambda (mi e)
                                     (when (eq? *exec-button* bba-button)
                                       (send bba-dialog show #t)))]))

(define show-pvalue-window-menu (new menu-item%
                                     [label "Show position value Window"]
                                     [parent retrieve-menu]
                                     [callback
                                      (lambda (mi e)
                                        (when (eq? *exec-button* pvalue-button)
                                          (send pvalue-dialog show #t)))]))

(define show-fraction-window-menu (new menu-item%
                                       [label "Show fractions Window"]
                                       [parent retrieve-menu]
                                       [callback
                                        (lambda (mi e)
                                          (when (eq? *exec-button* fraction-button)
                                            (send fraction-dialog show #t)))]))

(define show-clock-window-menu (new menu-item%
                                    [label "Show clock Window"]
                                    [parent retrieve-menu]
                                    [callback
                                     (lambda (mi e)
                                       (when (eq? *exec-button* clock-button)
                                         (send clock-dialog show #t)))]))

;;; Help menu

(define help-menu (new menu%
                       [parent main-menu-bar]
                       [label " Help "]))

(define menu-item-doc (new menu-item%
                           [label "Documentation"]
                           [parent help-menu]
                           [callback
                            (lambda (mi e)
                              (send doc-dialog show #t))]))

(define doc-dialog (new dialog%
                        [label "Math Quiz -- Instructions"]
                        [parent main-window]
                        [width 780]
                        [height 660]
                        [style '(close-button)]
                        [alignment '(right top)]))

(define doc-canvas (new editor-canvas%
                        [parent doc-dialog]
                        [editor doc-instructions]
                        [label "Instructions"]
                        [min-width 760]
                        [min-height 640]
                        [vert-margin 10]
                        [horiz-margin 10]
                        [style '(no-hscroll auto-vscroll no-focus)]))

(define menu-item-about (new menu-item%
                             [label "About Math Quiz"]
                             [parent help-menu]
                             [callback
                              (lambda (mi e)
                                (send about-dialog show #t))]))

(define about-dialog (new dialog%
                          [label "About Math Quiz"]
                          [parent main-window]
                          [width 540]
                          [height 520]
                          [style '(close-button)]
                          [alignment '(right top)]))

(define about-canvas (new editor-canvas%
                          [parent about-dialog]
                          [editor about-text]
                          [label "About"]
                          [min-width 520]
                          [min-height 500]
                          [vert-margin 10]
                          [horiz-margin 10]
                          [style '(no-hscroll auto-vscroll no-focus)]))

;;; Arithmetic problems
;;; ==================================================================                      

(define operation-msg (new message%
                           [parent h-pane-prompt]
                           [font message-bold-font]
                           [label op-start-label]
                           [vert-margin 10]
                           [horiz-margin 12]
                           [stretchable-width #f]
                           [stretchable-height #f]
                           [auto-resize #t]))

(define number-input (new text-field%
                          [parent h-pane-result]
                          [font message-bold-font]
                          [label "=  "]
                          [init-value input-label]
                          [enabled #t]
                          [min-width 150]
                          [min-height 30]
                          [vert-margin 6]
                          [horiz-margin 10]
                          [stretchable-width #f]
                          [stretchable-height #f]))

(define prompt-msg (new message%
                        [parent h-pane-status]
                        [label prompt-msg-label]
                        [font message-font]
                        [min-width 360]
                        [stretchable-width #t]
                        [vert-margin 2]
                        [horiz-margin 12]))
;;; ================================================================

;;; Comparison problems
;;; ================================================================

(define input-dialog (new frame%
                          [label "Comparison questions"]
                          [parent main-window]
                          [width 320]
                          [height 60]
                          [border 10]
                          [style '(no-resize-border)]
                          [alignment '(left center)]))

(define input-pane (new horizontal-pane%
                        [parent input-dialog]
                        [vert-margin 10]
                        [horiz-margin 10]
                        [alignment '(center center)]
                        [stretchable-width #t]
                        [stretchable-height #t]))

(define left-prompt (new message%
                         [parent input-pane]
                         [font message-bold-font]
                         [label ""]
                         [vert-margin 10]
                         [horiz-margin 10]
                         [stretchable-width #f]
                         [stretchable-height #f]
                         [auto-resize #t]))

(define comparison-input (new text-field%
                              [parent input-pane]
                              [font message-bold-font]
                              [label ""]
                              [init-value input-label]
                              [enabled #t]
                              [min-width 30]
                              [min-height 30]
                              [vert-margin 10]
                              [horiz-margin 10]
                              [stretchable-width #f]
                              [stretchable-height #f]))

(define right-prompt (new message%
                          [parent input-pane]
                          [font message-bold-font]
                          [label ""]
                          [vert-margin 10]
                          [horiz-margin 10]
                          [stretchable-width #f]
                          [stretchable-height #f]
                          [auto-resize #t]))

(define compare-button (new button%
                            [parent input-pane]
                            [label "Check"]
                            [font button-font]
                            [min-height start-button-height]
                            [enabled #f]
                            [vert-margin 10]
                            [horiz-margin 10]
                            [style '(border)]
                            [callback
                             (lambda (button event)
                               (let ((input (send comparison-input get-value)))
                                 (send comparison-input set-value "")
                                 (math-quiz-type (string-trim input))))]))

;;; ================================================================

;;; Odd - Even problems
;;; ================================================================

(define odd-even-dialog (new frame%
                             [label "Odd/Even questions"]
                             [parent main-window]
                             [width 280]
                             [height 60]
                             [border 10]
                             [style '(no-resize-border)]
                             [alignment '(left center)]))

(define odd-even-pane (new horizontal-pane%
                           [parent odd-even-dialog]
                           [min-width 280]
                           [min-height 60]
                           [vert-margin 0]
                           [horiz-margin 0]
                           [alignment '(left center)]
                           [stretchable-width #t]
                           [stretchable-height #t]))

(define odd-even-pane-left (new horizontal-pane%
                                [parent odd-even-pane]
                                [min-width 80]
                                [min-height 60]
                                [vert-margin 10]
                                [horiz-margin 10]
                                [alignment '(right center)]
                                [stretchable-width #t]
                                [stretchable-height #t]))

(define odd-even-pane-right (new horizontal-pane%
                                 [parent odd-even-pane]
                                 [min-width 200]
                                 [min-height 60]
                                 [vert-margin 10]
                                 [horiz-margin 10]
                                 [alignment '(left center)]
                                 [stretchable-width #t]
                                 [stretchable-height #t]))

(define odd-even-prompt (new message%
                             [parent odd-even-pane-left]
                             [font message-bold-font]
                             [label ""]
                             [vert-margin 10]
                             [horiz-margin 10]
                             [stretchable-width #f]
                             [stretchable-height #f]
                             [auto-resize #t]))

(define odd-even-radio-box (new radio-box%
                                [label "   "]
                                [choices '("odd" "even")]
                                [parent odd-even-pane-right]
                                [callback
                                 (lambda (rb e) (void))]))

(define odd-even-button (new button%
                             [parent odd-even-pane-right]
                             [label "Check"]
                             [font button-font]
                             [min-height start-button-height]
                             [enabled #f]
                             [vert-margin 10]
                             [horiz-margin 30]
                             [style '(border)]
                             [callback
                              (lambda (button event)
                                (let ((input
                                       (send odd-even-radio-box get-item-label
                                             (send odd-even-radio-box get-selection))))
                                  (math-quiz-type (string-trim input))))]))

;;; =================================================================

;;; Sequence (IQ) problems
;;; =================================================================

(define sequence-dialog (new frame%
                             [label "Sequence questions"]
                             [parent main-window]
                             [width 320]
                             [height 60]
                             [border 10]
                             [style '(no-resize-border)]
                             [alignment '(left center)]))

(define sequence-pane (new horizontal-pane%
                           [parent sequence-dialog]
                           [vert-margin 10]
                           [horiz-margin 10]
                           [alignment '(center center)]
                           [stretchable-width #t]
                           [stretchable-height #t]))

(define left-sequence-prompt (new message%
                                  [parent sequence-pane]
                                  [font message-bold-font]
                                  [label ""]
                                  [vert-margin 10]
                                  [horiz-margin 5]
                                  [stretchable-width #f]
                                  [stretchable-height #f]
                                  [auto-resize #t]))

(define sequence-input (new text-field%
                            [parent sequence-pane]
                            [font message-bold-font]
                            [label ""]
                            [init-value input-label]
                            [enabled #t]
                            [min-width 60]
                            [min-height 30]
                            [vert-margin 6]
                            [horiz-margin 5]
                            [stretchable-width #f]
                            [stretchable-height #f]))

(define right-sequence-prompt (new message%
                                   [parent sequence-pane]
                                   [font message-bold-font]
                                   [label ""]
                                   [vert-margin 10]
                                   [horiz-margin 5]
                                   [stretchable-width #f]
                                   [stretchable-height #f]
                                   [auto-resize #t]))

(define sequence-button (new button%
                             [parent sequence-pane]
                             [label "Check"]
                             [font button-font]
                             [min-height start-button-height]
                             [enabled #f]
                             [vert-margin 10]
                             [horiz-margin 20]
                             [style '(border)]
                             [callback
                              (lambda (button event)
                                (let ((input (send sequence-input get-value)))
                                  (send sequence-input set-value input-label)
                                  (math-quiz-type (string-trim input))))]))

;;; =================================================================

;;; Before Between After problems
;;; =================================================================

(define bba-dialog (new frame%
                        [label "Before Between After questions"]
                        [parent main-window]
                        [width 320]
                        [height 60]
                        [border 10]
                        [style '(no-resize-border)]
                        [alignment '(left center)]))

(define bba-pane (new horizontal-pane%
                      [parent bba-dialog]
                      [vert-margin 10]
                      [horiz-margin 10]
                      [alignment '(center center)]
                      [stretchable-width #t]
                      [stretchable-height #t]))

(define left-bba-prompt (new message%
                             [parent bba-pane]
                             [font message-bold-font]
                             [label ""]
                             [vert-margin 10]
                             [horiz-margin 5]
                             [stretchable-width #f]
                             [stretchable-height #f]
                             [auto-resize #t]))

(define bba-input (new text-field%
                       [parent bba-pane]
                       [font message-bold-font]
                       [label ""]
                       [init-value input-label]
                       [enabled #t]
                       [min-width 60]
                       [min-height 30]
                       [vert-margin 6]
                       [horiz-margin 5]
                       [stretchable-width #f]
                       [stretchable-height #f]))

(define right-bba-prompt (new message%
                              [parent bba-pane]
                              [font message-bold-font]
                              [label ""]
                              [vert-margin 10]
                              [horiz-margin 5]
                              [stretchable-width #f]
                              [stretchable-height #f]
                              [auto-resize #t]))

(define bba-button (new button%
                        [parent bba-pane]
                        [label "Check"]
                        [font button-font]
                        [min-height start-button-height]
                        [enabled #f]
                        [vert-margin 10]
                        [horiz-margin 20]
                        [style '(border)]
                        [callback
                         (lambda (button event)
                           (let ((input (send bba-input get-value)))
                             (send bba-input set-value input-label)
                             (math-quiz-type (string-trim input))))]))

;;; ==================================================================

;;; Position Value problems
;;; ==================================================================

(define pvalue-dialog (new frame%
                           [label "Position value questions"]
                           [parent main-window]
                           [width 300]
                           [height 120]
                           [border 10]
                           [style '(no-resize-border)]
                           [alignment '(left center)]))

(define pvalue-pane (new horizontal-pane%
                         [parent pvalue-dialog]
                         [vert-margin 10]
                         [horiz-margin 10]
                         [alignment '(center center)]
                         [stretchable-width #t]
                         [stretchable-height #t]))

(define pvalue-th-prompt (new message%
                              [parent pvalue-pane]
                              [font message-bold-font]
                              [label " "]
                              [vert-margin 10]
                              [horiz-margin 0]
                              [stretchable-width #f]
                              [stretchable-height #f]
                              [auto-resize #t]))

(define pvalue-hu-prompt (new message%
                              [parent pvalue-pane]
                              [font message-bold-font]
                              [label " "]
                              [vert-margin 10]
                              [horiz-margin 0]
                              [stretchable-width #f]
                              [stretchable-height #f]
                              [auto-resize #t]))

(define pvalue-te-prompt (new message%
                              [parent pvalue-pane]
                              [font message-bold-font]
                              [label " "]
                              [vert-margin 10]
                              [horiz-margin 0]
                              [stretchable-width #f]
                              [stretchable-height #f]
                              [auto-resize #t]))

(define pvalue-on-prompt (new message%
                              [parent pvalue-pane]
                              [font message-bold-font]
                              [label " "]
                              [vert-margin 10]
                              [horiz-margin 0]
                              [stretchable-width #f]
                              [stretchable-height #f]
                              [auto-resize #t]))

(define pvalue-radio-box (new radio-box%
                              [label "   "]
                              [choices '("ones" "tens" "hundreds" "thousands")]
                              [parent pvalue-pane]
                              [callback
                               (lambda (rb e) (void))]))

(define pvalue-button (new button%
                           [parent pvalue-pane]
                           [label "Check"]
                           [font button-font]
                           [min-height start-button-height]
                           [enabled #f]
                           [vert-margin 10]
                           [horiz-margin 30]
                           [style '(border)]
                           [callback
                            (lambda (button event)
                              (let ((input
                                     (send pvalue-radio-box get-selection)))
                                (math-quiz-type (number->string input))))]))

;;; ==================================================================

(define no-mouse-canvas%
  (class editor-canvas% ; The base class is editor-canvas%
    ; Define overriding method to handle mouse events
    (define/override (on-event event)
      (void)) ; do nothing
    (super-new)))

(define text-output (new no-mouse-canvas%
                         [parent h-pane-report]
                         [editor text-lines]
                         [label "Problem list"]
                         [min-width 540]
                         [min-height 530]
                         [vert-margin 6]
                         [horiz-margin 2]
                         [style '(no-hscroll auto-vscroll no-focus)]
                         [stretchable-width #t]
                         [stretchable-height #t]))

(define stop-button (new button%
                         [parent v-pane-stop]
                         [label "Stop"]
                         [font button-font]
                         [min-height start-button-height]
                         [enabled #f]
                         [vert-margin 10]
                         [horiz-margin 30]
                         [callback
                          (lambda (button event) (reset))]))

(define calc-button (new button%
                         [parent h-pane-result]
                         [label "Calculate"]
                         [font button-font]
                         [min-height start-button-height]
                         [enabled #f]
                         [vert-margin 6]
                         [horiz-margin 30]
                         [style '(border)]
                         [callback
                          (lambda (button event)
                            (let ((input (send number-input get-value)))
                              (send number-input set-value "")
                              (math-quiz-type (string-trim input))))]))

(define wiwi-button (new button%
                         [parent v-pane-pause]
                         [label "Pause"]
                         [font button-font]
                         [enabled #f]
                         [vert-margin 30]
                         [horiz-margin 6]
                         [min-width start-button-width]
                         [min-height start-button-height]
                         [callback
                          (lambda (button event)
                            (cond
                              (*wiwi* ; pause was on
                               (set! *wiwi* #f) ; enable for next set of exercises
                               (set! *wiwi-time* (- (current-seconds) *wiwi-start*))
                               (send text-lines insert
                                     (format "Break done   - time is running~n"))
                               (send text-lines change-style style-delta-black)
                               (send button enable #f)
                               ; resume exercise
                               (send *exec-button* enable #t)
                               (if (eq? *exec-button* calc-button)
                                   (send number-input enable #t)
                                   (disable/enable-input-fields #t))
                               (send button set-label "Pause"))
                              (else
                               (set! *wiwi* #t) ; set pause to on
                               (set! *wiwi-start* (current-seconds)) ; mark time
                               (send text-lines change-style style-delta-blue)
                               (send text-lines insert
                                     (format "On the break -  time suspended~n"))
                               ; disable all buttons and input fields
                               (send *exec-button* enable #f)
                               (send number-input enable #f)
                               (disable/enable-input-fields #f)
                               (send button set-label "Resume"))))]))
                            
(define start/-button (new button%
                           [parent v-start-arithmetic]
                           [label " / "]
                           [font button-font]
                           [min-width start-button-width]	 
                           [min-height start-button-height]
                           [enabled #t]
                           [vert-margin 6]
                           [horiz-margin 6]
                           [callback
                            (lambda (button event) (start/))]))

(define start100/-button (new button%
                              [parent v-start-arithmetic]
                              [label "100/10"]
                              [font button-font]
                              [min-width start-button-width]
                              [min-height start-button-height]
                              [enabled #t]
                              [vert-margin 6]
                              [horiz-margin 6]
                              [callback
                               (lambda (button event) (start100/))]))

(define start*-button (new button%
                           [parent v-start-arithmetic]
                           [label " * "]
                           [font button-font]
                           [min-width start-button-width]
                           [min-height start-button-height]
                           [enabled #t]
                           [vert-margin 6]
                           [horiz-margin 6]
                           [callback
                            (lambda (button event) (start*))]))

(define start10*-button (new button%
                             [parent v-start-arithmetic]
                             [label "10*10"]
                             [font button-font]
                             [min-width start-button-width]
                             [min-height start-button-height]
                             [enabled #t]
                             [vert-margin 6]
                             [horiz-margin 6]
                             [callback
                              (lambda (button event) (start10*))]))

(define start+-button (new button%
                           [parent v-start-arithmetic]
                           [label " + - "]
                           [font button-font]
                           [min-width start-button-width]
                           [min-height start-button-height]
                           [enabled #t]
                           [vert-margin 6]
                           [horiz-margin 6]
                           [callback
                            (lambda (button event) (start+-))]))

(define start<=>button (new button%
                            [parent v-start-popup]
                            [label "< = >"]
                            [font button-font]
                            [min-width start-button-width]
                            [min-height start-button-height]
                            [enabled #t]
                            [vert-margin 6]
                            [horiz-margin 6]
                            [callback
                             (lambda (button event) (start<=>))]))

(define start-odd/even-button (new button%
                                   [parent v-start-popup]
                                   [label "odd even"]
                                   [font button-font]
                                   [min-width start-button-width]
                                   [min-height start-button-height]
                                   [enabled #t]
                                   [vert-margin 6]
                                   [horiz-margin 6]
                                   [callback
                                    (lambda (button event) (start-odd/even))]))

(define start-sequence-button (new button%
                                   [parent v-start-popup]
                                   [label "sequence"]
                                   [font button-font]
                                   [min-width start-button-width]
                                   [min-height start-button-height]
                                   [enabled #t]
                                   [vert-margin 6]
                                   [horiz-margin 6]
                                   [callback
                                    (lambda (button event) (start-sequence))]))

(define start-bba-button (new button%
                              [parent v-start-popup]
                              [label " B B A "]
                              [font button-font]
                              [min-width start-button-width]
                              [min-height start-button-height]
                              [enabled #t]
                              [vert-margin 6]
                              [horiz-margin 6]
                              [callback
                               (lambda (button event) (start-bba))]))

(define start-pvalue-button (new button%
                                 [parent v-start-popup]
                                 [label "PosVal"]
                                 [font button-font]
                                 [min-width start-button-width]
                                 [min-height start-button-height]
                                 [enabled #t]
                                 [vert-margin 6]
                                 [horiz-margin 6]
                                 [callback
                                  (lambda (button event) (start-pvalue))]))

(define start-fraction-button (new button%
                                   [parent v-start-popup]
                                   [label "fractions"]
                                   [font button-font]
                                   [min-width start-button-width]
                                   [min-height start-button-height]
                                   [enabled #t]
                                   [vert-margin 6]
                                   [horiz-margin 6]
                                   [callback
                                    (lambda (button event) (start-fraction))]))

(define start-clock-button (new button%
                                [parent v-start-popup]
                                [label "clock"]
                                [font button-font]
                                [min-width start-button-width]
                                [min-height start-button-height]
                                [enabled #t]
                                [vert-margin 6]
                                [horiz-margin 6]
                                [callback
                                 (lambda (button event) (start-clock))]))

;;; ===============================================================

;;; callback functions
;;; ================================================================

(define (start+-)
  (case *level+-*
    ((1) (set! *time-factor* 1/2) ; minutes per problem
         (set! get-problem get-problem-2d+) ; setting the function
         (send text-lines insert
               (format "----------   limited plus exercises   ----------~n"))) 
    ((2) (set! *time-factor* 2/3)
         (set! get-problem get-problem-2d+-)
         (send text-lines insert
               (format "-------   limited plus minus exercises   -------~n")))
    ((3) (set! *time-factor* 3/2)
         (set! get-problem get-problem+-) ; setting the function         
         (send text-lines insert
               (format "-----------   plus minus exercises   -----------~n"))))
  (set! do-math do-math+) ; set arithmetic operation
  (set! setup setup-arithmetic) ; setup function
  (set! equal= =) ; setting simple equality test
  (set! *used-numbers* '())
  (send number-input enable #t)
  (start-quiz *n* 0))

(define (start10*)
  (set! *time-factor* 0.5) ; minutes per problem
  (send text-lines insert
        (format "------   multiplication table exercises   ------~n"))
  (set! do-math do-math+) ; set arithmetic operation
  (set! get-problem get-problem10*10)
  (set! setup setup-arithmetic) ; setup function
  (set! *used-numbers* '())
  (set! equal= =) ; setting simple equality test
  (set! *max-used-pairs* (kombinations *max*table* 2))
  (send number-input enable #t)
  (start-quiz *n* 0))

(define (start*)
  (set! *time-factor* 2) ; minutes per problem
  (send text-lines insert
        (format "---------   multiplication exercises   ---------~n"))
  (set! do-math do-math+) ; set arithmetic operation
  (set! get-problem get-problem*)
  (set! setup setup-arithmetic) ; setup function
  (set! *used-numbers* '())
  (set! equal= =) ; setting simple equality test
  (send number-input enable #t)
  (start-quiz *n* 0))

(define (start100/)
  (set! *time-factor* 1/2) ; minutes per problem
  (send text-lines insert
        (format "---------   division table exercises   ---------~n"))
  (set! do-math do-math+) ; set arithmetic operation
  (set! get-problem get-problem100/10)
  (set! setup setup-arithmetic) ; setup function
  (set! *used-numbers* '())
  (set! equal= =) ; setting simple equality test
  (set! *max-used-pairs* (kombinations *max*table* 2))
  (send number-input enable #t)
  (start-quiz *n* 0))

(define (start/)
  (set! *time-factor* 2) ; minutes per problem
  (send text-lines insert
        (format "------------   division exercises   ------------~n"))
  (set! do-math do-math+) ; set arithmetic operation
  (set! get-problem get-problem/)
  (set! setup setup-arithmetic) ; setup function
  (set! *used-numbers* '())
  (set! equal= approx=) ; setting approximation equal to 3 decimals
  (send number-input enable #t)
  (start-quiz *n* 0))

(define (start<=>)
  (set! *time-factor* 1/3) ; minutes per problem
  (send text-lines insert
        (format "-----------   comparison exercises   -----------~n"))
  (set! do-math do-math>) ; set non arithmetic operation
  (set! get-problem get-problem<=>)
  (set! setup setup-comparison) ; setup function
  (set! *used-numbers* '())
  (send input-dialog create-status-line)
  (send input-dialog set-status-text "> = <")
  (send input-dialog show #t)
  (send comparison-input enable #t)
  (start-quiz *n* 0))

(define (start-odd/even)
  (set! *time-factor* 1/5) ; minutes per problem
  (send text-lines insert
        (format "------------   odd even exercises   ------------~n"))
  (set! do-math do-math-odd/even) ; set non arithmetic operation
  (set! get-problem get-problem-odd-even)
  (set! setup setup-odd-even) ; setup function
  (set! *used-numbers* '())
  (send odd-even-dialog show #t)
  (start-quiz *n* 0))

(define (start-sequence)
  (case *sequence-difficulty*
    ((1) (set! *time-factor* 1))
    ((2) (set! *time-factor* 2))
    ((3) (set! *time-factor* 3))
    (else (error '*sequence-difficulty*)))
  (send text-lines insert
        (format "------------   sequence exercises   ------------~n"))
  (set! equal= =)
  (set! do-math do-math-sequence) ; set non arithmetic operation
  (set! get-problem get-problem-sequence)
  (set! setup setup-sequence) ; setup function
  (set! *used-numbers* '())
  (set! *max-used-pairs* 30) ; max number of exercises allowed
  (send sequence-dialog show #t)
  (send sequence-input enable #t)
  (start-quiz *n* 0))

(define (start-bba)
  (set! *time-factor* 1/2) ; minutes per problem
  (send text-lines insert
        (format "------   before between after exercises   ------~n"))
  (send bba-dialog create-status-line)
  (set! equal= =)
  (set! do-math do-math-bba) ; set non arithmetic operation
  (set! get-problem get-problem-bba)
  (set! setup setup-bba) ; setup function
  (set! *used-numbers* '())
  (set! *max-used-pairs* 30) ; max number of exercises allowed
  (send bba-dialog show #t)
  (send bba-input enable #t)
  (start-quiz *n* 0))

(define (start-pvalue)
  (set! *time-factor* 1/5) ; minutes per problem
  (send text-lines insert
        (format "---------   position value exercises   ---------~n"))
  (set! equal= =)
  (set! do-math do-math-pvalue) ; set non arithmetic operation
  (set! get-problem get-problem-pvalue)
  (set! setup setup-pvalue) ; setup function
  (set! *used-numbers* '())
  (set! *max-used-pairs* 90)
  (send pvalue-dialog show #t)
  (start-quiz *n* 0))

(define (start-fraction)
  (set! *time-factor* 1/3) ; minutes per problem
  (send text-lines insert
        (format "------------   fraction exercises   ------------~n"))
  (set! equal= =)
  (set! do-math do-math-fraction) ; set non arithmetic operation
  (send fraction-dialog create-status-line)
  (send fraction-dialog set-status-text "red-slices/all-slices")
  (set! get-problem get-problem-fraction)
  (set! setup setup-fraction) ; setup function
  (set! *used-numbers* '())
  (set! *max-used-pairs* (kombinations *max-slices* 2))
  (send fraction-dialog show #t)
  (send fraction-input enable #t)
  (start-quiz *n* 0))

(define (start-clock)
  (set! *time-factor* 1/2) ; minutes per problem
  (send text-lines insert
        (format "--------------   clock exercises   -------------~n"))
  (set! equal= clock=)
  (set! do-math do-math-clock) ; set non arithmetic operation
  (send clock-dialog create-status-line)
  (send clock-dialog set-status-text "hr:mn")
  (set! get-problem get-problem-clock)
  (set! setup setup-clock) ; setup function
  (set! *used-numbers* '())
  ;(set! *max-used-pairs* (kombinations *max-slices* 2))
  (send clock-dialog show #t)
  (send clock-input enable #t)
  (start-quiz *n* 0))

(define (disable/enable-start-buttons t/f)
  (for-each (lambda (b) (send b enable t/f))
            (list start+-button start10*-button start*-button
                  start100/-button start/-button start<=>button
                  start-odd/even-button start-sequence-button
                  start-bba-button start-pvalue-button start-fraction-button
                  start-clock-button)))

(define (disable/enable-set/font-menu t/f)
  (for-each (lambda (m) (send m enable t/f))
            (list set-n-exercises set-+-level set-max-table set-left-number
                  set-speed-% set-/-precision set-sequence-level set-fraction-slices
                  clear-reports set-all-fonts set-status-msg-font set-input-font
                  set-doc-font set-about-font set-report-font
                  set-button-font)))

(define (disable/enable-popup-window-menu t/f)
  (for-each (lambda (menu) (send menu enable t/f))
            (list show-compare-window-menu show-odd-even-window-menu
                  show-sequence-window-menu show-bba-window-menu
                  show-pvalue-window-menu show-fraction-window-menu
                  show-clock-window-menu)))

(define (disable/enable-dialog-show t/f)
  (for-each (lambda (window) (send window show t/f))
            (list odd-even-dialog input-dialog sequence-dialog bba-dialog
                  pvalue-dialog fraction-dialog clock-dialog)))

(define (disable/enable-input-fields t/f)
  (for-each (lambda (input) (send input enable t/f))
            (list comparison-input sequence-input bba-input fraction-input
                  clock-input)))

;;; Math quiz part
;;; ===============================================================

(define (start-quiz [n (state-problems *state*)] [err 0])
  (send stop-button enable #t)
  (disable/enable-start-buttons #f)
  (disable/enable-set/font-menu #f)
  (send number-input set-value "")
  (initialize-state n err)
  (when (>= n *min-break-ex*) ; if less than 10, probably just a penalty run. pause not permitted
    (send wiwi-button enable #t))
  (set! *wiwi* #f)
  (set! *wiwi-time* 0) ; time used for toilet :-)
  (set! *time-start* (current-seconds))
  (set! *allowed-time* (exact-ceiling (* n *time-factor* *speed-factor*)))
  (send text-lines change-style style-delta-blue)
  (send text-lines insert (msg1 n))
  (send text-lines change-style style-delta-black)
  (send text-lines insert (msg-separator))
  (send text-output set-editor text-lines)
  (setup))

(define (setup-arithmetic)
  (set! *exec-button* calc-button)
  (send show-compare-window-menu enable #f)
  (send calc-button enable #t)
  (send number-input enable #t)
  (send prompt-msg set-label
        (msg3 (state-question *state*) (state-problems *state*) (running-time)))
  (get-problem)
  (send operation-msg set-label (msg8 (problem-x *problem*)
                                      (show (problem-op *problem*))
                                      (problem-y *problem*))))

(define (setup-comparison)
  (set! *exec-button* compare-button)
  (send show-compare-window-menu enable #t)
  (send compare-button enable #t)
  (send number-input enable #f)
  (send prompt-msg set-label
        (msg3 (state-question *state*) (state-problems *state*) (running-time)))
  (get-problem)
  (send left-prompt set-label (number->string (problem-x *problem*)))
  (send right-prompt set-label (number->string (problem-y *problem*))))

(define (setup-odd-even)
  (set! *exec-button* odd-even-button)
  (send show-odd-even-window-menu enable #t)
  (send odd-even-button enable #t)
  (send number-input enable #f)
  (send prompt-msg set-label
        (msg3 (state-question *state*) (state-problems *state*) (running-time)))
  (get-problem)
  (send odd-even-prompt set-label (number->string (problem-x *problem*))))

(define (setup-sequence)
  (set! *exec-button* sequence-button)
  (set! get-sequence (choose-sequence-level))
  (send show-sequence-window-menu enable #t)
  (send sequence-button enable #t)
  (send number-input enable #f)
  (send prompt-msg set-label
        (msg3 (state-question *state*) (state-problems *state*) (running-time)))
  (get-problem)
  (send left-sequence-prompt set-label
        (format "~a  ~a  ~a"
                (first (problem-x *problem*))
                (second (problem-x *problem*))
                (third (problem-x *problem*))))
  (send right-sequence-prompt set-label
        (number->string (first (problem-y *problem*)))))

(define (setup-bba)
  (set! *exec-button* bba-button)
  (send show-bba-window-menu enable #t)
  (send bba-button enable #t)
  (send number-input enable #f)
  (send prompt-msg set-label
        (msg3 (state-question *state*) (state-problems *state*) (running-time)))
  (send left-bba-prompt set-label input-label) ; clearing old prompt
  (send right-bba-prompt set-label input-label)  
  (get-problem)
  (case (show (problem-op *problem*))
    ((after) 
     (send left-bba-prompt set-label (number->string (first (problem-x *problem*)))))
    ((between)
     (send left-bba-prompt set-label (number->string (first (problem-x *problem*))))
     (send right-bba-prompt set-label (number->string (third (problem-x *problem*)))))
    ((before)
     (send right-bba-prompt set-label (number->string (third (problem-x *problem*)))))
    (else (error 'setup-bba))))

(define (setup-pvalue)
  (set! *exec-button* pvalue-button)
  (send show-pvalue-window-menu enable #t)
  (send pvalue-button enable #t)
  (send pvalue-th-prompt set-color "black")
  (send pvalue-hu-prompt set-color "black")
  (send pvalue-te-prompt set-color "black")
  (send pvalue-on-prompt set-color "black")
  (send number-input enable #f)
  (send prompt-msg set-label
        (msg3 (state-question *state*) (state-problems *state*) (running-time)))
  (get-problem)
  (send pvalue-th-prompt set-label (first (problem-x *problem*)))
  (send pvalue-hu-prompt set-label (second (problem-x *problem*)))
  (send pvalue-te-prompt set-label (third (problem-x *problem*)))
  (send pvalue-on-prompt set-label (fourth (problem-x *problem*)))
  (case (show (problem-op *problem*))
    ((thousands) 
     (send pvalue-th-prompt set-color "red"))
    ((hundreds)
     (send pvalue-hu-prompt set-color "red"))
    ((tens)
     (send pvalue-te-prompt set-color "red"))
    ((ones)
     (send pvalue-on-prompt set-color "red"))
    (else (error 'setup-pvalue))))

(define (setup-fraction)
  (set! *exec-button* fraction-button)
  (send show-fraction-window-menu enable #t)
  (send fraction-button enable #t)
  (send number-input enable #f)
  (send prompt-msg set-label
        (msg3 (state-question *state*) (state-problems *state*) (running-time)))
  (get-problem)
  (send fraction-canvas on-paint))

(define (setup-clock)
  (set! *exec-button* clock-button)
  (send show-clock-window-menu enable #t)
  (send clock-button enable #t)
  (send number-input enable #f)
  (send prompt-msg set-label
        (msg3 (state-question *state*) (state-problems *state*) (running-time)))
  (get-problem)
  (send clock-canvas on-paint))

(define (choose-sequence-level)
  (case *sequence-difficulty*
    ((1) get-sequence-1)
    ((2) get-sequence-2)
    ((3) get-sequence-3)
    (else (error 'get-sequence-level))))

(define (initialize-state problems errors)
  (set-state-question! *state* 1)
  (set-state-problems! *state* problems)
  (set-state-mistakes! *state* errors))

(define (push-used! x)
  (unless (memv x *used-numbers*)
    (set! *used-numbers* (cons x *used-numbers*))))

(define (check-used x op y)
  (cond ((and (member x *used-numbers* =) (member y *used-numbers* =))
         (when (>= (length *used-numbers*) 85)
           (set! *used-numbers* '()))
         (get-problem)) ; look for new set of numbers
        (else
         (initialize-problem x op y)
         (push-used! x)
         (push-used! y))))

(define (check-used-pairs x op y [dividend #f])
  (let ((xy (cons x y)) (yx (cons y x)))
    (if (or (member xy *used-numbers* equal?)
            (member yx *used-numbers* equal?))
        (begin
          (check-overflow) ; free used number pairs if all used
          (get-problem)) ; look for new set of numbers
        (begin
          (if dividend
              (initialize-problem dividend op y)
              (initialize-problem x op y))
          (push-used! xy)
          (push-used! yx)))))

(define (check-used-sequence x op y)
  (if (member x *used-numbers* equal?)
      (begin
        (check-overflow) ; free used number pairs if all used
        (get-problem)) ; look for new set of numbers
      (begin
        (initialize-problem x op y)
        (push-used! x))))

(define (check-overflow)
  (when (>= (length *used-numbers*) *max-used-pairs*)
    (set! *used-numbers* '()))) ; free used pairs of numbers
        
(define (fact n)
  (if (zero? n)
      1
      (* n (fact (sub1 n)))))

(define (kombinations n m)
  (quotient (fact n) (fact (- n m))))

;;; Setting problems for different Start buttons (+/-), (10*10), (*), (100/10), (/) , (<=>)...

(define (get-problem-2d+)
  (define (get-left)
    (let ((left (random 13 (random 25 100))))
      (if (memv left *used-numbers*)
          (get-left)
          left)))
  (define (get-right left)
    (let* ((ones-l (modulo left 10))
           (ones-r (random 0 (- 10 ones-l)))
           (tens (random 1 (- 11 (quotient left 10)))) ; limiting sum to 112
           (right (+ (* tens 10) ones-r)))
      right))
  (let* ((left (get-left))
         (right (get-right left)))
    (check-used left plus right)))

(define (get-problem-2d+-)
  (define (get-left)
    (let ((left (random 23 100)))
      (if (memv left *used-numbers*)
          (get-left)
          left)))
  (define (get-right left)
    (let* ((ones-l (modulo left 10))
           (ones-r (random 0 (add1 ones-l)))
           (tens (random 1 (/ (- left ones-l) 10)))
           (right (+ (* tens 10) ones-r)))
      right))
  (let* ((op-list (list minus plus minus))
         (op (list-ref op-list (random (length op-list)))))
    (if (eq? (show op) '-)
        (let* ((left (get-left))
               (right (get-right left)))
          (check-used left op right))
        (get-problem-2d+))))

(define (get-problem+-)
  (let* ((op-list (list minus plus minus plus minus))
         (op (list-ref op-list (random 0 (length op-list)))) ; minus weighted 3/5
         (x (get-left-number (random 13 *left-number*) op))) ; min 2 for (-)
    (let ((y (if (eq? (show op) '+)
                 (get-right-number+ x (random 1 *left-number*))
                 (get-right-number- x (random 1 x)))))
      (check-used x op y))))

(define (get-problem10*10)
  (let ((op mult)
        (x (add1 (random 1 *max*table*))) ; range between 2 to *max*table*
        (y (add1 (random 1 *max*table*)))) ; range between 2 to *max*table*
    (check-used-pairs x op y)))

(define (get-problem*)
  (let ((op mult)
        (x (random 2 *left-number*)) ; range between 2 to *left-number* - 1
        (y (random 2 *left-number*))) ; range between 2 to *left-number* - 1
    (check-used x op y)))

(define (get-problem100/10)
  (let* ((op div)
         (x (add1 (random 1 *max*table*))) ; range between 2 to *max*table*
         (y (add1 (random 1 *max*table*))) ; range between 2 to *max*table*
         (dividend (* x y)))
    (check-used-pairs x op y dividend)))

(define (get-problem/)
  (let* ((op div)
         ; same algorithm as for (-)
         (x1 (get-left-number- (random 0 *left-digit*)))
         (x (if (<= x1 2) (add1 x1) x1))
         ; choose randomly from 2 up to Min of left number and (x)
         (y (random 2 (min *left-number* x))))
    (check-used x op y)))

(define (get-problem<=>)
  (let* ((op comp<=>) ; not a real operation
         (x (random 13 *left-number*))
         (rn-list (list (random 13 *left-number*)
                        (random 13 *left-number*)
                        (random 13 *left-number*)
                        x ; making sure "=" has at least 1/5 chance
                        (random 13 *left-number*)
                        (random 13 *left-number*)))
         (y (list-ref rn-list (random 0 (length rn-list)))))
    (check-used x op y)))

(define (get-problem-odd-even)
  (let* ((op odd/even) ; not a real operation
         (rn-list (list (random 73 *left-number*)
                        (random 53 *left-number*)
                        (random 0 50)
                        (random 0 10)))
         (x (list-ref rn-list (random 0 (length rn-list)))))
    (check-used x op x)))

(define (get-problem-sequence)
  (let* ((op IQ) ; not a real operation
         (seq (up-down (get-sequence)))
         (x (take seq 4))
         (y (list(last seq))))
    (check-used-sequence x op y)))

(define (get-problem-bba)
  (let* ((start-lst (list 39 49 59 69 79 89
                          (random 13 *left-number*)
                          (random 13 *left-number*)
                          (random 13 *left-number*)
                          (random 13 *left-number*)
                          38 48 58 68 78 88
                          (random 13 *left-number*)
                          (random 13 *left-number*)
                          (random 13 *left-number*)
                          (random 13 *left-number*)))
         (start-n (list-ref start-lst (random (length start-lst))))
         (x (list start-n (+ 1 start-n) (+ 2 start-n)))
         (y  (+ start-n 1))
         (op (choose-bba-op))) ; for show
    (send bba-dialog set-status-text (symbol->string (show op)))
    (check-used-sequence x op y)))

(define (get-problem-pvalue)
  (let* ((x (map number->string (get-pvalue-number 4 '())))
         (y (random (length x)))
         (op (choose-pvalue-op y))) ; for show
    (check-used-sequence x op y)))

(define (get-problem-fraction)
  (let* ((denominator (random 2 (add1 *max-slices*)))
         (nominator (random 1 denominator))
         (op fract))
    (check-used-pairs nominator op denominator)))

(define (get-problem-clock)
  (let* ((m-list (list (* (random 12) 5) (random 60)))
         (minute (list-ref m-list (random (length m-list)))) 
         (hour (random 12))
         (op clock))
    (check-used hour op minute)))

(define (get-pvalue-number n acc)
  (let ((digit (random 1 10)))
    (cond ((zero? n) acc)
          ((memq digit acc) (get-pvalue-number n acc))
          (else (get-pvalue-number (sub1 n) (cons digit acc))))))

(define (choose-pvalue-op y)
  (case y
    ((0) '(ones 0))
    ((1) '(tens 1))
    ((2) '(hundreds 2))
    ((3) '(thousands 3))
    (else (error 'choose-pvalue))))

(define (choose-bba-op)
  (case (random 5)
    ((0 1) '(before))
    ((2) '(between))
    ((3 4) '(after))
    (else (error 'choose-bba-op))))

(define (initialize-problem x op y)
  (set-problem-x! *problem* x)
  (set-problem-op! *problem* op)
  (set-problem-y! *problem* y))

;;; approximately equal (for division) - only counts *exponent* digits after decimal point
;;; returns number-string truncated to decimal places (*exponent*)
(define (approx= input calc)
  (let ((in-int (exact-floor (* input (expt 10 *exponent*))))
        (calc-int (exact-floor (* calc (expt 10 *exponent*)))))
    (cond
      ((= in-int calc-int) (truncate-result input))
      ((> (abs (- calc-int in-int)) 1) #f)
      (else
       (compare (string->list (number->string input))
                (string->list (number->string (exact->inexact calc)))
                input)))))

(define (compare input calc in)
  (cond ((and (char=? (car input) #\.) (char=? (car calc) #\.))
         (compare-decimal (cdr input) (cdr calc) *exponent* in))
        ((char=? (car input) (car calc)) (compare (cdr input) (cdr calc) in))
        (else #f)))

(define (compare-decimal input calc n in)
  (cond
    ((zero? n) (truncate-result in))
    ((or (empty? input) (empty? calc)) #f)
    ((char=? (car input) (car calc))
     (compare-decimal (cdr input) (cdr calc) (sub1 n) in))
    (else #f)))

(define (truncate-result input)
  (let* ((in-whole-len (string-length (number->string (exact-floor input))))
         (in-str (number->string input))
         (in-len (string-length in-str)))
    (substring in-str 0 (min (+ in-whole-len 1 *exponent*) in-len))))

(define (clock= hour minute h m)
  (and (= hour h) (= minute m)))

(define (do-math+ string x op y out) 
  (let* ((num (string->number string))
         (result (and num (equal= num ((run op) x y)))))
    (cond
      (result
       (if (string? result)
           (send text-lines insert (msg6 x op y result))
           (send text-lines insert (msg6 x op y num)))
       (set-state-question! *state* (add1 (state-question *state*)))
       (when (<= (state-question *state*) (state-problems *state*))
         (setup-arithmetic))) ; continue with next exercise
      (num
       (bell)
       (set-state-mistakes! *state* (add1 (state-mistakes *state*)))
       (send text-lines change-style style-delta-red)
       (send text-lines insert (msg7 x op y num))
       (send text-lines change-style style-delta-black))
      (else
       (bell)
       (send text-lines change-style style-delta-green)
       (send text-lines insert (msg9 string))
       (send text-lines change-style style-delta-black))))
  (send out set-editor text-lines))  

(define (do-math> string x op y out) 
  (let* ((op (check-<=> string))
         (result (and op ((run op) x y))))
    (cond
      (result
       (send text-lines insert (msg6 x op y (display-<=> string)))
       (set-state-question! *state* (add1 (state-question *state*)))
       (when (<= (state-question *state*) (state-problems *state*))
         (setup-comparison))) ; continue with next exercise
      (op
       (bell) ; just testing (play-sound "switch.oga" #f)
       (set-state-mistakes! *state* (add1 (state-mistakes *state*)))
       (send text-lines change-style style-delta-red)
       (send text-lines insert (msg7 x op y (display-<=> string)))
       (send text-lines change-style style-delta-black))
      (else
       (bell)
       (send text-lines change-style style-delta-green)
       (send text-lines insert (msg9 string))
       (send text-lines change-style style-delta-black))))
  (send out set-editor text-lines))  

(define (do-math-odd/even string x op y out)
  (let* ((op (check-odd/even string))
         (result (and op ((run op) x))))
    (cond
      (result
       (send text-lines insert (msg-odd/even x op y ""))
       (set-state-question! *state* (add1 (state-question *state*)))
       (when (<= (state-question *state*) (state-problems *state*))
         (setup-odd-even))) ; continue with next exercise
      (op
       (bell) ; just testing (play-sound "switch.oga" #f)
       (set-state-mistakes! *state* (add1 (state-mistakes *state*)))
       (send text-lines change-style style-delta-red)
       (send text-lines insert (msg-odd/even x op y "not"))
       (send text-lines change-style style-delta-black))
      (else
       (bell)
       (send text-lines change-style style-delta-green)
       (send text-lines insert (msg9 string))
       (send text-lines change-style style-delta-black))))
  (send out set-editor text-lines))

(define (do-math-sequence string x op y out)
  (let* ((missing-number-input (string->number string))
         (sequence-number (fourth x))
         (result (and missing-number-input
                      (equal= missing-number-input sequence-number))))
    (cond
      (result
       (send text-lines insert (msg-sequence x op y string))
       (set-state-question! *state* (add1 (state-question *state*)))
       (when (<= (state-question *state*) (state-problems *state*))
         (setup-sequence))) ; continue with next exercise
      (missing-number-input
       (bell)
       (set-state-mistakes! *state* (add1 (state-mistakes *state*)))
       (send text-lines change-style style-delta-red)
       (send text-lines insert (msg-sequence x op y string))
       (send text-lines change-style style-delta-black))
      (else
       (bell)
       (send text-lines change-style style-delta-green)
       (send text-lines insert (msg9 string))
       (send text-lines change-style style-delta-black))))
  (send out set-editor text-lines))  

(define (do-math-bba string x op y out)
  (let* ((missing-number-input (string->number string))
         (bba-number y)
         (result (and missing-number-input
                      (equal= missing-number-input bba-number))))
    (cond
      (result
       (send text-lines insert (msg-bba x "" op y string))
       (set-state-question! *state* (add1 (state-question *state*)))
       (when (<= (state-question *state*) (state-problems *state*))
         (setup-bba))) ; continue with next exercise
      (missing-number-input
       (bell)
       (set-state-mistakes! *state* (add1 (state-mistakes *state*)))
       (send text-lines change-style style-delta-red)
       (send text-lines insert (msg-bba x "not" op y string))
       (send text-lines change-style style-delta-black))
      (else
       (bell)
       (send text-lines change-style style-delta-green)
       (send text-lines insert (msg9 string))
       (send text-lines change-style style-delta-black))))
  (send out set-editor text-lines))

(define (do-math-pvalue string x op y out)
  (let* ((reply-input (string->number string))
         (pos-number y)
         (result (and reply-input
                      (equal= reply-input pos-number))))
    (cond
      (result
       (send text-lines insert (msg-pvalue result string))
       (set-state-question! *state* (add1 (state-question *state*)))
       (when (<= (state-question *state*) (state-problems *state*))
         (setup-pvalue))) ; continue with next exercise
      (reply-input
       (bell)
       (set-state-mistakes! *state* (add1 (state-mistakes *state*)))
       (send text-lines change-style style-delta-red)
       (send text-lines insert (msg-pvalue result string))
       (send text-lines change-style style-delta-black))
      (else
       (bell)
       (send text-lines change-style style-delta-green)
       (send text-lines insert (msg9 string))
       (send text-lines change-style style-delta-black))))
  (send out set-editor text-lines))

(define (do-math-fraction string x op y out)
  (let* ((fraction-input (string->number string))
         (fraction-number ((run op) x y))
         (result (and fraction-input
                      (equal= fraction-input fraction-number))))
    (cond
      (result
       (send text-lines insert (msg6 x op y string))
       (set-state-question! *state* (add1 (state-question *state*)))
       (when (<= (state-question *state*) (state-problems *state*))
         (setup-fraction))) ; continue with next exercise
      (fraction-input
       (bell)
       (set-state-mistakes! *state* (add1 (state-mistakes *state*)))
       (send text-lines change-style style-delta-red)
       (send text-lines insert (msg7 x op y string))
       (send text-lines change-style style-delta-black))
      (else
       (bell)
       (send text-lines change-style style-delta-green)
       (send text-lines insert (msg9 string))
       (send text-lines change-style style-delta-black))))
  (send out set-editor text-lines))

(define (do-math-clock string x op y out)
  (let* ((time-list (string->time-lst string))
         (result (and (cons? time-list)
                      (equal= (first time-list) (second time-list) x y))))
    (cond
      (result
       (send text-lines insert (msg6 x op y string))
       (set-state-question! *state* (add1 (state-question *state*)))
       (when (<= (state-question *state*) (state-problems *state*))
         (setup-clock))) ; continue with next exercise
      ((cons? time-list)
       (bell)
       (set-state-mistakes! *state* (add1 (state-mistakes *state*)))
       (send text-lines change-style style-delta-red)
       (send text-lines insert (msg7 x op y string))
       (send text-lines change-style style-delta-black))
      (else
       (bell)
       (send text-lines change-style style-delta-green)
       (send text-lines insert (msg9 string))
       (send text-lines change-style style-delta-black))))
  (send out set-editor text-lines))

(define (string->time-lst str)
  (let ((lst (string-split str ":")))
    (cond ((not (= (length lst) 2)) str)
          ((or (not (string->number (first lst)))
               (not (string->number (second lst)))) str)
          (else
           (let ((h (modulo (string->number (first lst)) 12))
                 (m (string->number (second lst))))
             (list h m))))))       

;;; driving logic

(define (math-quiz-type input) ; dispatch on do-math function
  (do-math input (problem-x *problem*) (problem-op *problem*)
           (problem-y *problem*) text-output)
  (math-quiz input))

(define (math-quiz input)
  "Ask the user a series of math problems. Mistakes trigger additional problems.
If alloted time for solving problems is exceeded, then additional problems are given."
  (when (> (state-question *state*) (state-problems *state*))
    (displayln "Questions done!") ; just for testing
    (let ((run-time (running-time))
          (mistakes (state-mistakes *state*)))
      (send text-lines change-style style-delta-green)
      (send text-lines insert (msg2 mistakes))
      (send text-lines change-style style-delta-black)
      (cond
        ((> run-time *allowed-time*)   
         (send text-lines change-style style-delta-green)
         (send text-lines insert (msg4 run-time))
         (send text-lines change-style style-delta-black)
         (start-quiz (penalty-time run-time) mistakes))
        ((> mistakes 0) (start-quiz (penalty-mistakes mistakes) 0))
        (else
         (send text-lines change-style style-delta-green)
         (send text-lines insert (msg5))
         (send text-lines change-style style-delta-black)
         (send text-lines insert (msg-separator)) ; keeping records af all exercises
         (send prompt-msg set-label prompt-msg-label-again)
         (send operation-msg set-label op-start-label)
         (send number-input set-value input-label)
         (send number-input enable #f)
         (disable/enable-start-buttons #t)
         (disable/enable-set/font-menu #t)
         ; program was not restarted! set-all-fonts will not function properly
         (when all-fonts-delta (send set-all-fonts enable #f))
         (send wiwi-button enable #f)
         (send *exec-button* enable #f)
         (disable/enable-popup-window-menu #f)
         (disable/enable-dialog-show #f)
         (send stop-button enable #f))))))

(define (reset)
  "Stopping the set of exercises and resetting"
  (send text-lines change-style style-delta-blue)
  (send text-lines insert (msg-stop))
  (send text-lines change-style style-delta-black)
  (send text-lines insert (msg-separator)) ; keeping records af all exercises
  (send prompt-msg set-label prompt-msg-label-again)
  (send operation-msg set-label op-start-label)
  (send number-input set-value input-label)
  ; other input fields
  (send comparison-input set-value input-label)
  (send sequence-input set-value input-label)
  (send bba-input set-value input-label)
  (send fraction-input set-value input-label)
  (send number-input enable #f)
  (disable/enable-start-buttons #t)
  (disable/enable-set/font-menu #t)
  ; program was not restarted! set-all-fonts will not function properly
  (when all-fonts-delta (send set-all-fonts enable #f))
  (send wiwi-button enable #f)
  (send *exec-button* enable #f)
  (disable/enable-popup-window-menu #f)
  (disable/enable-dialog-show #f)
  (send stop-button enable #f))

(define (check-<=> string)
  (case string
    (("=") comp=)
    ((">") comp>)
    (("<") comp<)
    (else #f)))

(define (check-odd/even string)
  (case string
    (("odd") is-odd)
    (("even") is-even)
    (else (error 'display-odd/even))))

(define (display-<=> string)
  (case string
    (("=") "equal")
    ((">") "greater")
    (("<") "smaller")
    (else (error 'display-<=>))))

(define (display-sequence string n)
  (if (equal= (string->number string) n)
      (format "missing number is ~a" string)
      (format "missing number is not ~a" string)))
  
;;; original lisp code

(define (penalty-mistakes mistakes) ; adjust additional exercises based on mistakes
  (if (> mistakes 3)
      *max-penalty-exercises*
      (case mistakes
        ((3) 5)
        ((2) 3)
        ((1) 1)
        (else 0)))) ; something went wrong
    
(define (penalty-time time)
  "Adjust number of additional exercises based on minutes exceeded, 
limited by *max-time-penalty-exercises*"
  (min *max-penalty-exercises*
       (exact-round (/ (- time *allowed-time*) *time-factor* *speed-factor*))))

(define (running-time)
  "Get time used so far - exclude wiwi time"
  (round (/ (- (current-seconds) *time-start* *wiwi-time*) 60)))

(define (get-left-number left op)
  (if (eq? (show op) '+)
      left
      (get-left-number- (random 0 *left-digit*)))) ; this can yield 0 !

;; weighting 0 in middle digit 1/3.777...???
(define (get-left-number- digit1)
  (let ((digit2 (list-ref `(,(random 0 10) 0 ,(random 0 10)) (random 0 3)))
        (digit3 (random 0 10)))
    (let ((x (+ (* digit1 100) (* digit2 10) digit3)))
      (if (< x 2) ; making sure get-right-number- does not spin forever
          (get-left-number- (random 0 *left-digit*))
          x))))

(define (get-right-number+ left right) right)

(define (get-right-number- left right)
  (cond
    ;((= left 1) 0) ; infinite loop, get clean set of numbers
    ((zero? (ones right)) (get-right-number- left (random  1 left))) ; avoiding zero
    (else right)))

(define (ones n)
  (remainder n 10))

;;; IQ part
;;; ============================================================
;;; Missing number in a sequence (IQ) problem for math-quiz program

(define IQ-seq-limit 20)

;;; easy exercises - just incrementing by a constant (1 - 5) and reverse

(define (get-inc)
  (let* ((inc-lst (list (random 1 3) (random 1 4) (random 1 5) (random 1 6)))
         (inc (list-ref inc-lst (random (length inc-lst)))))
    inc))

(define (get-sequence-1)
  (let* ((inc (get-inc))
         (begin (random 0 IQ-seq-limit))
         (lst (build-list 5 (lambda (i) (+ begin (* i inc))))))
    lst))

(define (up-down lst)
  (let* ((rev-lst '(#f #t #f))
         (reverse? (list-ref rev-lst (random (length rev-lst)))))
    (if reverse?
        (reverse lst)
        lst)))

;;; medium exercises increment is incrementing

(define inc-inc1 '(0 0 1 3 6)) ; increment increment by 1

(define (get-sequence-2)
  (let* ((seq1 (get-sequence-1))
         (inc (random 1 4)) ; 1 - 3 inc
         (inc-lst (make-list inc inc-inc1))
         (seq2 (add-sequences seq1 inc-lst)))
    seq2))

(define (add-sequences seq lsts)
  (if (null? lsts)
      seq
      (map + (car lsts) (add-sequences seq (cdr lsts)))))

;;; tough sequences; Fibonacci, inc=exp inc=exp */+ incremented

(define (exp-inc*sequence) ; tough
  (let ((begin (random 0 IQ-seq-limit))
        (inc (random 1 4))) ; max 3
    (build-list 5 (lambda (i) (+ begin (* i (* i inc)))))))

(define (exp-inc+sequence) ; tough
  (let ((begin (random 0 IQ-seq-limit))
        (inc (random 1 4))) ; max 3
    (build-list 5 (lambda (i) (+ begin (* i (+ i inc)))))))

(define (fibonacci n acc1 acc2 seq)
  (if (zero? n)
      (cons acc1 seq)
      (fibonacci (- n 1) acc2 (+ acc1 acc2) (cons acc1 seq))))

(define (fibonacci-sequence n)
  (reverse (fibonacci n 0 1 '())))

(define (get-sequence-3)
  (let* ((fibs (lambda ()
                 (take (drop (fibonacci-sequence IQ-seq-limit)
                             (random (- IQ-seq-limit 4))) 5)))
         (seq-lst
          (list fibs exp-inc*sequence exp-inc+sequence fibs))
         (chosen (list-ref seq-lst (random (length seq-lst)))))
    (chosen)))

;;; I/O part
;;; =============================================================

(define (msg1 n)
  (format "You have ~a minute(s) to complete ~a question(s).~n"
          *allowed-time* n))

(define (msg-separator)
  (format "________________________________________________~n~n"))

(define (msg2 mistakes)
  (format "~nAll questions solved, with ~a mistake(s) along the way.~n"
          mistakes))

(define (msg3 i n time)
  (format "Question number ~a of ~a --- Time used: ~a of ~a minutes."
          i n time *allowed-time*))

(define (msg4 time)
  (string-append
   (format "~nYou exceeded allowed time for exercises!~n")
   (format "Allowed time was ~a minutes, and you did it in ~a minutes.~n"
           *allowed-time* time)
   (format "You will get ~a additional exercise(s).~n" (penalty-time time))))

(define (msg5)
  (format "Bravo, exercise COMPLETED successfully.~n"))

(define (msg6 x op y result)
  (format "~a ~a ~a  = ~a~n" x (show op) y result))

(define (msg-odd/even x op y result)
  (format "~a is ~a ~a~n" x result (show op)))

(define (msg-sequence x op y result)
  (format "~a  ~a  ~a  ~a  ~a  -> ~a~n" (first x) (second x) (third x) result (first y)
          (display-sequence result (fourth x))))

(define (msg-bba x yes/no op y result)
  (case (show op)
    ((before) (format "~a is ~a before ~a~n" result yes/no (third x)))
    ((between) (format "~a is ~a between ~a and ~a~n" result yes/no (first x) (third x)))
    ((after) (format "~a is ~a after ~a~n" result yes/no (first x)))
    (else (error 'msg-bba))))

(define (msg-pvalue result input)
  (let ((x (problem-x *problem*))
        (pos (show (problem-op *problem*))))
    (if result
        (format "for number ~a~a~a~a ~a is ~a~n"
                (first x) (second x) (third x) (fourth x)
                pos (pad-pos (list-ref x (- 3 (cadr (problem-op *problem*))))
                             (problem-y *problem*)))
        (format "for number ~a~a~a~a ~a is not ~a~n"
                (first x) (second x) (third x) (fourth x)
                (show (choose-pvalue-op (string->number input)))
                (pad-pos (list-ref x (- 3 (cadr (problem-op *problem*))))
                         (problem-y *problem*))))))

(define (msg7 x op y result)
  (format "~a ~a ~a  \\= ~a~n" x (show op) y result))

(define (msg8 x op y)
  (format "~a ~a ~a" x op y))

(define (msg9 err-input)
  (format "erroneous input ( ~a ) - error will not be counted!~n" err-input))

(define (msg-stop)
  (format "~nExecution of exercises was stopped!~n"))

(define (pad-pos n pos)
  (string-append n (make-string pos #\0)))

;;; =================================================================
;;; Drawings
;;; =================================================================

;; prepare to draw a box around pie-chart
(define logo
  (set-pen-color (turn 90 (move 65 (turn 90 (move 65 (turtles 300 200))))) "white"))

;; draw a box to prevent cropping pie-chart image
(define (logo-box n world)
  (if (zero? n)
      (turn 90 (move 65 (turn 90 (move 67 world))))
      (logo-box (sub1 n) (turn 90 (set-pen-color (draw 130 world) "white")))))

;; drawing a pie-chart
(define (n-slice r d n theta world)
  (cond ((zero? d) (clean world))
        ((zero? n)
         (n-slice r (sub1 d) 0 theta
                  (slice r theta
                         (turn 90
                               (draw r
                                     (set-pen-width (set-pen-color world "black") 5))))))
        (else
         (n-slice r (sub1 d)(sub1 n) theta
                  (slice r theta
                         (turn 90
                               (draw r
                                     (set-pen-width (set-pen-color world "red") 5))))))))
;; drawing one slice
(define (slice r theta world)
  (if (<= theta 0)
      (turn 180 (draw r (turn 90 world)))
      (slice r (sub1 theta) (draw 1 (turn 1 world)))))

;; return turtles-world with pie-chart drawn
;; if startup call, return a dummy world
(define (get-turtles)
  (if (null? *used-numbers*) ; startup call
      (let ((denominator 1) (nominator 1))
        (n-slice 60 denominator nominator (/ 360 denominator) (logo-box 4 logo)))
      (let* ((denominator (problem-y *problem*))
             (nominator (problem-x *problem*)))
        (n-slice 60 denominator nominator (/ 360 denominator) (logo-box 4 logo)))))

(define (fraction-canvas-callback canvas dc)
  ;; clearing previous pie-chart
  (send dc draw-bitmap
        (pict->bitmap
         (disk 150 #:color "white" #:border-color "white"))
        70 46)
  ;; drawing new pie-chart
  (send dc draw-bitmap
        (pict->bitmap (turtles-pict
                       (get-turtles))) 80 55))

;;; ==================================================================
;;; Fractions problems
;;; ==================================================================

(define fraction-dialog (new frame%
                             [label "Fractions questions"]
                             [parent main-window]
                             [width 340]
                             [height 300]
                             [border 10]
                             [alignment '(left center)]))

(define fraction-canvas (new canvas%
                             [parent fraction-dialog]
                             [label "red=nominator/all=denominator"]
                             [min-width 300]
                             [min-height 250]
                             [vert-margin 10]
                             [horiz-margin 10]
                             [style '(border no-focus)]
                             [paint-callback fraction-canvas-callback]))

(define fraction-pane (new horizontal-pane%
                           [parent fraction-dialog]
                           [min-width 300]
                           [vert-margin 10]
                           [horiz-margin 10]
                           [alignment '(center center)]
                           [stretchable-width #t]
                           [stretchable-height #t]))

(define fraction-input (new text-field%
                            [parent fraction-pane]
                            [font message-bold-font]
                            [label ""]
                            [init-value input-label]
                            [enabled #t]
                            [min-width 70]
                            [min-height 30]
                            [vert-margin 10]
                            [horiz-margin 30]
                            [stretchable-width #f]
                            [stretchable-height #f]))

(define fraction-button (new button%
                             [parent fraction-pane]
                             [label "Check"]
                             [font button-font]
                             [min-height start-button-height]
                             [enabled #f]
                             [vert-margin 10]
                             [horiz-margin 30]
                             [style '(border)]
                             [callback
                              (lambda (button event)
                                (let ((input (send fraction-input get-value))
                                      (a-text (send fraction-input get-editor)))
                                  (send a-text erase) ; why only here?
                                  (send fraction-input set-value input-label)
                                  (math-quiz-type (strip-spaces input))))]))

(define (strip-spaces strng)
  (define (strip lst)
    (cond ((null? lst) lst)
          ((eq? (car lst) #\space) (strip (cdr lst)))
          (else (cons (car lst) (strip (cdr lst))))))
  (list->string (strip (string->list strng))))

;;; =================================================================
;;; Clock problems
;;; =================================================================

;;; funcions for drawing clock

(define logo1
  (set-pen-color (turn 90 (move 130 (turn 90 (move 130 (turtles 400 300))))) "white"))

;; draw a box to prevent cropping pie-chart image
(define (clock-box n world)
  (if (zero? n)
      (turn 180 (move 130 (turn 90 (move 130 world))))
      (clock-box (sub1 n) (turn 90 (set-pen-color (draw 260 world) "white")))))

(define clock-world
  (set-pen-color (turn -90 (move 115 (clock-box 4 logo1))) "gray"))

(define (draw-circle r theta world)
  (if (<= theta 0)
      (turn 180 (move r (turn -90 world)))
      (draw-circle r (sub1 theta) (draw 2 (turn -1 world)))))

(define (draw-hub r theta world)
  (if (<= theta 0)
      (turn 180 (move r (turn -90 world)))
      (draw-hub r (- theta 16) (draw 1 (turn -16 world)))))

(define (minute-marks r theta r-delta m-theta world)
  (if (<= theta 0)
      world
      (minute-marks
       r (- theta m-theta) r-delta m-theta
       (turn (- m-theta)
             (turn 180 (move r (turn 180 (draw r-delta (move (- r r-delta) world)))))))))

(define (hour-hand r h m world)
  (let ((theta (+ (* h 30) (/ m 2)))
        (d (* r 3/5)))
    (turn (+ 180 theta)
          (move d
                (turn 180
                      (draw d (set-pen-color (turn (- theta) world) "purple")))))))
    
(define (minute-hand r m world)
  (let ((theta (* m 360/60))
        (d (* r 3/4)))
    (turn (+ 180 theta)
          (move d
                (turn 180
                      (draw d (set-pen-color (turn (- theta) world) "black")))))))

(define (draw-clock r h m)
  (let* ((world1 (set-pen-color (draw-circle r 360 (set-pen-width clock-world 5)) "black"))
         (world2 (minute-marks r 360 5 (/ 360 60) (set-pen-width world1 2)))
         (world3 (minute-marks r 360 7 (/ 360 12) (set-pen-width world2 3)))
         (world4 (minute-hand r m (set-pen-width world3 4)))
         (world5 (hour-hand r h m (set-pen-width world4 7)))
         (world6 ; drawing hub
          (draw-hub 8 360 (turn -90 (draw 7 (set-pen-color world5 "black"))))))
    (clean world6)))

;; return turtles-world with pie-chart drawn
;; if startup call, return a dummy world
(define (get-clock-turtles)
  (if (null? *used-numbers*) ; startup call
      (let ((hour 0) (minute 0))
        (draw-clock 114 hour minute))  
      (let ((minute (problem-y *problem*))
            (hour (problem-x *problem*)))
        (draw-clock 114 hour minute))))

(define (clock-canvas-callback canvas dc)
  ;; clearing previous clock
  (send dc draw-bitmap
        (pict->bitmap
         (disk 250 #:color "white" #:border-color "white"))
        70 46)
  ;; drawing new clock
  (send dc draw-bitmap
        (pict->bitmap (turtles-pict
                       (get-clock-turtles))) 80 30))


(define clock-dialog (new frame%
                          [label "Clock questions"]
                          [parent main-window]
                          [width 440]
                          [height 360]
                          [border 10]
                          [alignment '(left center)]))

(define clock-canvas (new canvas%
                          [parent clock-dialog]
                          [label "hr:mn"]
                          [min-width 420]
                          [min-height 320]
                          [vert-margin 10]
                          [horiz-margin 10]
                          [style '(border no-focus)]
                          [paint-callback clock-canvas-callback]))

(define clock-pane (new horizontal-pane%
                        [parent clock-dialog]
                        [min-width 420]
                        [vert-margin 10]
                        [horiz-margin 10]
                        [alignment '(center center)]
                        [stretchable-width #t]
                        [stretchable-height #t]))

(define clock-input (new text-field%
                         [parent clock-pane]
                         [font message-bold-font]
                         [label ""]
                         [init-value input-label]
                         [enabled #t]
                         [min-width 70]
                         [min-height 30]
                         [vert-margin 10]
                         [horiz-margin 30]
                         [stretchable-width #f]
                         [stretchable-height #f]))

(define clock-button (new button%
                          [parent clock-pane]
                          [label "Check"]
                          [font button-font]
                          [min-height start-button-height]
                          [enabled #f]
                          [vert-margin 10]
                          [horiz-margin 30]
                          [style '(border)]
                          [callback
                           (lambda (button event)
                             (let ((input (send clock-input get-value))
                                   (a-text (send clock-input get-editor)))
                               (send a-text erase) ; why only here?
                               (send clock-input set-value input-label)
                               (math-quiz-type (strip-spaces input))))]))


;;; =================================================================
;;; Instructions
;;; =================================================================

(define instructions1
  "Start the program by clicking on one of Exercise buttons.

_______________________________________________________________
/          Starts 3 digit division exercises.
_______________________________________________________________
100/10     Starts division table exercises (up to 100/10).
_______________________________________________________________
*          Starts 3 digit multiplication exercises.
_______________________________________________________________
10*10      Starts multiplication table exercises (up to 10*10).
_______________________________________________________________
+ -        Starts the 3 digit addition/subtraction exercises.
_______________________________________________________________
< = >      Starts the 3 digit comparison exercises.
_______________________________________________________________
odd even   Starts the 3 digit odd or even exercises.
_______________________________________________________________
sequence   Starts a missing number in a sequence exercises.
_______________________________________________________________
B B A      Starts Before Between After number exercises.
_______________________________________________________________
PosVal     Starts Position Value exercises.
_______________________________________________________________
fractions  Starts Fractions exercises.
_______________________________________________________________
clock      Starts Clock exercises.
_______________________________________________________________\n\n"
  )

(define instructions2
  "Default number of exercises given in each choice above is 20,
but you can change this in \"Setup->Set number of exercises\" menu option to 10-30 exercises.
When one of the exercises is started, the \"Setup\" menu is disabled until the exercises are completed.

Default maximum factor (for both multiplication and division tables) is set to 10, but for a beginner student, this can be changed with \"Setup->Set max factor for * / table\" to 5-12.

Once you start the arithmetic (+ - * / 10*10 100/10) exercises, enter the result of requested exercise in the text field, and click \"Calculate\" button. Instead of clicking the \"Calculate\" button, you can also hit \"enter\" key on keyboard. If the result was correct, the confirmation will be printed in black in the panel below. If, on the other hand, the result was not correct, the same line will be printed in red.

Important note. All arithmetic exercises require exact (integer) results. Only exception is division (/) that requires almost always inexact, (real -- floating point) numbers, but limited only to 3 digits after the decimal point. No need to enter more - excess digits will be discarded!
You can change the default setting of 3 digits after decimal point in \"Setup->Set division precision\" menu. The pop-up slider allows 1-7 digit setting.
Having said that, of-course, if division exercise actually gives an integer result (as in 10 / 2 = 5) entering only 5 as a result is fine. Likewise if result has less than 3 digits after decimal point, as in (7 / 2 = 3.5), entering only 3.5 is okay.

The comparison exercise (<=>), works slightly differently. It opens a separate input window, and of-course does not require numerical result. Student is asked to input one of comparison symbols ( > = < ) depending on the two numbers shown to the left and the right of the input field, as in: 133  [     ]  211 . In this case the student should type < in the input field, as 133 is smaller than 211.
In case that the student accidentally closes the comparison input window (clicking the X on the title-bar of the window), before the set of exercises is completed, then click on the \"Lost and Found->Show Comparison Window\" and the window will pop up again in the same state it was closed. 

The odd/even exercise works also differently, but the input pop-up window is self explanatory. It displays the number, and two radio buttons labeled odd, and even. If the number is odd, click odd radio button, and vice versa. Then click \"Check\" button to send your choice to be evaluated. Hitting \"enter\" key also works.
If the student accidentaly closes/hides the even-odd input window, it can be retrieved from \"Lost and Found\" menu.

The sequence exercise also pops-up a window with sequence in question shown as following: 1  2  3  [  ]  5 . In this case the student should enter 4 in the input field and click on \"Check\" button, or just hit \"enter\" key.
Of-course not all sequences are as easy as the one shown here. There are 3 levels, that can be set from the \"Setup->Set sequence difficulty level\" menu. Level 1 (which is set as a default) is appropriate for first graders.

The (B B A) Before, Between, After, exercise pops the window which allowes the input of the requested number. As a help to the student the status line on the bottom of the window displays what is expected, as in; before, or between, or after.

The Position value exercise pops up the window wth 4 radio buttons labeled ones, tens, hundreds, and thousands. At the same time one four digit number is displayed with one of the digits being red. Corresponding to that digit position, the student should click the appropriate radio button.

The Fractions exercise pops up the window with pizza pie drawn in it, containing a random number of red and black slices. The student should enter in input field red-slices/all-slices, as in: 3/5 (if the drawing contained 3 red and 2 black slices). No need to enter spaces as in 3 / 5 , but the program can handle that as well.
The default size of \"pizza\" is 10 slices, but that can be changed with \"Setup->Set number of fraction slices\" submenu. Slider goes from 5-12 slices.

The Clock exercise pops up the window with clock face drawn in it. The student should enter hr:mn in the input field, and click Check, or hit enter key.

If wrong result was entered, the same line (in the report panel) will be printed in red, and the computer will beep. Program will not move on (give next problem) until original problem is answered correctly.
This will count as a mistake, and will be penalized after completion of exercises in the following way:
1 mistake = 1 additional exercise given
2 mistakes = 3 additional exercises given
3 mistakes = 5 additional exercises given
more than 3 mistakes = 10 additional exercises given

In case that the student does not focus the input field while entering the result, or he/she inputs a non number, or an invalid symbol for comparison exercises, the reminder will be printed in green, computer will still beep, but this will not be counted nor penalized as error.

Program is also counting the time that the student is using for exercises. At the onset of each set of exercises a certain number of minutes is allotted to exercises. Running time is reported on Status line when each new problem is printed. If the student exceeds the allowed time, penalty exercises will be given.
The number of penalty exercises is limited to maximum 10.
This will happen before penalty for mistakes is taken into the account. However all mistakes will still be counted and penalized after the time penalty is completed.

While a set of exercises is running, all start exercise buttons are disabled.

To change the number of exercises click on \"Setup->Set number of Exercises\" menu and the slider window will open.
Move the slider to desired value and close the slider window by clicking X in the top right corner.

Plus and minus exercises start by default at the easiest level; two digit addition only, with no carry over. This can be changed with \"Setup->Set + - difficulty level\" sub-menu. Level 1 is 2 digit limited addition, lebel 2 is 2 digit limited mixed addition/subtraction, and level 3 is 3 digit unlimited addition/subtraction.

The same goes for changing the maximum factor for multiplication/division tables. Slider will go from 5 (5*5) to 12 (12*12), but the default value is 10.
If you set it to 12, then consequently, division table works from 144/12.
The rationale for starting with (5*5), is for a young student to start easy, and then gradually increase difficulty by changing up to (12*12).

To change the max size of numbers (for all exercises, except for sequence, position value, and fractions) click on \"Setup->Set max size of numbers\" menu. Move the slider to desired size. Slider goes from 100 to 900 with default of 700. It is recommended to lower the number to say, max 350, to avoid huge results in (*) exercises.

To speed up the exercises, give less (or more) time to each exercise open the \"Setup->Set % of inc/dec allotted time\". The slider goes from 50 (%) to 150 (%), with the latter increasing the time to 150%, while first one decreases the time to 50%. Default is 100%, which does not change the default values set by the program, which are:

20 seconds per exercise for (+ -) level 1
30 seconds per exercise for (+ -) level 2
1.5 minutes per exercise for (+ -) level 3
2 minutes per exercise for (* /)
30 seconds per exercise for multiplication/division tables
20 seconds for comparison exercises
12 seconds for odd/even exercises
1 minutes for sequence level 1
2 minutes for sequence level 2
3 minutes for sequence level 3
30 seconds for Before Between After exercises
12 seconds for Position Value
20 seconds for Fractions
30 seconds for Clock

The button above all start exercise buttons is labeled \"Pause\". The purpose for this is a toilet break. If a student needs to go to the toilet, they can press this button, and exercises will be suspended. Most importantly the running time will be stopped, and the input-field will not accept any inputs.
When the student is ready to resume, she/he can click again on \"Resume\" button (after the first click the button label has changed from Pause to Resume), or just click return, and exercises will resume. The time spent during the break will not count.

On notebooks with small display, most of the default font sizes that the program is using are to small for comfortable use. Therefore the font adjustment \"Preferences\" menu was added to the program. That menu contains several sub-menus for changing the size of individual fonts, or all fonts at the same time in increments up to +3/-3 point sizes.
Whenever the font size(s) are changed with one of the Font sub-menus, the program has to be restarted for the change to take effect. However, there's no need to update fonts every time, as the program is (after every change of fonts) updating \".jmq-fontrc\" file (in the same directory as the program).

To exit/stop the running set of exercises, use the \"Stop\" button. You can then start the next set of exercises, with the same setup you have set before with the \"Setup\" menu.
The last submenu under \"Setup\" menu is \"Clear all reports\". This will erase all report printouts from previous exercises.

When finished with exercises, exit the program by clicking X in the right top corner of the main window."
  )

(send doc-instructions change-style style-delta-font-doc1-family)
(send doc-instructions change-style style-delta-font-doc1-weight)
(send doc-instructions insert instructions1)

(send doc-instructions change-style style-delta-font-doc2-family)
(send doc-instructions change-style style-delta-font-doc2-weight)
(send doc-instructions insert instructions2)

(define (silence-scroll)
  (send doc-instructions scroll-to-position 0)
  (send about-text scroll-to-position 0)
  (void))

;;; About

(define about1
  "Jasna's Math Quiz, v2.8

This program was developed when my daughter attended pre-school, and the teacher started teaching addition and subtraction.
When she switched to 2 digit addition/subtraction (limited version, not involving carry or borrow operations), I got tired of giving Jasna written exercises on sheets of paper, and checking her results. Therefore, this program was written to let the computer do the \"hard work\".
Initially I was following the limited choice of number system, but as Jasna got better and better, I added the full, unlimited 3 digit addition/subtraction.

Later, I added multiplication table (up to 10*10), 3 digit multiplication, division table (up to 100/10), 3 digit division, comparison (> = <), odd/even exercises, missing number in a sequence, before/between/after number, value position, fraction, and clock exercises.

The program is written in Racket (Scheme) compiler v8.6
It was developed on x86 based PC running Linux OS.
Windows version was tested and compiled on Windows 11. 
Raspberry pi version was tested on raspberry pi 4B.

The distribution stick contains all 3 (Linux, Windows, and Raspberry pi) standalone executable files.

Problems, or any suggestions, report to hrvoje064@gmail.com


Copyright, (c) 2022. Capt. Hrvoje Blazevic")

(send about-text change-style style-delta-font-about-family)
(send about-text insert about1)

(silence-scroll)

;;; ===============================================================
;;; Starting the program

(send main-window show #t)
;;; disable popup menus
(disable/enable-popup-window-menu #f)

