#! /usr/bin/env racket
#lang racket

(require racket/cmdline)
(require racket/string)
(require racket/port)

(struct infinitive (word regular?))
(struct result (success? elapsed-time))

(define (load-file filename regular?)
  (with-input-from-file filename
    (lambda ()
      (map (lambda (s)
             (infinitive s regular?))
           (string-split (port->string) #px"\n")))))

(define (test-single inf form)
  (let ([begin-sec (current-seconds)])
    (displayln (string-replace form "#"
                             (format "[~a]" (infinitive-word inf))))
    (let ([success? (not (char=? (read-char) #\x))]
          [elapsed-time (- (current-seconds) begin-sec)])
      (if success?
          (displayln (format "Succeeded in ~a seconds" elapsed-time))
          (displayln "Failed :("))
      (result success? elapsed-time))))

(define use-irregulars (make-parameter #t))
(define trials (make-parameter 10))

(define lang (command-line 
                      #:once-each
                      [("-r" "--regulars-only")
                       "Use only regular verbs" (use-irregulars #f)]
                      [("-t" "--trials") num-trials
                                         "Run <trials> number of trials"
                                         (trials (num-trials string->number))]
                      #:args (lang)
                      lang))

(define inf-reg (load-file (format "infinitives-reg-~a" lang) #t))
(define inf-irreg (when use-irregulars (load-file (format "infinitives-irreg-~a" lang) #f)))
(define forms (with-input-from-file
                (format "forms-~a" lang)
                (lambda () (string-split
                            (port->string) #px"\n"))))
(map (lambda (inf-form) (test-single (car inf-form) (cdr inf-form)))
     (take (shuffle
            (map (lambda (l) (cons (first l) (second l)))
                 (cartesian-product (append inf-reg inf-irreg)
                                    forms))) (trials)))
