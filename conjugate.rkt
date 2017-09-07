#! /usr/bin/env racket
#lang racket

(require racket/cmdline)
(require racket/string)
(require racket/port)
(require math/statistics)

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
                               (format "[~a]"
                                       (string-append (infinitive-word inf)
                                                      (if (infinitive-regular? inf)
                                                          "" "*")))))
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
                                         (trials (string->number num-trials))]
                      #:args (lang)
                      lang))

(define inf-reg (load-file (format "infinitives-reg-~a" lang) #t))
(define inf-irreg (if (use-irregulars)
                      (load-file (format "infinitives-irreg-~a" lang) #f)
                      '()))
(define forms (with-input-from-file
                (format "forms-~a" lang)
                (lambda () (string-split
                            (port->string) #px"\n"))))

(file-stream-buffer-mode (current-input-port) 'none)
(let* ([stty (find-executable-path "stty")]
       [starting-stty
        (let ([s (open-output-string)])
          (parameterize [(current-output-port s)]
            (unless (system* stty "-g")
              (error "Could not get the current stty settings"))
            (regexp-replace #rx"\r?\n$" (get-output-string s) "")))])
  (dynamic-wind
    (lambda ()
      (unless (system* stty "raw" "-echo" "opost")
        (error "Could not set the stty settings to raw")))
    (lambda ()
      (let ([results
             (map (lambda (inf-form) (test-single (car inf-form) (cdr inf-form)))
                  (take (shuffle
                         (map (lambda (l) (cons (first l) (second l)))
                              (cartesian-product (append inf-reg inf-irreg)
                                                 forms))) (trials)))])
        (displayln (format "Average time: ~s sec"
                           (round (exact->inexact
                                   (mean (map (lambda (r) (result-elapsed-time r))
                                              results))))))
        (let ([num-correct (length (filter (lambda (r) (result-success? r)) results))])
          (displayln (format "Correct: ~s  Incorrect: ~s:  ~s%"
                            num-correct
                            (- (length results) num-correct)
                            (round (exact->inexact
                                    (* 100 (/ num-correct (length results))))))))))
    (lambda ()
      (unless (system* stty starting-stty)
        (error "Could not reset the stty settings")))))
