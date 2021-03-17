#lang racket

(require net/base64)

(provide
  base-decode
  caddddr
  cdddddr
  cadddddr
  disp
  file-name-generator
  write-file
  )

(define (trans-+ s)
  (string-replace
    (string-replace s "_" "+")
    "-" "+"))

(define (check-format-multiple-4 s)
  (define s-len (string-length s))
  (let ((rem (remainder s-len 4)))
	(cond ((= rem 0) #t)
		  (else (- 4 rem)))))

(define (complete-base64-format s)
  (let ((check4 (check-format-multiple-4 s)))
	(cond ((eqv? check4 #t) s)
		  (else (string-append s
							  (make-string check4 #\=))))))

(define (base-decode s)
  (bytes->string/utf-8
	(base64-decode
      (string->bytes/utf-8 (trans-+
                             (complete-base64-format s))))))

(define (caddddr l)
  (car (cddddr l)))

(define (cdddddr l)
  (cdr (cddddr l)))

(define (cadddddr l)
  (car (cdddddr l)))

(define (disp s)
  (display s)(newline))

(define (file-name-generator)
  (define count 1)
  (define (get-file-name)
	(let ((file-name (string-append "./ssr_config/config" (number->string count ) ".json")))
	  (set! count (+ count 1))
	  file-name))
  get-file-name)

(define (write-file fn-generator config)
  (call-with-output-file (fn-generator)
                         (lambda (out) (displayln config out))
                         #:exists 'replace))

