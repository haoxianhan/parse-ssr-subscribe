#lang racket

(require
  "parse-ssr.rkt"
  "util.rkt")

(define ori-file-path "./node.txt")

(define proc-read
  (lambda (in)
	(define raw-str (read-line in))
	(define ssr-list (string-split (base-decode raw-str)))
	(for-each parse-ssr ssr-list)))

(call-with-input-file ori-file-path proc-read)

