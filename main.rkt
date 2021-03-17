#lang racket

(require
  json
  "parse-ssr.rkt"
  "util.rkt")

(define ori-file-path "./node.txt")

(define fn-generator (file-name-generator))

(define file-string (file->string ori-file-path))

(define config-template (string->jsexpr
						  (file->string "./config_template.json")))

(define (main)
  (let* ((raw-str file-string)
		 (ssr-list (string-split (base-decode raw-str))))
	(for-each (parse-ssr fn-generator config-template) ssr-list)))

(main)
