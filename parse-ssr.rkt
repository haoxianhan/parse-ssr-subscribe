#lang racket

(require
  json
  "util.rkt")

(provide
  parse-ssr)

; ssr format
; ssr://server:port:protocol:method:obfs:password_base64/?params_base64
; params_base64: obfsparam=obfsparam_base64&protoparam=protoparam_base64&remarks=remarks_base64&group=group_base64

; trunc ssr://
(define (trunc-ssr-head str)
  (substring str 6))

(define (split-args str)
  (string-split str ":"))

(define (split-args-other str)
  (string-split str "/?"))

(define (split-other-params str)
  (string-split str "&"))

(define (split-other-params-val str)
  (string-split str "="))

(define (gen-params-list str)
  (let ((pamar-list (split-other-params str)))
    (define (iter x acc)
      (let* ((para (split-other-params-val x))
             (key (string->symbol (car para)))
             (val (base-decode (cadr para)))
             (add-one (cons key val)))
        (cons add-one acc)))
    (foldl iter '() pamar-list)))

(define (parse-ssr str)
  (define conten-decoded (base-decode (trunc-ssr-head str)))
  (let* ((args-other (split-args-other conten-decoded))
         (args (split-args (car args-other)))
         (server        (car args))
         (server_port   (cadr args))
         (protocal      (caddr args))
         (method        (cadddr args))
         (obfs          (caddddr args))
         (password      (base-decode (cadddddr args)))
         (other-list    (gen-params-list (cadr args-other)))
         (config (make-hash (append (list (cons 'server         server)
                                          (cons 'server_port    server_port)
                                          (cons 'protocal       protocal)
                                          (cons 'method         method)
                                          (cons 'obfs           obfs)
                                          (cons 'password       password))
                                    other-list))))
    (write-file (jsexpr->string config))))

(define (write-file config)
  (call-with-output-file "./config.json"
                         (lambda (out) (displayln config out))
                         #:exists 'append))

