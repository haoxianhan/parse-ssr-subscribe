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

(define (fill-config config-list config-temp)

  (define (key-convert key)
    (cond ((eqv? key 'server) 'server)
          ((eqv? key 'port) 'server_port)
          ((eqv? key 'password) 'password)
          ((eqv? key 'method) 'method)
          ((eqv? key 'protocol) 'protocol)
          ((eqv? key 'protoparam) 'protocol_param)
          ((eqv? key 'obfs) 'obfs)
          ((eqv? key 'obfsparam) 'obfs_param)
          (else '())))

  (define (iter l result)
    (cond ((null? l) result)
          (else (let* ((x (car l))
                       (key (car x))
                       (val (cdr x))
                       (key-conv (key-convert key))
                       (result-acc (cond ((hash-has-key? result key-conv) (hash-set result key-conv val))
                                         (else (hash-set result key val)))))
                  (iter (cdr l) result-acc)))))

  (iter config-list config-temp))

(define (parse-ssr fn-generator config-template)
  (lambda (str)
    (define conten-decoded (base-decode (trunc-ssr-head str)))
    (let* ((config-temp    config-template)
           (args-other (split-args-other conten-decoded))
           (args (split-args (car args-other)))
           (server        (car args))
           (server_port   (cadr args))
           (protocol      (caddr args))
           (method        (cadddr args))
           (obfs          (caddddr args))
           (password      (base-decode (cadddddr args)))
           (other-list    (gen-params-list (cadr args-other)))
           (config-list   (append (list (cons 'server         server)
                                        (cons 'port           server_port)
                                        (cons 'protocol       protocol)
                                        (cons 'method         method)
                                        (cons 'obfs           obfs)
                                        (cons 'password       password))
                                  other-list))
           (config (fill-config config-list config-temp)))
      (write-file fn-generator (jsexpr->string config)))))

