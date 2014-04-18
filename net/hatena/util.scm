;;;
;;; Hatena utility module
;;;

(define-module net.hatena.util
  (use rfc.base64)
  (use rfc.sha1)
  (use srfi-13)
  (use srfi-19)
  (use math.mt-random)
  (use gauche.uvector)
  (use gauche.charconv)
  (use text.tr)
  (use rfc.http)

  (export
   wsse-header
   make-query-params
   blog-date->string
   compose-query
   <hatena-api-error>
   ))
(select-module net.hatena.util)

;;
;; A convenience macro to construct query parameters, skipping
;; if #f is given to the variable.
;;

(define-macro (make-query-params . vars)
  `(cond-list
    ,@(map (lambda (v)
             `(,v `(,',(string-tr (x->string v) "-" "_") ,,v)))
           vars)))

(define-condition-type <hatena-api-error> <error> #f
  (status #f)
  (headers #f)
  (body #f))

(define (%-fix str)
  (regexp-replace-all* str #/%[\da-fA-F][\da-fA-F]/
                       (lambda (m) (string-upcase (m 0)))))

(define (compose-query params)
  (%-fix (http-compose-query #f params 'utf-8)))

(define-method blog-date->string ((date <top>))
  (x->string date))

(define-method blog-date->string ((date <string>))
  date)

(define-method blog-date->string ((date <date>))
  (date->string date "~Y~m~d"))

(define (wsse-header username token)
  (let ([created (date->string (current-date) "~4")]
		[nonce (let1 s (make <mersenne-twister> :seed (sys-time))
				 (let1 v (make-u32vector 10)
				   (mt-random-fill-u32vector! s v)
				   (u32vector->string v)))])
	(format
	 "UsernameToken Username=\"~a\", PasswordDigest=\"~a\", Nonce=\"~a\", Created=\"~a\""
	 username
	 (base64-encode-string (sha1-digest-string (string-append nonce created token)))
	 (base64-encode-string nonce)
	 created)))

