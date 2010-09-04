;;;
;;; Hatena utility module
;;;

(define-module net.hatena.util
  (use rfc.base64)
  (use rfc.sha1)
  (use srfi-19)
  (use math.mt-random)
  (use gauche.uvector)
  (use gauche.charconv)
  (export
   wsse-header
   ))
(select-module net.hatena.util)

(define (wsse-header username password)
  (let ((created (date->string (current-date) "~4"))
		(nonce (let1 s (make <mersenne-twister> :seed (sys-time))
				 (let1 v (make-u32vector 10)
				   (mt-random-fill-u32vector! s v)
				   (u32vector->string v)))))
	(format
	 "UsernameToken Username=\"~a\", PasswordDigest=\"~a\", Nonce=\"~a\", Created=\"~a\""
	 username
	 (base64-encode-string (sha1-digest-string (string-append nonce created password)))
	 (base64-encode-string nonce)
	 created)))

(provide "net/hatena/util")
