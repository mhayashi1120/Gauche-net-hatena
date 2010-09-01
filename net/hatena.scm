;;;
;;; Hatena AtomPub access module
;;;

;; http://d.hatena.ne.jp/keyword/%A4%CF%A4%C6%A4%CA%B5%BB%BD%D1%CA%B8%BD%F1
;; http://d.hatena.ne.jp/keyword/%A4%CF%A4%C6%A4%CA%A5%C0%A5%A4%A5%A2%A5%EA%A1%BCAtomPub
;; http://www.ietf.org/rfc/rfc5023.txt

(define-module net.hatena
  (use rfc.http)
  (use file.util)
  (use oasis.wsse)
  (use sxml.ssax)
  (use sxml.sxpath)
  (use sxml.serializer)
  (use sxml.tools)
  (use srfi-13)
  (use srfi-19)
  (use text.tr)
  (use util.list)

  (export
   <hatena-cred>
   <hatena-api-error>

   hatena-diary/sxml

   hatena-diary/draft/sxml
   hatena-diary/draft/get/sxml
   hatena-diary/draft/post/sxml
   hatena-diary/draft/put/sxml
   hatena-diary/draft/delete

   hatena-diary/draft/publish/sxml

   hatena-diary/blog/sxml
   hatena-diary/blog/get/sxml
   hatena-diary/blog/post/sxml
   hatena-diary/blog/put/sxml
   hatena-diary/blog/delete
   ))
(select-module net.hatena)

;;TODO post/put return date and id
;; define-class?
;;TODO test group hatena
;;TODO blog GET return text/html..
;; hatena-diary/draft/{get,post,put} content should have been text 

(define-class <hatena-cred> ()
  ((username :init-keyword :username)
   (password :init-keyword :password)
   (group :init-keyword :group :init-value #f)))

(define-condition-type <hatena-api-error> <error> #f
  (status #f)
  (headers #f)
  (body #f))

;;
;; A convenience macro to construct query parameters, skipping
;; if #f is given to the variable.
;;

(define-macro (make-query-params . vars)
  `(cond-list
    ,@(map (lambda (v)
             `(,v `(,',(string-tr (x->string v) "-" "_") ,,v)))
           vars)))

(define (%-fix str)
  (regexp-replace-all* str #/%[\da-fA-F][\da-fA-F]/
                       (lambda (m) (string-upcase (m 0)))))

;;
;; Hatena interface methods
;;

;; service list
(define (hatena-diary/sxml cred user)
  (call/wsse->sxml cred 'get #`"/,|user|/atom"))

;; select draft entries 
(define (hatena-diary/draft/sxml cred :key (page #f))
  (call/wsse->sxml cred 'get (draft-path cred)
				   :params (make-query-params page)))

;; select existing draft entry
(define (hatena-diary/draft/get/sxml cred entry-id)
  (call/wsse->sxml cred 'get (draft-entry-path cred entry-id)))

;; create new draft entry
(define (hatena-diary/draft/post/sxml cred title content updated)
  (call/wsse->sxml cred 'post (draft-path cred)
				   :request-sxml (create-blog-sxml title content updated)))

;; update existing draft entry
(define (hatena-diary/draft/put/sxml cred entry-id title content updated)
  (call/wsse->sxml cred 'put (draft-entry-path cred entry-id)
				   :request-sxml (create-blog-sxml title content updated)))

;; delete existing draft entry (No error if entry exists or not)
(define (hatena-diary/draft/delete cred entry-id)
  (call/wsse->sxml cred 'delete (draft-entry-path cred entry-id)))

;; publish draft to blog entry
(define (hatena-diary/draft/publish/sxml cred entry-id)
  (call/wsse->sxml cred 'put (draft-entry-path cred entry-id) 
				   :opts '(:X-HATENA-PUBLISH 1)))

;; select blog entries (Non titled entry is not listed.)
(define (hatena-diary/blog/sxml cred :key (page #f))
  (call/wsse->sxml cred 'get (blog-path cred)
  				   :params (make-query-params page)))

;; select existing blog entry
(define (hatena-diary/blog/get/sxml cred date entry-id)
  (call/wsse->sxml cred 'get (blog-entry-path cred date entry-id)))

;; create new blog entry
(define (hatena-diary/blog/post/sxml cred title content updated)
  (call/wsse->sxml cred 'post (blog-path cred)
				   :request-sxml (create-blog-sxml title content updated)))

;; update existing blog entry
(define (hatena-diary/blog/put/sxml cred date entry-id title content updated)
  (call/wsse->sxml cred 'put (blog-entry-path cred date entry-id)
				   :request-sxml (create-blog-sxml title content updated)))

;; delete existing blog entry (No error if entry exists or not)
(define (hatena-diary/blog/delete cred date entry-id)
  (call/wsse->sxml cred 'delete (blog-entry-path cred date entry-id)))

;;
;; Internal methods
;;

(define (compose-query params)
  (%-fix (http-compose-query #f params 'utf-8)))

(define (draft-path cred)
  #`"/,(ref cred 'username)/atom/draft")

(define (draft-entry-path cred entry-id)
  #`"/,(draft-path cred)/,|entry-id|")

(define (blog-path cred)
  #`"/,(ref cred 'username)/atom/blog")

(define (blog-entry-path cred date entry-id)
  #`"/,(blog-path cred)/,(blog-date->string date)/,|entry-id|")

(define (create-blog-sxml title content updated)
  (let* ((sxml (create-sxml-header))
		 (children (sxml:content-raw sxml))
		 (entry (create-blog-entry title content updated)))
	(sxml:change-content! sxml (append children (list entry)))
	sxml))

;; FIXME any method?
(define (create-sxml-header)
  `(*TOP* (*PI* xml "version=\"1.0\" encoding=\"utf-8\"")))

;; FIXME create xml node like this???
(define (create-blog-entry title content updated)
  (let1 entry '(entry)
	(sxml:add-attr! entry '(xmlns "http://purl.org/atom/ns#"))
	(sxml:change-content! entry `((title ,title)
								  (content ,content (@ (type "text/plain")))
								  (updated ,(date->string updated "~4"))))
	entry))

(define-method blog-date->string ((date <string>))
  date)

(define-method blog-date->string ((date <date>))
  (date->string date "~Y~m~d"))

(define (call/wsse->sxml cred method path :key (request-sxml #f) (params #f) (opts '()))

  (define (call)
	(let ((wsse (wsse-header (ref cred 'username) (ref cred 'password)))
		  (host (or (and (ref cred 'group) #`"(ref cred 'group).g.hatena.ne.jp")
					"d.hatena.ne.jp")))
	  (case method
		[(get) (apply http-get host (create-query-path)
					  :X-WSSE wsse opts)]
		[(delete) (apply http-delete host (create-query-path)
						 :X-WSSE wsse opts)]
		[(post) (apply http-post host (create-query-path)
					   (create-request-body)
					   :X-WSSE wsse opts)]
		[(put) (apply http-put host (create-query-path)
					  (create-request-body)
					  :X-WSSE wsse opts)])))

  (define (create-request-body)
	(or (and request-sxml (srl:sxml->xml request-sxml))
		""))

  (define (create-query-path)
	(if params
	  #`",|path|?,(compose-query params)"
	  path))

  (define (retrieve status headers body)
    (check-api-error status headers body)
    (values (if (string=? body "") 
			  #f
			  (call-with-input-string body (cut ssax:xml->sxml <> '())))
			headers))

  (call-with-values call retrieve))

(define (check-api-error status headers body)
  ;; Created a resource return 201
  (unless (member status '("200" "201"))
	(error <hatena-api-error>
		   :status status :headers headers :body body
		   body)))

