;;;
;;; Hatena AtomPub access module
;;;

;; http://developer.hatena.ne.jp/ja/documents/diary/apis/atom
;; http://www.ietf.org/rfc/rfc5023.txt
;; http://www.hyuki.com/techinfo/hatena_diary_writer.html
;; Gauche 0.9 or later

(define-module net.hatena.diary
  (use file.util)
  (use gauche.charconv)
  (use net.hatena.util)
  (use rfc.cookie)
  (use rfc.http)
  (use rfc.json)
  (use srfi-1)
  (use srfi-13)
  (use srfi-19)
  (use sxml.serializer)
  (use sxml.ssax)
  (use sxml.sxpath)
  (use sxml.tools)
  (use text.tr)
  (use util.list)

  (export
   <hatena-cred>
   <hatena-api-error>
   <hatena-blog-entry>

   hatena-diary/sxml
   hatena-diary/draft/sxml
   hatena-diary/draft/get/sxml
   hatena-diary/draft/post/sxml
   hatena-diary/draft/put/sxml
   hatena-diary/draft/delete
   hatena-diary/draft/title&contents
   hatena-diary/draft/publish/sxml
   hatena-diary/blog/sxml
   hatena-diary/blog/get/sxml
   hatena-diary/blog/post/sxml
   hatena-diary/blog/put/sxml
   hatena-diary/blog/delete
   hatena-diary/draft/post/id
   hatena-diary/blog/post/id
   hatena-diary/draft/publish/id
   hatena-diary/blog/title&contents
   hatena-diary-parse-blog-id
   hatena-diary-parse-draft-id
   hatena-diary/draft/entries
   hatena-diary/blog/entries

   diary/sxml
   diary/draft/sxml
   diary/draft/get/sxml
   diary/draft/post/sxml
   diary/draft/put/sxml
   diary/draft/delete
   diary/draft/title&contents
   diary/draft/publish/sxml
   diary/blog/sxml
   diary/blog/get/sxml
   diary/blog/post/sxml
   diary/blog/put/sxml
   diary/blog/delete
   diary/draft/post/id
   diary/blog/post/id
   diary/draft/publish/id
   diary/blog/title&contents
   diary-parse-blog-id
   diary-parse-draft-id
   diary/draft/entries
   diary/blog/entries

   ))
(select-module net.hatena.diary)

(define-constant *ns-binding* 
  '((atom . "http://www.w3.org/2005/Atom")
    (app . "http://www.w3.org/2007/app")
    (hatena . "http://www.hatena.ne.jp/info/xmlns#")))

;;TODO test group hatena
;;TODO blog GET return text/html..
;; diary/draft/{get,post,put} content should be text 

(define-class <hatena-cred> ()
  ((username :init-keyword :username)
   (password :init-keyword :password)
   (group :init-keyword :group :init-value #f)))

(define-class <hatena-blog-entry> ()
  ((draft? :init-keyword :draft?)
   (updated :init-keyword :updated)
   (published :init-keyword :published)
   (date-id :init-keyword :date-id)   	; can be #f if draft
   (entry-id :init-keyword :entry-id)	; can be #f
   (edit-url :init-keyword :edit-url)
   (published-url :init-keyword :published-url)
   (author-name :init-keyword :author-name)
   (title :init-keyword :title)
   (content :init-keyword :content)))

;;
;; Hatena interface methods
;;

;; service list
(define (diary/sxml cred)
  (call/wsse->sxml cred 'get #`"/,(ref cred 'username)/atom"))

;;
;; Hatena draft methods
;;

;; select draft entries 
(define (diary/draft/sxml cred :key (page #f))
  (call/wsse->sxml cred 'get (draft-path cred)
				   :params (make-query-params page)))

;; select existing draft entry
(define (diary/draft/get/sxml cred entry-id)
  (call/wsse->sxml cred 'get (draft-entry-path cred entry-id)))

;; create new draft entry
(define (diary/draft/post/sxml cred title content updated)
  (call/wsse->sxml cred 'post (draft-path cred)
				   :request-sxml (create-blog-sxml title content updated)))

;; create new draft entry and return created id
(define (diary/draft/post/id cred title content updated)
  (let* ((sxml (diary/draft/post/sxml cred title content updated))
		 (id (car ((sxpath '("atom:entry" "atom:id" *text*) *ns-binding*) sxml))))
	(diary-parse-draft-id id)))

;; update existing draft entry
(define (diary/draft/put/sxml cred entry-id title content updated)
  (call/wsse->sxml cred 'put (draft-entry-path cred entry-id)
				   :request-sxml (create-blog-sxml title content updated)))

;; delete existing draft entry (No error if entry exists or not)
(define (diary/draft/delete cred entry-id)
  (call/wsse->sxml cred 'delete (draft-entry-path cred entry-id)))

;; publish draft to blog entry
(define (diary/draft/publish/sxml cred entry-id)
  (call/wsse->sxml cred 'put (draft-entry-path cred entry-id) 
				   :opts '(:X-HATENA-PUBLISH 1)))

;; publish draft to blog entry
(define (diary/draft/publish/id cred entry-id)
  (let* ((sxml (diary/draft/publish/sxml cred entry-id))
		 (id (car ((sxpath '("atom:entry" "atom:id" *text*) *ns-binding*) sxml))))
	(diary-parse-blog-id id)))

(define (diary/draft/title&contents cred entry-id)
  (let1 sxml (diary/draft/get/sxml cred entry-id)
	(values
     ((nspath-car '("atom:entry" "atom:title" *text*)) sxml)
     ((nspath-car '("atom:entry" "atom:content" *text*)) sxml))))

;;
;; Hatena blog methods
;;

;; select blog entries (Non titled entry is not listed.)
(define (diary/blog/sxml cred :key (page #f))
  (call/wsse->sxml cred 'get (blog-path cred)
  				   :params (make-query-params page)))

;; select existing blog entry
(define (diary/blog/get/sxml cred date entry-id)
  (call/wsse->sxml cred 'get (blog-entry-path cred date entry-id)))

;; create new blog entry
(define (diary/blog/post/sxml cred title content updated)
  (call/wsse->sxml cred 'post (blog-path cred)
				   :request-sxml (create-blog-sxml title content updated)))

;; create new blog entry and return created date and id
(define (diary/blog/post/id cred title content updated)
   (let* ((sxml (diary/blog/post/sxml cred title content updated))
		  (id (car ((sxpath '("atom:entry" "atom:id" *text*) *ns-binding*) sxml))))
	 (diary-parse-blog-id id)))

;; update existing blog entry
(define (diary/blog/put/sxml cred date entry-id title content updated)
  (call/wsse->sxml cred 'put (blog-entry-path cred date entry-id)
				   :request-sxml (create-blog-sxml title content updated)))

;; delete existing blog entry (No error if entry exists or not)
(define (diary/blog/delete cred date entry-id)
  (call/wsse->sxml cred 'delete (blog-entry-path cred date entry-id)))

(define (diary/blog/title&contents cred date entry-id)
  (let1 sxml (diary/blog/get/sxml cred date entry-id)
    (values
     ((nspath-car '("atom:entry" "atom:title" *text*)) sxml)
     ((nspath-car '("atom:entry" "hatena:syntax" *text*)) sxml))))

;;
;; Parsing methods
;;

(define (diary-parse-blog-id raw-id)
  (if-let1 m (#/-([0-9]+)(?:-([0-9]+))?$/ raw-id)
	(values (m 1) (m 2))
	(values #f #f)))

(define (diary-parse-draft-id raw-id)
  ((#/-([0-9]+)$/ raw-id) 1))

;;
;; Utility methods
;;

;; select draft entries (Non titled entry is not listed.)
(define (diary/draft/entries cred :key (page #f))
  (create-entries cred #t :page page))

;; select blog entries (Non titled entry is not listed.)
(define (diary/blog/entries cred :key (page #f))
  (create-entries cred #f :page page))

;;
;; Internal methods
;;

(define (nspath path)
  (sxpath path *ns-binding*))

(define (nspath-car path)
  (let ((x (nspath path)))
	(^a (let ((obj (x a)))
		  (if (null? obj)
			#f
			(car obj))))))

(define (create-entries cred draft? . opts)
  (let* ((method (if draft? 
				   diary/draft/sxml
				   diary/blog/sxml))
		 (idparser (if draft?
					 (lambda (id) (values #f (diary-parse-draft-id id)))
					 diary-parse-blog-id)))
	(let1 sxml (apply method cred opts)
	  (map
	   (lambda (entry)
		 (let1 id ((nspath-car '("atom:id" *text*)) entry)
		   (receive (date-id entry-id) (idparser id)
			 (make <hatena-blog-entry>
			   :draft? draft?
			   :updated (atom-string->date ((nspath-car '("atom:updated" *text*)) entry))
			   :published (atom-string->date ((nspath-car '("atom:published" *text*)) entry))
			   :link ((nspath-car '("atom:link" *text*)) entry)
			   :date-id date-id
			   :entry-id entry-id
			   :edit-url ((nspath-car '("atom:link[@rel='edit']" @ href *text*)) entry)
			   :published-url ((nspath-car '("atom:link[@rel='alternate']" @ href *text*)) entry)
			   :author-name ((nspath-car '("atom:author" "atom:name" *text*)) entry)
			   :title ((nspath-car '("atom:title" *text*)) entry)
			   :content ((nspath-car '("atom:content" *text*)) entry)))))
	   ((nspath '("atom:feed" "atom:entry")) sxml)))))

(define (atom-string->date string)
  (let1 format "~Y-~m-~dT~H:~M:~S~z"
	(if-let1 m (#/([0-9]+):([0-9]+)$/ string)
	  (string->date (string-append (m 'before) (m 1) (m 2)) format)
	  (string->date string format))))

(define (draft-path cred)
  #`"/,(ref cred 'username)/atom/draft")

(define (draft-entry-path cred entry-id)
  #`",(draft-path cred)/,|entry-id|")

(define (blog-path cred)
  #`"/,(ref cred 'username)/atom/blog")

(define (blog-entry-path cred date entry-id)
  #`",(blog-path cred)/,(blog-date->string date)/,|entry-id|")

(define (create-blog-sxml title content updated)
  (let* ((sxml (create-sxml-header))
		 (children (sxml:content-raw sxml))
		 (entry (create-blog-entry title content updated)))
	(sxml:change-content sxml (append children (list entry)))))

(define (create-sxml-header)
  (list '*TOP* (list '*PI* 'xml "version=\"1.0\" encoding=\"utf-8\"")))

;;TODO text/plain or text/html?
(define (create-blog-entry title content updated)
  (let1 entry '(entry)
	(sxml:add-attr! entry '(xmlns "http://purl.org/atom/ns#"))
	(sxml:change-content entry `((title ,title)
								 (content ,content (@ (type "text/plain")))
								 (updated ,(date->string updated "~4"))))))

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

(define hatena-diary/sxml diary/sxml)
(define hatena-diary/draft/sxml diary/draft/sxml)
(define hatena-diary/draft/get/sxml diary/draft/get/sxml)
(define hatena-diary/draft/post/sxml diary/draft/post/sxml)
(define hatena-diary/draft/put/sxml diary/draft/put/sxml)
(define hatena-diary/draft/delete diary/draft/delete)
(define hatena-diary/draft/title&contents diary/draft/title&contents)
(define hatena-diary/draft/publish/sxml diary/draft/publish/sxml)
(define hatena-diary/blog/sxml diary/blog/sxml)
(define hatena-diary/blog/get/sxml diary/blog/get/sxml)
(define hatena-diary/blog/post/sxml diary/blog/post/sxml)
(define hatena-diary/blog/put/sxml diary/blog/put/sxml)
(define hatena-diary/blog/delete diary/blog/delete)
(define hatena-diary/draft/post/id diary/draft/post/id)
(define hatena-diary/blog/post/id diary/blog/post/id)
(define hatena-diary/draft/publish/id diary/draft/publish/id)
(define hatena-diary/blog/title&contents diary/blog/title&contents)
(define hatena-diary-parse-blog-id diary-parse-blog-id)
(define hatena-diary-parse-draft-id diary-parse-draft-id)
(define hatena-diary/draft/entries diary/draft/entries)
(define hatena-diary/blog/entries diary/blog/entries)

