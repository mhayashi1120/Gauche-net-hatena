;;;
;;; Test net.twitter
;;;

(use gauche.test)
(use file.util)

(test-start "net.hatena.diary")
(load "./net/hatena/diary.scm")
(use net.hatena.diary)
(test-module 'net.hatena.diary)

(use srfi-19)
(debug-print-width #f)

(define *cred* 
  (and-let* ((file "./.test-settings.scm")
			 (settings 
			  (and (file-exists? file)
				   (with-input-from-file file
					 read))))
	(make <hatena-cred>
	  :username (cdr (assq 'user settings))
	  :password (cdr (assq 'password settings)))))

(define-macro (test-wait* name expected expr)
  `(begin
	 ;; avoid sequential access.
	 (sys-sleep 5)
	 (test* ,name ,expected ,expr)))

(when *cred*
  (test-wait* "hatena-diary/sxml" #t (pair? (hatena-diary/sxml *cred*)))
  (test-wait* "hatena-diary/draft/sxml" #t (pair? (hatena-diary/draft/sxml *cred*)))
  (test-wait* "hatena-diary/blog/sxml" #t (pair? (hatena-diary/blog/sxml *cred*)))

  (let1 entry-id (hatena-diary/draft/post/id *cred* "TEST1 件名" "CONTENT1 内容" (current-date))
	(test-wait* "hatena-diary/draft/put/sxml" #t (pair? (hatena-diary/draft/put/sxml *cred* entry-id "TEST2 件名" "CONTENT2 内容" (current-date))))
	(test-wait* "hatena-diary/draft/get/sxml" #t (pair? (hatena-diary/draft/get/sxml *cred* entry-id)))
	(hatena-diary/draft/delete *cred* entry-id))

  (let1 draft-id (hatena-diary/draft/post/id *cred* "TEST3 件名" "CONTENT3 内容" (current-date))
	(receive (date blog-id) (hatena-diary/draft/publish/id *cred* draft-id)
	  (test-wait* "hatena-diary/blog/get/sxml" #t (pair? (hatena-diary/blog/get/sxml *cred* date blog-id)))
	  (hatena-diary/blog/delete *cred* date blog-id)))

  (receive (date blog-id) (hatena-diary/blog/post/id *cred* "TEST4 件名" "CONTENT4 内容" (current-date))
	(test-wait* "hatena-diary/blog/get/sxml" #t (pair? (hatena-diary/blog/get/sxml *cred* date blog-id)))
	(test-wait* "hatena-diary/blog/put/sxml" #t (pair? (hatena-diary/blog/put/sxml *cred* date blog-id "TEST5 件名" "CONTENT5 内容" (current-date))))
	(test-wait* "hatena-diary/blog/get/sxml" #t (pair? (hatena-diary/blog/get/sxml *cred* date blog-id)))
	(receive (title contents) (hatena-diary/blog/title&contents *cred* date blog-id)
	  (test-wait* "hatena-diary/blog/title&contents" "TEST5 件名" title)
	  (test-wait* "hatena-diary/blog/title&contents" "CONTENT5 内容" contents))
	(hatena-diary/blog/delete *cred* date blog-id)))


(test-end)





