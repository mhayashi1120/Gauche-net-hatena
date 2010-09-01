;;;
;;; Test net.twitter
;;;

(use gauche.test)

(test-start "net.hatena")
(use net.hatena)
(test-module 'net.hatena)


(define *cred* (make <hatena-cred>
				 :username (cdr (assq 'user settings))
				 :password (cdr (assq 'password settings))))

(define *ns-binding* '(h . "http://www.w3.org/2005/Atom"))

(let* ((settings
		(with-input-from-file "./.test-settings.scm"
		  read))
	   (entry-id #f)
	   (sxml #f))

  (test* "hatena-diary/sxml" #t (pair? (hatena-diary/sxml *cred*)))
  (test* "hatena-diary/draft/sxml" #t (pair? (hatena-diary/draft/sxml *cred*)))
  (test* "hatena-diary/blog/sxml" #t (pair? (hatena-diary/blog/sxml *cred*)))

  
   ;; hatena-diary/draft/post/sxml
   ;; hatena-diary/draft/put/sxml
   ;; hatena-diary/draft/get/sxml
   ;; hatena-diary/draft/delete

   ;; hatena-diary/draft/post/sxml
   ;; hatena-diary/draft/publish/sxml

   ;; hatena-diary/blog/delete

   ;; hatena-diary/blog/post/sxml
   ;; hatena-diary/blog/put/sxml
   ;; hatena-diary/blog/get/sxml
   ;; hatena-diary/blog/delete

   )


(test-end)





