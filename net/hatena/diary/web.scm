(define-module net.hatena.diary.web
  (use gauche.charconv)
  (use net.hatena.util)
  (use rfc.822)
  (use rfc.cookie)
  (use rfc.base64)
  (use rfc.md5)
  (use rfc.http)
  (use rfc.json)
  (use srfi-1)
  (use srfi-13)
  (use text.tr)
  (use util.list)
  (use parser.peg)

  (export
   diary-log
   diary-list
   diary-text/title&contents
   
   diary-upload-file/json

   diary-entry
   diary-post

   ;; TODO testing
   diary-newest-log
  ))
(select-module net.hatena.diary.web)

;;TODO parse response header? rfc822-header-ref -> mime-parse-content-type
(define-constant hatena-encoding 'euc-jp)
(define-constant hatena-host "d.hatena.ne.jp")
(define-constant hatena-cuonter-host "counter.hatena.ne.jp")

;;
;; web (Special feature)
;;

;; for Emacs (put 'with-web-session 'scheme-indent-function 2)
(define-macro (with-web-session cred cookie . body)
  `(let1 ,cookie (web-login ,cred)
	 (unwind-protect
	  (begin
		,@body)
	  (web-logout ,cookie))))

;;
;; High level API
;;

(define (diary-upload-file/json cred name file)
  (with-web-session cred cookie
    (define (call)
      (let ((user (ref cred 'username)))
        (web-post cookie
                  hatena-host
                  #`"/,|user|/files"
                  `((rkm ,(rkm-value cookie))
                    (mode "json")
                    (name ,name)
                    (file :file ,file)
                    ;; TODO to send multiple file?
                    ;; (num 1)
                    ;; (max 1)
                    ))))

    (define (retrieve status headers body)
      (check-status status headers body)
      (values (parse-json-string (code-convert body)) headers))

    (call-with-values call retrieve)))

;;TODO https
;;FIXME cannot read no entry-id entry
(define (diary-text/title&contents cred date entry-id)
  (with-web-session cred cookie
    (define (call)
      (let ((user (ref cred 'username))
            (date-id (blog-date->string date))
            (entry-id (or entry-id "")))
        (web-get cookie
                 hatena-host
                 #`"/,|user|/,|date-id|/,|entry-id|?mode=json&now=,(hatena-now)")))
	
    (define (retrieve status headers body)
      (check-status status headers body)

      (let1 alist (parse-json-string (code-convert body))
        ;;TODO about newline
        (values (assoc-ref alist "title") (string-append (assoc-ref alist "body") "\n"))))

    (call-with-values call retrieve)))

(define (diary-log cred :optional when)
  (with-web-session cred cookie
    (define (call)
      (let* ((cid 1)
             (mode "download")
             (month (case when ((1) "prev") ((2) "prev2") (else "this")))
             (params (make-query-params cid mode month)))
        (web-get cookie hatena-cuonter-host
                 #`"/,(ref cred 'username)/downloadlog?,(compose-query params)")))

    (define (retrieve status headers body)
      (define (parse-filename)
        (if-let1 pair (assoc "content-disposition" headers)
          (cond
           ((#/filename="([^"]+)"/ (cadr pair)) =>
            (lambda (m) (m 1)))
           ((#/filename=(.+)/ (cadr pair)) =>
            (lambda (m) (m 1))))))
      (check-status status headers body)
      ;;TODO return values
      (values body (parse-filename)))

    (call-with-values call retrieve)))

(define (diary-newest-log cred)
  (with-web-session cred cookie
    (define (call)
      (let* ((cid 1)
             (date "2011-04-08") ;;TODO
             (type "daily")
             (params (make-query-params cid date type)))
        (web-get cookie hatena-cuonter-host
                 #`"/,(ref cred 'username)/log?,(compose-query params)")))

    (define (retrieve status headers body)
      (check-status status headers body)
      (print body)
      ;;TODO return values
      (peg-parse-string %html-logs body))

    (call-with-values call retrieve)))

;;
;; Low level API
;;

(define (web-login cred)
  (define (call)
    (let1 query (http-compose-query #f (list 
    									(list :name (ref cred 'username)) 
    									(list :password (ref cred 'password)))
    								'utf-8)
      (http-post "www.hatena.ne.jp" "/login" query :no-redirect #t)))

  (define (retrieve status headers body)
    (check-status status headers body)
    ;; create cookie string
    (string-join
     (construct-cookie-string
      (filter
       (lambda (c)
         (and (string? (car c))
              (member (car c) '("b" "rk"))))
       (concatenate
        (map
         (lambda (c)
           (parse-cookie-string (list-ref c 1)))
         (filter (lambda (x)
                   (and (pair? x)
                        (string-ci=? (car x) "set-cookie")))
                 headers)))))
     "; "))

  (call-with-values call retrieve))

(define (web-logout cookie)
  (define (call)
    (web-get cookie "www.hatena.ne.jp" "/logout"))

  (define (retrieve status headers body)
    (check-status status headers body)
    headers)

  (call-with-values call retrieve))

(define (rkm-value cookie)
  (define (md5-base64 string)
    (string-copy
     (base64-encode-string
      (md5-digest-string string))
     0 22))
  (let1 c (parse-cookie-string cookie)
    (md5-base64 (cadr (assoc "rk" c)))))

(define (web-post cookie server request-uri body . args)
  (apply http-post server request-uri body 
         :Cookie cookie :no-redirect #t args))

(define (web-get cookie server request-uri . args)
  (apply http-get server request-uri
         :Cookie cookie :no-redirect #t args))

(define (check-status status headers body)
  (unless (string=? status "200")
    (error <hatena-api-error>
           :status status :headers headers :body body
           body)))

(define (code-convert body)
  (ces-convert body hatena-encoding (gauche-character-encoding)))

;;
;; experimental
;;

;;TODO 
;; * twitter notification
;; * trivial
(define (diary-post cred file trivial)
  ;;TODO
  ;; (post-data 
  ;;       (concat "dummy=1"
  ;;   	    "&mode=enter"
  ;;   	    "&body=" body 
  ;;   	    "&trivial=" trivial 
  ;;   	    "&title=" title 
  ;;   	    "&day=" day 
  ;;   	    "&month=" month 
  ;;   	    "&year=" year 
  ;;   	    ;; if "date" element exists ,
  ;;   	    ;; command can't create the new page at hatena
  ;;   	    "&rkm=" (hatena-rkm-value)
  ;;   	    (if (hatena-check-newpage referer) 
  ;;   		(concat "&date=" date))
  ;;   	    "&timestamp=" timestamp ))
  
  )

(define %html-char 
  (let ((%special-char ($or
                        ($do [($string "&gt;")] ($return ">"))
                        ($do [($string "&lt;")] ($return "<"))
                        ($do [($string "&amp;")] ($return "&"))
                        ($do [($string "&quot;")] ($return "\""))
                        ($do [($string "&nbsp;")] ($return " "))))
        (%raw-char ($do [c anychar] ($return c))))
    ($or %special-char %raw-char)))

(define-class <log-item> ()
  (
   (time :init-keyword :time)
   (url :init-keyword :url)
   (user-agent :init-keyword :user-agent)
   (lang :init-keyword :lang)
   (display :init-keyword :display)
   (host :init-keyword :host)
   (referer :init-keyword :referer)
   ))

(define %html-logs 
  (let* ((%ws ($skip-many ($one-of #[ \t\r\n])))
         (%begin-tag ($char #\<))
         (%begin-endtag ($seq %begin-tag ($char #\/)))
         (%end-tag ($char #\>))
         ;;FIXME `>' in attribute value
         (%rest-of-tag ($seq ($skip-many ($none-of #[>])) %end-tag)))

    (define %begin-logtable-tag 
      ($seq %begin-tag 
            %ws ($string "table") 
            %ws ($string "id=\"log_table\"") 
            %ws ($char #\>)))

    (define %table-row
      (let* ((%ws ($skip-many ($one-of #[ \t\r\n])))
             (%begin ($string "<td>"))
             (%end ($string "</td>")))

        (define ($getter %getter)
          ($do %ws %begin
               %ws [val %getter]
               %ws %end
               %ws
               ($return (rope->string val))))

        (let ((%rawcell ($getter ($many-till %html-char %end)))
              (%hrefcell ($getter ($or
                                   ($do
                                    [($many-till %ws ($string "<a"))]
                                    [($string "<a href='")]
                                    [url ($many-till %html-char ($char #\'))]
                                    [($many-till anychar %end)]
                                    ($return url))
                                   ($do
                                    [($many-till anychar %end)]
                                    ($return ""))))))
          ($do
           %ws [($string "<tr>")]
           [time %rawcell]
           [url %hrefcell]
           [agent %rawcell]
           [lang %rawcell]
           [display %rawcell]
           [host %rawcell]
           [referer %hrefcell]
           %ws [($string "</tr>")]
           %ws
           ($return (make <log-item> :time time :url url :user-agent agent 
                          :lang lang :display display 
                          :host host :referer referer))))))
    
    (let* ((%before-table ($many-till anychar %begin-logtable-tag)))

      ($do
       %before-table
       %begin-logtable-tag
       %ws
       [($between ($string "<tr>") ($many-till anychar ($string "</tr>")) ($string "</tr>"))]
       [texts ($many %table-row)]
       [$return texts]))))

;;TODO multiple textarea?
(define %html-textarea 
  (let* ((%ws ($skip-many ($one-of #[ \t\r\n])))
         (%begin-tag ($char #\<))
         (%begin-endtag ($seq %begin-tag ($char #\/)))
         (%end-tag ($char #\>))
         ;;FIXME `>' in attribute value
         (%rest-of-tag ($seq ($skip-many ($none-of #[>])) %end-tag)))

    (define ($begin-tag name)
      ($seq %begin-tag %ws ($string name) %ws %rest-of-tag))

    (define ($end-tag name)
      ($seq %begin-endtag %ws ($string name) %ws %rest-of-tag))

    (let* ((%begin-textarea ($begin-tag "textarea"))
           (%end-textarea ($end-tag "textarea"))
           (%before-textarea ($many-till anychar %begin-textarea)))

      ($do
       [%before-textarea]
       [%begin-textarea]
       [text ($many-till %html-char %end-textarea)]
       ;;TODO what means rope->string?
       [$return (rope->string text)]))))

(define (diary-entry cred date)
  (with-web-session cred cookie
    (define (call)
      (let ((user (ref cred 'username))
            (date-id (blog-date->string date)))
        (web-get cookie
                 hatena-host
                 #`"/,|user|/edit?date=,|date-id|")))
  
    (define (retrieve status headers body)
      (check-status status headers body)
      (peg-parse-string %html-textarea 
                        (code-convert body)))

    (call-with-values call retrieve)))

(define (web-diary-entries cred date)
  (diary-entry cred date))

(define %html-attribute
  (let* ((%html-attrkey anychar)
         (%ws ($skip-many ($one-of #[ \t\r\n])))
         (%dquote ($char #\"))
         (%squote ($char #\'))
         (%html-attrval ($or
                         ($between %dquote ($not %dquote) %dquote)
                         ($between %squote ($not %squote) %squote)
                         ($many-till ($or ($not %ws) ($not ($char #\>)))
                                     ($or %ws ($char #\>))))))
    ($seq %html-attrkey ($char #\=) %html-attrval)))

;;TODO list
;; http://d.hatena.ne.jp/mhayashi1120/archive?mode=edit&mid=1
;; twitter_notification_enabled=0
;; twitter_notification_prefix=
;; trivial=0
;; mixi_check_notification_enabled=0

(define (diary-list cred :optional (page #f))
  (with-web-session cred cookie
    (define (call)
      (let* ((mid page)
             (mode "edit")
             (params (make-query-params mode mid)))
        (web-get cookie hatena-host
                 #`"/,(ref cred 'username)/archive?,(compose-query params)")))

    (define (retrieve status headers body)
      (check-status status headers body)
      (code-convert body))

    (call-with-values call retrieve)))

(define (hatena-now)
  (* (sys-time) 1000))

