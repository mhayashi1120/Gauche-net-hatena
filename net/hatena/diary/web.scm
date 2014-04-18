(define-module net.hatena.diary.web
  (use gauche.charconv)
  (use net.hatena.util)
  (use parser.peg)
  (use rfc.822)
  (use rfc.base64)
  (use rfc.cookie)
  (use rfc.http)
  (use rfc.json)
  (use rfc.md5)
  (use srfi-1)
  (use srfi-13)
  (use srfi-14)
  (use srfi-19)
  (use text.tr)
  (use util.list)

  (export
   diary-log
   diary-list
   diary-text/title&contents

   diary-draft-preview

   diary-upload-file/json

   diary-entry
   diary-post

   diary-newest-log diary-today-log
   diary-today-log$ diary-recent-log$
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
	  (begin ,@body)
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
                    (file ,(read-as-bytes file))
                    ;; TODO to send multiple file?
                    (num 0)
                    (max 0)))))

    (define (retrieve status headers body)
      (check-status status headers body)
      (values (parse-json-string (decode-string body)) headers))

    (call-with-values call retrieve)))

;;TODO https
;;FIXME cannot read no entry-id entry
(define (diary-text/title&contents cred date entry-id)
  (with-web-session cred cookie
    (define (call)
      (let* ([user (ref cred 'username)]
             [date-id (blog-date->string date)]
             [entry-id (or entry-id "")]
             [url #`"/,|user|/,|date-id|/,|entry-id|?mode=json&now=,(hatena-now)"])
        (web-get cookie hatena-host url)))
	
    (define (retrieve status headers body)
      (check-status status headers body)

      (let1 alist (parse-json-string (decode-string body))
        ;;TODO about newline
        (values (assoc-ref alist "title")
                (string-append (assoc-ref alist "body") "\n"))))

    (call-with-values call retrieve)))

(define (diary-draft-preview cred title body)
  (with-web-session cred cookie
    (define (call)
      (let* ([tm (current-date)]
             [year (date-year tm)]
             [month (date-month tm)]
             [day (date-day tm)]
             [mode "enter"]
             [rkm (rkm-value cookie)]
             ;; 多分無視される項目。
             ;; "確認する" が元の値
             [preview "any"]
             [title (encode-string title)]
             [body (encode-string body)]
             [params (make-query-params 
                      title body mode rkm preview
                      year month day)]
             [url #`"/,(ref cred 'username)/draft"])

        (web-post cookie hatena-host url params)))

    (define (retrieve status headers body)
      (check-status status headers body)
      (values (decode-string body) headers))
    
    (call-with-values call retrieve)))

(define (diary-log cred :optional when)
  (with-web-session cred cookie
    (define (call)
      (let* ((cid 1)
             (mode "download")
             (month (case when [(1) "prev"] [(2) "prev2"] [else "this"]))
             (params (make-query-params cid mode month)))
        (web-get cookie hatena-cuonter-host
                 #`"/,(ref cred 'username)/downloadlog?,(compose-query params)")))

    (define (retrieve status headers body)
      (define (parse-filename)
        (if-let1 pair (assoc "content-disposition" headers)
          (cond
           ((#/filename="([^\"]+)\"/ (cadr pair)) =>
            (lambda (m) (m 1)))
           ((#/filename=(.+)/ (cadr pair)) =>
            (lambda (m) (m 1))))))
      (check-status status headers body)
      (values body (parse-filename)))

    (call-with-values call retrieve)))

;; return log list that is not yet contained `diary-log'
;; TODO FQDN to IP address.
(define (diary-newest-log cred)
  (with-web-session cred cookie
    (retrieve-log-page cred cookie (current-date) 1)))

;; for small log size only
;; *** DO NOT USE *** this function for too many access blog.
(define (diary-today-log cred)
  (with-web-session cred cookie
    (let loop ((res '())
               (page 1))
      (receive (log next)
          (retrieve-log-page cred cookie (current-date) page)
        (if next
          (loop (append res log) next)
          (sort (delete-duplicates (append res log))
                (^ (x y) (string>? (car x) (car y)))))))))

;; lazy log parser. Allows you to handle too many log.
(define (diary-today-log$ cred :optional (page #f))
  (with-web-session cred cookie
    (receive (log next)
        (retrieve-log-page cred cookie (current-date) (or page 1))
      (append log
              (if next
                (lazy (diary-today-log$ cred next))
                '())))))
      
;; lazy log parser. Allows you to handle too many log.
(define (diary-recent-log$ cred :optional (date (current-date)) (page #f))
  (with-web-session cred cookie
    (receive (log next)
        (retrieve-log-page cred cookie date (or page 1))
      (append log
              (cond
               [next
                (lazy (diary-recent-log$ cred date next))]
               [(let1 yest (date-add-days date -1)
                  (and (< (diff-days (current-date) yest) 3)
                       yest)) => 
                    (^ (yest) 
                      (lazy (diary-recent-log$ cred yest)))]
               [else
                '()])))))

;;
;; Low level API
;;

(define (web-login cred)
  (define (call)
    (let1 query (http-compose-query 
                 "/login"
                 `((:name ,(ref cred 'username)) 
                   (:password ,(ref cred 'password)))
                 'utf-8)
      (http-post "www.hatena.ne.jp" query #f :no-redirect #t :secure #t)))

  (define (retrieve status headers body)
    (check-status status headers body)
    (let1 cookies (concatenate
                   (map
                    (lambda (c)
                      (parse-cookie-string (list-ref c 1)))
                    (filter (lambda (x)
                              (and (pair? x)
                                   (string-ci=? (car x) "set-cookie")))
                            headers)))
      (unless (and (assoc "b" cookies)
                   (assoc "rk" cookies))
        (error <hatena-api-error>
               :status status :headers headers :body body
               "Login failed"))
      ;; create cookie string
      (string-join
       (construct-cookie-string
        (filter
         (lambda (c)
           (and (string? (car c))
                (member (car c) '("b" "rk"))))
         cookies))
       "; ")))

  (call-with-values call retrieve))

(define (web-logout cookie)
  (define (call)
    (web-get cookie "www.hatena.ne.jp" "/logout" :secure #t))

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

  (and-let* ((c (parse-cookie-string cookie))
             (rk (assoc "rk" c)))
    (md5-base64 (cadr rk))))

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

(define (decode-string str)
  (ces-convert str hatena-encoding (gauche-character-encoding)))

(define (encode-string str)
  (ces-convert str #f hatena-encoding))

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

(define (retrieve-log-page cred cookie date page)
  (define (call)
    (let* ((cid 1)
           (date (date->string date "~Y-~m-~d"))
           (type "daily")
           (params (make-query-params page cid date type)))
      (web-get cookie hatena-cuonter-host
               #`"/,(ref cred 'username)/log?,(compose-query params)")))

  (define (retrieve status headers body)
    (check-status status headers body)
    (parse-logs body))

  (call-with-values call retrieve))

(define %html-char 
  (let ((%special-char 
         ($or
          ($do [($s "&gt;")] ($return #\>))
          ($do [($s "&lt;")] ($return #\<))
          ($do [($s "&amp;")] ($return #\&))
          ($do [($s "&quot;")] ($return #\"))
          ($do [($s "&nbsp;")] ($return #\space))))
        (%raw-char ($do [c ($none-of #[<>])] ($return c))))
    ($or %special-char %raw-char)))

(define (parse-logs html)
  (define %ws
    ($skip-many ($one-of #[ \t\r\n])))

  (define %html-attr
    ($do 
     [v ($many ($one-of #[a-zA-Z0-9]))]
     ($return (apply string v))))

  (define ($html-attr-value quote)
    ($do 
     %ws
     [($c quote)]
     [v ($many
         ($none-of (list->char-set (list quote))))]
     [($c quote)]
     ($return (apply string v))))

  (define %html-value
    ($or
     ($html-attr-value #\")
     ($html-attr-value #\')
     ($do [v ($many ($none-of #[ \t\n>]) 1)] 
          ($return (apply string v)))))

  (define %html-attribute
    ($do
     [name %html-attr]
     [($c #\=)]
     [value %html-value]
     %ws
     ($return (list name value))))

  (define %html-attributes
    ($do
     %ws
     [attrs ($sep-by %html-attribute %ws)]
     ($return attrs)))

  (define ($html-start-tag tag)
    ($try
     ($do
      [($c #\<)]
      %ws
      [($string-ci tag)]
      [attrs %html-attributes]
      [($c #\>)]
      ($return attrs))))

  (define ($html-end-tag tag)
    ($try
     ($seq
      ($s "</")
      %ws
      ($string-ci tag)
      %ws
      ($c #\>))))

  (define ($cell p)
    ($do
     %ws
     [($html-start-tag "td")]
     [v p]
     [($html-end-tag "td")]
     %ws
     ($return (string-trim v))))

  (define %table-item
    (let ((%raw ($do 
                 [v ($many %html-char)]
                 ($return (apply string v))))
          (%url 
           ($or
            ($do
             %ws
             [attrs ($html-start-tag "a")]
             ;; throw away rest of results
             [($many-till anychar ($html-end-tag "td"))]
             ($return (car (assoc-ref attrs "href"))))
            ($do
             [v ($many-till %html-char ($html-end-tag "td"))]
             ($return (apply string v))))))
      ($try
       ($do
        %ws
        [($html-start-tag "tr")]
        [time ($cell %raw)]
        [url ($cell %url)]
        [agent ($cell %raw)]
        [lang ($cell %raw)]
        [display ($cell %raw)]
        [host ($cell %raw)]
        [referer ($cell %url)]
        [($html-end-tag "tr")]
        ($return 
         (receive (display-size display-bit)
             (cond
              ((string= display "")
               (values "" ""))
              ((#/^(.*)x([0-9]+)$/ display) =>
               (^m (values (m 1) (m 2))))
              (else
               (values display "")))
           (list time host referer agent lang display-size display-bit url)))))))

  (define %skip-table-header
    ($seq
     %ws
     ($html-start-tag "tr")
     ($many-till anychar ($html-end-tag "tr"))
     ($html-end-tag "tr")
     %ws))

  (define %log-table-parser
    ($do
     ($many-till anychar 
                 ($try 
                  ($do [attrs ($html-start-tag "table")] 
                       (if (and-let* ((p (assoc-ref attrs "id"))
                                      (id (car p))
                                      (pred (equal? id "log_table")))
                             #t)
                         ($return #t)
                         ($fail "Not yet")))))
     [($html-start-tag "table")]
     %skip-table-header
     [c ($many-till anychar ($html-end-tag "table"))]
     [d ($html-end-tag "table")]
     ($return (apply string c))))

  (define %log-row-parser
    ($many %table-item))
  
  (let* ([s (string-incomplete->complete html #\〓)]
         [p (open-input-string s)]
         [table (peg-parse-port %log-table-parser p)]
         [log (peg-parse-string %log-row-parser table)])
    (port-seek p 0)
    (let1 next
        (let loop ((l (read-line p)))
          (cond
           ((eof-object? l) #f)
           ;; magic word
           ((#/<a[ \t]+href=\".*page=([0-9]+).*>次の[0-9]+件/ l) =>
            (^m (string->number (m 1))))
           (else
            (loop (read-line p)))))
      (values log next))))

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
                        (decode-string body)))

    (call-with-values call retrieve)))

(define (web-diary-entries cred date)
  (diary-entry cred date))

;;TODO list
;; http://d.hatena.ne.jp/mhayashi1120/archive?mode=edit&mid=1
;; twitter_notification_enabled=0
;; twitter_notification_prefix=
;; trivial=0
;; mixi_check_notification_enabled=0

(define (diary-list cred :optional (page #f))
  (with-web-session cred cookie
    (define (call)
      (let* ([mid page]
             [mode "edit"]
             [params (make-query-params mode mid)])
        (web-get cookie hatena-host
                 #`"/,(ref cred 'username)/archive?,(compose-query params)")))

    (define (retrieve status headers body)
      (check-status status headers body)
      (decode-string body))

    (call-with-values call retrieve)))

;; TODO file->string some byte (ex: 0xc2) destroy the rest of bytes 
(define (read-as-bytes file)
  (use gauche.uvector)
  (let1 f (open-input-file file)
    (let loop ([c (read-byte f)]
               [res '()])
      (cond
       [(eof-object? c)
        (u8vector->string (list->u8vector (reverse res)))]
       [else
        (loop (read-byte f) (cons c res))]))))

(define (hatena-now)
  (* (sys-time) 1000))

(define (date-add-days date days)
  (time-utc->date
   (add-duration
    (date->time-utc date)
    (make-time time-duration 0 (* (* 24 60 60) days)))))

(define (diff-days date1 date2)
  (quotient (diff-seconds date1 date2) (* 24 60 60)))

(define (diff-seconds date1 date2)
  (time-second
   (time-difference
    (date->time-utc date1) (date->time-utc date2))))
