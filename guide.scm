;;;
;;;  GUIDE
;;;
(define-module guide
  (use srfi-1)
  (use srfi-13)
  (use gauche.process)
  (use gauche.vport)
  (use file.util)
  (use text.diff)
  (use ggc.file.util)
  (use ggc.util.circular-list)
  (use ggc.term.with-raw-mode)
  (use ggc.text.segment)
  (export guide guide-main)
  )
(select-module guide)

(define debug? #f)

;;;
;;; ggc.text.segment extension.
;;;
(define (text-goto-line text lino)
  (let ((tmp (text-end-of-line text 0 lino)))
    (text-beginning-of-line text tmp 1)))

(define (text-gen-getc text)
  (let ((i 0)
        (text text))
    (lambda ()
      (let getc ()
        (cond ((null? text) (eof-object))
              ((= i (string-length (car text)))
               (set! text (cdr text))
               (set! i 0)
               (getc))
              ((< i (string-length (car text)))
               (let ((r (string-ref (car text) i)))
                 (inc! i)
                 r))
              (else (error "something wrong")))))))
;;;
;;;
;;;
(define (get-real-time)
  (receive (sec usec) (sys-gettimeofday)
    (+ sec (* usec 1e-6))))

(define-condition-type <quit> <message-condition> #f)

(define (quit . args) (apply error (cons <quit> args)))

;;;
;;;  TEXT-BUFFER CLASS
;;;
(define-class <text-buffer> ()
  ((text      :init-value   '() 
              :init-keyword :text        
              :accessor text-of)
   (point     :init-value   0 
              :accessor point-of)
   (mark      :init-value   0
              :accessor mark-of)
   (name      :init-value #f
              :init-keyword :name 
              :accessor name-of)
   (follow    :init-value #f
              :accessor follow-cursor?)
   (persistent :init-value #f
               :accessor is-persistent?)
   (modified? :init-value #f
              :accessor is-modified?)
   (readonly? :init-value #f
              :accessor is-readonly?)
   (lmtime    :init-value 0.0
              :accessor modified-time-of) ; time of last text change
   (filename  :init-value #f 
              :accessor filename-of)
   (fmtime    :init-value 0                    ; mtime of the associated file
              :accessor mtime-of)
   (history   :init-value '()
              :init-keyword :history 
              :accessor history-of)
   (key-map   :init-value #f
              :accessor key-map-of
              :init-keyword :key-map)
   ))

(define (reinitialize-buffer buf)
  (set! (text-of buf)       '())
  (set! (point-of buf)       0)
  (set! (mark-of buf)        0)
  (set! (name-of buf)        #f)
  (set! (follow-cursor? buf) #f)
  (set! (is-persistent? buf) #f)
  (set! (is-modified? buf)   #f)
  (set! (is-readonly? buf)   #t)
  (set! (modified-time-of buf) 0)
  (set! (filename-of buf)    #f)
  (set! (mtime-of buf)       0)
  (set! (history-of buf)    '())
  (set! (key-map-of buf)    #f)
  )

(define (update-buffer buf m pos arg)

  (define (do-insert str)
    (let-syntax ((update! (syntax-rules ()
                            ((_ loc)
                             (if (>= loc pos)
                                 (inc! loc (string-length str)))))))
      (set! (text-of buf) (text-insert (text-of buf) pos str))
      (update! (point-of buf))
      (update! (mark-of buf))))

  (define (do-delete count)
    (let-syntax ((update! (syntax-rules ()
                            ((_ loc)
                             (cond ((> loc (+ pos count))
                                    (dec! loc (+ pos count)))
                                   ((> loc pos)
                                    (set! loc pos)))))))
      ;; XXX need to check range of count
      (set! (text-of buf) (text-delete (text-of buf) pos count))
      (update! (point-of buf))
      (update! (mark-of buf))))

  (define (do-replace text)
    (set! (text-of  buf) text)
    (set! (point-of buf) 0)
    (set! (mark-of  buf) 0))

  (if (is-readonly? buf)  (quit "Read only buffer"))

  (let ((curtime (get-real-time))
        (modtime (modified-time-of buf)))
    (if (> (- curtime modtime) 0.2) (buffer-push-history buf)))
    
  (case m
    ((insert)  (do-insert  arg))
    ((delete)  (do-delete  arg))
    ((replace) (do-replace arg))
    (else
     (message "update-buffer: unknown method: ~a" m)))

  (set! (modified-time-of buf) (get-real-time))
  (set! (is-modified? buf) #t)
  )

(define (buffer-flatten-text buf)
  ;; flatten does not modify contents of text
  (set! (text-of buf) (text-flatten (text-of buf))))

(define-method insert ((buf <text-buffer>) 
                       (pos <integer>)
                       (str <string>))
  (update-buffer buf 'insert pos str))

(define-method delete ((buf    <text-buffer>)
                       (pos    <integer>)
                       (count  <integer>))
  (update-buffer buf 'delete pos count))

(define (buffer-replace-text buf text)
  (update-buffer buf 'replace 0 text))

(define (buffer-let-window-always-follow-point buf)
  (set! (follow-cursor? buf) #t))

(define (buffer-make-persistent buf)
  (set! (is-persistent? buf) #t))

(define (buffer-set-filename buf fname)
  (set! (filename-of buf) fname)
  (let ((mtime (if (file-is-regular? fname)
                   (sys-stat->mtime (sys-stat (filename-of buf)))
                   0)))
    (set! (mtime-of buf) mtime)
    (set! (modified-time-of buf) mtime)))

(define (buffer-match-mtime? buf)
  (and (mtime-of buf)
       (filename-of buf)
       (= (mtime-of buf)
          (sys-stat->mtime (sys-stat (filename-of buf))))))

(define (buffer-load-from-file buf fname)
  (if (and (file-exists? fname)
           (file-is-regular? fname))
      (let ((str (file->string fname)))
        (buffer-replace-text buf (list str))
        (set! (is-modified? buf) #f)
        (if (file-is-readonly? fname)
            (set! (is-readonly? buf) #t))
        (buffer-set-filename buf fname)
        (set! (history-of buf) '())
        (buffer-push-history buf))
      (quit "Could not read file")))

(define (buffer-save-to-file buf fname)
  (buffer-flatten-text buf)
  (call-with-output-file fname
    (lambda (p) (display (car (text-of buf)) p)))
  (set! (is-modified? buf) #f)
  (buffer-set-filename buf fname)
  (message "Wrote ~s" fname))

(define (buffer-erase buf)
  (buffer-replace-text buf '())
  (set! (history-of buf) '())
  (set! (is-modified? buf) #f))

(define (buffer-delete-region buf from to)
  (cond ((= from to) #f)
        ((> from to) 
         (buffer-delete-region buf to from))
        (else
         (delete buf from (- to from)))))

(define-method region->string ((buf <text-buffer>)
                              (from <integer>)
                              (to   <integer>))
  (cond ((> from to) (region->string buf to from))
        ((= from to) "")
        (else
         (text->string (text-get-text (text-of buf) from (- to from))))))
(define-method region->string ((buf <text-buffer>))
  (region->string buf (point-of buf) (mark-of buf)))

(define-method line-number-of ((buf <text-buffer>))
  (let ((text (text-of buf))
        (pos  (point-of buf)))
    (text-line-number text pos)))

(define-method number-of-lines-of ((buf <text-buffer>))
  (text-number-of-lines (text-of buf) 0))

;;;
;;;  HISTORY
;;;
(define-class <text-moment> ()
  ((point   :init-value 0
            :init-keyword :point
            :accessor point-of)
   (text    :init-value '()
            :init-keyword :text
            :accessor text-of)
   (mod?    :init-keyword :modified?
            :accessor is-modified?)
   (time    :init-value 0
            :init-keyword :modified-time
            :accessor modified-time-of)))

(define-method make-moment ((buf <text-buffer>))
  (make <text-moment>
    :point         (point-of         buf) 
    :text          (text-of          buf)
    :modified?     (is-modified?     buf)
    :modified-time (modified-time-of buf)))

(define (history-find-mtime history mtime)
  (find (lambda (moment)
          (= mtime (mtime-of moment)))
        history))

(define (history-find-last-saved history)
  (find (lambda (moment) 
          (not (is-modified? moment)))
        history))

(define (buffer-push-history buf)
  (push! (history-of buf) (make-moment buf)))

(define (buffer-pop-history buf)
  ;; so called ``undo''
  (if (null? (history-of buf))
      (quit "buffer does not have history")
      (let ((moment (pop! (history-of buf))))
        (set! (point-of         buf) (point-of         moment))
        (set! (text-of          buf) (text-of          moment))
        (set! (is-modified?     buf) (is-modified?     moment))
        (set! (modified-time-of buf) (modified-time-of moment))
        )))

(define-method find-history-by-mtime ((buf <text-buffer>) mtime)
  (history-find-mtime (history-of buf) mtime))

(define-method find-last-saved-history ((buf <text-buffer>) n)
  (history-find-last-saved (history-of buf)))

;;;
;;;   TEXT BUFFER PORT
;;;
(define-class <input-text-buffer-port> (<virtual-input-port>)
  ((text-buffer :init-keyword :text-buffer
                :accessor text-buffer-of)))

(define-class <output-text-buffer-port> (<virtual-output-port>)
  ((text-buffer :init-keyword :text-buffer
                :accessor text-buffer-of)))
   
(define (open-input-text-buffer buf)
  (let ((port (make <input-text-buffer-port> :text-buffer buf)))
    (define (getc)
      (let ((ch (text-get-character (text-of  buf)
                                    (point-of buf))))
        (if ch (inc! (point-of buf)))
        ch))
    (slot-set! port 'getc getc)
    port))

(define (open-output-text-buffer buf)
  (let ((port (make <output-text-buffer-port> :text-buffer buf)))
    (define (putc ch)
      (insert (text-buffer-of port) (string ch)))
    (define (puts str)
      (insert (text-buffer-of port) str))
    (slot-set! port 'putc putc)
    (slot-set! port 'puts puts)
    port))

(define-method with-output-to ((buf  <text-buffer>)
                               (thunk <procedure>))
  (let ((port #f))
    (dynamic-wind
        (lambda () (set! port (open-output-text-buffer buf)))
        (lambda () (with-output-to-port port thunk))
        (lambda () (close-output-port port)))))

(define-method with-input-from ((buf <text-buffer>)
                                (thunk <procedure>))
  (let ((port #f))
    (dynamic-wind
        (lambda () (set! port (open-input-text-buffer buf)))
        (lambda () (with-input-from-port port thunk))
        (lambda () (close-input-port port)))))

;;;
;;;
;;;
(define-syntax define-command
  (syntax-rules ()
    ((_ sym (buf) body ...)
     (begin
       (define-method sym ((buf <text-buffer>)
                           _)
         body ...)
       (define-method sym ((buf <text-buffer>))
         (sym buf 1))
       (define-method sym ((name <string>))
         (sym (get-buffer name)))
       (define-method sym (_)
         (sym (current-buffer) _))
       (define-method sym ()
         (sym (current-buffer) 1))))
    ((_ sym (buf arg) body ...)
     (begin
       (define-method sym ((buf <text-buffer>) arg)
         body ...)
       (define-method sym ((buf <text-buffer>))
         (sym buf 1))
       (define-method sym ((name <string>))
         (sym (get-buffer name) 1))
       (define-method sym (arg)
         (sym (current-buffer) arg))
       (define-method sym ()
         (sym (current-buffer) 1))))))

(define-command set-mark-command (buf)
  (set! (mark-of buf) (point-of buf))
  (message "Mark Set"))

(define-command exchange-point-and-mark (buf)
  (let ((p (point-of buf))
        (m (mark-of buf)))
    (set! (point-of buf) m)
    (set! (mark-of buf) p)))

(define-command point (buf) (point-of buf))
(define (point-min . x)  0)
(define-command point-max (buf) (text-size (text-of buf)))

;;;
;;;  BUFFER MANAGEMENT
;;;
(define (make-unique-buffer-name name)
  (let lp ((candidate name)
           (n 1))
    (cond ((get-buffer candidate)
           (lp (format #f "~a<~d>" name n) 
               (+ n 1)))
          (else candidate))))

(define (make-buffer name)
  (let ((buf (make <text-buffer> 
               :name (make-unique-buffer-name name)
               :key-map (copy-key-map (global-key-map-of the-editor)))))
    (circular-append! (buffers-of the-editor) buf)
    buf))

(define (remove-buffer-from-the-editor buf)
  (if (eq? buf (current-buffer))
      (circular-pop! (buffers-of the-editor))
      (let ((cbuf (current-buffer)))
        (select-buffer buf)
        (circular-pop! (buffers-of the-editor))
        (select-buffer cbuf))))

(define (kill-buffer-function buf)

  (define (remove-buffer-from-windows buf)
    (for-each (lambda (frame)
              (for-each (lambda (win)
                          (if (eq? buf (buffer-of win) )
                              (set! (buffer-of win) (current-buffer))))
                        (windows-of frame)))
            (frames-of the-editor)))

  (cond ((is-persistent? buf) (quit "You can't kill this buffer"))
        (else (reinitialize-buffer buf)
              (remove-buffer-from-the-editor buf)
              (remove-buffer-from-windows buf))))

(define (kill-buffer name-or-buf)
  (define (ask-if-really-be-killed buf)
    (if (not 'yes)
        (quit "Quit")))
  (let ((buf (get-buffer name-or-buf)))
    (cond ((and buf (is-readonly? buf))
           (ask-if-really-be-killed buf)
           (kill-buffer-function buf))
          (buf
           (kill-buffer-function buf))
          (else #f))))

(define (select-buffer-function pred)
  (if (circular-find-and-bring-it-top! (buffers-of the-editor) pred)
      (current-buffer)
      (quit "No such buffer")))

(define-method select-buffer ((name <string>))
  (select-buffer-function (lambda (e) (string=? (name-of e) name))))

(define-method select-buffer ((buf <text-buffer>))
  (select-buffer-function (lambda (e) (eq? e buf))))

(define (switch-to-next-buffer args)
  (set! (buffers-of the-editor) (cdr (buffers-of the-editor))))

(define (switch-to-buffer arg)
  (let* ((next (name-of (cadr (buffer-list))))
         (prompt  (format #f "Switch to buffer (default ~a): " next))
         (name    (read-from-mini-buffer prompt '())))
    (if (string=? name "")
        (switch-to-next-buffer '())
        (select-buffer name))))

(define (find-buffer-function pred) 
  (cond ((circular-find (buffer-list) pred) => car)
        (else #f)))

(define (get-buffer name)
  (find-buffer-function (lambda (e) (string=? name (name-of e)))))

(define (get-buffer-filename name)
  (find-buffer-function (lambda (e)
                          (and (filename-of e)
                               (string=? name (filename-of e))))))

(define (get-buffer-create name)
  (let ((buf (get-buffer name)))
    (if buf buf (make-buffer name))))

;;;
;;;  FILE
;;;
(define (find-file-noselect fname)
  (if (relative-path? fname)
      (find-file-noselect (build-path (current-directory) fname))
      (cond ((get-buffer-filename fname)
             => (lambda (x) x))
            
            ((not (file-exists? fname))
             (let ((buf (make-buffer (sys-basename fname))))
               (buffer-set-filename buf fname)
               buf))

            ((file-is-regular? fname)
             (let ((buf (make-buffer (sys-basename fname))))
               (buffer-load-from-file buf fname)
               buf))

            ((file-is-directory? fname)
             (let* ((buf (get-buffer-create fname))
                    (out (open-output-text-buffer buf)))
               (erase buf)
               (call-with-input-process (format #f "ls -l ~a" fname)
                 (lambda (in) 
                   (copy-port in out)))
               (close-output-port out)
               (beginning-of-buffer buf)
               buf))

            (else
             (quit "find-file-noselect: something wrong")
             #f))))

(define (find-file fname)
  (if (string? fname)
      (let ((buf (find-file-noselect fname)))
        (if buf (select-buffer buf)))
      (let ((fn (read-from-mini-buffer "Find File: " '())))
        (find-file fn))))

(define-command save-buffer (buf n)
  (cond ((not (filename-of buf))
         (buffer-flatten-text buf)
         (message "buffer is not associated with file"))
        ((not (is-modified? buf))
         (message "(No changes need to be saved)"))
        ((= (mtime-of buf) 0)    ; virgin file
         (buffer-save-to-file buf (filename-of buf)))
        ((not (buffer-match-mtime? buf))
         (quit "File has changed on disk"))
        (else
         (buffer-save-to-file buf (filename-of buf)))))

(define-command kill-buffer (buf n)
  (kill-buffer-function buf))

;;;
;;;  EDIT COMMANDS
;;;

(define-method insert ((buf <text-buffer>)
                       (str <string>))
  (insert buf (point-of buf) str))
(define-method insert ((buf <text-buffer>) x) (insert buf (x->string x)))
(define-method insert (x)                     (insert (current-buffer) x))

(define-command delete (buf count)            (delete buf (point-of buf) count))
(define-command erase (buf)  (buffer-erase buf))

(define-method kill-region ((buf <text-buffer>)
                            (from <integer>)
                            (to   <integer>))
  (let ((str (region->string buf from to)))
    (buffer-delete-region buf from to)
    (set! (kill-ring-of the-editor) (list str))))

(define-command kill-region (buf)
  (kill-region buf (mark-of buf) (point-of buf))
  (set! (point-of buf) (mark-of buf)))

(define-method copy-region ((buf <text-buffer>)
                            (from <integer>)
                            (to   <integer>))
  (let ((str (region->string buf from to)))
    (set! (kill-ring-of the-editor) (list str))))

(define-command copy-region (buf)
  (copy-region buf (mark-of buf) (point-of buf))
  (message "Region copied"))

(define-command yank (buf n)
  (insert buf (car (kill-ring-of the-editor))))

(define-command undo (buf n)
  (dotimes (x n) (buffer-pop-history buf)))

(define-command insert-tab (buf count)
  (dotimes (x count) (insert "    ")))

(define-command backward-delete (buf count)
  (let ((pp (point-of buf)))
    (backward-char buf count)
    (delete buf (- pp (point-of buf)))))

(define-command beginning-of-line (buf count)
  (set! (point-of buf) 
        (text-beginning-of-line (text-of buf) (point-of buf) count)))
    
(define-command end-of-line (buf count)
  (set! (point-of buf)
        (text-end-of-line (text-of buf) (point-of buf) count)))

(define (buffer-delete-line buf n proc)
  (let ((f (point-of buf))
        (e (text-end-of-line (text-of buf) (point-of buf) n)))
    (cond ((= f (point-max)) #f)
          ((= f e)
           (forward-char)
           (backward-delete))
          (else
           (proc buf f e)))))

(define-command kill-line (buf n)
  (buffer-delete-line buf n kill-region))

(define-command delete-line (buf n)
  (buffer-delete-line buf n buffer-delete-region))

(define-command goto-line (buf n)
  (set! (point-of buf) (text-goto-line (text-of buf) n)))

(define-command goto-line-interactive (buf n)
  (let ((ln (string->number (read-from-mini-buffer "Goto line: " '()))))
    (if (and ln (integer? ln) (> ln 0))
        (goto-line buf ln)
        (message "Invalid number"))))

(define-command beginning-of-buffer (buf n)
  (set-mark-command buf)
  (set! (point-of buf) 0))

(define-command end-of-buffer (buf n)
  (set-mark-command buf)
  (set! (point-of buf) (text-size (text-of buf))))

(define-command previous-line (buf n)
  (set! (point-of buf) 
        (text-previous-line (text-of buf) (point-of buf) n)))

(define-command next-line (buf n)
  (set! (point-of buf) 
        (text-next-line (text-of buf) (point-of buf) n)))

(define-command new-line (buf n)
  (dotimes (x n) 
    (insert buf "\n")))

(define-command open-line (buf n)
  (new-line buf n)
  (backward-char buf n))

(define-command forward-char (buf n)
  (let ((size (text-size (text-of buf))))
    (inc! (point-of buf) n)
    (cond ((< (point-of buf) 0)
           (set! (point-of buf) 0)
           (message "Beginning of buffer"))
          ((< size (point-of buf))
           (set! (point-of buf) size)
           (message "End of buffer")))))

(define-command backward-char (buf n) 
  (forward-char buf (- n)))

(define-command search-forward (buf str)
  (receive (p txt) 
      (text-search-forward (text-of buf) (point-of buf) str)
    (cond ((not p)
           (message "\"~a\" not found" str) 
           #f)
          (else
           (set! (point-of buf) (+ p (string-length str))) 
           #t))))

(define-command search-backward (buf str)
  (receive (p txt) 
      (text-search-backward (text-of buf) (point-of buf) str)
    (cond ((not p)
           (message "\"~a\" not found~%" str)
           #f)
          (else
           (set! (point-of buf) (if (> p 0) p 0))
           #t))))

(define-command transpose-chars (buf n)
  (dotimes (x n)
    (let ((ch (text-get-character (text-of buf) (point-of buf))))
      (delete buf 1)
      (backward-char buf 1)
      (insert ch)
      (forward-char buf 1))))

;;;
;;;   LIST BUFFERS
;;;
(define (print-buffers)
  (define (prn buf)
    (format #t "  ~14,,,,14:a ~8d   ~,,,,30:a~%"
            (if (> (string-length (name-of buf)) 20)
                (substring (name-of buf) 
                           (- (string-length (name-of buf)) 20)
                           (string-length (name-of buf)))
                (name-of buf))
            (text-size (text-of buf))
            (filename-of buf)))
  (format #t "  ~14,,,,14:a ~8@a   ~,,,,30:a~%" "Name" "Size" "File")
  (circular-for-each prn (buffers-of the-editor)))

(define (list-buffers arg)
  (let* ((buf  (get-buffer-create "*Buffer List*"))
         (port (open-output-text-buffer buf)))
    (erase buf)
    (with-output-to-port port print-buffers)
    (close-output-port port)
    (buffer-flatten-text buf)
    (select-buffer buf)))

;;;
;;;   DESCRIBE BINDINGS
;;;
(define (describe-bindings arg)
  (let* ((buf (get-buffer-create "*Help*"))
         (port (open-output-text-buffer buf)))
    (erase buf)
    (with-output-to-port port
      (lambda () (print-bindings (key-map-of (current-buffer)))))
    (close-output-port port)
    (buffer-flatten-text buf)
    (select-buffer buf)))

;;;
;;;   DIFF BUFFER
;;;
(define-command print-diff (buf n)
  (define (writer str type)
    (if type
        (format #t "~a ~a~%" type str)
        (format #t "   ~a~%" str)))
  (let ((moment (find-last-saved-history buf n)))
    (if moment
        (let ((now  (text->string (text-of buf)))
              (then (text->string (text-of moment))))
          (format #t "--- ~a" (sys-ctime (modified-time-of moment)))
          (format #t "+++ ~a" (sys-ctime (modified-time-of buf)))
          (diff-report then now :writer writer)))))

(define-command diff-buffer (buf n)
  (let* ((outbuf (get-buffer-create "*Diff*"))
         (port   (open-output-text-buffer outbuf)))
    (erase outbuf)
    (with-output-to-port port (lambda () (print-diff buf n)))
    (close-output-port port)
    (buffer-flatten-text outbuf)
    (select-buffer outbuf)))

;;;
;;;   EVAL REGION
;;;
(define (print-values . vals)
  (display "=>")
  (for-each (lambda (x) (display " ") (write/ss x)) vals))

(define (eval-and-print expr)
  (write expr)
  (newline)
  (call-with-values (lambda () (eval expr (interaction-environment)))
    print-values))

(define-method eval-region ((buf <text-buffer>)
                            (from <integer>)
                            (to   <integer>))
  (define (eval-port)
    (let lp ((expr (read))
             (vals '()))
      (if (eof-object? expr) 
          vals
          (lp (read) 
              (call-with-values 
                  (lambda () (eval expr (interaction-environment)))
                list)))))
  
  (let ((str (region->string buf from to)))
    (print str)
    (apply print-values (with-input-from-string str eval-port))))
        
(define-method eval-region (x)
  (eval-region (current-buffer)
               (mark-of (current-buffer))
               (point-of (current-buffer))))

(define (eval-buffer x)
  (eval-region (current-buffer) (point-min) (point-max)))

(define (read-last-sexp text pos count)
  (if (= count 0) 
      #f
      (let ((str (text->string (text-get-text text pos count))))
        (with-input-from-string str
          (lambda ()
            (guard (e ((condition-has-type? e <read-error>)
                       (read-last-sexp text (+ pos 1) (- count 1))))
                   (let lp ((v (read)) 
                            (w #f))
                     (if (eof-object? v)
                         w
                         (lp (read) v)))))))))

(define-command eval-last-sexp (buf)
  ;; XXX This needs to be smarter.
  (let ((text (text-of buf)))
    (cond ((let ((beg (text-beginning-of-line text (point-of buf) 1)))
             (and beg (read-last-sexp text beg (- (point-of buf) beg))))
           => eval-and-print)
          ((let ((beg (text-search-backward text (point-of buf) "\n(")))
             (and beg (read-last-sexp text beg (- (point-of buf) beg))))
           => eval-and-print)
          ((read-last-sexp text 0 (point-of buf))
           => eval-and-print)
          (else (message "Could not find sexp")))))

(define-command eval-next-closing-sexp (buf)
  (let ((save (point-of buf)))
    (cond ((text-search-backward (text-of buf) (point-of buf) "\n(")
           => (lambda (p) (set! (point-of buf) p)))
          (else (set! (point-of buf) 0)))
    (dynamic-wind 
        (lambda () #f)
        (lambda ()
          (let lp ((expr (with-input-from buf read)))
            (cond ((>= (point-of buf) save)
                   (eval-and-print expr))
                  (else "Could not find sexp"))))
        (lambda ()
          (set! (point-of buf) save)))))

;;;
;;;  SEXP
;;;
(define-command insert-and-find-matching-paren (buf)

  (define (show-cursor-for-a-while-at pos)
    (let ((win (current-window)))
      (set! (tmp-point-of win) pos)
      (put-alarm-list! (lambda () (set! (tmp-point-of win) #f)))
      (sys-alarm 1)))
      
  (insert buf #\))
  (let ((save (point-of buf)))
    (cond ((text-search-backward (text-of buf) (point-of buf) "\n(")
           => (lambda (p) (set! (point-of buf) p)))
          (else (set! (point-of buf) 0)))
    (dynamic-wind 
        (lambda () #f)
        (lambda ()
          (let lp ((pos (point-of buf))
                   (str (text->string (text-get-text (text-of buf)
                                                     (point-of buf)
                                                     (- save (point-of buf))))))
            (guard (e ((condition-has-type? e <read-error>)
                       (lp (+ pos 1)
                           (substring str 1 (string-length str)))))
                   (let ((x (with-input-from-string str (lambda () 
                                                          (read)
                                                          (peek-char)))))
                     (cond ((>= pos save)
                            (error <quit> "unmatched paren"))
                           ((and (eof-object? x)
                                 (text-find-character-after (text-of buf) pos #\())
                            => show-cursor-for-a-while-at)
                           (else
                            (lp (+ pos 1)
                                (substring str 1 (string-length str)))))))))
        (lambda ()
          (set! (point-of buf) save)))))

;;;
;;;  TEXT-WINDOW AND FRAME
;;;
(define-class <frame> ()
  ((minibuf :init-value #f
            :accessor mini-buffer-of)
   (windows :init-value '()
            :accessor windows-of)
   (window  :init-value #f
            :accessor current-window-of)
   (width   :init-value 80
            :init-keyword :width
            :accessor width-of)
   (height  :init-value 25
            :init-keyword :height
            :accessor height-of)))

(define-class <text-window> ()
  ((buffer :init-keyword :buffer
           :accessor buffer-of)
   (start  :init-value 1        ; starting point of buffer for display
           :init-keyword :start
           :accessor start-of)
   (height :init-value 23       ; number of lines
           :init-keyword :hight
           :accessor height-of)
   (point  :init-value 0        ; cursor point
           :accessor point-of)
   (tmp    :init-value #f       ; tmp cursor point
           :accessor tmp-point-of)
   (frame  :init-keyword :frame
           :accessor frame-of)))

(define-method get-event ((frame <frame>)) #f)
(define-method event-pending? ((frame <frame>)) #f)
(define-method get-size  ((frame <frame>)) (values 0 0))
(define-method display-frame ((frame <frame>)) #f)
(define-method display-frame () (display-frame (current-frame)))
(define-method ring-bell  ((frame <frame>) n) #t)
(define-method ring-bell (n)  (ring-bell (current-frame) n))
(define-method keyboard-quit ((frame <frame>) n)
  (ring-bell frame n) (quit "Quit"))
(define-method keyboard-quit (n) (keyboard-quit (current-frame) n))

(define-method split-window ((frame <frame>))
  (let* ((win (current-window-of frame))
         (len (height-of win)))
    (if (< (quotient len 2) 3)
        (quit "Current Window is too small"))
    (let ((new-win (make <text-window> :frame frame))
          (new-len (quotient len 2)))
      (set! (buffer-of new-win) (buffer-of win))
      (set! (height-of new-win) new-len)
      (set! (point-of  new-win) (point-of win))
      (set! (height-of win) (- (height-of win) new-len 1))

      ;; put new-win right befeore win
      (let lp ((w (windows-of frame))
               (r '()))
        (cond ((null? w)
               (quit "Current window is not belong to the frame"))
              ((eq? (car w) win)
               (set! (windows-of frame) 
                     (append (reverse r) (cons new-win w))))
              (else (lp (cdr w) (cons (car w) r)))))
      (set! (current-window-of frame) new-win)
      new-win)))
(define-method split-window (x)  (split-window (current-frame)))
(define-method split-window ()  (split-window (current-frame)))

(define-method delete-window ((frame <frame>)
                              (win <text-window>))
  (cond ((<= (length (windows-of frame)) 1)
         (quit "Can't delete sole window"))
        ((not (memq win (windows-of frame)))
         (quit "Window does not belong to the frame")))
  (select-window win)
  (inc! (height-of (next-window frame)) (+ 1 (height-of win)))
  (set! (windows-of frame) (delete! win (windows-of frame) eq?))
  (if (eq? win (current-window-of frame))
      (set! (current-window-of frame) (car (windows-of frame)))))
(define-method delete-window ((frame <frame>))
  (delete-window frame (current-window)))
(define-method delete-window ((win <text-window>))
  (delete-window (current-frame) win))
(define-method delete-window (x)
  (delete-window (current-frame) (current-window)))
(define-method delete-window ()
  (delete-window (current-frame) (current-window)))
(define (delete-other-window x)
  (delete-window (current-frame) (next-window)))

(define-method select-window ((frame <frame>) (win <text-window>))
  (cond ((not (memq win (windows-of frame)))
         (quit "Window does not belong to the frame"))
        ((not (follow-cursor? (buffer-of win)))
         (set! (point-of (buffer-of win)) (point-of win))))
  (set! (current-window-of frame) win))
(define-method select-window ((win <text-window>))
  (select-window (current-frame) win))

(define-method next-window ((frame <frame>))
  (let ((w (memq (current-window) (windows-of frame))))
    (cond ((and w (null? (cdr w)))
           (car (windows-of frame)))
          (w (cadr w)))))
(define-method next-window () (next-window (current-frame)))

(define (other-window . n) (select-window (next-window)))

;;;
;;;  WINDOW
;;;
(define-method recenter ((win <text-window>) _)
  (let* ((len   (height-of win))
         (buf   (buffer-of win))
         (lino  (line-number-of (buffer-of win)))
         (newln (- lino (quotient len 2))))
    (if (> newln 1)
        (set! (start-of win) newln)
        (set! (start-of win) 1))))

(define (recenter-window _) (recenter (current-window) _))

(define-method scroll-up ((win <text-window>) n)
  (let* ((buf     (buffer-of win))
         (sln     (start-of win))
         (hlen    (quotient (height-of win) 2))
         (nol     (number-of-lines-of (buffer-of win)))
         (newln   (+  sln (* hlen n))))
    (if (<= newln  (- nol hlen))
        (let ((pos (text-goto-line (text-of buf) newln)))
          (set! (start-of win) newln)
          (set! (point-of buf) pos))
        (message "End of buffer"))))
(define-method scroll-up (n) (scroll-up (current-window) n))

(define-method scroll-down ((win <text-window>) n)
  (let* ((buf   (buffer-of win))
         (sln   (text-line-number (text-of buf) (start-of win)))
         (hlen  (quotient (height-of win) 2))
         (nol   (number-of-lines-of (buffer-of win)))
         (newln   (- sln (* hlen n))))
    (if (>= newln 1)
        (let* ((pos (text-goto-line (text-of buf) newln)))
          (set! (start-of win) newln)
          (set! (point-of buf) pos))
        (message "Beginning of buffer"))))
(define-method scroll-down (arg) (scroll-down (current-window) arg))

;;;;
;;;;   KEY MAP (EVENT MAP?)
;;;;
(define (make-key-map)               (make-hash-table))
(define (key-map-put! kmap key obj)  (hash-table-put! kmap key obj))
(define (key-map-get kmap key)       (hash-table-get  kmap key #f))
(define (key-map? obj)               (hash-table? obj))
(define (key-map-for-each proc kmap) (hash-table-for-each kmap proc))
(define (key-map-map proc kmap)      (hash-table-map kmap proc))

(define (key-map->list kmap)
  (key-map-map (lambda (key obj)
                 (if (key-map? obj)
                     (list key (key-map->list obj))
                     (list key obj)))
               kmap))

(define (list->key-map lis)
  (let ((kmap (make-key-map)))
    (for-each (lambda (elm)
                (let ((key (list-ref elm 0))
                      (obj (list-ref elm 1)))
                  (if (pair? obj)
                      (key-map-put! kmap key (list->key-map obj))
                      (key-map-put! kmap key obj))))
              lis)
    kmap))

(define (copy-key-map kmap)
  (list->key-map (key-map->list kmap)))

(define (char->control-code ch)
  (case ch ((#\@) 0)  ((#\\) 28)  
           ((#\]) 29) ((#\/) 127)
    (else
     (- (char->integer ch)
        (char->integer #\a)
        -1))))

(define (control-code->char int)
  (case int ((0)   #\@) ((28)  #\\)
            ((29)  #\]) ((127) #\/)
    (else
     (integer->char (+ int (char->integer #\a) -1)))))

(define (key-stack->string stack)
  (with-output-to-string 
    (lambda ()
      (for-each (lambda (x)
                  (cond ((= x 27)
                         (display "M-"))
                        ((< x 31)
                         (display "C-")
                         (display (control-code->char x))
                         (display " "))
                        (else 
                         (display (integer->char x))
                         (display " "))))
                (reverse stack)))))

(define (list->key-spec lis)
  (with-output-to-string 
    (lambda ()
      (for-each (lambda (x)
                  (cond ((= x 27)
                         (display "\\M-"))
                        ((< x 31)
                         (display "\\C-")
                         (display (control-code->char x)))
                        (else 
                         (display (integer->char x)))))
                lis))))

(define (key-spec->list spec)

  (define (handle-modifier ch lis)
    (cond ((char=? ch #\C)
           (read-char)                  ; #\-
           (let ((x (read-char)))
             (read-spec (read-char)
                        (cons (char->control-code x)
                              lis))))
          ((char=? ch #\M)
           (read-char)                  ; #\-
           (let ((x (read-char)))
             (read-spec (read-char) 
                        (cons (char->integer x)
                              (cons 27 lis))))) ;; ESC=27 
          (else
           (quit "unknown modifier"))))

  (define (read-spec ch lis)
    (cond ((eof-object? ch)  (reverse lis))
          ((char=? ch #\\)
           (handle-modifier (read-char) lis))
          (else
           (read-spec (read-char) 
                      (cons (char->integer ch)
                            lis)))))

  (with-input-from-string spec 
    (lambda () (read-spec (read-char) '()))))

(define (define-key kmap spec cmd)
  (let lp ((stack (key-spec->list spec))
           (kmap kmap))
    (cond ((null? stack) #f)
          ((null? (cdr stack))
           (key-map-put! kmap (car stack) cmd))
          ((key-map? (key-map-get kmap (car stack)))
           (lp (cdr stack)  
               (key-map-get kmap (car stack))))
          (else
           (let ((nkmap (make-key-map)))
             (key-map-put! kmap (car stack) nkmap)
             (lp (cdr stack) nkmap))))))

(define (global-set-key spec cmd)
  (define-key (global-key-map-of the-editor) spec cmd)
  (for-each (lambda (buf)
              (define-key (key-map-of buf) spec cmd))
            (buffers-of the-editor)))

(define (key-map-cmpfn a b) (< (car a) (car b)))

(define (print-bindings kmap)
  (let ((lis (key-map->list kmap)))
    (print-bindings-list (sort lis key-map-cmpfn) '())))

(define (print-bindings-list lis stack)
  (for-each (lambda (elm)
              (let ((key (list-ref elm 0))
                    (obj (list-ref elm 1)))
                (cond ((pair? obj)
                       (print-bindings-list (sort obj key-map-cmpfn) 
                                            (cons key stack)))
                      ((procedure? obj)
                       (format #t "~10a ~a~%"
                               (key-stack->string (cons key stack))
                               (procedure-info obj)))
                      (else
                       (format #t "~10a ~a~%"
                               (key-stack->string (cons key stack))
                               obj)))))
            lis))

;;;
;;;   MINI BUFFER
;;;
(define-class <mini-buffer> (<text-buffer>)
  ((prompt :init-value ""
           :init-keyword :prompt
           :accessor prompt-of)
   ))

(define (make-mini-buffer frame prompt kmap)
  (let ((minibuf (make <mini-buffer> 
                   :name (make-unique-buffer-name " *Mini-Buffer*")
                   :key-map (copy-key-map kmap)
                   :prompt prompt)))
    (circular-append! (buffers-of the-editor) minibuf)
    (set! (mini-buffer-of frame) minibuf)
    minibuf))

(define (kill-mini-buffer frame)
  (cond ((mini-buffer-of frame)
         => (lambda (minibuf)
              (set! (prompt-of minibuf) "")
              (reinitialize-buffer minibuf)
              (remove-buffer-from-the-editor minibuf)
              (set! (mini-buffer-of frame) #f)))
        (else (quit "No mini-buffer to kill"))))

(define (make-exit-from-mini-buffer frame minibuf curbuf k)
  (lambda _
    (cond ((eq? minibuf (mini-buffer-of frame))
           (let ((str (text->string (text-of minibuf))))
             ;;(kill-mini-buffer frame)
             ;;(select-buffer curbuf)
             (k str)))
          (else 
           (error "exit-from-mini-buffer")))))

(define-method read-from-mini-buffer ((frame  <frame>)
                                      (prompt <string>)
                                      (kmap   <hash-table>)
                                      hist)
  (call/cc (lambda (k)
             (let* ((curbuf  (current-buffer))
                    (minibuf (make-mini-buffer frame prompt kmap))
                    (efmb    (make-exit-from-mini-buffer frame minibuf 
                                                         curbuf k)))

               (define-key (key-map-of minibuf) "\\C-m" efmb)
               (select-buffer minibuf)
               (dynamic-wind
                   (lambda () #f)
                   interactive-loop0
                   (lambda ()
                     (kill-mini-buffer frame)
                     (select-buffer curbuf)))))))

(define-method read-from-mini-buffer ((prompt <string>)
                                      hist)
  (read-from-mini-buffer (current-frame) 
                         prompt 
                         (default-mini-buffer-key-map-of the-editor)
                         hist))

(define (test-mini-buffer . _)
  (let ((str (read-from-mini-buffer "TEST: " '())))
    (format #t "test-mini-buffer: ~s~%" str)))

;;;
;;;   COMMAND LOOP
;;;
(define (universal-argument n)
  (interactive-loop1 (+ n 4) (key-map-of (current-buffer)) '()))

(define (interactive-loop0)
  (let loop ((kmap (key-map-of (current-buffer))))
    (update-display)
    (interactive-loop1 1 kmap '())
    (loop (key-map-of (current-buffer)))))

(define-class <event> ()
  ((kind  :init-value #f
          :accessor kind-of
          :init-keyword :kind)))

(define-class <key-event> (<event>)
  ((frame :init-value #f
          :accessor frame-of
          :init-keyword :frame)
   (key   :init-value #f
          :accessor key-of
          :init-keyword :key)))

(define (get-key-event)
  (let lp ((f (frames-of the-editor)))
    (cond ((null? f) #f)
          ((event-pending? (car f))
           (make <key-event> :kind 'key :frame (car f) :key (get-event (car f))))
          (else
           (lp (cdr f))))))

(define (get-timer-event) #f)

(define (wait-for-event)
  (cond ((get-key-event) => values)
        ((get-timer-event) => values)
        (else
         ;;; !!!
         (make <key-event> :kind 'key :frame (car (frames-of the-editor))
               :key (get-event (car (frames-of the-editor)))))))

(define (interactive-loop1 arg kmap stack)

  (define (unbound-error stack)
    (errorf <quit> "~a is not defined" (key-stack->string stack)))

  (let* ((e     (wait-for-event))
         (frame (frame-of e))
         (key   (key-of e))
         (obj   (key-map-get kmap key)))
    (cond ((key-map? obj)
           (interactive-loop1 arg obj (cons key stack)))
          ((symbol? obj)
           (message "~a: not implemented yet" obj))
          ((eq? key 7) 
           (erase-echo-area frame)
           (keyboard-quit frame 1))
          ((not obj)
           (erase-echo-area frame)
           (if (null? stack)
               (insert (integer->char key))
               (unbound-error (cons key stack))))
          ((eq? (procedure-info obj) 'no-such-command)
           (erase-echo-area frame)
           (unbound-error (cons key stack)))
          (else
           (erase-echo-area frame)
           (obj arg)))))

(define (erase-echo-area frame)
  (let* ((buf (get-buffer "*Messages*"))
         (text (text-of buf))
         (end  (text-size text))
         (beg  (text-beginning-of-line text end 1)))
    (if (not (= beg end))
        (insert buf end "\n"))))

(define (update-display)
  (sync-buffers-and-windows)
  (display-frame))

(define (interactive-loop)

  (define (handle-error c)
    (format (current-error-port)
            "*** ERROR: ~a"
            (slot-ref c 'message))
    (report-error c)
    (interactive-loop))

  (define (handle-quit c)
    (display (slot-ref c 'message))
    (interactive-loop))

  (guard (c ((<error> c) (handle-error c))
	    ((<quit>  c) (handle-quit  c)))
	 (interactive-loop0)))

(define sync-current-buffer-and-window
  (let ((prev-buf #f)
        (prev-win #f))
    (lambda ()
      (cond ((mini-buffer-of (current-frame)) #t)
            ((and prev-win prev-buf)
             (if (not (eq? prev-win (current-window)))
                 (select-buffer (buffer-of (current-window))))
             (if (not (eq? prev-buf (current-buffer)))
                 (set! (buffer-of (current-window)) (current-buffer)))
             (set! (point-of (current-window)) (point-of (current-buffer)))
             (set! prev-buf  (current-buffer))
             (set! prev-win  (current-window)))
            (else
             (set! prev-buf  (current-buffer))
             (set! prev-win  (current-window)))))))

(define (sync-buffers-and-windows)
  (sync-current-buffer-and-window))

;;;
;;;  EDITOR
;;;
(define-class <editor> ()
  ((kmap   :init-thunk make-key-map
           :accessor global-key-map-of)
   (mbmap  :init-thunk make-key-map
           :accessor default-mini-buffer-key-map-of)
   (buffs  :init-value '()
           :accessor buffers-of)
   (frames :init-value '()
           :accessor frames-of)
   (kring  :init-value '()
           :accessor kill-ring-of)
   (alarm-list :init-value '()
               :accessor alarm-list-of)
   (cont   :init-value #f
           :accessor continuation-of)
))

(define the-editor       #f)
(define (buffer-list)    (buffers-of the-editor))
(define (current-buffer) (car (buffer-list)))
(define (current-frame)  (car (frames-of the-editor)))
(define (current-window) (current-window-of (current-frame)))

(define (no-such-command . _) #f)
(define (stay-quiet . _) #t)

(define (initialize-key-map editor)
  (initialize-global-key-map editor)
  (initialize-default-mini-buffer-key-map editor))

(define (initialize-default-mini-buffer-key-map editor)
  (let ((kmap (default-mini-buffer-key-map-of editor)))
    (define-key kmap "\\C-@"  set-mark-command)
    (define-key kmap "\\C-a"  beginning-of-line)
    (define-key kmap "\\C-b"  backward-char)
    (define-key kmap "\\C-c"  no-such-command)
    (define-key kmap "\\C-d"  delete)
    (define-key kmap "\\C-e"  end-of-line)
    (define-key kmap "\\C-f"  forward-char)
    (define-key kmap "\\C-g"  keyboard-quit)
    (define-key kmap "\\C-h"  backward-delete)
    (define-key kmap "\\C-/"  backward-delete) ; DEL
    (define-key kmap "\\C-i"  stay-quiet)      ; mini-buffer-completion
    (define-key kmap "\\C-j"  stay-quiet)
    (define-key kmap "\\C-k"  kill-line)
    (define-key kmap "\\C-l"  no-such-command)
    (define-key kmap "\\C-m"  no-such-command)
    (define-key kmap "\\C-n" 'mini-buffer-next-history)
    (define-key kmap "\\C-o"  no-such-command)
    (define-key kmap "\\C-p" 'mini-buffer-previous-history)
    (define-key kmap "\\C-q"  no-such-command)
    (define-key kmap "\\C-r"  no-such-command)
    (define-key kmap "\\C-s"  no-such-command)
    (define-key kmap "\\C-t"  transpose-chars)
    (define-key kmap "\\C-u"  no-such-command)
    (define-key kmap "\\C-v"  no-such-command)
    (define-key kmap "\\C-w"  kill-region)

    (define-key kmap "\\C-x\\C-c" exit-guide)

    (define-key kmap "\\C-y"  yank)
    (define-key kmap "\\C-z"  no-such-command)
    
    ))

(define (initialize-global-key-map editor)
  (let ((kmap (global-key-map-of editor)))
    (define-key kmap ")" insert-and-find-matching-paren)
    (define-key kmap "\\C-@"  set-mark-command)
    (define-key kmap "\\C-a"  beginning-of-line)
    (define-key kmap "\\C-b"  backward-char)
    (define-key kmap "\\C-d"  delete)
    (define-key kmap "\\C-e"  end-of-line)
    (define-key kmap "\\C-f"  forward-char)
    (define-key kmap "\\C-g"  keyboard-quit)
    (define-key kmap "\\C-h"  backward-delete)
    (define-key kmap "\\C-/"  backward-delete) ; DEL
    (define-key kmap "\\C-i"  insert-tab)
    (define-key kmap "\\C-j"  eval-last-sexp)
    (define-key kmap "\\C-k"  kill-line)
    (define-key kmap "\\C-l"  recenter-window)
    (define-key kmap "\\C-m"  new-line)
    (define-key kmap "\\C-n"  next-line)
    (define-key kmap "\\C-o"  open-line)
    (define-key kmap "\\C-p"  previous-line)
    (define-key kmap "\\C-q"  no-such-command)
    (define-key kmap "\\C-r"  no-such-command)
    (define-key kmap "\\C-s"  no-such-command)
    (define-key kmap "\\C-t"  transpose-chars)
    (define-key kmap "\\C-u"  universal-argument)
    (define-key kmap "\\C-v"  scroll-up)
    (define-key kmap "\\C-w"  kill-region)
    (define-key kmap "\\C-y"  yank)
    (define-key kmap "\\C-z"  suspend-guide)
    (define-key kmap "\\M-g"  goto-line-interactive)
    (define-key kmap "\\M-v"  scroll-down)
    (define-key kmap "\\M-w"  copy-region)
    (define-key kmap "\\M-x"  no-such-command)
    (define-key kmap "\\M->"  end-of-buffer)
    (define-key kmap "\\M-<"  beginning-of-buffer)
    (define-key kmap "\\C-\\" no-such-command)
    (define-key kmap "\\C-]"  no-such-command)

    (define-key kmap "\\C-c\\C-c" eval-buffer)
    (define-key kmap "\\C-c\\C-r" eval-region)
    (define-key kmap "\\C-c\\C-m" eval-next-closing-sexp)
    (define-key kmap "\\C-ct" test-mini-buffer)

    (define-key kmap "\\C-xb" switch-to-buffer)
    (define-key kmap "\\C-xd" describe-bindings)
    (define-key kmap "\\C-xk" kill-buffer)
    (define-key kmap "\\C-xo" other-window)
    (define-key kmap "\\C-x0" delete-window)
    (define-key kmap "\\C-x1" delete-other-window)
    (define-key kmap "\\C-x2" split-window)
    (define-key kmap "\\C-xu" undo)
    (define-key kmap "\\C-x\\C-b" list-buffers)
    (define-key kmap "\\C-x\\C-c" exit-guide)
    (define-key kmap "\\C-x\\C-d" diff-buffer)
    (define-key kmap "\\C-x\\C-e" eval-last-sexp)
    (define-key kmap "\\C-x\\C-f" find-file)
    (define-key kmap "\\C-x\\C-s" save-buffer)
    (define-key kmap "\\C-x\\C-x" exchange-point-and-mark)
    ))

(define (put-alarm-list! thunk)
  (push! (alarm-list-of the-editor) thunk))

(define (message . args)
  (apply format (cons (current-error-port) args)))

(define (exit-guide arg)
  (for-each delete-frame (frames-of the-editor))
  (let ((e the-editor))
    (set! the-editor #f)
    (if (continuation-of e)
        ((continuation-of e) 'exit)
        (exit 0))))

(define (suspend-guide arg)
  (for-each delete-frame (frames-of the-editor))
  (call/cc (lambda (k)
             (let ((r (continuation-of the-editor)))
               (set! (continuation-of the-editor) k)
               (r 'suspend)))))

(define (resume-guide k)
  (let ((r (continuation-of the-editor)))
    (set! (continuation-of the-editor) k)
    (r 0)))

(define (make-empty-frame frame-class)
  (let ((frame (make frame-class)))
    (receive (w h) (get-size frame)
      (set! (width-of frame)  w)
      (set! (height-of frame) h))
    frame))
  
(define-method add-frame ((editor <editor>)
                          (frame-class <class>))
  (let* ((frame  (make-empty-frame frame-class))
         (window (make <text-window> :frame frame)))

    (set! (windows-of frame) (list window))
    (set! (current-window-of frame) window)

    (set! (buffer-of window) (car (buffers-of editor)))
    (set! (height-of window) (- (height-of frame) 3))

    (push! (frames-of editor) frame)))

(define (make-editor cont)

  (let ((editor (make <editor>))
        (msgbuf (make <text-buffer> :name "*Messages*")))

    (initialize-key-map editor)
    (set! (key-map-of msgbuf) (copy-key-map (global-key-map-of editor)))
    (buffer-let-window-always-follow-point msgbuf)
    (buffer-make-persistent msgbuf)
    (set! (buffers-of editor) (circular-list msgbuf))
    (set! (continuation-of editor) cont)

    editor))

;;;
;;;  INIT MAIN
;;;
(define (alarm-handler x)
  (for-each (lambda (x) (x)) (alarm-list-of the-editor))
  (set! (alarm-list-of the-editor) '())
  (update-display))

(define (initialize-the-editor cont)
  (set! the-editor (make-editor cont))
  (add-frame the-editor <vt100-frame>)
  (set-signal-handler! SIGWINCH vt100-sigwinch-handler)
  (set-signal-handler! SIGALRM  alarm-handler)
  (sync-buffers-and-windows)
  (let ((schbuf (make-buffer "*scratch*")))
    (select-buffer schbuf)))

;;;
;;; main to be called from command line
;;; 
(define (guide-main args)
  (sys-system "stty min 1 time 0")      ; SunOS 5.8
  (let guide ()
    (let ((status (if the-editor
                      (call/cc (lambda (k) (resume-guide k)))
                      (with-raw-mode (lambda () (apply run (cdr args)))))))
      (case status
        ((exit) 0)
        ((suspend) (sys-kill (sys-getpid) SIGSTOP) (guide))
        (else   1))))
  ;; Do we need to restore min and time above?
  )

;;;
;;; main to be called from gosh's REPL
;;; 
(define (guide . args)

  (define (check args)
    ;; check if all args are valid filename string
    ;; return #t if error found
    ;; return #f if no error found
    (find (lambda (e) (not (string? e))) args)) ; for now

  (define (print-usage)
    (print "(guide \"file\" ... )") 'error)

  (cond ((check args) (print-usage))
        (the-editor
         (call/cc (lambda (k)
                    (for-each find-file args)
                    (resume-guide k))))
        (else
         (sys-system "stty min 1 time 0")      ; SunOS 5.8
         (with-raw-mode  (lambda () (apply run2 args))))))
;;;
;;;
;;;
(if debug?
    (define (with-output-and-error-to-port port thunk) (thunk))
    (define (with-output-and-error-to-port port thunk)
      (with-error-to-port port (lambda () (with-output-to-port port thunk))))
)

;;;
;;; standard emacs like startup
;;;
(define (run . args)
  (call/cc
   (lambda (cont)
     (initialize-the-editor cont)
     (sync-buffers-and-windows)
     (let ((errp (open-output-text-buffer (get-buffer "*Messages*"))))
       (with-output-and-error-to-port errp 
                                      (lambda () 
                                        (for-each find-file args)
                                        (interactive-loop)))))))
;;;
;;; split-window with *Messages* on top
;;;
(define (run2 . args)
  (call/cc
   (lambda (cont)
     (initialize-the-editor cont)
     (sync-buffers-and-windows)
     (let ((errp (open-output-text-buffer (get-buffer "*Messages*"))))
       (with-output-and-error-to-port errp 
                                      (lambda () 
                                        (for-each find-file args)
                                        (select-buffer "*Messages*")
                                        (sync-buffers-and-windows)
                                        (split-window)
                                        (other-window)
                                        (sync-buffers-and-windows)
                                        (switch-to-next-buffer '())
                                        (interactive-loop)))))))
;;;
;;;  VT100 FRAME :-)
;;;
(define-class <vt100-frame> (<frame>)
  ((input-port  :init-thunk standard-input-port
                :accessor input-port-of)
   (output-port :init-thunk standard-output-port
                :accessor output-port-of)))

(define-method delete-frame ((frame <vt100-frame>))
  (with-output-to-port (output-port-of frame)
    (lambda ()
      (vt100-cursor-position 1 (height-of frame))
      (vt100-show-cursor)
      (flush)
      )))

(define (vt100-sigwinch-handler sig)
  (receive (x y) (vt100-get-size)
    (let* ((win    (car (windows-of (current-frame))))
           (height (height-of (current-frame)))
           (newh   (+ (height-of win) (- y height))))
      (cond ((and (= x (width-of  (current-frame)))
                  (= y (height-of (current-frame)))) #t)
            ((> newh 4)
             (set! (width-of  (current-frame)) x)
             (set! (height-of (current-frame)) y)
             (set! (height-of win) newh)
             (display-frame))
            (else
             (error "Terminal height too short!!!"))))))

(define (print-text text from to)
  (for-each display (text-get-text text from (- to from))))

(define-method ring-bell ((frame <vt100-frame>) n)
  (dotimes (x n) (write-byte 7 (output-port-of frame))))

(define-method get-size ((frame <vt100-frame>)) (vt100-get-size))

(define-method get-event ((frame <vt100-frame>))
  
  (define (read-utf8 read-one)
    (let ((x1 (read-byte))
          (x2 (read-one)))
      (cond ((= #b110 (bit-field x1 5 8))
             (+ (* (logand x1 #b00011111) 64)
                (* (logand x2 #b00111111) 1)))
            ((= #b1110 (bit-field x1 4 8))
             (let ((x3 (read-one)))
               (+ (* (logand x1 #b00011111) 64 64)
                  (* (logand x2 #b00111111) 64)
                  (* (logand x3 #b00111111) 1))))
            ((= #b11110 (bit-field x1 3 8))
             (let ((x3 (read-one))
                   (x4 (read-one)))
               (+ (* (logand x1 #b00011111) 64 64 64)
                  (* (logand x2 #b00111111) 64 64)
                  (* (logand x3 #b00111111) 64)
                  (* (logand x4 #b00111111) 1))))
            ((= #b111110 (bit-field x1 2 8))
             (let ((x3 (read-one))
                   (x4 (read-one))
                   (x5 (read-one)))
               (+ (* (logand x1 #b00011111) 64 64 64 64)
                  (* (logand x2 #b00111111) 64 64 64)
                  (* (logand x3 #b00111111) 64 64)
                  (* (logand x4 #b00111111) 64)
                  (* (logand x5 #b00111111) 1))))
            ((= #b111110 (bit-field x1 2 8))
             (let ((x3 (read-one))
                   (x4 (read-one))
                   (x5 (read-one))
                   (x6 (read-one)))
               (+ (* (logand x1 #b00011111) 64 64 64 64 64)
                  (* (logand x2 #b00111111) 64 64 64 64)
                  (* (logand x3 #b00111111) 64 64 64)
                  (* (logand x4 #b00111111) 64 64)
                  (* (logand x5 #b00111111) 64)
                  (* (logand x6 #b00111111) 1))))
            (else
             (error "in get-event")))))

  ;; In old MacOSX each UTF-8 byte is preambled by SYN(0x16)
  (define (read-one-macosx) (read-byte) (read-byte))

  (with-input-from-port (input-port-of frame)
    (lambda ()
      (let ((x (peek-byte)))
        (cond ((= x 22)
               (read-byte)
               (if (or (not (char-ready? (current-input-port)))
                       (= (logand #x80 (peek-byte)) 0))
                   22
                   (read-utf8 read-one-macosx)))
              ((> x 127)
               (read-utf8 read-byte))
              (else
               (read-byte)))))))

(define-method event-pending? ((frame <vt100-frame>))
  (char-ready? (input-port-of frame)))

(define-method display-frame ((frame <vt100-frame>))

  (define (display-the-last-line buf)
    (let* ((text  (text-of buf))
           (end   (+ (text-size text) 1))
           (beg   (text-beginning-of-line text end 1))
           (start (if (< (- end beg) (width-of frame))
                      beg
                      (- end (width-of frame)))))
      (print-text text start end)
      (vt100-clear-eol)))

  (define (display-mini-buffer buf)
    (display (prompt-of buf))
    (display-the-last-line buf))

  (define (display-echo-area)
    (cond ((mini-buffer-of frame) => display-mini-buffer)
          ((get-buffer "*Messages*") => display-the-last-line)))

  (define (display-window win)

    (define (display-mode-line buf cx cy)
      (let* ((text (text-of buf))
             (name (name-of buf))
             (pos  (point-of buf))
             (cln  (text-line-number text pos))
             (segs (length text)))
        (vt100-reverse-video)
        (let ((str (format #f "-~a ~,,,,30a  L~d P~d S~d (~d,~d)" 
                           (cond ((is-modified? buf) #\*)
                                 ((is-readonly? buf) #\%)
                                 (else #\-))
                           name cln pos segs cx cy)))
          (display str)
          (display (make-string (- (width-of frame)
                                   (string-length str)) #\-))
          (newline))
        (vt100-normal-video)))

    (let* ((buf  (buffer-of win))
           (text (text-of buf))
           (pos  (cond ((tmp-point-of win)
                        (tmp-point-of win))
                       ((follow-cursor? buf)
                        (point-of buf))
                       (else
                        (point-of win))))
           (lns  (vt100-beginning-of-lines text (width-of frame)))
           (cln  (vt100-line-number pos lns))
           (tln  (length lns)))

      (if (< (start-of win) 1)     (set! (start-of win) 1))
      (if (<= tln (height-of win)) (set! (start-of win) 1))
      (if (<  cln (start-of win))  (set! (start-of win) cln))
      (if (>= cln (+ (start-of win) (height-of win)))
          (set! (start-of win) (- cln (height-of win) -1)))

      (receive (w h cx cy)
          (let* ((start-pos (if (> (start-of win) 0)
                                (list-ref lns (- (start-of win) 1))
                                0))
                 (end-pos   (if (< (+ (start-of win) (height-of win))
                                   tln)
                                (list-ref lns (+ (start-of win) (height-of win)))
                                -1))
                 (count     (if (> end-pos 0) (- end-pos start-pos) -1)))
            (vt100-display-text (text-get-text (text-of buf) start-pos count)
                                (- pos start-pos)
                                (width-of frame)
                                (height-of win)))
        (cond ((> cy (height-of win))
               (message "rollover cy=~d  (height-of win)=~d~%" cy (height-of win))
               ;;
               ;; In case cursor is out of window, scroll up.
               ;; This happens line at the bottom is wider than the window.
               ;;
               (inc! (start-of win) -4)
               (vt100-cursor-up (height-of win))
               (display-window win))
              (else
               (dotimes (x h)
                 (vt100-clear-eol)
                 (newline))
               (display-mode-line buf cx cy)
               (values cx cy))))))

  (define (out)
    (let ((x 0) (y 0)
          (current-window-found? #f))
      (vt100-hide-cursor)
      (vt100-cursor-home)
      (for-each (lambda (win)
                  (receive (cx cy) (display-window win)
                    (if (eq? win (current-window))
                        (begin
                          (set! current-window-found? #t)
                          (set! x cx)
                          (set! y (+ y cy))))
                    (if (not current-window-found?)
                        (set! y (+ y (height-of win) 1)))))
                (windows-of frame))
      (display-echo-area)
      (if (mini-buffer-of frame)
          (vt100-move-cursor-to-echo frame)
          (vt100-cursor-position x y))
      (vt100-show-cursor)
      (flush)
      ))

  (display (with-output-to-string out) (output-port-of frame))
  ;;(with-output-to-port (output-port-of frame) out)
  )

;;;
;;;
;;;
;;(define (vt100-clear-screen)  (display "\x1b[H\x1b[2J"))
(define (vt100-clear-eol)     (display "\x1b[0K"))
(define (vt100-clear-bol)     (display "\x1b[1K"))
(define (vt100-clear-line)    (display "\x1b[2K"))
(define (vt100-clear-eos)     (display "\x1b[0J"))
(define (vt100-clear-bos)     (display "\x1b[1J"))
(define (vt100-clear-screen)  (display "\x1b[2J"))
(define (vt100-reverse-video) (display "\x1b[7m"))
(define (vt100-normal-video)  (display "\x1b[0m"))

(define (vt100-hide-cursor)   (display "\x1b[?25l"))
(define (vt100-show-cursor)   (display "\x1b[?25h"))
(define (vt100-cursor-home)   (display "\x1b[H"))
(define (vt100-cursor-position x y) (format #t "\x1b[~d;~dH" y x))
(define (vt100-cursor-up    n)      (format #t "\x1b[~dA" n))
(define (vt100-cursor-down  n)      (format #t "\x1b[~dB" n))
(define (vt100-cursor-right n)      (format #t "\x1b[~dC" n))
(define (vt100-cursor-left  n)      (format #t "\x1b[~dD" n))

(define (vt100-cursor-position1 x y)
  (format #t "\x1b[~d;0H" y)
  (if (not (= x 0))
      (format #t "\x1b[~dC" x)))

(define (vt100-get-size)
  ;; "VAR=VAL;"
  (define (get-val str)
    (let ((x (cadr (string-split str #\=))))
      (string->number (substring x 0 (- (string-length x) 1)))))
  (with-input-from-process "resize -u"
    (lambda ()
      (let ((x (get-val (read-line)))
            (y (get-val (read-line))))
        (values x y)))))

(define (vt100-move-cursor-to-echo frame)
  (let ((minibuf (mini-buffer-of frame)))
    (vt100-cursor-position (+ (string-length (prompt-of minibuf))
                              (point-of minibuf) 1)
                           (- (height-of frame) 1))))

(define (vt100-char-width ch)
  ;; XXX this is not correct!
  (cond ((number? ch) 0)
        ((< (char->ucs ch) 32)  0)
        ((< (char->ucs ch) 256) 1)
        (else 2)))

(define (vt100-string-width str idx)
  (if (<= idx 0)
      0
      (let lp ((width 0)
               (i     0))
        (if (< i idx)
            (lp (+ width (vt100-char-width (string-ref str i 0)))
                (+ i 1))
            width))))

(define (vt100-string-index-by-width str width)
  (let ((len (string-length str)))
    (let loop ((i 0)
               (width width))
      (cond ((> i len) len)
            ((= width 0) i)
            ((< width 0) (if (> i 0) (- i 1) 0))
            (else
             (loop (+ i 1)
                   (- width 
                      (vt100-char-width (string-ref str i 0)))))))))

;;;
;;; Returns list of points.
;;; Each points is at the beginning of a line.
;;;
(define (vt100-beginning-of-lines text width)
  (define getc (text-gen-getc text))
  (if (<= width 2) (error "too narrow"))
  (let lp ((c   (getc))
           (pos 0)
           (w   0)
           (r   (list 0)))
    (let ((next (+ pos 1)))
      (cond ((eof-object? c) (reverse r))
            ((char=? c #\newline)
             (lp (getc) next 0 (cons next r)))
            ((>= (+ w (vt100-char-width c)) width)
             (lp c pos 0 (cons pos r)))
            (else
             (lp (getc) next (+ w (vt100-char-width c)) r))))))

;;;
;;; find line number of pos from return value of vt100-beginning-of-lines.
;;;
(define (vt100-line-number pos lns)
  (if (< pos 0)
      0
      (let lp ((ln   0)
               (lns  lns))
        (cond ((null? lns) ln)
              ((< pos (car lns)) ln)
              (else
               (lp (+ ln 1) (cdr lns)))))))

;;;
;;; Display text inside box of width and height.
;;; Returns cursor posision x y and number of empty lines h.
;;;
(define (vt100-display-text text pos width height)
  (let loop ((text text)
             (pos  pos)
             (wc   0)
             (w    width)
             (h    height)
             (x    1)
             (y    1))
    (cond ((null? text) (values w h x y))
          ((= h 0)      (values w 0 x y))
          ((< h 0)      (error "h < 0!!"))
          (else
           (receive (pos wc w h x y) 
               (vt100-display-string (car text) pos wc w h x y width)
             (loop (cdr text) pos wc w h x y))))))

(define (vt100-display-string str pos wc w h x y full-width)
  (cond ((= h 0) (values pos wc w 0 x y))
        ((< h 0) (error "h < 0!!"))
        ((string-scan str #\newline)
         => (lambda (idx) 
              (receive (pos wc w h x y)
                  (vt100-display-line (substring str 0 idx) pos wc w h x y full-width)
                (vt100-clear-eol)
                (cond ((= h 0) (values pos wc w 0 x y))
                      ((< h 0) (error "h < 0!"))
                      (else
                       (newline)
                       (vt100-display-string (substring str (+ idx 1)
                                                        (string-length str)) ; str
                                             (- pos 1)          ; pos
                                             0                  ; wc
                                             full-width         ; w
                                             (- h 1)            ; h
                                             (if (> pos 0) 1       x) ; x
                                             (if (> pos 0) (+ y 1) y) ; y
                                             full-width))))))
        (else
         (vt100-display-line str pos wc w h x y full-width))))

(define (vt100-display-line str pos wc w h x y full-width)
  (cond ((= h 0) (values pos wc w h x y))
        ((< h 0) (error "h < 0!!"))
        ((string=? str "") (values pos wc w h x y))
        ((= w 0)
         (display "\\\n")
         (vt100-display-line str pos 0 full-width (- h 1) x y full-width))
        (else
         (let* ((len   (string-length str))
                (dispw (vt100-string-width str len)))
           (cond ((< dispw w)
                  (display str)
                  (values (- pos len)
                          (+ wc dispw)
                          (- w  dispw)
                          h
                          (if (and (> pos 0) (<= pos len))
                              (+ wc (vt100-string-width str pos) +1)
                              x)
                          y))
                 (else
                  (let ((idx (vt100-string-index-by-width str (- w 1))))
                    (display (substring str 0 idx))
                    (display "\\\n")
                    (vt100-display-line (substring str idx len)
                                        (- pos idx)
                                        0
                                        full-width
                                        (- h 1)
                                        (if (and (> pos 0) (<= pos idx))
                                            (+ wc (vt100-string-width str pos) +1)
                                            x)
                                        (if (> pos idx) (+ y 1) y)
                                        full-width))))))))

(provide "guide")
;;; EOF
