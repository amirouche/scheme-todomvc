;; scheme helpers

(define (pk . args)
  (apply console-log (cons ";;;" args))
  (car (reverse args)))

(define (acons a b alist)
  (cons (cons a b) alist))

(define (alist-delete alist key)
  (let loop ((alist alist)
	     (out '()))
    (if (null? alist)
	out
	(if (equal? (caar alist) key)
	    (loop (cdr alist) out)
	    (loop (cdr alist) (cons (car alist) out))))))

(define (alist-set alist key value)
  (acons key value (alist-delete alist key)))

(define (alist-ref alist key)
  (let loop ((alist alist))
    (cond
     ((null? alist) #f)
     ((equal? (caar alist) key) (cdar alist))
     (else (loop (cdr alist))))))

(define (string-prefix? prefix string)
  (let ((l (string-length prefix)))
    (if (< (string-length string) l)
	#f
	(let ((other (substring string 0 l)))
	  (equal? other prefix)))))

;; snabbdom bindings

(define %patch (js-eval "patch"))

(define (patch old new)
  (js-call %patch old new)
  new)

(define %h (js-eval "h"))

(define (events->js-obj events)
  (alist->js-obj (map (lambda (name+proc)
			(cons (car name+proc) (js-closure (cdr name+proc))))
		      events)))

(define (h tag events children)
  (js-call %h tag events (list->js-array children)))

(define (attrs->js-obj attrs)
  (let loop ((attrs attrs)
	     (on '())
	     (out '()))
    (if (null? attrs)
	(alist->js-obj `(("on" . ,(alist->js-obj on)) ("attrs" . ,(alist->js-obj out))))
	(let ((name (symbol->string (caar attrs)))
	      (value (cdar attrs)))
	  (if (string-prefix? "on-" name)
	      (loop (cdr attrs)
		    (cons (cons (substring name 3 (string-length name))
				(js-closure value))
			  on)
		    out)
	      (loop (cdr attrs)
		    on
		    (cons (cons name value) out)))))))

(define (sxml->h element)
  (if (not (list? element))
      element
      (let ((tag (symbol->string (car element))))
	(let ((attrs (cadr element)))
	  (if (and (list? attrs) (eq? (car attrs) '@))
	      (h (string-append tag "#") (attrs->js-obj (cdr attrs)) (map sxml->h (cddr element)))
	      (h (string-append tag "#") (alist->js-obj '()) (map sxml->h (cdr element))))))))

;; navigo bindings

(define (make-navigo root)
  (if (null? root)
      (js-new "Navigo")
      (js-new "Navigo" root)))

(define (navigo-on navigo pattern proc)
  (js-invoke navigo "on" pattern (js-closure (lambda (param) (proc (js-obj->alist param))))))

(define (navigo-default navigo proc)
  (js-invoke navigo "on" (js-closure proc)))

(define (navigo-not-found navigo proc)
  (js-invoke navigo "notFound" (js-closure proc)))

(define (navigo-navigate navigo path)
  (js-invoke navigo "navigate" path))

(define (navigo-resolve navigo)
  (js-invoke navigo "resolve"))

;; FIXME: use biwascheme bindings

(define (event-target event)
  (js-ref event "target"))

(define (event-target-value event)
  (js-ref (event-target event) "value"))

(define (event-target-checked event)
  (js-ref (event-target event) "checked"))

(define (event-key event)
  (js-ref event "key"))

(define (event-prevent-default event)
  (js-invoke event "preventDefault"))

;; app

;; FIXME: workaround the fact that ($ "#app") doesn't work
(define body (car (js-array->list ($ "body"))))
(define main (element-new '(div "getting started")))
(element-append-child! body main)

;; framework-ish stuff

(define (render)
  (set! main (patch main (sxml->h (view *state*)))))

(define (make-action proc)
  (lambda args
    (set! *state* (apply (proc *state*) args))
    (render)))

(define *state* `((page . home)
		  (todos . ((0 . ("Learn Scheme" . #f))))))

;; login

(define (login:username-changed state)
  (lambda (event)
    (let ((username (event-target-value event)))
      (alist-set state 'username username))))

(define (login:enter-clicked state)
  (lambda ()
    (let ((username (alist-ref state 'username)))
      (if username
	  `((page . home) (username . ,username))
	  (alist-set state 'error "a username must be provided")))))

(define (todo:checked todo-uid)
  (lambda (state)
    (lambda (event)
      (let ((checked (event-target-checked event)))
	(let* ((todos (alist-ref state 'todos))
	       (todo (alist-ref todos todo-uid)))
	  (alist-set state 'todos
		     (alist-set todos todo-uid (cons (car todo) checked))))))))

(define (todo:destroy-clicked todo-uid)
  (lambda (state)
    (lambda (event)
      (let ((todos (alist-ref state 'todos)))
	(alist-set state 'todos (alist-delete todos todo-uid))))))

(define (todo:view todo)
  `(li (@ (class . ,(if (cddr todo) "completed" "")))
       (div (@ (class . "view"))
	    (input (@ (class . "toggle")
		      (type . "checkbox")
		      (checked . ,(if (cddr todo) "t" ""))
		      (on-click . ,(make-action (todo:checked (car todo))))))
	    (label ,(cadr todo))
	    (button (@ (class . "destroy")
		       (on-click . ,(make-action (todo:destroy-clicked (car todo)))))))))

(define (todos:active state)
  (filter (lambda (pair) (not (cddr pair))) (alist-ref state 'todos)))

(define (todos:completed state)
  (filter cddr (alist-ref state 'todos)))

(define (todos:clear-completed-clicked state)
  (lambda (event)
    (event-prevent-default event)
    (alist-set state 'todos (todos:active state))))

(define (new-todo:keypressed state)
  (lambda (event)
    (let ((key (event-key event)))
      (if (equal? key "Enter")
	  (let ((new (event-target-value event))
		(todos (list-sort (alist-ref state 'todos))))
	    (if (null? todos)
		(begin
		  (element-empty! (event-target event))  ;; Y U NO CLEAR MY INPUT?!
		  (alist-set state 'todos (acons 0 (cons new #f) '())))
		(let ((uid (+ 1 (caar (reverse todos)))))
		  (element-empty! (event-target event))  ;; Y U NO CLEAR MY INPUT?!
		  (alist-set state 'todos (acons uid (cons new #f) todos)))))
	  state))))

(define (filters:all state)
  (lambda (event)
    (event-prevent-default event)
    (alist-set state 'page 'home)))

(define (filters:active state)
  (lambda (event)
    (event-prevent-default event)
    (alist-set state 'page 'active)))

(define (filters:completed state)
  (lambda (event)
    (event-prevent-default event)
    (alist-set state 'page 'completed)))

;; home

(define (home:view state)
  `(section (@ (class . "todoapp"))
	    (header (@ (class . "header"))
		    (h1 "todos")
		    (input (@ (class . "new-todo")
			      (placeholder . "What needs to be done?")
			      (autofocus . "t")
			      (on-keypress . ,(make-action new-todo:keypressed)))))
	    (section (@ (class . "main"))
		     (ul (@ (class . "todo-list"))
			 ,@(map todo:view (list-sort (alist-ref state 'todos)))))
	    (footer (@ (class . "footer"))
		    (span (@ (class . "todo-count"))
			  ,(length (todos:active state))
			  " items left")
		    (ul (@ (class . "filters"))
			(li (a (@ (href . "#")
				  (class . "selected")
				  (on-click . ,(make-action filters:all)))
			       "All"))
			(li (a (@ (href . "#")
				  (on-click . ,(make-action filters:active)))
			       "Active"))
			(li (a (@ (href . "#")
				  (on-click . ,(make-action filters:completed)))
			       "Completed")))
		    (button (@ (class . "clear-completed")
			       (on-click . ,(make-action todos:clear-completed-clicked)))
			    "Clear completed"))))

(define (active:view state)
  `(section (@ (class . "todoapp"))
	    (header (@ (class . "header"))
		    (h1 "todos")
		    (input (@ (class . "new-todo")
			      (placeholder . "What needs to be done?")
			      (autofocus . "t")
			      (on-keypress . ,(make-action new-todo:keypressed)))))
	    (section (@ (class . "main"))
		     (ul (@ (class . "todo-list"))
			 ,@(map todo:view (list-sort (todos:active state)))))
	    (footer (@ (class . "footer"))
		    (span (@ (class . "todo-count"))
			  ,(length (todos:active state))
			  " items left")
		    (ul (@ (class . "filters"))
			(li (a (@ (href . "#")
				  (on-click . ,(make-action filters:all)))
			       "All"))
			(li (a (@ (href . "#")
				  (class . "selected")
				  (on-click . ,(make-action filters:active)))
			       "Active"))
			(li (a (@ (href . "#")
				  (on-click . ,(make-action filters:completed)))
			       "Completed")))
		    (button (@ (class . "clear-completed")
			       (on-click . ,(make-action todos:clear-completed-clicked)))
			    "Clear completed"))))


(define (completed:view state)
  `(section (@ (class . "todoapp"))
	    (header (@ (class . "header"))
		    (h1 "todos")
		    (input (@ (class . "new-todo")
			      (placeholder . "What needs to be done?")
			      (autofocus . "t")
			      (on-keypress . ,(make-action new-todo:keypressed)))))
	    (section (@ (class . "main"))
		     (ul (@ (class . "todo-list"))
			 ,@(map todo:view (list-sort (todos:completed state)))))
	    (footer (@ (class . "footer"))
		    (span (@ (class . "todo-count"))
			  ,(length (todos:active state))
			  " items left")
		    (ul (@ (class . "filters"))
			(li (a (@ (href . "#")
				  (on-click . ,(make-action filters:all)))
			       "All"))
			(li (a (@ (href . "#")
				  (on-click . ,(make-action filters:active)))
			       "Active"))
			(li (a (@ (href . "#")
				  (class . "selected")
				  (on-click . ,(make-action filters:completed)))
			       "Completed")))
		    (button (@ (class . "clear-completed")
			       (on-click . ,(make-action todos:clear-completed-clicked)))
			    "Clear completed"))))

(define (view state)
  (pk 'state state)
  (case (alist-ref state 'page)
    ((home) (home:view state))
    ((active) (active:view state))
    ((completed) (completed:view state))    
    (else `(h1 "unknown page state"))))


(render)
