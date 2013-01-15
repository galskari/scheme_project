;;; compiler.scm
;;;
;;; Programmer: ???


(define with (lambda (s f) (apply f s)))

(define member?
  (lambda (a s)
    (ormap
     (lambda (b) (eq? a b))
     s)))

(define file->list
  (lambda (filename)
    (let ((port (open-input-file filename)))
      (letrec ((loop
		(lambda ()
		  (let ((ch (read-char port)))
		    (if (eof-object? ch) '()
			(cons ch (loop)))))))
	(let ((s (loop)))
	  (close-input-port port)
	  s)))))

(define make-char-between?
  (lambda (char<=?)
    (lambda (char-from char-to)
      (lambda (char)
	(and (char<=? char-from char)
	     (char<=? char char-to))))))

;;; The scanner recognizes parenthesis and single quote.
;;; It knows to ignore comments up to the end of the current input line,
;;; as well as whitespaces.

(define list->tokens
  (letrec ((st-init
	    (lambda (s)
	      (cond
	       ((null? s) '())
	       ((char=? (car s) #\;) (st-comment s))
	       ((char=? (car s) #\.) `((dot) ,@(st-init (cdr s))))
	       ((char=? (car s) #\') `((single-quote) ,@(st-init (cdr s))))
	       ((char=? (car s) #\`) `((quasiquote) ,@(st-init (cdr s))))
	       ((char=? (car s) #\,) (st-unquote (cdr s)))
	       ((char=? (car s) #\() `((lparen) ,@(st-init (cdr s))))
	       ((char=? (car s) #\)) `((rparen) ,@(st-init (cdr s))))
	       ((char=? (car s) #\#) (st-hash (cdr s)))
	       ((char=? (car s) #\") (st-string (cdr s) '()))
	       ((char-whitespace? (car s)) (st-init (cdr s)))
	       ((char-symbol? (car s))
		(st-symbol/number (cdr s) (list (car s))))
	       (else (scanner-error "What's this" s)))))
	   (st-unquote
	    (lambda (s)
	      (cond ((null? s) `((,'unquote) ,@(st-init '())))
		    ((char=? (car s) #\@)
		     `((,'unquote-splicing) ,@(st-init (cdr s))))
		    (else `((,'unquote) ,@(st-init s))))))
	   (st-symbol/number
	    (lambda (s chars)
	      (cond ((null? s)
		     `(,(make-symbol/number-token chars) ,@(st-init '())))
		    ((char-symbol? (car s))
		     (st-symbol/number (cdr s) (cons (car s) chars)))
		    ((char-delimiter? (car s))
		     `(,(make-symbol/number-token chars) ,@(st-init s)))
		    (else (scanner-error "At the end of a symbol: " s)))))
	   (st-string
	    (lambda (s chars)
	      (cond ((null? s)
		     (scanner-error "Expecting a \" char to close the string"))
		    ((char=? (car s) #\")
		     `((string ,(list->string (reverse chars)))
		       ,@(st-init (cdr s))))
		    ((char=? (car s) #\\) (st-meta-char (cdr s) chars))
		    (else (st-string (cdr s) (cons (car s) chars))))))
	   (st-meta-char
	    (lambda (s chars)
	      (cond ((null? s)
		     (scanner-error
		      "Expecting a string meta-char; reached EOF"))
		    ((char=? (car s) #\\) (st-string (cdr s) (cons #\\ chars)))
		    ((char=? (car s) #\") (st-string (cdr s) (cons #\" chars)))
		    ((char-ci=? (car s) #\n)
		     (st-string (cdr s) (cons #\newline chars)))
		    ((char-ci=? (car s) #\r)
		     (st-string (cdr s) (cons #\return chars)))
		    ((char-ci=? (car s) #\t)
		     (st-string (cdr s) (cons #\tab chars)))
		    ((char-ci=? (car s) #\f)
		     (st-string (cdr s) (cons #\page chars)))
		    (else (scanner-error "What kind of a meta-char is " s)))))
	   (st-hash
	    (lambda (s)
	      (cond ((null? s)
		     (scanner-error
		      "Expecting something after #, but reached end"))
		    ((char=? (car s) #\() `((vector) ,@(st-init (cdr s))))
		    ((char=? (car s) #\\) (st-char-1 (cdr s)))
		    ((char-ci=? (car s) #\f)
		     `((boolean #f) ,@(st-init (cdr s))))
		    ((char-ci=? (car s) #\t)
		     `((boolean #t) ,@(st-init (cdr s))))
		    ((char=? (car s) #\;) `((comment) ,@(st-init (cdr s))))
		    (else (scanner-error
			   "Expecting t, f, \\, ( after #, but found" s)))))
	   (st-char-1
	    (lambda (s)
	      (cond ((null? s) (error 'scanner "Must be one char after #\\"))
		    (else (st-char (cdr s) (list (car s)))))))
	   (st-char
	    (lambda (s chars)
	      (cond ((null? s) `((char ,(make-char chars)) ,@(st-init '())))
		    ((char-delimiter? (car s))
		     `((char ,(make-char chars)) ,@(st-init s)))
		    (else (st-char (cdr s) (cons (car s) chars))))))
	   (st-comment
	    (lambda (s)
	      (cond ((null? s) (st-init '()))
		    ((char=? (car s) #\newline) (st-init (cdr s)))
		    (else (st-comment (cdr s)))))))
    (lambda (s)
      (st-init s))))

(define make-symbol/number-token
  (lambda (chars)
    (let* ((string (list->string (reverse chars)))
	   (maybe-number (string->number string)))
      (if (number? maybe-number)
	  `(number ,maybe-number)
	  `(symbol ,(string->symbol (string-downcase string)))))))

(define make-char
  (lambda (chars)
    (cond ((null? chars) (scanner-error "Found #\\ without any char"))
	  ((null? (cdr chars)) (car chars))
	  (else (let* ((string (list->string (reverse chars)))
		       (maybe-number (string->number string 8)))
		  (if (number? maybe-number)
		      (integer->char maybe-number)
		      (cond ((string-ci=? string "return") #\return)
			    ((string-ci=? string "newline") #\newline)
			    ((string-ci=? string "space") #\space)
			    ((string-ci=? string "tab") #\tab)
			    ((string-ci=? string "page") #\page)
			    (else (scanner-error
				   "Can't recognize the following character: "
				   (format "#\\~s" string))))))))))

(define char-alphabetic? ((make-char-between? char-ci<=?) #\a #\z))
(define char-decimal? ((make-char-between? char<=?) #\0 #\9))

(define char-symbol?
  (let ((punc-chars (string->list "!@$%^*-_=+<>./?:")))
    (lambda (char)
      (or (char-alphabetic? char)
	  (char-decimal? char)
	  (ormap 
	   (lambda (punc-char) (char=? punc-char char))
	   punc-chars)))))

(define char-whitespace?
  (lambda (char)
    (char<=? char #\space)))

(define char-delimiter?
  (lambda (char)
    (or (char-whitespace? char)
	(not (char-symbol? char)))))

(define scanner-error
  (lambda (message s)
    (if (null? s)
	(error 'list-tokens message)
	(error 'list-tokens
	       (format "~a: [~s]~a"
		 message
		 (car s)
		 (list->string (cdr s)))))))

(define file->tokens
  (lambda (filename)
    (list->tokens
     (file->list filename))))

(define string->tokens
  (lambda (string)
    (list->tokens
     (string->list string))))




(define getSexpressionS
  (lambda (toks ret-Sexp)
    (getSexpression toks
		    (lambda (Sexpr tokes)
		      (getSexpressionS tokes
				       (lambda (Sexprs tok)
					 (ret-Sexp (cons Sexpr Sexprs) tok))))
		    (lambda () (ret-Sexp '() toks)))))

(define getSexpression
  (lambda (token ret-Sexpr ret-f)
    (cond ((null? token) (ret-f))
	  ((or (eq? (caar token) 'boolean)
	       (eq? (caar token) 'char)
	       (eq? (caar token) 'number)
	       (eq? (caar token) 'string)
	       (eq? (caar token) 'symbol))
	   (ret-Sexpr (cadar token) (cdr token)))
	  ((and (pair? token) (eq? (caar token) 'comment)) (getSexpression (cdr token)
									   (lambda (Sexp toks)
									     (getSexpression toks ret-Sexpr ret-f))
									   (lambda () (error 'reader "wrong comment syntax"))))
	  ((and (pair? token )(eq? (caar token) 'unquote)) (getSexpression (cdr token)
									   (lambda (Sexp tokens)
									     (ret-Sexpr (list 'unquote Sexp) tokens))
									   ret-f))
	  
	  ((and (pair? token) (eq? (caar token) 'single-quote)) (getSexpression (cdr token)
										(lambda (Sexp tokens)
										  (ret-Sexpr (list 'quote Sexp) tokens))
										ret-f))
	  ((and (pair? token) (eq? (caar token) 'quasiquote)) (getSexpression (cdr token)
									      (lambda (Sexp tokens)
										(ret-Sexpr (list 'quasiquote Sexp) tokens))
									      ret-f))

	  ((and (pair? token) (eq? (caar token) 'unquote-splicing)) (getSexpression (cdr token)
										    (lambda (Sexp tokens)
										      (ret-Sexpr (list 'unquote-splicing Sexp) tokens))
										    ret-f))
	  ((and (pair? token) (eq? (caar token) 'vector)) (getSexpressionS (cdr token)
									   (lambda (sexps tokens)
									     (cond ((eq? (caar tokens) 'rparen) (ret-Sexpr (list->vector sexps) (cdr tokens)))
										   (else (error 'reader "Vector error."))))))
	  
	  ((and (pair? token) (eq? (caar token) 'lparen)) (getSexpressionS (cdr token)
									   (lambda (sexprs toks) 
									     (cond ((null? toks) (ret-f))
										   ((eq? (caar toks) 'rparen) (ret-Sexpr sexprs (cdr toks)))
										   ((eq? (caar toks) 'dot) (getSexpression (cdr toks) (lambda (exp tokens)
																	(cond ((null? toks) (error 'reader "No Closing Paren."))
																	      ((eq? (caar tokens) 'rparen) (ret-Sexpr (append sexprs exp) (cdr tokens)))
																	      (else (error 'reader "dot error."))
																	      )
																	) ret-f))
										   (else (error 'reader "rParen error."))
										   )
									     )))
	  
	  ((and (pair? token) (eq? (caar token) 'dot)) (ret-f))	
	  ((and (pair? token) (eq? (caar token) 'rparen)) (ret-f))
	  (else (error 'reader "General syntax error."))
	  )))




(define tokens->sexprs
  (lambda (tokens)
    (getSexpressionS tokens (lambda (exp toks)
			      (if (null? toks) exp (error 'reader "Syntax Error"))))))

(define build-lambda
  (lambda (e ret-pro ret-imp ret-sym)
    (cond ((pair? e) (build-lambda (cdr e)
				   ret-pro
				   (lambda (s a) (ret-imp (cons (car e) s) a))
				   (lambda() (ret-imp (list (car e)) (cdr e)))))
	  ((null? e) (ret-pro))
	  (else (ret-sym)))))

(define no-doubles
  (lambda (vars)
    (cond ((null? vars) #t)
	  ((pair? vars) (and (not (member? (car vars) (cdr vars))) (no-doubles (cdr vars)))))))

(define parse
  (lambda (sexpr)
    (cond 
     ((and (list? sexpr) (eq? (car sexpr) 'lambda))
      (let* ((arg1 (cadr sexpr))
	     (body (parse (beginify (cddr sexpr))))
	     (e 	(build-lambda arg1
				      (lambda () `(lambda-simple, arg1, body))
				      (lambda (s a) `(lambda-opt, s, a, body))
				      (lambda () `(lambda-variadic, arg1, body)))))
	(if (no-doubles (cadr e)) e (error 'tag-parser "double var name"))
	))
     ((if2? sexpr)
      (with sexpr
	(lambda (_ test dit)
	  `(if-3 ,(parse test) ,(parse dit) (const ,*void-object*)))))
     ((if3? sexpr)
      (with sexpr
	(lambda (_ test dit dif)
	  `(if-3 ,(parse test) ,(parse dit) ,(parse dif)))))
     ((and (pair? sexpr) (eq? (car sexpr) 'define))
      (cond ((pair? (cadr sexpr)) `(define ,(parse (get-name-for-mit sexpr)),(parse `(lambda ,(get-vars-for-mit sexpr),(get-body-for-mit sexpr)))))
	    (else (if (pair? (cdr sexpr))
		      `(define, (parse (cadr sexpr)),(parse (caddr sexpr))) (error 'tag-parser "wrong define")))))
     ((and (pair? sexpr)(eq? (car sexpr) 'begin))
      `(seq , (map parse (cdr sexpr))))
     ((and (pair? sexpr) (eq? (car sexpr) 'quote)) `(const, (cadr sexpr)))
     ((and (pair? sexpr) (eq? (car sexpr) 'quasiquote)) (parse (expand-qq (cadr sexpr))))
     ((and (pair? sexpr) (eq? (car sexpr) 'or))
      (cond ((null? (cdr sexpr)) #f)
	    ((null? (cddr sexpr)) (parse (cadr sexpr)))
	    (else `(or , (map parse (cdr sexpr))))))
     ((and (pair? sexpr) (eq? (car sexpr) 'and)) (parse (and->if (cdr sexpr))))
     ((and (pair? sexpr) (eq? (car sexpr) 'let)) (parse (let-expand sexpr)))
     ((and (pair? sexpr) (eq? (car sexpr) 'cond)) (parse (cond-expand (cdr sexpr))))
     ((and (pair? sexpr) (eq? (car sexpr) 'let*)) (parse (let*-expand sexpr)))
     ((and (pair? sexpr) (eq? (car sexpr) 'letrec)) (parse (expand-letrec sexpr)))
     ((list? sexpr) `(applic ,(parse (car sexpr)) , (map parse (cdr sexpr))))     

     ((or (number? sexpr)
	  (boolean? sexpr)
	  (char? sexpr)
	  (string? sexpr)
	  (vector? sexpr)) `(const ,sexpr))
     ((symbol? sexpr)
      (if (reserved-word? sexpr) (error 'tag-parser "reserved word") `(var ,sexpr)))
     (else (error 'general-parsing "Syntax Error"))
     )))

(define reserved-word?
  (lambda (e)
    (ormap
     (lambda (kw) (eq? e kw))
     *reserved-words*)))

(define *reserved-words*
  '(and begin cond define do else if lambda
	let let* letrec or quasiquote
	quote set! unquote unquote-splicing))
(define *void-object* (if #f #f))

(define void? (lambda (e) (eq? e *void-object*)))

(define with (lambda (s f) (apply f s)))	   

(define if2?
  (lambda (sexpr)
    (and (pair? sexpr)
	 (eq? (car sexpr) 'if)
	 (pair? (cdr sexpr)) ; there is a test
	 (pair? (cddr sexpr)) ; there is a dit
	 (null? (cdddr sexpr)))))

(define if3?
  (lambda (sexpr)
    (and (pair? sexpr)
	 (eq? (car sexpr) 'if)
	 (pair? (cdr sexpr)) ; there is a test
	 (pair? (cddr sexpr)) ; there is a dit
	 (pair? (cdddr sexpr)) ; there is a dif
	 (null? (cddddr sexpr)))))
(define get-name-for-mit
  (lambda (sexpr)
    (caadr sexpr)))

(define get-vars-for-mit
  (lambda (sexpr)
    (cdadr sexpr)))

(define get-body-for-mit
  (lambda (sexpr)
    (caddr sexpr)))

(define beginify
  (lambda (es)
    (cond ((null? es) *void-object*)
	  ((null? (cdr es)) (car es))
	  (else `(begin ,@es)))))

(define and->if
  (lambda (x)
    (if (null? x) #t
    (if (null? (cdr x))
	(car x)
	`(if ,(car x),(and->if (cdr x)),#f)))))

(define let-expand (lambda (let-exp)
		     (if (eq? 'let (car let-exp))
			 (let* ((Decls (cadr let-exp))
				(Body (cddr let-exp)) (Vars (map car Decls)) (Vals (map cadr Decls)))
			   `((lambda ,Vars ,@Body ) ,@Vals))
			 (error 'let-expansion "not-a-let-exp"))))

(define cond-expand
  (lambda (sexpr)
    (cond
     ((null? sexpr) (error 'cond-expand "syntax error"))
     ((eq? (caar sexpr) 'else) (beginify (cdar sexpr)))
     ((null? (cdr sexpr)) `(if , (caar sexpr),(beginify (cdar sexpr))))
     (else `(if, (caar sexpr) , (beginify (cdar sexpr)) , (cond-expand (cdr sexpr)))))))




;;
;;
;;let* macro expansion
;;
;;
(define let*-expand
  (lambda (sexpr)
    (cond
     ((or (null? sexpr) (not (pair? sexpr)) (null? (car sexpr)) (null? (cadr sexpr))) (error 'let*-expand "syntax error"))
     (else
      (let*
	  ((Decls (cadr sexpr))
	   (Body (cddr sexpr))
	   (Vars (map car Decls))
	   (Vals (map cadr Decls)))
	(let*-helper Vars Vals Body))))))

(define let*-helper
  (lambda (Vars Vals Body)
    (cond
     ((null? (cdr Vars)) `((lambda,(list (car Vars)),(car Body)), (car Vals)))
     (else `((lambda ,(list (car Vars)),(let*-helper (cdr Vars) (cdr Vals) Body)) , (car Vals))))))

(define Yn
  (lambda fs
    (let ((ms (map
		  (lambda (fi)
		    (lambda ms
		      (apply fi
			     (map (lambda (mi)
				    (lambda args
				      (apply (apply mi ms) args)))
			       ms))))
		fs)))
      (apply (car ms) ms))))

(define expand-letrec
  (lambda (e)
    (with e
      (lambda (_letrec ribs . exprs)
	(let* ((names `(,(gensym) ,@(map car ribs)))
	       (fs `((lambda ,names ,@exprs)
		     ,@(map (lambda (rib) `(lambda ,names ,(cadr rib)))
			 ribs))))
	  `(Yn ,@fs))))))

(define expand-qq
  (lambda (e)
    (cond ((unquote? e) (cadr e))
	  ((unquote-splicing? e) (error 'expand-qq "unquote-splicing here makes no sense!"))
	  ((pair? e)
	   (let ((a (car e))
		 (b (cdr e)))
	     (cond ((unquote-splicing? a) `(append ,(cadr a) ,(expand-qq b)))
		   ((unquote-splicing? b) `(cons ,(expand-qq a) ,(cadr b)))
		   (else `(cons ,(expand-qq a) ,(expand-qq b))))))
	  ((vector? e) `(list->vector ,(expand-qq (vector->list e))))
	  ((or (null? e) (symbol? e)) `',e)
	  (else e))))

(define ^quote?
  (lambda (tag)
    (lambda (e)
      (and (pair? e)
	   (eq? (car e) tag)
	   (pair? (cdr e))
	   (null? (cddr e))))))

(define unquote? (^quote? 'unquote))
(define unquote-splicing? (^quote? 'unquote-splicing))
(define quasiquote? (^quote? 'quasiquote))



(define search-in-rib
  (lambda (a s ret-min ret-nf)
    (cond ((null? s) (ret-nf))
	  ((eq? (car s) a) (ret-min 0))
	  (else (search-in-rib a (cdr s)
			       (lambda (min) (ret-min (+ 1 min)))
			       ret-nf)))))
(define search-in-ribs
  (lambda (a env ret-maj+min ret-nf)
    (if (null? env)
	(ret-nf)
	(search-in-rib a (car env)
		       (lambda (min) (ret-maj+min 0 min))
		       (lambda () (search-in-ribs a (cdr env)
						  (lambda (maj min)
						    (ret-maj+min (+ 1 maj) min))


						  ret-nf))))))

;;lambda variadic missing
;;what is @

(define pe->lex-pe
  (letrec ((run (lambda (pe params env)
		  (cond ((eq? (car pe) 'var)
			 (with pe (lambda (_ v)
				    (search-in-rib v params
				     (lambda (min) `(pvar ,v ,min))
				     (lambda ()
				       (search-in-ribs v env
						       (lambda (maj min)
							 `(bvar ,v ,maj ,min))
						       (lambda () `(fvar ,v))))))))
			((eq? (car pe) 'const)
			 pe)
			((eq? (car pe) 'lambda-simple)
			 (with pe
			   (lambda (_ arg1 body)
			     `(lambda-simple ,arg1 ,(run body arg1 (cons params env))))))
			((eq? (car pe) 'lambda-opt)
			 (with pe
			   (lambda (_ arg1 opt body)
			     (let ((arg2 `(,@arg1 ,opt)))
			       `(lambda-opt ,arg1 ,opt ,(run body arg2 (cons params env)))))))
			((pe-lambda-variadic? pe)
			 (with pe
			   (lambda (_ args body)
			`(lambda-variadic, args, (run body (list args)  (cons params env))))))
			((eq? (car pe) 'if-3)
			 (with pe
			   (lambda (_ test dit dif)
				    `(if-3 ,(run test params env)
					   ,(run dit params env)
					   ,(run dif params env)))))
			((eq? (car pe) 'applic)
			 (with pe (lambda (_ proc vars)
				    `(applic, (run proc params env)
					      ,(map (lambda (x)
						      (run x params env)) vars)))))
			((eq? (car pe) 'seq)
			 (with pe (lambda (_ seq)
				    `(seq, (map (lambda (x)
						  (run x params env)) seq)))))
			((eq? (car pe) 'or)
			 (with pe (lambda (_ tests)
				    `(or, (map (lambda (x)
						  (run x params env)) tests)))))
			((eq? (car pe) 'define)
			 (with pe (lambda (_ name defined)
				    `(define, (run name params env)
					      ,(run defined params env)))))
			(else (error 'pe->lex-pe
				     (format "Stuck here: ~s" pe)))))))
    (lambda (pe)
      (run pe '() '()))))

(define annotate-tc
  (lambda (pe)
    (run pe #t)))

(define pe-if?
  (lambda (x)
    (eq? (car x) 'if-3)))

(define pe-const?
  (lambda (x)
    (eq? (car x) 'const)))
(define pe-lambda-simple?
  (lambda (x)
    (eq? (car x) 'lambda-simple)))
(define pe-lambda-opt?
  (lambda (x)
    (eq? (car x) 'lambda-opt)))
(define pe-lambda-variadic?
  (lambda (x)
    (eq? (car x) 'lambda-variadic)))

(define pe-seq?
  (lambda (x)
    (eq? (car x) 'seq)))

(define pe-or?
  (lambda (x)
    (eq? (car x) 'or)))

(define pe-define?
  (lambda (x)
    (eq? (car x) 'define)))

(define pe-applic?
  (lambda (x)
    (eq? (car x) 'applic)))



(define pe-var?
  (lambda (x)
    (or (eq? (car x) 'fvar) (eq? (car x) 'pvar) (eq? (car x) 'bvar))))

(define run
  (lambda (pe tp?)
    (cond ((pe-const? pe) pe)

	  ((pe-var? pe) pe)

	  ((pe-if? pe)
	   (with pe
	     (lambda (_ test dit dif)
	       `(if-3, (run test #f)
		       ,(run dit tp?)
		       ,(run dif tp?)))))
	  ((pe-seq? pe)
	   (with pe
	     (lambda (_ seqs)
	       `(seq, (myMap run tp? seqs)))))
	  
	  ((pe-or? pe)
	   (with pe
	     (lambda (_ ors)
	       `(or, (myMap run tp? ors)))))

	  ((pe-define? pe)
	    (with pe
	      (lambda (_ var defined)
		`(define, var, (run defined #f)))))
	  ((pe-applic? pe)
	   (with pe
	     (lambda (_ proc vars)
	       (if tp?
		   `(tc-applic, (run proc #f), (myMap run #f vars))
		   `(applic, (run proc #f), (myMap run #f vars))))))
	  ((pe-lambda-simple? pe)
	   (with pe
	     (lambda (_ vars exp)
	       `(lambda-simple, vars, (run exp #t)))))
	   ((pe-lambda-opt? pe)
	   (with pe
	     (lambda (_ vars rest exp)
	       `(lambda-opt, vars, rest, (run exp #t)))))

	  ((pe-lambda-variadic? pe)
	   (with pe
	     (lambda (_ vars exp)
	       `(lambda-variadic, vars, (run exp #t)))))
		   
	  
	  
	  

	  (else (error 'run-tc
				     (format "Stuck here: ~s" pe))))))

;map with false except of the last
(define myMap
  (lambda (f tp? list)
    (cond ((null? list) `())
    ((null? (cdr list))
	`(,(f (car list) tp?)))
	(else `(,(f (car list) #f) ,@(myMap f tp? (cdr list)))))))


	  
(define test
  (lambda (e)
    (annotate-tc
     (pe->lex-pe
      (parse e)))))