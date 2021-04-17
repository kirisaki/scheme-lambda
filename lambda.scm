(use gauche.record)

(define-record-type Var (var n) var? (n var-name))
(define-record-type Lam (lam n e) lam? (n lam-name) (e lam-expr))
(define-record-type App (app f e) app? (f app-func) (e app-expr))

(define (name n) (if (symbol? n) (symbol->string n) n))

(define (show e)
  (cond
   ((var? e) (let ((n (var-name e))) (string-append "(Var " (name n)  ")"))) 
   ((lam? e) (let ((n (lam-name e)) (e (lam-expr e))) (string-append "(Lam " (name n) " " (show e) ")")))
   ((app? e) (let ((f (app-func e)) (e (app-expr e))) (string-append "(App " (show f) " " (show e) ")")))
   (else (error "Unexpected expr."))
   )
  )
