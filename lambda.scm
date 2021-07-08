(use gauche.record)
(use gauche.collection)

(define-record-type Var (var n) var? (n var-name))
(define-record-type Lam (lam n e) lam? (n lam-name) (e lam-expr))
(define-record-type App (app f e) app? (f app-func) (e app-expr))


(define (show e)
  (cond
   ((var? e) (let ((n (var-name e))) (string-append "(Var " n  ")"))) 
   ((lam? e) (let ((n (lam-name e)) (e (lam-expr e))) (string-append "(Lam " n " " (show e) ")")))
   ((app? e) (let ((f (app-func e)) (e (app-expr e))) (string-append "(App " (show f) " " (show e) ")")))
   (else (error "Unexpected expr at show."))
   )
  )

(define (free e)
  (cond
   ((var? e) (list (var-name e)))
   ((lam? e) (delete (lam-name e) (free (lam-expr e))))
   ((app? e) (concatenate (list (free (app-func e)) (free (app-expr e)))))
   (else (error "Unexpected expr at free."))
   )
  )

(define (make-subst)
  (let ((i 0))
    (define (subst e2 x e1)
      (cond
       ((var? e1)
        (if (string=? x (var-name e1)) e2 e1))
       ((lam? e1)
        (if (string=? x (lam-name e1))
            (if (member x (free e2))
                (let ((y (string-append (lam-name e1) (write-to-string i))))
                  (set! i (+ i 1))
                  (lam y (subst e2 x (subst (var y) (lam-name e1) (lam-expr e1)))))
                (lam (lam-name e1) (subst e2 x (lam-expr e1))))
            e1))
       ((app? e1) (app (subst e2 x (app-func e1)) (subst e2 x (app-expr e1))))
       (else (error "Unexpected expr at free."))
       ))
    (lambda (x y z) (subst x y z))
    ))

(list
 (free (app (lam "x" (var "y")) (var "x")))
 )

(let ((subst (make-subst)))
  (list
   ;(show (subst (var "x") "y" (var "y")))
   ;(show (subst (var "w") "x" (lam "x" (var "x"))))
   (show (subst (lam "x" (var "x")) "x" (lam "x" (var "x"))))
   (show (subst (var "x") "y" (lam "x" (app (var "y") (var "x")))))
))
