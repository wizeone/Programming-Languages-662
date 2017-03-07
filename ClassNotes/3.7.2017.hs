First class function -> a value

(app f a) = [i |-> a]b

Substitution is key in function evaluation.

(eval (Bind i v b)) = eval (substitute i (eval v) b)

(App f a) = let (lambda i b) = eval f
                a' = eval a
            in eval (substitute i a' b)

eval env (Bind i v a) = let v' = eval env v in
                          eval ((i,v'):env) b

eval env (App f a) = let (Lambda i b) = eval env f
                        a' = eval env a
                     in eval ((i,a'):env) b
-----------------------------------------------------------
bind f = (lambda x in x+1) in
            app f 3

representation of env.
evn = [] at start
after the in: env = [(f, (lambda x (plus (id "x" (num 1)))))]

                        == after eval we get
                              app (lambda x (plus (id "x" (num 1)))) 3
value correlation seen above              i   b                      a'

new env = [(x,3), (f, (lambda x (plus (id "x" (num 1)))))]

== (Plus (id, "x") (Num 1))
== (Plus (Num 3)   (Num 1))
== (Num 4)

-------------------------------------------------------------

bind n = 1 in
  bind f = (lambda x in x + n) in
    app f 1

after first bind env = [(n,1)]
after second bind env = [(f, (lambda ___)), (n,1)]
when app is being called env = [(x,1), (f, (lambda__)), (n,1)]

x+n
1+n
1+1
2
