(import [__future__ [division]])
(import math)
(import [operator :as op])

(setv Symbol str)
(setv List list)
(setv Number (, int float))

(defn parse [program]
	"Read a Scheme expression from a string."
	(read-from-tokens (tokenize program)))

(defn tokenize [s]
	"Convert a string into a list of tokens."
	(.split (.replace (.replace s "(" " ( ") ")" " ) ")))

(defn read-from-tokens [tokens]
	"Read an expression from a sequence of tokens."
	(if (= (len tokens) 0)
		(raise (SyntaxError "unexpected EOF while reading")))

	(setv token (.pop tokens 0))
	(if (= token "(")
			(do
				(setv L [])
				(while (not (= (get tokens 0) ")"))
					(.append L (read-from-tokens tokens)))
				(.pop tokens 0)
				(return L))
			(= token ")")
			(raise (SyntaxError "unexpected )"))
			(return (atom token))))

(defn atom [token]
	"Numbers become numbers; every other token is a symbol."
	(try
		(return (int token))
	(except [ValueError]
		(try
			(return (float token))
		(except [ValueError]
			(return (Symbol token)))))))

;;; Environments

(defn standard-env []
	"An environment with some Scheme standard procedures."
	(setv env (Env))
	(env.update (vars math))
	(env.update
		{"+" op.add  "-" op.sub  "*" op.mul  "/" op.truediv
		 ">" op.gt "<" op.lt  ">=" op.ge  "<=" op.le  "=" op.eq
		 "abs" abs
		 "append" op.add
		 "apply" (fn [f &rest args] (f #* args))
		 "begin" (fn [&rest x] (get x -1))
		 "car" (fn [x] (get x 0))
		 "cdr" (fn [x] (cut x 1))
		 "cons" (fn [x y] (+ [x] y))
		 "eq?" op.is_
		 "equal?" op.eq
		 "length" len
		 "list" (fn [&rest x] (list x))
		 "list?" (fn [x] (isinstance x list))
		 "map" map
		 "max" max
		 "min" min
		 "not" op.not_
		 "null?" (fn [x] (= x []))
		 "number?" (fn [x] (isinstance x Number))
		 "procedure?" callable
		 "round" round
	     "symbol?" (fn [x] (isinstance x Symbol))})
	env)

(defclass Env [dict]
	"And environment: a dict of {'var':val} pairs with an outer Env."

	(defn __init__ [self &optional [parms (,)] [args (,)] [outer None]]
		(.update self (zip parms args))
		(setv self.outer outer))

	(defn find [self var]
		"Find the innermost Env where var appears"
		(if (in var self)
			self
			(self.outer.find var))))

(setv global-env (standard-env))

;;; The REPL

(defn repl [&optional [prompt "lis.hy> "]]
	"A prompt-read-eval-print loop."
	(while True
		(setv val (eval (parse (input prompt))))
		(if (not (none? val))
			(print (lispstr val)))))

(defn lispstr [exp]
	"Convert a Python object back into a Lisp-readable string."
	(if (isinstance exp List)
		(+ "(" (.join " " (map lispstr exp)) ")")
		(str exp)))

(defclass Procedure [object]
	"A user-defined Scheme procedure."

	(defn __init__ [self parms body env]
		(setv (, self.parms self.body self.env) (, parms body env)))

	(defn __call__ [self &rest args]
		(eval self.body (Env self.parms args self.env))))

(defn eval [x &optional [env global-env]]
	"Evaluate an expression in an environment."
	(if
		(= (get x 0) "'") (do
							(setv exp (cut (list x) 1))
							(.join "" exp))

		(isinstance x Symbol) (get (env.find x) x)

		(not (isinstance x List)) x

		(= (get x 0) "quote") (do
								(setv (, _ exp) x)
								exp)

		(= (get x 0) "if") (do
							(setv (, _ test conseq alt) x)
							(setv exp (if (eval test env) conseq alt))
							(eval exp env))

		(= (get x 0) "define") (do
								(setv (, _ var exp) x)
								(setv (get (env.find var) var) (eval exp env)))

		(= (get x 0) "set!") (do
							  (setv (, _ var exp) x)
							  (setv (get (env.find var) var) (eval exp env)))

		(= (get x 0) "lambda") (do
								(setv (, _ parms body) x)
								(Procedure parms body env))
								
		(do
			(setv proc (eval (get x 0) env))
			(setv args (lfor  exp (cut x 1)  (eval exp env)))
			(proc #* args))))

(repl)
