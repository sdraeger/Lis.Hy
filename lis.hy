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

	(if (= '(' token))
		(setv L [])
		(while (!= (get tokens 0) ')')
			(.append L (read-from-tokens tokens)))
		(.pop tokens 0)
		L)
