(defun fact (n)
	(if (equal n 1)
		1
		(* n (fact (- n 1)))))
		
(print (fact 4))

(defun accum (n)
	(if (equal n 1)
		'(1)
		(cons n (accum (- n 1)))))
		
(print (accum 15))

(defun disp (n)
	(if (equal n 1)
		'(1)
		(cond 
			((oddp n) (cons n (disp (- n 1))))
			(t (disp (- n 1))))))
			
(print (disp 7))

(print (cons (accum 27) (disp 8)))

(defun greet ()
	(prin1 "Please type your name: ")
	(let ((name (read)))
		(prin1 "Nice to meet you, ")
		(prin1 name)))
			
