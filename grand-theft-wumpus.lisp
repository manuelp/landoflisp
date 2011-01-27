(defparameter *congestion-city-nodes* nil)
(defparameter *congestion-city-edges* nil)
(defparameter *visited-nodes* nil)
(defparameter *node-num* 30)
(defparameter *edge-num* 45)
(defparameter *worm-num* 3)
(defparameter *cop-odds* 15)

; () -> numero random tra 1 e *node-num*
(defun random-node ()
  (1+ (random *node-num*)))

; (a b) -> ((a.b) (b.a))
(defun edge-pair (a b)
  (unless (eql a b)
    (list (cons a b) (cons b a))))

; () -> ((a.b) (b.a) (c.d) (d.c) ...)
;
; dove a,b,c,d,... sono numeri random generati da #'random-node
(defun make-edge-list ()
  (apply #'append (loop repeat *edge-num*
			collect (edge-pair (random-node) (random-node)))))

; ---- Questi parametri sono per il debugging
(defparameter *test-nodes* '(a b c d e f g h i j))
(defparameter *test-edges* '((a . b) (b . a) 
			(a . c) (c . a) 
			(a . e) (e . a) 
			(e . d) (d . e) 
			(e . f) (f . e)
			(g . h) (h . g)
			(g . i) (i . g)))
; ----

(defun direct-edges (node edge-list)
  (remove-if-not (lambda (x)
		   (eql node (car x)))
		 edge-list))

(defun get-connected (node edge-list)
  (let ((visited nil))
    (labels ((traverse (node)
		       (unless (member node visited)
			 (push node visited)
			 (mapc (lambda (edge)
				 (traverse (cdr edge)))
			       (direct-edges node edge-list)))))
	    (traverse node))
    visited))

(defun find-islands (nodes edge-list)
  (let ((islands nil))
    (labels ((find-island (nodes)
			  (let* ((connected (get-connected (car nodes) edge-list))
				 (unconnected (set-difference nodes connected)))
			    (push connected islands)
			    (when unconnected
			      (find-island unconnected)))))
	    (find-island nodes))
    islands))
    
(defun connect-with-bridges (islands)
  (when (cdr islands)
    (append (edge-pair (caar islands) (caadr islands))
	    (connect-with-bridges (cdr islands)))))

(defun connect-all-islands (nodes edge-list)
  (append (connect-with-bridges (find-islands nodes edge-list))
	  edge-list))