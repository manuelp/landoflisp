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

(defun make-city-edges ()
  (let* ((nodes (loop for i from 1 to *node-num*
		      collect i))
	 (edge-list (connect-all-islands nodes (make-edge-list)))
	 (cops (remove-if-not (lambda (x)
				(zerop (random *cop-odds*)))
			      edge-list)))
    (add-cops (edges-to-alist edge-list) cops)))

; Converte la lista di connessioni in una alist (doppia) a cui ad ogni
; nodo è associata una serie di liste ad un solo elemento, il quale
; rappresenta un nodo raggiungibile direttamente da quello
; considerato.
(defun edges-to-alist (edge-list)
  (mapcar (lambda (node1)
	    (cons node1
		  (mapcar (lambda (edge)
			    (list (cdr edge)))
			  (remove-duplicates (direct-edges node1 edge-list)
					     :test #'equal))))
	  (remove-duplicates (mapcar #'car edge-list))))

; mapcar sui nodi, poi mapcar interno sui nodi connessi ad ognuno. Nel
; mapcar più interno costruisce un edge-pari per il nodo sorgente e
; destinazione e controlla l'eventuale intersezione con la lista delle
; connessioni con la polizia. Se c'è allora crea una lista con il nodo
; e il simbolo 'COPS, altrimenti ritorna direttamente la lista singola
; con il nodo di destinazione (es. (2)).
;
; alist: ((1 (2)) (2 (1) (3)) (3 (2)))
; cops: (2 . 1)
; result: ((1 (2)) (2 (1 COPS) (3)) (3 (2)))
(defun add-cops (edge-alist edges-with-cops)
  (mapcar (lambda (x)
	    (let ((node1 (car x))
		  (node1-edges (cdr c)))
	      (cons node1
		    (mapcar (lambda (edge)
			      (let ((node2 (car edge)))
				(if (intersection (edge-pair node1 node2)
						  edges-with-cops
						  :test #'equal)
				    (list node2 'cops)
				  edge)))
			    node1-edges))))
	  edge-alist))