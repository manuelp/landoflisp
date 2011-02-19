(load "graph-util")

(defparameter *congestion-city-nodes* nil)
(defparameter *congestion-city-edges* nil)
(defparameter *player-pos* nil)
(defparameter *visited-nodes* nil)
(defparameter *node-num* 30)
(defparameter *edge-num* 45)
(defparameter *worm-num* 3)
(defparameter *cop-odds* 15)
(defparameter *game-over* nil)

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
		  (node1-edges (cdr x)))
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

(defun neighbors (node edge-alist)
  (mapcar #'car (cdr (assoc node edge-alist))))

(defun within-one (a b edge-alist)
  (member b (neighbors a edge-alist)))

(defun within-two (a b edge-alist)
  (or (within-one a b edge-alist)
      (some (lambda (x)
	      (within-one x b edge-alist))
	    (neighbors a edge-alist))))

; Aggiunge i simboli del wumpus, dei glown-worms e gli indizi (blood,
; lights and sirens) creando una nuova alist.
(defun make-city-nodes (edge-alist)
  (let ((wumpus (random-node))
	(glown-worms (loop for i below *worm-num*
			   collect (random-node))))
    (loop for n from 1 to *node-num*
	  collect (append (list n)
			  (cond ((eql n wumpus) '(wumpus))
				((within-two n wumpus edge-alist) '(blood!)))
			  (cond ((member n glown-worms)
				 '(glown-worm))
				((some (lambda (worm)
					 (within-one n worm edge-alist))
				       glown-worms)
				 '(lights!)))
			  (when (some #'cdr (cdr (assoc n edge-alist)))
			    '(sirens!))))))

(defun find-empty-node ()
  (let ((x (random-node)))
    (if (cdr (assoc x *congestion-city-nodes*))
	(find-empty-node)
      x)))

(defun draw-city ()
  (ugraph->png "city" *congestion-city-nodes* *congestion-city-edges*))

; Crea una lista parziale di nodi di congestion city a partire dalla posizione
; attuale. Mostra solo i nodi direttament e raggiungibili con una
; connessione diretta e senza informazioni sugli attributi di quei
; nodi.
(defun known-city-nodes ()
  (mapcar (lambda (node)
	    (if (member node *visited-nodes*)
		(let ((n (assoc node *congestion-city-nodes*)))
		  (if (eql node *player-pos*)
		      (append n '(*))
		    n))
	      (list node '?)))
	  (remove-duplicates
	   (append *visited-nodes*
		   (mapcan (lambda (node)
			     (mapcar #'car
				     (cdr (assoc node *congestion-city-edges*))))
			   *visited-nodes*)))))

; Crea le connessioni parziali tra i nodi visitati e quelli
; direttamente connessi. Conserva gli attributi della connessione
; (cops) solo se sia il nodo sorgente che di destinazione sono stati
; visitati.
(defun known-city-edges ()
  (mapcar (lambda (node)
	    (cons node (mapcar (lambda (x)
				 (if (member (car x) *visited-nodes*)
				     x
				   (list (car x))))
			       (cdr (assoc node *congestion-city-edges*)))))
	  *visited-nodes*))

(defun draw-known-city ()
  (ugraph->png "know-city" (known-city-nodes) (known-city-edges)))

(defun msg (text)
  (princ text)
  (fresh-line))

(defun new-game ()
  (setf *congestion-city-edges* (make-city-edges))
  (setf *congestion-city-nodes* (make-city-nodes *congestion-city-edges*))
  (setf *player-pos* (find-empty-node))
  (setf *visited-nodes* (list *player-pos*))
  (setf *game-over* nil)
  (draw-city)
  (draw-known-city)
  (msg "---[ Grand Theft Wumpus ]---"))

(defun walk (pos)
  (handle-direction pos nil))

(defun charge (pos)
  (handle-direction pos t))

(defun handle-direction (pos charging)
  (let ((edge (assoc pos (cdr (assoc *player-pos* *congestion-city-edges*)))))
    (if edge
	(handle-new-place edge pos charging)
      (msg "That location does not exist!"))))

(defun handle-new-place (edge pos charging)
  (let* ((node (assoc pos *congestion-city-nodes*))
	 (has-worm (and (member 'glown-worm node)
			(not (member pos *visited-nodes*)))))
    (pushnew pos *visited-nodes*)
    (setf *player-pos* pos)
    (draw-known-city)
    (cond ((member 'cops edge) (game-over "You rand into the cops. Game Over."))
	  ((member 'wumpus node) (if charging
				     (game-over "You found the wumpus!")
				   (game-over "You ran into the Wumpus. Game Over.")))
	  (charging (game-over "You wasted your last bullet. Game Over."))
	  (has-worm (let ((new-pos (random-node)))
		      (princ "You ran into a Glow Worm Gang! You're now at ")
		      (princ new-pos)
		      (fresh-line)
		      (handle-new-place nil new-pos nil))))))
		 
(defun game-over (text)
  (msg text)
  (setf *game-over* t)
  (new-game))

; CLI
(defparameter *allowed-commands* '(new-game walk charge quit))

(defun game-repl ()
  (let* ((cmd (read-from-string (concatenate 'string "(" (read-line) ")"))))
    (unless (or (eq (car cmd) 'quit)
		*game-over*)
      (if (member (car cmd) *allowed-commands*)
	  (eval cmd)
	(progn
	  (princ "Invalid command!")
	  (fresh-line)))
      (game-repl))))

(new-game)
(game-repl)