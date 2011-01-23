; --- The Wizard's Adventure

; Seguono le strutture dati di nodi e spigoli (association lists o alist)
(defparameter *nodes* 
  '((living-room (you are in the living-room. a wizard is snoring loudly on the couch.))
    (garden (you are in a beautiful garden. there is a well in front of you.))
    (attic (you are in the attic. there is a giant welding torch in the corner.))))

(defparameter *edges* 
  '((living-room (garden west door) (attic upstairs ladder)) 
    (garden (living-room east door)) 
    (attic (living-room downstairs ladder))))

; assoc per recuperare liste in base ad una chiave
(defun describe-location (location nodes)
  (cadr (assoc location nodes)))
	
; quasiquoting
(defun describe-path (edge)
  `(there is a ,(caddr edge) going ,(cadr edge) from here.))

;higher-order functions (and the 2 namespaces: variables + functions)
(defun describe-paths (location edges)
  (apply #'append (mapcar #'describe-path (cdr (assoc location edges)))))

(defparameter *objects* '(whiskey bucket frog chain))
(defparameter *object-locations* '((whiskey living-room)
				   (bucket living-room)
				   (chain garden)
				   (frog garden)))

; Definita una funzione interna di tipo predicate che ritorna T se
; l'oggetto specificato e' presente nel loco passato come parametro
; (closure) controllando nella alist che mappa gli oggetti ai lochi in
; cui si trovano.
;
; Dopodiche' uso di tale funzione per rimuovere dalla lista di oggetti
; tutti quelli che non sono presenti nel loco che ci interessa, usando
; la funzione appena definita come predicate.
(defun objects-at (loc objs obj-locs)
  (flet ((at-loc-p (obj)
	     (eq (cadr (assoc obj obj-locs)) loc)))
    (remove-if-not #'at-loc-p objs)))

; Usando la funzione precedente che produce una lista di oggetti
; presenti nella locazione passata come parametro, definisce una
; funzione per descriverla tramite il quasiquoting e la applica ad
; ognuno degli oggetti ritornati da objects-at. Le descrizioni vengono
; semplicemente concatenate.
(defun describe-objects (loc objs obj-locs)
  (labels ((describe-obj (obj)
	   `(There is a ,obj on the floor.)))
    (apply #'append (mapcar #'describe-obj (objects-at loc objs obj-locs)))))

(defparameter *location* 'living-room)

(defun look ()
	(append (describe-location *location* *nodes*)
			(describe-paths *location* *edges*)
			(describe-objects *location* *objects* *object-locations*)))

; A partire dalla direzione (simbolo), prende in *edges* tutte le vie che 
; partono dalla *location* corrente e tra questi elementi cerca quello con la
; direction specificata. Se la direzione (dalla locazione corrente) e' valida, 
; sposta il giocatore e descrivi la nuova locazione, altrimenti stampa 
; un messaggio di errore.
(defun walk (direction)
	(let ((next (find direction
					(cdr (assoc *location* *edges*))
					:key #'cadr)))
		(if next
			(progn (setf *location* (car next))
				   (look))
			'(you cannot go that way.))))

; Controlla se l'oggetto che voglio prendere e' nella lista di quelli di quelli
; presenti nella locazione in cui sono. Se si, aggiungi alla lista la nuova locazione.
; Dato che push e' una convenience per (cons 'elem '(lista locazioni), il nuovo elemento 
; viene inserito in testa alla lista. assoc restituisce la prima occorrenza, 
; quindi usando la coppia push/assoc si ottiene l'illusione di modificare una lista 
; al tempo stesso conservandone lo storico (ma incrementa l'uso di memoria, 
; per sostituirli sul serio occorre usare un'altra tecnica).
(defun pickup (object)
	(cond ((member object (objects-at *location* *objects* *object-locations*))
		   (push (list object 'body) *object-locations*)
		   `(you are now carrying the ,object))
		  (t '(you cannot get that.))))

(defun inventory ()
	(cons 'items- (objects-at 'body *objects* *object-locations*)))

; I/O: implementazione di una REPL custom
(defun game-repl ()
	(let ((cmd (game-read)))
		(unless (eq (car cmd) 'quit)
			(game-print (game-eval cmd))
			(game-repl))))

(defun game-read ()
  (let ((cmd (read-from-string
	      (concatenate 'string "(" (read-line) ")"))))
    (flet ((quote-it (x)
		     (list 'quote x)))
	  (cons (car cmd) (mapcar #'quote-it (cdr cmd))))))

(defparameter *allowed-commands* '(look walk pickup inventory))

(defun game-eval (sexp)
  (if (member (car sexp) *allowed-commands*)
      (eval sexp)
    '(i do not know that command.)))


(defun tweak-text (lst caps lit)
  (when lst
    (let ((item (car lst))
	  (rest (cdr lst)))
      (cond ((eq item #\space) (cons item (tweak-text rest caps lit)))
	    ((member item '(#\! #\? #\.)) (cons item (tweak-text rest t lit)))
	    ((eq item #\") (cons item (tweak-text rest caps (not lit))))
	    (lit (cons item (tweak-text rest nil lit)))
	    (caps (cons (char-upcase item) (tweak-text rest nil lit)))
	    (t (cons (char-downcase item) (tweak-text rest nil nil)))))))

(defun game-print (lst)
	(princ (coerce (tweak-text (coerce (string-trim "() " (prin1-to-string lst))
					   'list)
				   t
				   nil)
		       'string))
	(fresh-line))
