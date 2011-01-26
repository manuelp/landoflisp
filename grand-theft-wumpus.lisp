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