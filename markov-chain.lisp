;; Markov chain !

(setf *l* '( a b c a e f g h g i r a t u b n r l l f s a q w s j j k ))

(defun subchains (l)
  (cond
   ( (null l) () )
   ( t (cons (cons  (car l) (followers (car l) *l*)) (subchains (cdr l)) )))) ;; get a subgroup for a given char and add the next that adds...[]

(defun followers (char l) ;; get the chars that follow a given char
  (cond
   ( (null l) ())
   ( (eq (car l) char) (cons (cadr l) (followers char (cdr l))) ) ;; if the char analized its the same cons it, else keep recursing
   ( t (followers char (cdr l)))))

(defun pick (char l) ;; pick a random char that is not the first
  (nth (+ 1 (random (- (length (assoc char l)) 1))) (assoc char l))) ;; range (1, length -1)

(defun generate-chain (len char l)
  (cond
   ( (zerop len) () )
   ( t (let ((random-char (pick char l))) ; declares local variable
	 (cons random-char (generate-chain (- len 1) random-char l))) ))) ;; cons the random pick with another that cons with another one....

(generate-chain 10 'a (subchains *l*))


		     
