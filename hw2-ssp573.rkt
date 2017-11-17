(define L (list '1 '2 '3 '4) )
(write "Input-list for Qa Qb Qe:")L

#|Question a|#
(define (foldr f s L)(if (null? L) s (f (car L) (foldr f s (cdr L)))))
(write 'Answer-b-)(write 'seed-value=0:)(foldr + 0 L)
  
#|Question b|#
(define (rev L)(if (null? (cdr L))(list (car L))(append (rev (cdr L)) (list (car L)))))
(define (paramreverse fn A)(apply fn (rev A)))
(write 'Answer-b:)(paramreverse - L)


#|Question c|#
(define M (list '3 '2 '1 '4 '100 '2))
(define (insertion-sort L)(if (not (null? L))                                                  #|function to sort k=list ibn descending order|#
                    (add (car L) (insertion-sort (cdr L)))
                    '()
                 )
)
(define (add e A)(if(null? A)                                                                  #|helper function for insertion sort which puts the element in the correct position|# 
                    (cons e A)
                    (if (>= e (car A))
                        (cons e A)
                        (cons (car A) (add e (cdr A)))
                        )
                    )
)
(define (lengthof L)(                                                                          #|helper function to find length of a list|#
                      if (null? L) (+ 0 0) (+ 1 (lengthof (cdr L))) 
                      )
)
(define (k-largest L k)(if (< k 1) '() (if (> k (lengthof L))
                                           (append (insertion-sort L) '())                                      #|when k > length(L), entire L is returned|#
                                           (cons (car (insertion-sort L)) (k-largest (cdr (insertion-sort L)) (- k 1)))
                                           )
                           )
)
(write 'Answer-c:)
(write "Input-list:")M
(k-largest M 3)


#|Question d|#
(define FL (list car cdr cadr ))                                                          #|list of operations|#
(define LA '((A B) (C D E) (A B C D E)))                                                  #|list of argument lists|#
(define (mapfun FL LA)(                                                                   #|function to map argument lists to operations, i.e op[i] arg[i]|#
                      if (not (null? (cdr FL)))(cons ((car FL) (car LA)) (mapfun (cdr FL) (cdr LA)))(list ((car FL) (car LA)))
                      ))
(write 'Answer-d:)
(write "List of operations: ")FL
(write "List of arguments: ")LA
(mapfun FL LA)



#|Question e|#
(define (filter pred L)(
                        if (not (null? (cdr L)))(
                           if (pred (car L))(cons (car L) (filter pred (cdr L)))(filter pred (cdr L))    #|function to check the predicate with every element in L|#
                             )
                           (if (pred (car L))(list (car L))(cdr L))                                      #|base case where L is singleton|#
                       )
)
(write 'Answer-e:)
(filter even? L)
(write 'predicate=even?)
