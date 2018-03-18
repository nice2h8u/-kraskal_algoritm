;*********
;Krascal algoritm at lisp
;Алгоритм Краскала на лиспе
;
;input > (Kraskal '((d e 2) (e c 3) (b c 1) (c d 4) (a b 5)  (a z 6) (c a 2) (b d 3 )))
;ouput => (((A Z 6) (E C 3) (C A 2) (D E 2) (B C 1)))



;check list for first / second letter of edge
(defun listCheck (rebro spisok &optional letter)
(cond ((null spisok) nil)
((and (null letter) (or (eq (caar spisok) (car rebro)) ;;если
(eq (cadar spisok) (car rebro)) ;;одна буква совпадает с ребром
))spisok) ;;возращаем список
((and (eq 1 letter)(or (eq (caar spisok) (cadr rebro)) ;;если
(eq (cadar spisok) (cadr rebro)) ;;одна буква совпадает с ребром
))spisok)
((null letter)(listCheck rebro (cdr spisok)))
(t(listCheck rebro (cdr spisok) 1))))

;cons two lists
(defun foo(A B)

  (if A

    (if B

      (append (list (car A) (car B)) (foo (cdr A) (cdr B))) A)

    (if B B nil)))

 
;support for foo
(defun bar(A B &optional (R nil))

  (if A

    (if B

      (bar (cdr A) (cdr B) (append R (list (car A) (car B))))

      (append R A))

    (if B

      (append R B) R)))

;return combination of safe edge + (nil/two combined list/ list)  
(defun allListCheck (rebro spisok countlist twolists )
(cond ((null spisok) (if (eq countList 2)    (cons(cons rebro twolists)nil)  (if(eq countList 1)  (cons( cons rebro twolists)nil) (cons(cons rebro nil)nil))))

    ((and (not (null (listcheck rebro (car spisok) nil ))) (not(null(listcheck rebro (car spisok)  1 )))) nil);если в подсписке две буквы совпадают, то ребро небезопасно 

    ((or (not (null (listcheck rebro (car spisok) nil ))) (not(null(listcheck rebro (car spisok)  1))));если есть одно ребро в списке количество ребер в посписке+1, а
(allListCheck rebro (cdr spisok) (+ countlist 1) (foo   twolists (car spisok)   ))) ;к результату прибавляем текущий посписок 
     (t(alllistcheck rebro (cdr spisok) countlist twolists  ))))

(defun ssort (w)
  (sort w #'< :key #'caddr))                    
 
; delete trash combination which had turned out after alllistcheck from main list  
(defun deletecopy (lst rebro)

  (if (null lst)

      nil

      (if (or (not (null (listcheck rebro (car lst) nil ))) (not(null(listcheck rebro (car lst)  1 ))))

          (deletecopy (cdr lst) rebro)

          (cons (car lst) (deletecopy (cdr lst) rebro)))))


;main f which do all calculating
(defun mainf(rebrolist list)
  (cond ((null rebrolist)list)
     ((null(alllistcheck (car rebrolist) list 0 `())) (mainf (cdr rebrolist) list))
   (T(mainf (cdr rebrolist)( cons (car(alllistcheck (car rebrolist) list 0 `()))(deletecopy list (car(car(alllistcheck (car rebrolist) list 0 `())))) )
))))


;kraskal f
(defun Kraskal (list)
 (mainf (ssort list) `()))
        

