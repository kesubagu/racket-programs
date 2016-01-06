;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname advanced) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;;****************************************************
;;    Keshav Bagla, student number-20603369        
;;    CS 135, Fall 2015
;;    Assingment 7, Question 2                
;;****************************************************







(define-struct staff-member (id name dept))
;; A Staff-Member is a (make-staff-member Nat Str Str)
;; requires: id is unique
;; (i.e., every staff-member with the same id also has the same name)
(define-struct salary (staff-id base bonus))
;; A Salary is a (make-salary Nat Num Num)


(define-struct staff-node (staff left right))
;; A Staff-Node is a
;;(make-staff-node Staff-Member Staff-BST Staff-BST)
;; requires: ids of all Staff-Member on the left
;; subtree are smaller than the id of Staff-Member
;; ids of all Staff-Member on the right subtree are larger than
;; the id of Staff-Member

;; A Staff-BST is one of:
;;* empty
;;* Staff-Node

(define staff1 (make-staff-member 1 "John" "Engineering"))
(define staff2 (make-staff-member 2 "Adam" "R&D"))
(define staff3 (make-staff-member 3 "Albert" "Engineering"))
(define staff4 (make-staff-member 4 "Liz" "Finance"))
(define staff5 (make-staff-member 5 "Anne" "Defence"))
(define staff6 (make-staff-member 6 "Suzy" "R&D"))
(define staff7 (make-staff-member 7 "Leslie" "R&D"))
(define staff8 (make-staff-member 8 "Josh" "Engineering"))
(define staff9 (make-staff-member 9 "Kesu" "Engineering"))


(define salary1 (make-salary 1 50000 10))
(define salary2 (make-salary 2 74000 250))
(define salary3 (make-salary 3 85000 10))
(define salary4 (make-salary 4 60000 0))
(define salary5 (make-salary 5 93000 100))
(define salary6 (make-salary 6 68500 0))
(define salary7 (make-salary 7 250000 0))
(define salary8 (make-salary 8 120000 0))
(define salary9 (make-salary 8 100000 0))


;;Sample staff-list
(define staff-list (list staff1 staff2 staff3 staff4
                         staff5 staff6 staff7 staff8))
;;Sample Salary-list
(define sal-list (list salary1 salary2 salary3 salary4
                       salary7 salary8))






#|
(add-staff-bst staff-BST staff-to-add) consumes a binary search
tree and a staff-member, and adds them to the appropriate
location based on the staff members ID

add-staff-bst: Staff-BST Staff-Member -> Staff-BST
|#
;;Examples
(check-expect (add-staff-bst
                (make-staff-node staff5
                                 (make-staff-node staff3 empty empty) empty)
                staff6)
              (make-staff-node
                (make-staff-member 5 "Anne" "Defence")
                (make-staff-node
                 (make-staff-member 3 "Albert" "Engineering") empty empty)
                (make-staff-node
                 (make-staff-member 6 "Suzy" "R&D") empty empty)))

(check-expect (add-staff-bst
                (make-staff-node staff5 empty
                                 (make-staff-node staff8 empty empty))
                staff2)
             (make-staff-node (make-staff-member 5 "Anne" "Defence")
                              (make-staff-node
                               (make-staff-member 2 "Adam" "R&D") empty empty)
                              (make-staff-node
                               (make-staff-member 8 "Josh" "Engineering")
                               empty empty)))


;;DEFINITION
(define (add-staff-bst staff-BST staff-to-add)
  (cond
    [(empty? staff-BST) (make-staff-node staff-to-add empty empty)]
    
    [(> (staff-member-id (staff-node-staff staff-BST))
        (staff-member-id staff-to-add))
     (make-staff-node
      (staff-node-staff staff-BST)
      (add-staff-bst (staff-node-left staff-BST) staff-to-add)
      (staff-node-right staff-BST))]
    
    [(< (staff-member-id (staff-node-staff staff-BST))
        (staff-member-id staff-to-add))
     (make-staff-node
      (staff-node-staff staff-BST)
      (staff-node-left staff-BST)
      (add-staff-bst (staff-node-right staff-BST) staff-to-add))]))

;;TESTS
;;Left insertion
(check-expect (add-staff-bst
               (make-staff-node staff6 (make-staff-node staff5 empty empty)
                                (make-staff-node staff9 empty empty)) staff3)
              (make-staff-node (make-staff-member 6 "Suzy" "R&D")
                               (make-staff-node
                                (make-staff-member 5 "Anne" "Defence")
                                (make-staff-node
                                 (make-staff-member 3 "Albert" "Engineering")
                                 empty empty) empty)
                                (make-staff-node
                                 (make-staff-member 9 "Kesu" "Engineering")
                                 empty empty)))
;;Right insertion
(check-expect (add-staff-bst
               (make-staff-node staff6 (make-staff-node staff5 empty empty)
                                (make-staff-node staff8 empty empty)) staff9)
              (make-staff-node (make-staff-member 6 "Suzy" "R&D")
                               (make-staff-node
                                (make-staff-member 5 "Anne" "Defence")
                                empty empty)
                               (make-staff-node
                                (make-staff-member 8 "Josh" "Engineering")
                                empty
                                (make-staff-node
                                 (make-staff-member 9 "Kesu" "Engineering")
                                 empty empty))))
;;EMPTY INSERTION
(check-expect (add-staff-bst empty staff4)
              (make-staff-node (make-staff-member 4 "Liz" "Finance")
                               empty empty))


      







;;(add-staff-bst (add-staff-bst (add-staff-bst
;;  (add-staff-bst (add-staff-bst empty staff4)
;;     staff3) staff1) staff6) staff2)

#|
(create-staff-bst-from-list list-of-staff) consumes a list of
  staff members and produces a binary search tree, that
  contains all the members from the list.

create-staff-bst-from-list: (listof Staff-Members) -> Staff-BST
|#
;;EXAMPLES
(check-expect (create-staff-bst-from-list (list staff3 staff4))
              (make-staff-node (make-staff-member 4 "Liz" "Finance")
                               (make-staff-node
                                (make-staff-member 3 "Albert" "Engineering")
                                empty empty) empty))
(check-expect (create-staff-bst-from-list (list staff6 staff2))
              (make-staff-node (make-staff-member 2 "Adam" "R&D")
                               empty (make-staff-node
                                      (make-staff-member 6 "Suzy" "R&D")
                                      empty empty)))
;;DEFINITION
(define (create-staff-bst-from-list list-of-staff)
  (cond
    [(empty? list-of-staff) empty]
    [else (add-staff-bst (create-staff-bst-from-list
                          (rest list-of-staff))
                         (first list-of-staff))]))


;;TESTS
(check-expect (create-staff-bst-from-list (list staff2))
              (make-staff-node (make-staff-member 2 "Adam" "R&D") empty empty))

(check-expect (create-staff-bst-from-list (list staff6 staff2 staff5))
              (make-staff-node (make-staff-member 5 "Anne" "Defence")
                               (make-staff-node
                                (make-staff-member 2 "Adam" "R&D")
                                empty empty)
                               (make-staff-node
                                (make-staff-member 6 "Suzy" "R&D")
                                empty empty)))

(check-expect (create-staff-bst-from-list (list staff2 staff5 staff9 staff7))
              (make-staff-node (make-staff-member 7 "Leslie" "R&D")
                               (make-staff-node
                                (make-staff-member 5 "Anne" "Defence")
                                (make-staff-node
                                 (make-staff-member 2 "Adam" "R&D")
                                 empty empty) empty)
                               (make-staff-node
                                (make-staff-member 9 "Kesu" "Engineering")
                                empty empty)))
(check-expect (create-staff-bst-from-list empty) empty)

(check-expect (create-staff-bst-from-list (list staff3 staff2 staff8 staff4))
              (make-staff-node (make-staff-member 4 "Liz" "Finance")
                               (make-staff-node
                                (make-staff-member 2 "Adam" "R&D") empty
                                (make-staff-node
                                 (make-staff-member 3 "Albert" "Engineering")
                                 empty empty))
                               (make-staff-node
                                (make-staff-member 8 "Josh" "Engineering")
                                empty empty)))

(check-expect (create-staff-bst-from-list (list staff9 staff8 staff5 staff6))
              (make-staff-node (make-staff-member 6 "Suzy" "R&D")
                               (make-staff-node
                                (make-staff-member 5 "Anne" "Defence")
                                empty empty)
                               (make-staff-node
                                (make-staff-member 8 "Josh" "Engineering")
                                empty
                                (make-staff-node
                                     (make-staff-member 9 "Kesu" "Engineering")
                                     empty empty))))










#|
(search-by-id id-to-search staff-BST) consumes a
  staff-members id, and searches for the staff
  member with the respective ID, on the BST, and
  returns the staff-members info upon finding it.

search-by-id: Num Staff-BST -> Staff-Member
|#
;;EXAMPLES
(check-expect (search-by-id 8 (make-staff-node
                               (make-staff-member 4 "Liz" "Finance")
                               (make-staff-node
                                (make-staff-member 2 "Adam" "R&D") empty
                                (make-staff-node
                                 (make-staff-member 3 "Albert" "Engineering")
                                 empty empty))
                               (make-staff-node
                                (make-staff-member 8 "Josh" "Engineering")
                                empty empty)))
              (make-staff-member 8 "Josh" "Engineering"))

(check-expect (search-by-id 3 (make-staff-node
                               (make-staff-member 4 "Liz" "Finance")
                               (make-staff-node
                                (make-staff-member 2 "Adam" "R&D") empty
                                (make-staff-node
                                 (make-staff-member 3 "Albert" "Engineering")
                                 empty empty))
                               (make-staff-node
                                (make-staff-member 8 "Josh" "Engineering")
                                empty empty)))
              (make-staff-member 3 "Albert" "Engineering"))




(define (search-by-id id-to-search staff-BST)
  (cond
    [(empty? staff-BST) empty]
    [(= id-to-search (staff-member-id (staff-node-staff staff-BST)))
     (staff-node-staff staff-BST)]
    [(> id-to-search (staff-member-id (staff-node-staff staff-BST)))
     (search-by-id id-to-search (staff-node-right staff-BST))]
    [(< id-to-search (staff-member-id (staff-node-staff staff-BST)))
     (search-by-id id-to-search (staff-node-left staff-BST))]))


;;TESTS
(check-expect (search-by-id 5 (make-staff-node
                               (make-staff-member 5 "Anne" "Defence")
                                 (make-staff-node
                                  (make-staff-member 2 "Adam" "R&D")
                                   empty empty)
                                 (make-staff-node
                                  (make-staff-member 6 "Suzy" "R&D")
                                   empty empty)))
              (make-staff-member 5 "Anne" "Defence"))

(check-expect (search-by-id 2 (make-staff-node
                               (make-staff-member 5 "Anne" "Defence")
                                 (make-staff-node
                                  (make-staff-member 2 "Adam" "R&D")
                                   empty empty)
                                 (make-staff-node
                                  (make-staff-member 6 "Suzy" "R&D")
                                   empty empty)))
              (make-staff-member 2 "Adam" "R&D"))

(check-expect (search-by-id 6 (make-staff-node
                               (make-staff-member 5 "Anne" "Defence")
                                 (make-staff-node
                                  (make-staff-member 2 "Adam" "R&D")
                                   empty empty)
                                 (make-staff-node
                                  (make-staff-member 6 "Suzy" "R&D")
                                   empty empty)))
              (make-staff-member 6 "Suzy" "R&D"))

(check-expect (search-by-id 9 (make-staff-node
                               (make-staff-member 5 "Anne" "Defence")
                                 (make-staff-node
                                  (make-staff-member 2 "Adam" "R&D")
                                   empty empty)
                                 (make-staff-node
                                  (make-staff-member 6 "Suzy" "R&D")
                                   empty empty)))
              empty)





#|
(who-to-fire sal-list staff-BST threshold-amount) consumes a
  list of salaries, a BST, and a number. The function then uses
  produces a sorted list of staff-members that have a salary
  higher than the threshold amount.

who-to-fire:
  (listof Salary) Staff-BST Num -> (listof Staff-Member)
|#
;;EXAMPLES
(check-expect (who-to-fire sal-list staff-bst 70000)
              (list (make-staff-member 2 "Adam" "R&D")
                    (make-staff-member 3 "Albert" "Engineering")
                    (make-staff-member 7 "Leslie" "R&D")
                    (make-staff-member 8 "Josh" "Engineering")))

(check-expect (who-to-fire sal-list staff-bst 150000)
              (list (make-staff-member 7 "Leslie" "R&D")))

(define (who-to-fire sal-list staff-BST threshold-amount)
  (cond
    [(empty? sal-list) empty]
    
    [(> (+ (salary-base (first sal-list))
           (salary-bonus (first sal-list)))
        threshold-amount)
     (cons (search-by-id (salary-staff-id (first sal-list)) staff-BST)
           (who-to-fire (rest sal-list) staff-BST threshold-amount))]
    
    [else (who-to-fire (rest sal-list) staff-BST threshold-amount)]))

;;Sample staff BST for testing purposes
(define staff-bst (create-staff-bst-from-list staff-list))

;;TESTS
(check-expect (who-to-fire empty staff-bst 100) empty)

(check-expect (who-to-fire sal-list staff-bst 100)
              (list
               (make-staff-member 1 "John" "Engineering")
               (make-staff-member 2 "Adam" "R&D")
               (make-staff-member 3 "Albert" "Engineering")
               (make-staff-member 4 "Liz" "Finance")
               (make-staff-member 7 "Leslie" "R&D")
               (make-staff-member 8 "Josh" "Engineering")))

(check-expect (who-to-fire sal-list staff-bst 100000)
              (list (make-staff-member 7 "Leslie" "R&D")
                    (make-staff-member 8 "Josh" "Engineering")))

(check-expect (who-to-fire sal-list staff-bst 1000000) empty)











#|
(remove-from-bst staff-bst id-to-remove) consumes a staff BST
  and a staff-members id and removes the respective staff
  member from the BST.

remove-from-bst: Staff-BST Num -> Staff-BST
|#
;;EXAMPLES
(check-expect (remove-from-bst staff-bst2 2)
             (make-staff-node
              (make-staff-member 5 "Anne" "Defence")
              (make-staff-node
               (make-staff-member 3 "Albert" "Engineering")
               empty empty)
              (make-staff-node
               (make-staff-member 7 "Leslie" "R&D")
               empty (make-staff-node
                      (make-staff-member 8 "Josh" "Engineering")
                      empty empty))))

(check-expect (remove-from-bst staff-bst3 7)
              (make-staff-node
               (make-staff-member 5 "Anne" "Defence")
               (make-staff-node (make-staff-member 3 "Albert" "Engineering")
                                (make-staff-node
                                 (make-staff-member 2 "Adam" "R&D")
                                 empty empty) empty)
               (make-staff-node (make-staff-member 6 "Suzy" "R&D")
                                empty
                                (make-staff-node
                                 (make-staff-member 8 "Josh" "Engineering")
                                 empty empty))))

(check-expect (remove-from-bst staff-bst2 4)
              (make-staff-node
                (make-staff-member 5 "Anne" "Defence")
                (make-staff-node
                 (make-staff-member 3 "Albert" "Engineering")
                 (make-staff-node
                  (make-staff-member 2 "Adam" "R&D") empty empty) empty)
                (make-staff-node
                 (make-staff-member 7 "Leslie" "R&D") empty
                 (make-staff-node
                  (make-staff-member
                   8 "Josh" "Engineering")
                  empty empty))))





(define (remove-from-bst staff-bst id-to-remove)
  (cond
    [(empty? staff-bst) empty]
    #|

    [(and (empty? (staff-node-left staff-bst))
          (empty? (staff-node-right staff-bst)))
     (make-staff-node
      (staff-node-staff staff-bst) empty empty)]
        
    [(empty? (staff-node-left staff-bst))
      (staff-node-right staff-bst)]
    |#
    
    [(> (staff-member-id (staff-node-staff staff-bst))
        id-to-remove)
     (make-staff-node
      (staff-node-staff staff-bst)
      (remove-from-bst (staff-node-left staff-bst) id-to-remove)
      (staff-node-right staff-bst))]
    
    [(< (staff-member-id (staff-node-staff staff-bst))
        id-to-remove)
     (make-staff-node
      (staff-node-staff staff-bst)
      (staff-node-left staff-bst)
      (remove-from-bst (staff-node-right staff-bst) id-to-remove))]

   [(empty? (staff-node-left staff-bst))
      (staff-node-right staff-bst)]
    
   [(= (staff-member-id (staff-node-staff staff-bst))
        id-to-remove)
     (make-staff-node (staff-node-staff (staff-node-left staff-bst))
                      (staff-node-left (staff-node-left staff-bst))
                      (staff-node-right staff-bst))]))

;;Sample BST for testing
(define staff-bst2 (create-staff-bst-from-list
                    (list staff2 staff3 staff8 staff7 staff5)))
(define staff-bst3 (create-staff-bst-from-list
                    (list staff2 staff6 staff3 staff8 staff7 staff5)))


;;TESTING
(check-expect (remove-from-bst staff-bst2 4)
              (make-staff-node
                (make-staff-member 5 "Anne" "Defence")
                (make-staff-node
                 (make-staff-member 3 "Albert" "Engineering")
                 (make-staff-node
                  (make-staff-member 2 "Adam" "R&D") empty empty) empty)
                (make-staff-node
                 (make-staff-member 7 "Leslie" "R&D") empty
                 (make-staff-node
                  (make-staff-member
                   8 "Josh" "Engineering")
                  empty empty))))

(check-expect (remove-from-bst staff-bst2 5)
              (make-staff-node
               (make-staff-member 3 "Albert" "Engineering")
               (make-staff-node (make-staff-member 2 "Adam" "R&D")
                                empty empty)
               (make-staff-node
                (make-staff-member 7 "Leslie" "R&D")
                empty (make-staff-node
                       (make-staff-member 8 "Josh" "Engineering")
                       empty empty))))
 
(check-expect (remove-from-bst staff-bst2 2)
             (make-staff-node
              (make-staff-member 5 "Anne" "Defence")
              (make-staff-node
               (make-staff-member 3 "Albert" "Engineering")
               empty empty)
              (make-staff-node
               (make-staff-member 7 "Leslie" "R&D")
               empty (make-staff-node
                      (make-staff-member 8 "Josh" "Engineering")
                      empty empty))))


(check-expect (remove-from-bst staff-bst3 6)
              (make-staff-node
               (make-staff-member 5 "Anne" "Defence")
               (make-staff-node
                (make-staff-member 3 "Albert" "Engineering")
                (make-staff-node (make-staff-member 2 "Adam" "R&D")
                                 empty empty) empty)
               (make-staff-node
                (make-staff-member 7 "Leslie" "R&D")
                empty
                (make-staff-node
                 (make-staff-member 8 "Josh" "Engineering") empty empty))))

(check-expect (remove-from-bst staff-bst3 7)
              (make-staff-node
               (make-staff-member 5 "Anne" "Defence")
               (make-staff-node (make-staff-member 3 "Albert" "Engineering")
                                (make-staff-node
                                 (make-staff-member 2 "Adam" "R&D")
                                 empty empty) empty)
               (make-staff-node (make-staff-member 6 "Suzy" "R&D")
                                empty
                                (make-staff-node
                                 (make-staff-member 8 "Josh" "Engineering")
                                 empty empty))))
              


(check-expect (remove-from-bst staff-bst3 8)
              (make-staff-node
               (make-staff-member 5 "Anne" "Defence")
               (make-staff-node
                (make-staff-member 3 "Albert" "Engineering")
                (make-staff-node (make-staff-member 2 "Adam" "R&D")
                                 empty empty) empty)
               (make-staff-node (make-staff-member 7 "Leslie" "R&D")
                                (make-staff-node
                                 (make-staff-member 6 "Suzy" "R&D")
                                 empty empty) empty)))



(check-expect (remove-from-bst staff-bst2 9)
              (make-staff-node
               (make-staff-member 5 "Anne" "Defence")
               (make-staff-node (make-staff-member 3 "Albert" "Engineering")
                                (make-staff-node
                                 (make-staff-member 2 "Adam" "R&D")
                                 empty empty) empty)
               (make-staff-node (make-staff-member 7 "Leslie" "R&D")
                                empty
                                (make-staff-node
                                 (make-staff-member 8 "Josh" "Engineering")
                                 empty empty))))

