;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname staff) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
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
;; requires: base, bonus ≥ 0

;; A Staff-List is a (listof Staff-Member)
;; requires: elements are sorted by increasing id
;; A Salary-List is a (listof Salary)
;;Sample data used in examples:
(define staff1 (make-staff-member 1 "John" "Engineering"))
(define staff2 (make-staff-member 2 "Adam" "R&D"))
(define staff3 (make-staff-member 3 "Albert" "Engineering"))
(define staff4 (make-staff-member 4 "Liz" "Finance"))
(define staff5 (make-staff-member 5 "Anne" "Defence"))
(define staff6 (make-staff-member 6 "Suzy" "R&D"))
(define staff7 (make-staff-member 7 "Leslie" "R&D"))
(define staff8 (make-staff-member 8 "Josh" "Engineering"))

(define salary1 (make-salary 1 50000 10))
(define salary2 (make-salary 2 74000 250))
(define salary3 (make-salary 3 85000 10))
(define salary4 (make-salary 4 60000 0))
(define salary5 (make-salary 5 93000 100))
(define salary6 (make-salary 6 68500 0))
(define salary7 (make-salary 7 250000 0))
(define salary8 (make-salary 8 120000 0))

(define staff-list (list staff1 staff2 staff3 staff4
                         staff5 staff6 staff7 staff8))
(define sal-list (list salary1 salary2 salary3 salary4
                       salary7 salary8))
(define dept-list (list "Engineering" "Defence" "R&D" "Management"))



;; PART A
(define (add-staff list-of-staff to-add)
  (cond
    [(empty? list-of-staff) (cons to-add empty)]
    [(= (staff-member-id (first list-of-staff))
        (staff-member-id to-add))
     (cons (first list-of-staff) (rest list-of-staff))]
    [(> (staff-member-id (first list-of-staff))
        (staff-member-id to-add))
     (cons to-add list-of-staff)]
    [else
     (cons (first list-of-staff)
           (add-staff (rest list-of-staff) to-add))]))

#|
(add-staff staff-list (make-staff-member 6.7 "Kesu" "Coolguy"))
(add-staff staff-list (make-staff-member 3 "Kesu" "Coolguy"))
(add-staff staff-list (make-staff-member 9 "Kesu" "Coolguy"))
|#


;;PART B

(define (update-staff-info list-of-staff to-add)
  (cond
    [(empty? list-of-staff) (cons to-add empty)]
    [(= (staff-member-id (first list-of-staff))
        (staff-member-id to-add))
     (cons to-add (rest list-of-staff))]
    [(> (staff-member-id (first list-of-staff))
        (staff-member-id to-add))
     (cons to-add list-of-staff)]
    [else
     (cons (first list-of-staff)
           (update-staff-info (rest list-of-staff) to-add))]))
#|
SOME TESTS
(update-staff-info
 staff-list (make-staff-member 6.7 "Kesu" "Coolguy"))
(update-staff-info staff-list (make-staff-member 3 "Kesu" "Coolguy"))
(update-staff-info staff-list (make-staff-member 9 "Kesu" "Coolguy"))
|#

;;HELPER
(define (info-maker staff)
  (string-append (number->string (staff-member-id staff))
                 " "
                 (staff-member-name staff)
                 " "
                 (staff-member-dept staff)))

;;PART C
(define (all-staff-info list-of-staff)
  (cond
    [(empty? list-of-staff) empty]
    [else (cons (info-maker (first list-of-staff))
                (all-staff-info (rest list-of-staff)))]))



(define (dept-counter list-of-staff dept)
 (cond
   [(empty? list-of-staff) 0]
   [(equal? (staff-member-dept (first list-of-staff)) dept)
    (+ 1 (dept-counter (rest list-of-staff) dept))]
   [else (dept-counter (rest list-of-staff) dept)]))


;;PART D
(define (count-staff-by-dept list-of-staff dept-list)
  (cond
    [(empty? dept-list) empty]
    [else
     (cons (dept-counter list-of-staff (first dept-list))
           (count-staff-by-dept list-of-staff (rest dept-list)))]))

;;(count-staff-by-dept (list staff1 staff2 staff3
;;                           staff4 staff5 staff6) dept-list)



;;AVERAGE DEPT SALARY GETTER
(define (avg-salary-by-dept list-of-staff sal-list dept-list)
  (cond
    [(empty? dept-list) empty]
   
    [(zero? (dept-counter list-of-staff (first dept-list)))
      (cons 0 (avg-salary-by-dept list-of-staff sal-list
                               (rest dept-list)))]
    [else
     (cons (/ (dept-total-salary sal-list list-of-staff
                                 (first dept-list))
              (dept-counter list-of-staff (first dept-list)))
           (avg-salary-by-dept list-of-staff sal-list
                                 (rest dept-list)))]))

;;COMPUTES TOTAL MONEY BEING SPENT ON A DEPT
(define (dept-total-salary sal-list list-of-staff dept)
  (cond
    [(empty? list-of-staff) 0]
    [(equal? (staff-member-dept (first list-of-staff)) dept)
     (+ (get-salary sal-list (staff-member-id (first list-of-staff)))
        (dept-total-salary sal-list (rest list-of-staff) dept))]  
    [else (dept-total-salary sal-list (rest list-of-staff) dept)]))

;;OBTAINS SALARY BY ID
(define (get-salary sal-list staff-id)
  (cond
    [(empty? sal-list) 0]
    [(= (salary-staff-id (first sal-list)) staff-id)
     (+ (salary-base (first sal-list))
        (salary-bonus (first sal-list)))]
    [else (get-salary (rest sal-list) staff-id)]))

(avg-salary-by-dept staff-list sal-list dept-list)