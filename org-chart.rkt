;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname org-chart) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;;****************************************************
;;    Keshav Bagla, student number-20603369        
;;    CS 135, Fall 2015
;;    Assingment 7, Question 3                
;;****************************************************




(define-struct supervisor (id subordinates))
;; A Supervisor is a (make-supervisor Nat (listof Org-Chart))
;; requires: id values are unique


;; An Org-Chart is one of:
;;* Nat
;;* Supervisor

;;SAMPLE ORG-CHART
(define sample
 (make-supervisor 6 (list (make-supervisor 14 (list
                                              (make-supervisor 121
                                                               '(214 241 217))
                                              1 3))
                          13 41 512
                         (make-supervisor 15
                                         (list (make-supervisor 156
                                                                (list 22 21 23))
                                                   12 65)))))
(define sample2
 (make-supervisor 6 (list 4 5 (make-supervisor 89 (list
                                                   (make-supervisor
                                                    52'(231 2121 531))
                                                   (make-supervisor
                                                    41 '(24 56 773))))
                          (make-supervisor 87 (list 24 22 1 51)))))

#|
(list-maker subs-list) consumes a listof org-chart and produces
   list numbers, that correspond to the ID's of all employees
   that were included in the listof org-chart.

list-make: (listof Org-Chart) -> (listof Num)
|#
;;Examples
(check-expect (list-maker (list 2 5 (make-supervisor 54 '(28 300 47))
                                (make-supervisor 4 '(22 62))))
              (list 2 5 54 4 28 300 47 22 62))
(check-expect (list-maker '(89 43 33.5)) (list 89 43 33.5))


(define (list-maker subs-list)
  (cond
    [(empty? subs-list) empty]
    [(number? (first subs-list))
     (cons (first subs-list)
           (list-maker (rest subs-list)))]
    [(supervisor? (first subs-list))
     (list-maker (super-maker (rest subs-list)
                              (first subs-list)))]))
;;TESTS

(check-expect (list-maker empty) empty)
(check-expect (list-maker '(2 5 1 24)) '(2 5 1 24))
(check-expect (list-maker (list 2 5 1 (make-supervisor 4 '(22 62))))
              (list 2 5 1 4 22 62))
(check-expect (list-maker (list 2 5 1
                                (make-supervisor 4
                                                 (list
                                                  (make-supervisor 8 '(42 27))
                                                           62))))
              (list 2 5 1 4 8 62 42 27))




#|
(super-maker rem-list supervisor) consumes a partial list and
   and supervisor, and the function adds, the superviser ID
   to the list and appends the supervisers subordinates
   to the end of the list. The function occurs in mutual 

super-maker: (listof Org-Chart) Supervisor -> (listof Org-Chart)
|#
;;EXAMPLES
(check-expect (super-maker (list 11 421 5) (make-supervisor 92 '(32 61 1)))
              (list 92 11 421 5 32 61 1))
(check-expect (super-maker empty
                          (make-supervisor 6 (list (make-supervisor 8 '(41 51))
                                                   2 5)))
              (list 6 8 2 5 41 51))


;;Definiton
(define (super-maker rem-list supervisor)
  (append (cons (supervisor-id supervisor) rem-list)
          (list-maker (supervisor-subordinates supervisor))))


;;TESTS
(check-expect (super-maker (list 8 421 21) (make-supervisor 7 '(32 41 1)))
              (list 7 8 421 21 32 41 1))

(check-expect
 (super-maker (list 8 421 21) (make-supervisor 7 (list
                                                 (make-supervisor 72 '(4 22 5))
                                                  41 1)))
 (list 7 8 421 21 72 41 1 4 22 5))

(check-expect (super-maker empty (make-supervisor 6 '(4 2 5)))
              (list 6 4 2 5))



#|
(search-subs sub-list super-id) consumes a list of subordinates
  and a superiors ID, and uses direct-reports function to search
  for the given superiors ID in list of subordinates. If a subordinate
  is a supervisor, then their subordintes are also checked for a matching ID.

search-subs: (listof Org-Chart) -> (listof Num)
|#
;;Examples
(check-expect (search-subs (list (make-supervisor 41
                                                  (list
                                                   (make-supervisor 2
                                                                    '(5 7 9))
                                                   42)) 51) 2)
              (list 5 7 9))

(check-expect (search-subs (list (make-supervisor 3 '(1 2)) 21 51) 3)
              (list 1 2))


;;Definition
(define (search-subs sub-list super-id)
  (cond
    [(empty? sub-list) empty]
    [(not (empty? (direct-reports (first sub-list) super-id)))
     (direct-reports (first sub-list) super-id)]
    [else (search-subs (rest sub-list) super-id)]))

(check-expect (search-subs '(5 21 51) 5) empty)
(check-expect (search-subs '(5 21 51) 65) empty)
(check-expect (search-subs (list (make-supervisor 41 '(25 42)) 21 51) 41)
              (list 25 42))

(check-expect (search-subs (list (make-supervisor 41
                                                  (list
                                                   (make-supervisor 82
                                                                    '(4 12 5))
                                                   42)) 51) 82)
              (list 4 12 5))

(check-expect (search-subs (list (make-supervisor 41
                                                  (list
                                                   (make-supervisor 82
                                                                    '(4 12 5))
                                                   42)) 51) 41)
              (list 82 42 4 12 5))






#|
(direct-reports org-chart super-id) consumes an Org-chart and an
  employee id, and produces a list of all employees supervised
  directly or indirectly by the given ID. If the employee does
  not supervise anyone or does not exist the function produces empty

direct-reports: Org-Chart -> (listof Num)
|#
;;Examples
(check-expect (direct-reports sample2 52)
              (list 231 2121 531))
(check-expect (direct-reports sample2 89)
              (list 52 41 231 2121 531 24 56 773))

;;Definition
(define (direct-reports org-chart super-id)
  (cond
    [(number? org-chart) empty]
    [(and (supervisor? org-chart)(= (supervisor-id org-chart) super-id))
     (list-maker (supervisor-subordinates org-chart))]
    [else (search-subs (supervisor-subordinates org-chart) super-id)]))

;;Tests
(check-expect (direct-reports sample 6)
              (list 14 13 41 512 15 121 1 3 214 241 217 156 12 65 22 21 23))

(check-expect (direct-reports sample 15)
              (list 156 12 65 22 21 23))

(check-expect (direct-reports sample2 4) empty) ;;No subs
(check-expect (direct-reports sample2 23) empty) ;;Leaf node
(check-expect (direct-reports sample2 881) empty) ;;Doesnt exist






#|
(vacation-approval org-chart approve-id) consumes an Org-chart and an
  employee id, a produces a list employee ID, that indirectly or directly
  supervise the given ID. If the ID isn't supervised by anyone or doesnt
  exist, an empty list is produced.

direct-reports: Org-Chart -> (listof Num)
|#
;;Examples
(check-expect (vacation-approval sample2 4) (list 6))
(check-expect (vacation-approval sample2 2121)(list 6 89 52))

(define (vacation-approval org-chart approve-id)
  (cond
    [(number? org-chart) empty]
    [(not (member? approve-id
                   (direct-reports org-chart (supervisor-id org-chart)))) empty]
    [(member? approve-id (direct-reports org-chart (supervisor-id org-chart)))
     (cons (supervisor-id org-chart)
           (vacation-lister (supervisor-subordinates org-chart)
                            approve-id))]))

;;Tests
(check-expect (vacation-approval sample 6) empty)
(check-expect (vacation-approval sample 15) (list 6))
(check-expect (vacation-approval sample 121) (list 6 14))
(check-expect (vacation-approval sample 241) (list 6 14 121))



#|
(search-subs sub-list super-id) consumes a list of subordinates
  and a employee ID, and uses vacation-approval function to search
  for a superior who supervises the given ID.

search-subs: (listof Org-Chart) -> (listof Num)
|#
;:Examples
(check-expect (vacation-lister (supervisor-subordinates sample) 15) empty)
(check-expect (vacation-lister (cons sample empty) 15) (list 6))

(define (vacation-lister subs-list approve-id)
  (cond
    [(empty? subs-list) empty]
    [(not (empty? (vacation-approval (first subs-list) approve-id)))
     (vacation-approval (first subs-list) approve-id)]
    [else (vacation-lister (rest subs-list) approve-id)]))

;;TESTS
(check-expect (vacation-lister (list 9 4 1 6) 6) empty)
(check-expect (vacation-lister (list 9 4 1 6) 22) empty)
(check-expect (vacation-lister (list (make-supervisor 24 '(5 1 51 6)) 4 1) 6)
              (list 24))

(check-expect (vacation-lister (list 8
                                     (make-supervisor 24
                                                      (list 5 1
                                                            (make-supervisor
                                                             81 '(5 21 60)) 6))
                                     61)
                               60)
              (list 24 81))


