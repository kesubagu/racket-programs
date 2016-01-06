;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname email) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor mixed-fraction #f #t none #f () #t)))
(define-struct email-record (day-id hours-worked emails-sent))
;; An Email-Record is a (make-email-record Nat Num Nat)
;; requires: hours-worked >= 0

(define-struct daily-stats (staff-id staff-name emails))
;; A Daily-Stats is a (make-daily-stats Nat Str (listof Email-Record))
;; requires: each day-id in emails is unique

;;Examples
(define stats1 (list (make-email-record 1 7 20) (make-email-record 2 8 50)
                     (make-email-record 3 6 30) (make-email-record 4 8 100)
                     (make-email-record 5 7 50)))

(define stats2 (list (make-email-record 1 10 80) (make-email-record 2 5 30)
                     (make-email-record 3 7 30) (make-email-record 4 7 80)
                     (make-email-record 5 12 80)))

(define stats3 (list (make-email-record 4 6 80)))
(define stats4 (list (make-email-record 4 6 22) (make-email-record 5 6 22)
                     (make-email-record 4 6 23)))

(define em1 (make-daily-stats 1 "Justin" empty))
(define em2 (make-daily-stats 2 "Tolu" stats1))
(define em3 (make-daily-stats 10 "Liz" stats2))



;;PART A
#|
(avg-emails stat) consumes a Daily-Stat and produces the average
  number of emails sent per hour by the respective staff member.

avg-emails: Daily-Stats -> Num
|#
;;Examples:
(check-expect (avg-emails em3) 300/41)
(check-expect (avg-emails em2) 250/36)

(define (avg-emails stat)
  (local
    #|
     (hours-total email-rec) consumes a list of email records
        and calculates the sum of all the hours worked.

     highest-emails email-recs:
        (listof Email-Records) -> Num 
   |#
   [(define (hours-total email-rec)
       (cond
         [(empty? email-rec) 0]
         [else (+ (email-record-hours-worked (first email-rec))
                  (hours-total (rest email-rec)))]))
    
     #|
     (email-total email-rec) consumes a list of email records
        and calculates the sum of all emails sent.

     highest-emails email-recs:
        (listof Email-Records) -> Nat 
     |#
    (define (email-total email-rec)
       (cond
         [(empty? email-rec) 0] 
         [else (+ (email-record-emails-sent (first email-rec))
                  (email-total (rest email-rec)))]))]
    (cond
      [(empty? (daily-stats-emails stat)) 0]
      [else (/ (email-total (daily-stats-emails stat))
       (hours-total (daily-stats-emails stat)))])))

;;TESTS
(check-expect (avg-emails em1) 0)
(check-expect (avg-emails em2) 250/36)
(check-expect (avg-emails em3) 300/41)
(check-expect (avg-emails (make-daily-stats 11 "Keshav" stats3)) 80/6)



;;PART B
#|
(highest-email-record stat) consumes a daily stat and produces
  a list of email records which contains the email-recs
  on which the highest number of emails were sent.

highest-email-record: Daily-Stats -> (listof Email-Record)
|#
;;Examples:
(check-expect (highest-email-record em2)
              (list (make-email-record 4 8 100)))
(check-expect (highest-email-record (make-daily-stats 11 "Keshav" stats4))
              (list (make-email-record 4 6 23)))

(define (highest-email-record stat)
  (local
    [#|
     (highest-emails email-recs highest-so-far) consumes a list of
       emails and finds the largest number of emails sent.

     highest-emails email-recs:
        (listof Email-Records) -> Nat  
     |#
     (define (highest-emails email-recs highest-so-far)
       (cond
         [(empty? email-recs) highest-so-far]
         [(> (email-record-emails-sent (first email-recs))
             highest-so-far)
          (highest-emails (rest email-recs)
                          (email-record-emails-sent (first email-recs)))]
         [else (highest-emails (rest email-recs) highest-so-far)]))

     #|
     (highest-constructor email-rec upper-limit) consumes a list of
       email records and the largest number of emails sent in that set
       and produces a new list that contains the email records
       which has the highest number of emails.

     highest-emails email-recs:
        (listof Email-Records) -> (listof Email-Records)  
     |#
     (define (highest-constructor email-rec upper-limit)
       (cond
         [(empty? email-rec) empty]
         [(= upper-limit (email-record-emails-sent (first email-rec)))
          (cons (first email-rec)
                (highest-constructor (rest email-rec) upper-limit))]
         [else (highest-constructor (rest email-rec) upper-limit)]))]

    (highest-constructor (daily-stats-emails stat)
                         (highest-emails (daily-stats-emails stat) 0))))

;;TESTS
(check-expect (highest-email-record em2)
              (list (make-email-record 4 8 100)))
(check-expect (highest-email-record em3)
              (list (make-email-record 1 10 80)
                    (make-email-record 4 7 80)
                    (make-email-record 5 12 80)))
(check-expect (highest-email-record (make-daily-stats 11 "Keshav" stats3))
              (list (make-email-record 4 6 80)))
(check-expect (highest-email-record (make-daily-stats 11 "Keshav" empty))
              empty)









