;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname cards) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;;****************************************************
;;    Keshav Bagla, student number-20603369        
;;    CS 135, Fall 2015
;;    Assingment 4, Question 4                     
;;****************************************************



;;PART A
(define-struct card(strength speed intelligence charm))
;; A card is a (make-card Nat Nat Nat Nat)
;;Requires: all Nat > 0



;;PART B
#|
(card-to-list card-input) converts a card into a list.
card-to-list: Card -> (listof Nat)
|#
;;Examples
(check-expect (card-to-list(make-card 3 5 1 1))
              (cons 3 (cons 5 (cons 1 (cons 1 empty)))))
(check-expect (card-to-list(make-card 1 1 1 7))
              (cons 1 (cons 1 (cons 1 (cons 7 empty)))))

(define (card-to-list card-input)
  
  (cons (card-strength card-input)
  (cons (card-speed card-input)
  (cons(card-intelligence card-input)
  (cons (card-charm card-input) empty)))))

;;TESTS
(check-expect (card-to-list(make-card 3 5 1 1))
              (cons 3 (cons 5 (cons 1 (cons 1 empty)))))
(check-expect (card-to-list(make-card 1 2 6 1))
              (cons 1 (cons 2 (cons 6 (cons 1 empty)))))
(check-expect (card-to-list(make-card 3 3 3 1))
              (cons 3 (cons 3 (cons 3 (cons 1 empty)))))


#|
(list-to-card list-input) converts a list of 4 positive natural numbers
       into a card as defined above.

list-to-card: (listof Nat) -> Card
requires: (length (listof Nat)) = 4 and all Nat>0
|#
;;Examples
(check-expect (list-to-card (cons 1 (cons 1(cons 1 (cons 7 empty)))))
              (make-card 1 1 1 7))
(check-expect (list-to-card (cons 2 (cons 2 (cons 2 (cons 4 empty)))))
              (make-card 2 2 2 4))

(define (list-to-card list-input)
  (make-card
   (first list-input)
   (first (rest list-input))
   (first (rest (rest list-input)))
   (first (rest (rest (rest list-input))))))

;;TESTS
(check-expect (list-to-card (cons 3 (cons 3 (cons 3 (cons 1 empty)))))
              (make-card 3 3 3 1))
(check-expect (list-to-card (cons 1 (cons 2 (cons 3 (cons 4 empty)))))
              (make-card 1 2 3 4))
(check-expect (list-to-card (cons 3 (cons 2 (cons 2 (cons 3 empty)))))
              (make-card 3 2 2 3))







(define regular-card 10)

;;PART C
#|
(card-regular? card-to-test) checks to see if all the attributes of the
    card add up to a normal amount as defined by regular-card
card-regular?: Card -> Bool
|#
;;Examples
(check-expect (card-regular? (make-card 3 2 4 1)) true)
(check-expect (card-regular? (make-card 1 1 1 7)) true)

(define (card-regular? card-to-test)
  (= regular-card (add-qualities card-to-test)))
;;TESTS
(check-expect (card-regular? (make-card 1 2 3 4)) true)
(check-expect (card-regular? (make-card 1 4 1 7)) false) ;; Too much
(check-expect (card-regular? (make-card 1 1 1 3)) false) ;; Too less





;PART D
#|
(card-battle your-card opponent-card) compares your card with the
     opponents card, and outputs either Draw, Win, or Lose after
     comparing all the cards attributes
card-battle: Card Card -> Symb
|#
;;Examples
(check-expect (card-battle (make-card 4 2 1 3)
                           (make-card 1 1 1 7)) 'win)
(check-expect (card-battle (make-card 1 2 1 3)
                           (make-card 1 1 1 7)) 'draw)



(define (card-battle your-card opponent-card)
  (cond
    [(= (compare-two-card
         (card-to-list your-card)
         (card-to-list opponent-card)) 

        (compare-two-card
         (card-to-list opponent-card)
         (card-to-list your-card)))
     'draw]
    
    [(> (compare-two-card
         (card-to-list your-card)
         (card-to-list opponent-card))

        (compare-two-card
         (card-to-list opponent-card)
         (card-to-list your-card)))
     'win]

    
    [(< (compare-two-card
         (card-to-list your-card)
         (card-to-list opponent-card))

        (compare-two-card
         (card-to-list opponent-card)
         (card-to-list your-card)))
     'lose]))


;;TESTS
(check-expect (card-battle (make-card 2 2 3 2)
                           (make-card 2 1 2 5)) 'Win)
(check-expect (card-battle (make-card 4 2 3 1)
                           (make-card 1 3 1 5)) 'Draw)
(check-expect (card-battle (make-card 1 1 1 7)
                           (make-card 3 3 3 1)) 'Lose)












;;HELPER
#|
(compare-two-card card1 card2) consumes two cards in the form of a list
          and determines the number of attribtutes of card1 that are
          better than card2

compare-two-card: (listof Nat) (listof Nat) -> Nat
Requires: (length (listof Nat)) = 4) and all Nat>0
|#
;;Examples
(check-expect(compare-two-card (card-to-list (make-card 5 2 1 2))
                               (card-to-list (make-card 3 3 3 1))) 2) 
(check-expect(compare-two-card (card-to-list (make-card 6 2 1 2))
                               (card-to-list (make-card 1 2 1 6))) 1)


(define (compare-two-card card1 card2)
(cond
  [(empty? card1) 0]
  [(> (first card1) (first card2))
      (+ 1 (compare-two-card (rest card1) (rest card2)))]
  [else (compare-two-card (rest card1) (rest card2))]))

;;TESTS
(check-expect(compare-two-card (card-to-list (make-card 3 3 3 1))
                               (card-to-list (make-card 1 2 1 6))) 3)
(check-expect(compare-two-card (card-to-list (make-card 1 2 3 4))
                               (card-to-list (make-card 4 2 3 1))) 1)
(check-expect(compare-two-card (card-to-list (make-card 1 6 2 1))
                               (card-to-list (make-card 1 2 1 6))) 2)






(define sample-cards (cons (make-card 1 1 1 7)
                     (cons (make-card 3 3 3 1)
                     (cons (make-card 2 2 2 4)
                     (cons (make-card 1 2 3 4) empty)))))


;;PART E
#|
(card-select list-of-cards opponent-card) consumes a list of cards
     and the opponents card and returns the first from the list
     that will beat the opponents card.

card-select: (listof Card) Card -> (anyof Card Bool)
|#
;;EXAMPLES
(check-expect (card-select sample-cards (make-card 3 3 1 3)) false)
(check-expect (card-select sample-cards (make-card 1 1 1 7))
              (make-card 3 3 3 1))

(define (card-select list-of-cards opponent-card)
  (cond
    
    [(empty? list-of-cards) false]
    
    [(> (compare-two-card (card-to-list (first list-of-cards))
                          (card-to-list opponent-card))
        (compare-two-card (card-to-list opponent-card)
                          (card-to-list (first list-of-cards))))
     (first list-of-cards)]

    [else (card-select (rest list-of-cards) opponent-card)]))

;; TESTS
(check-expect (card-select sample-cards (make-card 3 3 1 3)) false)
(check-expect (card-select sample-cards (make-card 1 1 5 3))
              (make-card 2 2 2 4))
(check-expect (card-select sample-cards (make-card 3 2 2 3))
              (make-card 3 3 3 1))




;;HELPER
#|
(add-qualities card-to-add) adds all the attributes of a card.

add-qualities: Card -> Nat
|#
;;Examples
(check-expect (add-qualities (make-card 3 2 2 1)) 8)
(check-expect (add-qualities (make-card 0 0 0 0)) 0)

(define (add-qualities card-to-add)
  (+ (card-strength card-to-add)
     (card-speed card-to-add)
     (card-intelligence card-to-add)
     (card-charm card-to-add)))

;;TESTS
(check-expect (add-qualities (make-card 3 1 5 3)) 12)
(check-expect (add-qualities (make-card 1 2 2 5)) 10)
(check-expect (add-qualities (make-card 2 2 2 4)) 10)


;; PART F
(define collector-min 3)
#|
(count-collector-cards list-of-cards) consumes a list of cards and
   determines how many of those cards are collector cards, i.e, the
   sum of their attributes is greater than collector-min but less than
   collector-max.

count-collector-cards: (listof Card) -> Nat
|#
;;Examples
(check-expect (count-collector-cards
               (cons (make-card 3 2 5 1)
                (cons (make-card 4 2 3 1)
                  (cons (make-card 2 2 2 4)
                   (cons (make-card 1 1 1 9) empty))))) 2)
(check-expect (count-collector-cards
               (cons (make-card 3 2 4 1)
                (cons (make-card 4 2 3 1)
                  (cons (make-card 2 2 2 4)
                   (cons (make-card 1 1 1 7) empty))))) 0)


(define (count-collector-cards list-of-cards)
  (cond
    
    [(empty? list-of-cards) 0]
    
    [(and (not (= (add-qualities (first list-of-cards)) regular-card))
          (> (add-qualities (first list-of-cards)) collector-min))
     (+ 1 (count-collector-cards (rest list-of-cards)))]
    
    [else (count-collector-cards (rest list-of-cards))]))


;;TESTS
(check-expect (count-collector-cards
               (cons (make-card 3 2 4 1)
                (cons (make-card 4 2 3 1)
                 (cons (make-card 2 2 2 4)
                  (cons (make-card 1 1 1 7) empty))))) 0)
(check-expect (count-collector-cards
               (cons (make-card 3 2 4 1)
                (cons (make-card 1 1 1 1)
                 (cons (make-card 2 4 1 1)
                  (cons (make-card 5 1 1 3) empty))))) 2)
(check-expect (count-collector-cards
               (cons (make-card 4 5 1 5)
                (cons (make-card 6 4 2 1)
                 (cons (make-card  3 3 3 5)
                  (cons (make-card 1 5 1 2) empty))))) 4)
(check-expect (count-collector-cards
               (cons (make-card 0 1 1 1)
                (cons (make-card 5 4 2 1)
                 (cons (make-card 5 5 2 1)
                  (cons (make-card 8 1 9 1) empty))))) 3)
