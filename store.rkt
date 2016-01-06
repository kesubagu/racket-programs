;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname store) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;;****************************************************
;;    Keshav Bagla, student number-20603369        
;;    CS 135, Fall 2015
;;    Assingment 5, Question 2                   
;;****************************************************


(define-struct product(name price taxable?))
;;A product is a (make-product Symb Num Bool)

;;Example list of products used for testing.
(define inventory (list (make-product 'Mouse 19.50 true)
                        (make-product 'Wire 2.99 false)
                        (make-product 'Doughnut 5.59 false)
                        (make-product 'Comp 1040.00 true)
                        (make-product 'Cards 32.50 true)
                        (make-product 'Rocket 365.50 true)
                        (make-product 'Textbook 199.50 true)
                        (make-product 'CS-Pens 1.50 true)
                        (make-product 'Notebooks 9.99 true)
                        (make-product 'TShirt 7.50 true)))


;;PART A
#|
(applicant-score applicant-skill employer) consumes a a product name
     in the form of a symbol and a list of products and produces
     true if the product is on the list.
have-product?: Sym (listof Products) -> Bool
|#
;;Examples
(check-expect (have-product? 'CS-Pens inventory) true)
(check-expect (have-product? 'Comp inventory) true)

(define (have-product? product list-of-products)
  (cond
    [(empty? list-of-products) false]
    [(symbol=? (product-name (first list-of-products))
               product) true]
    [else (have-product? product (rest list-of-products))]))

;;TESTS
(check-expect (have-product? 'Comp inventory) true)
(check-expect (have-product? 'Wire inventory) true)
(check-expect (have-product? 'Arts-shirt inventory) false)
(check-expect (have-product? 'Cat inventory) false)
(check-expect (have-product? 'Doughnut empty) false)






;;PART B
#|
(valid-order? list-of-symbols list-of-products) consumes the names of
    a list of desired products, and list of available products and
    produces true if all the desired products are available, and false
    otherwise.

valid-order?: (listof Symb) (listof Product) -> Bool
|#
;;Examples
(check-expect (valid-order? (list 'Wire 'Textbook 'Comp 'Rocket)
                            inventory) true)
(check-expect (valid-order? (list 'Cards 'CS-Pens 'Notebooks 'Mouse)
                            inventory) true)

(define (valid-order? list-of-symbols list-of-products)
  (cond
    [(empty? list-of-symbols) true]
    [(and (have-product? (first list-of-symbols) list-of-products)
          (valid-order? (rest list-of-symbols) list-of-products)) true]
    [else false]))

;;TESTS
(check-expect (valid-order? (list 'Cards 'CS-Pens 'Notebooks 'Mouse)
                            inventory) true)
(check-expect (valid-order? (list 'Cards 'CS-Pens 'Notebooks 'Mouse)
                            inventory) true)
(check-expect (valid-order? (list 'Cat 'CS-Pens 'Notebooks 'Mouse)
                            inventory) false) ;; 1 Items doesnt exist
(check-expect (valid-order? (list 'Cat 'Crayons 'Paint 'Canvas)
                            inventory) false) ;; No items exist
(check-expect (valid-order? empty
                            inventory) true) ;; Empty order is valid




;;PART C
(define tax-rate 0.13)
#|
(after-tax product-price) consumes a product price and computes the
   new price after tax is added to the product.

after-tax: Num -> Num
|#
;;Examples
(check-expect (after-tax 100) 113)
(check-expect (after-tax 53) 59.89)

;;DEFINITION
(define (after-tax product-price)
  (+ (* tax-rate product-price) product-price))

;;TESTS
(check-expect (after-tax 0) 0)
(check-expect (after-tax 25) 28.25)
(check-expect (after-tax 100) 113)


#|
(budget-items list-of-products budget) consumes a list of products
    and a budget, and produces another list of items, which are products
    on the list and within the budget, after calculating tax, if
    applicable.
budget-items: (listof Product) Num -> (listof Product)
|#
;; Examples
(check-expect (budget-items inventory 300)
              (list (make-product 'Mouse 19.50 true)
                    (make-product 'Wire 2.99 false)
                    (make-product 'Doughnut 5.59 false)
                    (make-product 'Cards 32.50 true)
                    (make-product 'Textbook 199.5 true)
                    (make-product 'CS-Pens 1.50 true)
                    (make-product 'Notebooks 9.99 true)
                    (make-product 'TShirt 7.50 true)))


;;DEFINITION
(define (budget-items list-of-products budget)
  (cond
    [(empty? list-of-products) empty]
 
    [(product-taxable? (first list-of-products))
      (cond
        [(<= (after-tax (product-price (first list-of-products)))
             budget)
         (cons (first list-of-products)
               (budget-items (rest list-of-products) budget))]
        [else (budget-items (rest list-of-products) budget)])]
    
    [else
     (cond
       [(<= (product-price (first list-of-products)) budget)
        (cons (first list-of-products)
              (budget-items (rest list-of-products) budget))]
       [else (budget-items (rest list-of-products) budget)])]))

;; TESTS
(check-expect (budget-items inventory 0) empty) ;; Nothing within budget

(check-expect (budget-items inventory 20) ;;Part of list within budget
              (list (make-product 'Wire 2.99 false)
                    (make-product 'Doughnut 5.59 false)
                    (make-product 'CS-Pens 1.50 true)
                    (make-product 'Notebooks 9.99 true)
                    (make-product 'TShirt 7.50 true)))

(check-expect (budget-items inventory 2000) ;; Whole list within budget
              (list (make-product 'Mouse 19.50 true)
                        (make-product 'Wire 2.99 false)
                        (make-product 'Doughnut 5.59 false)
                        (make-product 'Comp 1040.00 true)
                        (make-product 'Cards 32.50 true)
                        (make-product 'Rocket 365.50 true)
                        (make-product 'Textbook 199.50 true)
                        (make-product 'CS-Pens 1.50 true)
                        (make-product 'Notebooks 9.99 true)
                        (make-product 'TShirt 7.50 true))) 

 

#|
(order-sum order-list list-of-products) consumes a list of products
    and seperates each symbol from the list and invokes the find-and-add
    function.
order-sum: (lisof Symb) (listof Product) -> Num
|#
;;Examples
(check-expect (order-sum (list 'Comp 'Mouse) inventory) 1197.235)
(check-expect (order-sum (list 'Wire) inventory) 2.99)

;;DEFINITION
(define (order-sum order-list list-of-products)
  (cond
    [(empty? order-list) 0]
    [else (+ (find-and-add (first order-list) list-of-products)
             (order-sum (rest order-list) list-of-products))]))

(check-expect (order-sum (list 'Rocket) inventory) 413.015)
(check-expect (order-sum (list 'Cards 'Textbook 'TShirt)
                         inventory) 270.635)
(check-expect (order-sum empty inventory) 0)







#|
(find-and-add product list-of-products) looks for a symbol in a list
   and if it is there, it adds the price, with tax if applicable
   and continues looking for another recurrence of the same symbol.

find-and-add: Symb (listof Product) -> Num
|#
;;Examples
(check-expect (find-and-add 'TShirt inventory) 8.475)
(check-expect (find-and-add 'Rocket inventory) 413.015)

;DEFINITION
(define (find-and-add product list-of-products)
  (cond
    [(empty? list-of-products) 0]
    [(symbol=? product (product-name (first list-of-products)))
     (cond
         [(product-taxable? (first list-of-products))
          (+ (after-tax(product-price (first list-of-products)))
             (find-and-add product (rest list-of-products)))]
         
         [(not(product-taxable? (first list-of-products)))
          (+ (product-price (first list-of-products))
             (find-and-add product (rest list-of-products)))])]
    
    [else (find-and-add product (rest list-of-products))]))

;;TESTS
(check-expect (find-and-add 'TShirt inventory) 8.475)
(check-expect (find-and-add 'Cat inventory) 0);; Product doesnt exist
(check-expect (find-and-add 'Comp inventory) 1175.2)




#|
(round-off to-round) round a number down to two decimal places.

round-off: Num -> Num
|#
;;Examples
(check-expect (round-off 24.2588) 24.25)
(check-expect (round-off 0.2588) 0.25)

(define (round-off to-round)
 (/ (floor (* to-round 100)) 100))

;;TESTS
(check-expect (round-off 24.28999) 24.28)
(check-expect (round-off 0) 0)
(check-expect (round-off 1082.999) 1082.99)






#|
(total-order order-list list-of-products) produces the total price of
   an order after rounding own. If the order is invalid
   then it produces 'cannot-fulfill

total-order: (listof Symb) (listof) -> Num
|#
;;Examples
(check-expect (total-order (list 'Comp 'CS-Pens 'Mouse 'TShirt)
                           inventory) 1207.4)
(check-expect (total-order (list 'Comp 'Textbook 'Notebooks 'TShirt)
                           inventory) 1420.39)


;;DEFINITION
(define (total-order order-list list-of-products)
  (cond
    [(valid-order? order-list list-of-products)
     (round-off (order-sum order-list list-of-products))]
    [else 'cannot-fulfill]))

;;TESTS
(check-expect (total-order empty inventory) 0)

(check-expect
    (total-order (list 'Comp 'CS-Pens 'Mouse 'TShirt 'Rocket 'Doughnut)
                  inventory) 1626.01)
(check-expect
  (total-order (list 'Cat 'CS-Pens 'Comp 'TShirt 'Rocket 'Doughnut)
               inventory) 'cannot-fulfill)

(check-expect (total-order (list 'Comp 'Textbook 'Notebooks 'TShirt)
                           inventory) 1420.39)



