;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname puzzle) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The following line is REQUIRED (do not remove)
(require "puzlib.rkt")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Place your Personal Identification here
;;****************************************************
;;    Keshav Bagla, student number-20603369        
;;    CS 135, Fall 2015
;;    Assingment 10, Question 2                     
;;****************************************************

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; DATA DEFINITIONS

;; A Puzzle is a (list (listof String) (listof String))

;; A Grid is a (listof (listof Char))

(define-struct wpos (row col horiz? len))
;; A WPos (Word Position) is a (make-wpos Nat Nat Bool Nat)
;; requires: len > 1

(define-struct state (grid positions words))
;; A State is a (make-state Grid (listof WPos) (listof Str))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; CONSTANTS FOR TESTING:

(define puzz01 (read-puzzle "puzzle01.txt"))
(define grid-abc '((#\A #\B #\C) (#\X #\Y #\Z)))
(define grid1 (map string->list '("###.##" "..####" "##..##")))
(define test-puzzle2 '(("#..###..####" "###..####...")
                       ("Cat" "Pants" "Dog" "Farm")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; PROVIDED HELPER:

;; (flip wp) transposes wp by reversing row/col and negating horiz?
;; flip: WPos -> WPos
;; Example:
(check-expect (flip (make-wpos 3 4 true 5))
              (make-wpos 4 3 false 5))

(define (flip wp)
  (make-wpos (wpos-col wp) (wpos-row wp) (not (wpos-horiz? wp)) (wpos-len wp)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; REQUIRED FUNCTIONS:

#|
(transpose g) consumes  grid g and converts the colums into rows.

transpose: Grid -> Grid
|#
;; Examples:
(check-expect (transpose grid-abc) '((#\A #\X) (#\B #\Y) (#\C #\Z)))
(check-expect (transpose '((#\a #\v) (#\b #\g)))
              '((#\a #\b) (#\v #\g)))

(define (transpose g)
  (cond
    [(empty? g) empty]
    [(empty? (first g)) empty]
    [else
     (cons (foldr (lambda (ele base) (cons (first ele) base)) empty g)
           (transpose (map rest g)))]))

;; Tests:
(check-expect (transpose empty) empty)
(check-expect (transpose '((#\A #\B))) '((#\A) (#\B)))
(check-expect (transpose '((#\A #\B #\C) (#\C #\D #\E) (#\F #\G #\I)))
              '((#\A #\C #\F) (#\B #\D #\G) (#\C #\E #\I)))
(check-expect (transpose '((#\A) (#\B) (#\C))) '((#\A #\B #\C)))




                                                 

#|
(find-wpos loc row) consumes a list of characters (loc) and a row number (row)
   and produces a list of all possible wpos that could arise from the list
   of characters, and excludes any Wpos that has a length of 1.

find-wpos: (listof Char) Nat -> (listof WPos)
|#
;; Examples:
(check-expect (find-wpos (string->list "####") 0)
              (list (make-wpos 0 0 true 4)))
(check-expect (find-wpos (string->list "####...####") 0)
              (list (make-wpos 0 0 true 4) (make-wpos 0 7 true 4)))

(define (find-wpos loc row)
  (local
    [#|
     (counter loc col-pos row-pos) consumes a list of character (loc) and column
        number (col-pos) and a row number (row-pos) and uses the information
        to find and produce a list of Wpos

      counter: (listof Char) Nat Nat -> (listof WPos)
     |#
     (define (counter loc col-pos row-pos)
       (cond
         [(empty? loc) empty]
         [(equal? (first loc) empty-cell)
          (cons (make-wpos row-pos col-pos true (count-empties loc))
                (counter (list-skip loc (count-empties loc))
                         (+ col-pos (count-empties loc)) row-pos))]
         [else (counter (rest loc) (add1 col-pos) row-pos)]))

       #|
        (count-empties lst) consumes a list and produces the number of
           consecutive empty cells from the starting position.

        count-empties: (listof Char) -> Nat
       |#
       (define (count-empties lst)
         (cond
           [(empty? lst) 0]
           [(equal? (first lst) empty-cell)
            (+ 1 (count-empties (rest lst)))]
           [else 0]))

       #|
        (list-skip lst pos-to-skip) consumes a list (lst) and position
          (pos-to-skip), and cuts the list at the given position, and produces
           the rest of the list.

        list-skip: (listof Any) Nat -> (listof Any)   
       |#
       (define (list-skip lst pos-to-skip)
         (cond
           [(empty? lst) empty]
           [(= 0 pos-to-skip) lst]
           [else (list-skip (rest lst) (sub1 pos-to-skip))]))]
    
    (filter (lambda (wpos) (not (= 1 (wpos-len wpos))))
            (counter loc 0 row))))


;; Tests:
(check-expect (find-wpos (string->list "###") 5)
              (list (make-wpos 5 0 true 3)))
(check-expect (find-wpos (string->list "..####..") 5)
              (list (make-wpos 5 2 true 4)))

(check-expect (lists-equiv?
               (find-wpos (string->list "..####...###..") 5)
               (list (make-wpos 5 2 true 4)
                     (make-wpos 5 9 true 3)))
              true)
(check-expect (find-wpos (string->list "#.#..#.#") 5)
              empty)
(check-expect (find-wpos (string->list "#.###..#.#") 5)
              (list (make-wpos 5 2 true 3)))
(check-expect (find-wpos (string->list "#.#######.#") 5)
              (list (make-wpos 5 2 true 7)))
(check-expect (find-wpos (string->list "........") 5) empty)




#|
(initial-state puzzle) consumes a Puzzle (puzzle) and produces an State that has
   all possible combinations of Wpos (horizontal and vertical)
  
initial-state: Puzzle -> State
|#
;;Examples:
(check-expect (initial-state puzz01)
              (make-state (list (list #\# #\# #\#))
                          (list (make-wpos 0 0 true 3))
                          (list "CAT")))
(check-expect (initial-state '(("###" "##.") ("DOG" "GO" "ON" "NO")))
              (make-state (list (list #\# #\# #\#) (list #\# #\# #\.))
                          (list (make-wpos 0 0 true 3)
                                (make-wpos 1 0 true 2)
                                (make-wpos 0 0 false 2)
                                (make-wpos 0 1 false 2))
                          (list "DOG" "GO" "ON" "NO")))

  

(define (initial-state puzzle)
  (local
    [#|
      (grid-maker los) consumes a listof strings and converts the strings
         into a Grid.

      grid-maker: (listof Strings) -> Grid
     |#
     (define (grid-maker los)
       (map string->list los))

     
     #|
      (wpos-maker grid row) consumes a grid and row number and produces a list
          horizontal wpos in each row of the grid.
         into a Grid.

      grid-maker: (listof Strings) -> Grid
     |#
     (define (wpos-maker grid row)
       (cond
         [(empty? grid) empty]
         [else (append (find-wpos (first grid) row)
                       (wpos-maker (rest grid) (add1 row)))]))]

    (make-state (grid-maker (first puzzle))
                (append (wpos-maker (grid-maker (first puzzle)) 0)
                        (map flip (wpos-maker
                                   (transpose (grid-maker (first puzzle))) 0)))
                (second puzzle))))
;; Tests:
(check-expect (initial-state test-puzzle2)
              (make-state
                 '((#\# #\. #\. #\# #\# #\# #\. #\. #\# #\# #\# #\#)
                   (#\# #\# #\# #\. #\. #\# #\# #\# #\# #\. #\. #\.))
                  (list
                    (make-wpos 0 3 true 3)
                    (make-wpos 0 8 true 4)
                    (make-wpos 1 0 true 3)
                    (make-wpos 1 5 true 4)
                    (make-wpos 0 0 false 2)
                    (make-wpos 0 5 false 2)
                    (make-wpos 0 8 false 2))
                 (list "Cat" "Pants" "Dog" "Farm")))

(check-expect (initial-state '(("###" "##.") ("DOG" "GO" "ON" "NO")))
              (make-state (list (list #\# #\# #\#) (list #\# #\# #\.))
                          (list (make-wpos 0 0 true 3)
                                (make-wpos 1 0 true 2)
                                (make-wpos 0 0 false 2)
                                (make-wpos 0 1 false 2))
                          (list "DOG" "GO" "ON" "NO")))









#|
(extract-wpos g wp) consumes a grid (g) and Wpos (wp) and produces the
   respective cells of the grid as indicated by the given Wpos

extract-wpos: Grid WPos -> (listof Char)
|#

;; Examples: 
 (check-expect (extract-wpos grid-abc (make-wpos 0 0 true 2)) '(#\A #\B))
 (check-expect (extract-wpos grid-abc (make-wpos 0 0 false 2)) '(#\A #\X))
 (check-expect (extract-wpos grid-abc (make-wpos 0 2 false 2)) '(#\C #\Z))

(define (extract-wpos g wp)
  (local
    [#|
      (extract-horizontal g row col len) consumes a grid (g) and a row and
         column (col )and length (len), and extracts characters from the grid 
         horizontally starting from the given row and column, up to the given
         length.

      extract-horizontal: Grid Nat Nat Nat -> (listof Char)
     |#
     (define (extract-horizontal g row col len)
       (cond
         [(= 0 len) empty]
         [else (cons (list-ref (list-ref g row) col)
                     (extract-horizontal g row (add1 col) (sub1 len)))]))


     #|
      (extract-vertical g row col len) consumes a grid (g) and a row and
         column (col )and length (len), and extracts characters from the grid 
         vertically starting from the given row and column, up to the given
         length.

      extract-vertical: Grid Nat Nat Nat -> (listof Char)
     |#
     (define (extract-vertical g row col len)
       (cond
         [(= 0 len) empty]
         [else (cons (list-ref (list-ref g row) col)
               (extract-vertical g (add1 row) col (sub1 len)))]))]
    
    (cond
      [(wpos-horiz? wp) (extract-horizontal g (wpos-row wp) (wpos-col wp)
                                            (wpos-len wp))]
      [else (extract-vertical g (wpos-row wp) (wpos-col wp)
                              (wpos-len wp))])))

;; Tests:
(check-expect (extract-wpos grid1 (make-wpos 0 0 true 3))
              (list #\# #\# #\#))
(check-expect (extract-wpos grid1 (make-wpos 0 0 false 3))
              (list #\# #\. #\#))
(check-expect (extract-wpos grid1 (make-wpos 1 2 true 3))
              (list #\# #\# #\#))
(check-expect (extract-wpos grid1 (make-wpos 1 3 false 2))
              (list #\# #\.))










#|
(replace-wpos g wp loc) consumes a grid (g), a Wpos (wp) and a list of char
   (loc) and substitutes the given list of characters into a position on
    the grid which is given by the Wpos (wp)

replace-wpos: Grid WPos (listof Char) -> Grid
 requires: len in WPos is equal to length of (listof Char)
|#

;; Examples:
(check-expect (replace-wpos grid-abc (make-wpos 0 0 true 2) '(#\J #\K))
              '((#\J #\K #\C) (#\X #\Y #\Z)))
(check-expect (replace-wpos grid-abc (make-wpos 0 0 false 2) '(#\J #\K))
              '((#\J #\B #\C) (#\K #\Y #\Z)))
(check-expect (replace-wpos grid-abc (make-wpos 0 2 false 2) '(#\J #\K))
              '((#\A #\B #\J) (#\X #\Y #\K)))

(define (replace-wpos g wp loc)
  (local
    [#|
      (replace col g-loc r-loc) consumes a column number (col), a list of chars
        from a grid (g-loc) and list of char to replace (r-loc). The function
        then changes the characters in g-loc to the characters in r-loc
        starting from the given column number.

      replace: Nat (listof Char) (listof Char) -> (listof Char)
     |#
     (define (replace col g-loc r-loc)
       (cond
         [(empty? r-loc) g-loc]
         [(= col 0)
          (cond
            [(empty? r-loc) g-loc]
            [else (cons (first r-loc) (replace col (rest g-loc)
                                               (rest r-loc)))])]
         [else (cons (first g-loc) (replace (sub1 col) (rest g-loc) r-loc))]))


     #|
      (search-replace g row col loc) consumes a grid and searches for the
         respective row number that the listof Char's need to be replaced into,
         and upon finding the row, it calls the replace function to substitute
         the characters.

      search-replace: Grid Nat Nat (listof Char) -> Grid
     |#
     (define (search-replace g row col loc)
       (cond
         [(= row 0) (append (list (replace col (first g) loc)) (rest g))]
         [else (cons (first g)
                     (search-replace (rest g) (sub1 row)
                                     col loc))]))]
    (cond
      [(wpos-horiz? wp) (search-replace g (wpos-row wp) (wpos-col wp) loc)]
      [else (transpose
             (search-replace (transpose g) (wpos-col wp) (wpos-row wp) loc))])))

;; Tests:
(check-expect (replace-wpos grid1 (make-wpos 0 0 true 3) (string->list "CAT"))
              '((#\C #\A #\T #\. #\# #\#)
                (#\. #\. #\# #\# #\# #\#)
                (#\# #\# #\. #\. #\# #\#)))
(check-expect (replace-wpos grid1 (make-wpos 1 3 true 3) (string->list "RAT"))
              '((#\# #\# #\# #\. #\# #\#)
                (#\. #\. #\# #\R #\A #\T)
                (#\# #\# #\. #\. #\# #\#)))
(check-expect (replace-wpos grid1 (make-wpos 1 1 false 2) (string->list "NO"))
              '((#\# #\# #\# #\. #\# #\#)
                (#\. #\N #\# #\# #\# #\#)
                (#\# #\O #\. #\. #\# #\#)))
(check-expect (replace-wpos grid1 (make-wpos 0 4 false 3) (string->list "DOG"))
             '((#\# #\# #\# #\. #\D #\#)
               (#\. #\. #\# #\# #\O #\#)
               (#\# #\# #\. #\. #\G #\#)))






#|
(fit? word cells) consumes a list of character corresponding to a word (word)
    and another list of char corresponding to a row in a grid (cells) and
    produces a boolean, indicating whether a given word can fit into the row
    A word can fit into a row if each cell is empty, or the two corresponding
    letters match.

fit?: (listof Char) (listof Char) -> Bool
|#
;; Examples:
 (check-expect (fit? (string->list "STARWARS") (string->list "S##RW##S")) true)
 (check-expect (fit? (string->list "STARWARS") (string->list "S##RT##K")) false)

(define (fit? word cells)
  (cond
    [(and (empty? word) (empty? cells)) true]
    [(or (empty? word) (empty? cells)) false]
    [(or (equal? (first cells) (first word))
         (equal? (first cells) empty-cell))
     (and true (fit? (rest word) (rest cells)))]
    [else false]))

;; Tests:
(check-expect (fit? (string->list "CAT") (string->list "###")) true)
(check-expect (fit? (string->list "CAT") (string->list "##.")) false)
(check-expect (fit? (string->list "CAT") (string->list "C#T")) true)
(check-expect (fit? (string->list "CAT") (string->list "#B#")) false)
(check-expect (fit? (string->list "CAT") (string->list "####")) false)







#|
(neighbours s) consumes a state  (s) and produces a list of states
  with one additional word placed into the grid.

neighbours: State -> (listof State)
|#
;; Examples:
(check-expect (neighbours (make-state (list (list #\# #\# #\#))
                          (list (make-wpos 0 0 true 3))
                          (list "CAT")))
              (list (make-state '((#\C #\A #\T)) empty empty)))

(check-expect (neighbours (make-state (list (list #\# #\# #\#)
                                            (list #\# #\# #\#))
                          (list (make-wpos 1 0 true 3)
                                (make-wpos 0 0 true 3)
                                (make-wpos 0 0 false 2)
                                (make-wpos 0 1 false 2)
                                (make-wpos 0 2 false 2))
                          (list "CAT" "DOG")))
              (list (make-state (list (list #\# #\# #\#) (list #\C #\A #\T))
                                (list (make-wpos 0 0 true 3)
                                      (make-wpos 0 0 false 2)
                                      (make-wpos 0 1 false 2)
                                      (make-wpos 0 2 false 2))
                                (list "DOG"))
                    (make-state (list (list #\# #\# #\#) (list #\D #\O #\G))
                                (list (make-wpos 0 0 true 3)
                                      (make-wpos 0 0 false 2)
                                      (make-wpos 0 1 false 2)
                                      (make-wpos 0 2 false 2))
                                (list "CAT"))))


(define (neighbours s)
  (local
    [#|
     (grid-char? c) consumes a char (c) and checks whether it is
       an empty cell or a blocked cell character.
      grid-char?: Char -> Bool
     |#
     (define (grid-char? c)
      (or (char=? c unused-cell) (char=? c empty-cell)))

     
     #|
      (count-letters sub-grid) determines the number of letters
         in a given row or column (sub-grid).
      count-letters: (listof Char) -> Nat
     |#
     (define (count-letters sub-grid)
       (length (filter (lambda (char) (not (grid-char? char))) sub-grid)))

     #|
      (highest-wpos grid lo-wpos high-so-far) consumes a grid (gridd),
        a list of wpos (lo-wpos), and an accuumulator wpos (high-so-far)
        and produces find the wpos, whose corresponding location on the grid
        has the most letters that arent grid characters.

      highest-wpos: Grid (listof Wpos) Wpos -> Wpos
     |#
     (define (highest-wpos grid lo-wpos high-so-far)
       (cond
         [(empty? lo-wpos) high-so-far]
         [(> (count-letters (extract-wpos grid (first lo-wpos)))
             (count-letters (extract-wpos grid high-so-far)))
          (highest-wpos grid (rest lo-wpos) (first lo-wpos))]
         [else (highest-wpos grid (rest lo-wpos) high-so-far)]))

     #|Constant defintion|#
     (define best-wpos (highest-wpos (state-grid s) (state-positions s)
                                     (first (state-positions s))))
     
     #|
      (state-maker words state) consumes a list of words (words), and a
        state (state), and produces a list of states with one additional
        word placed into the grid location, corresponding to best-wpos.

      state-maker: (listof Str) State -> (listof State)
     |#
     (define (state-maker words state)
       (cond
         [(empty? words) empty]
         [(fit? (string->list (first words))
                (extract-wpos (state-grid state) best-wpos))
          (cons (make-state
                  (replace-wpos (state-grid state) best-wpos
                           (string->list (first words)))
                  (filter (lambda (ele) (not (equal? ele best-wpos)))
                          (state-positions state))
                  (remove (first words) (state-words state)))
                (state-maker (rest words) state))]
         
        [else (state-maker (rest words) state)]))]

    (state-maker (state-words s) s)))

;; Tests:
(check-expect (neighbours (make-state '((#\C #\# #\#))
                                      (list (make-wpos 0 0 true 3))
                                      '("CAT" "DOG" "CAR")))
              (list (make-state '((#\C #\A #\T)) empty '("DOG" "CAR"))
                    (make-state '((#\C #\A #\R)) empty '("CAT" "DOG"))))

(check-expect (neighbours (make-state '((#\C #\# #\T))
                                      (list (make-wpos 0 0 true 3))
                                      '("CAT")))
              (list (make-state '((#\C #\A #\T)) empty empty)))


(check-expect (neighbours (make-state (list (list #\# #\# #\#)
                                            (list #\# #\A #\T))
                          (list (make-wpos 1 0 true 3)
                                (make-wpos 0 0 true 3)
                                (make-wpos 0 0 false 2)
                                (make-wpos 0 1 false 2)
                                (make-wpos 0 2 false 2))
                          (list "CAT" "RAT")))
              (list (make-state (list (list #\# #\# #\#)(list #\C #\A #\T))
                                (list (make-wpos 0 0 true 3)
                                      (make-wpos 0 0 false 2)
                                      (make-wpos 0 1 false 2)
                                      (make-wpos 0 2 false 2))
                                (list "RAT"))
                    (make-state (list (list #\# #\# #\#) (list #\R #\A #\T))
                                (list (make-wpos 0 0 true 3)
                                      (make-wpos 0 0 false 2)
                                      (make-wpos 0 1 false 2)
                                      (make-wpos 0 2 false 2))
                                (list "CAT"))))






;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; PROVIDED FUNCTIONS:

;; (solved? s) determines if s is a solved criss-cross problem
;;   by checking if all of the word positions have been filled
;; solved?: State -> Bool
(define (solved? s)
  (empty? (state-positions s)))


;; (criss-cross puzzle) produces a list of strings corresponding
;;   to the solution of the the criss-cross puzzle,
;;   or false if no solution is possible
;; criss-cross: Puzzle -> (anyof false (listof Str))

(define (criss-cross puzzle)
  (local [(define result (solve (initial-state puzzle)
                                neighbours
                                solved?))]
    (cond [(false? result) false]
          [else (map list->string (state-grid result))])))

; (check-expect (criss-cross puzz01) '("CAT"))

;; note that [solve] (used in criss-cross above) is provided in puzlib.rkt

;; when you are all done, you can use disp to
;; view your solutions:

;;(disp (criss-cross (read-puzzle "puzzle12.txt")))

;; NOTE: Do NOT leave top-level expressions in your code.
;;       In other words, when your code is run, only the
;;       check-expect message "All X tests passed!"
;;       should appear in the interactions window

