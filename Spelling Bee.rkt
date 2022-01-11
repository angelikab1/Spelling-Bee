;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname hw8-all-problems) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/universe)
(require 2htdp/image)
(require 2htdp/batch-io)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Introduction

;; In this assignment, you will continue working on Spelling Bee (Homework 3).
;; You will rely on several concepts and techniques that you've learned since
;; Homework 3, to build a version of Spelling Bee that is significantly closer
;; to the real game. In particular, you will:
;;
;; 1. Move to a seven-letter Spelling Bee, instead of a five-letter game,
;;
;; 2. Implement scoring,
;;
;; 3. Support the backspace / delete key, so that players can correct their
;;    word, and
;;
;; 4. Check that the entered word is in a dictionary.

;; NOTE #1: Follow the "no halloween colors" and 2+ check-expects rule for
;; all function designs.
;;
;; NOTE #2: In the original Spelling Bee, we restricted you from using certain
;; functions. For this assignment, the only restricted functions are those
;; in the class style guide.
;;
;; NOTE #3: Despite having fewer restrictions, we still expect good program
;; design. For example, lists are the appropiate type of data to represent
;; scored words and available letters, and this assignment asks you to update
;; your program to use lists. It is possible to immediately convert a 
;; [List-of 1String] into a String. But, that is not the approach we want you
;; to take. We want you to use list abstractions when possible.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Step 0: Meet With Your Partner

;; You must do this assignment with your assigned partner. However, since you
;; did the previous stage alone, that means you have multiple implementations to
;; Spelling Bee to use as a starting point. Which one will you use? Making that
;; decision is part of the assignment.
;;
;; Note: If neither you nor your partner did well on HW3, we strongly encourage
;; you to use our HW3 sample solution as a starting point.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Step 1: Introducing Lists
;;
;; When we worked on HW3, we did not know about lists, which are fundamental to
;; good program design. Instead, we played tricks with strings, such as using
;; newlines to separate found words. Now that we are familiar with lists, we
;; are going to modify Spelling Bee to use them in two places:
;;
;; a. Revise your data definition for Letters to either:
;;
;;    - Represent the available letters as an [NE-List-of 1String], or
;;
;;    - Represent the non-center letters as a [List-of 1String]
;;
;; b. Revise your World data definition to represent the list of words found
;;    as a [List-of String]. Thus you should no longer use "\n" in your
;;    examples of World.
;;

;; [TODO] Revise World and Letters as described above.

; A Letters is [NEList-of 1String]
; Interpretation: a list of all the letters usable in the spelling bee game

(define L-1 (list "a" "t" "r" "i" "s" "n" "e"))
(define L-2 (list "a" "e" "x" "p" "l" "i" "n"))
(define L-3 (list "e" "v" "i" "u" "o" "n" "s"))
(define L-4 (list "c" "v" "i" "u"))
(define L-5 (list "c" "v" "i" "u" "d"))

(define (l-temp l)
  (...
   (cond
     [(empty? (rest l)) ...
      (first l) ...]
     [(cons? (rest l)) ...
      (first l) ...
      (l-temp (rest l)) ...])))

(define-struct world [letters input words score])

; A World is a (make-world Letters String [List-of String] Nat)
; Interpretation: a world is the available letters to the player, the current input,
; and the list of worlds created by the player so far

(define W-1 (make-world L-1 "" '() 0))
(define W-2 (make-world L-2 "" (list "lane") 1))
(define W-3 (make-world L-2 "plain" (list "lane") 1))
(define W-4 (make-world L-2 "" '() 0))

(define (w-temp w)
  (... (l-temp (world-letters w)) ...
       (world-input w) ...
       (los-temp (world-words w))
       (world-score w) ...))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Step 2: More Letters
;;
;; The update that you made in Step 1 should allow your program to support any
;; number of available letters (though you need at least one letter at the
;; center). However, your old examples use exactly five letters.
;;
;; 1. Construct new examples and check-expects that have varying numbers of
;;    available letters.
;;
;;  2. Modify letters->image so that it either:
;;
;;     - Assumes that there are exactly seven letters (i.e., as in real
;;       Spelling Bee), or
;;
;;     - Supports any number of available letters (this is not required)

;; [TODO] New examples and check-expects with varying numbers of available
;; letters.

;; [TODO] Update letters->image to display seven letters (or optionally,
;; any number of letters)


(define BACKGROUND (empty-scene 500 500 "grey"))


;; word->image : String -> Image
;;
;; Displays a word as an image with the color and size that we want

(check-expect (word->image "hello") (text "hello" 30 "black"))
(check-expect (word->image "hey") (text "hey" 30 "black"))

(define (word->image w)
  (text w 30 "black"))


;; letter->image : 1String -> Image
;;
;; Displays a single letter as an image

(check-expect (letter->image "X")
              (overlay (text "X" 30 "blue") (square 40 "outline" "black")))
(check-expect (letter->image "Y")
              (overlay (text "Y" 30 "blue") (square 40 "outline" "black")))

(define (letter->image s)
  (overlay (text s 30 "blue")
           (square 40 "outline" "black")))


;; letters->image : Letters -> Image
;;
;; Displays a Letters.

(check-expect (letters->image L-1)
              (above
               (beside (letter->image (second L-1))
                       (letter->image (third L-1)))
               (beside (letter->image (fourth L-1))
                       (letter->image (first L-1))
                       (letter->image (fifth L-1)))
               (beside (letter->image (sixth L-1))
                       (letter->image (seventh L-1)))))

(check-expect (letters->image L-2)
              (above
               (beside (letter->image (second L-2))
                       (letter->image (third L-2)))
               (beside (letter->image (fourth L-2))
                       (letter->image (first L-2))
                       (letter->image (fifth L-2)))
               (beside (letter->image (sixth L-2))
                       (letter->image (seventh L-2)))))

(define (letters->image l)
  (above
   (beside (letter->image (second l))
           (letter->image (third l)))
   (beside (letter->image (fourth l))
           (letter->image (first l))
           (letter->image (fifth l)))
   (beside (letter->image (sixth l))
           (letter->image (seventh l)))))


;; world->image : World -> Image
;;
;; Displays a World.

(check-expect (world->image W-4)
              (place-image
               (word->image (string-append "Score: 0"))
               250 30
               (place-image
                (letters->image (world-letters W-4))
                130 220
                (place-image
                 (word->image (string-append "Enter your word:\n"))
                 130 320
                 (place-image
                  (word->image
                   (string-append "Words:\n"))
                  400 250 BACKGROUND)))))

(check-expect (world->image W-3)
              (place-image
               (word->image (string-append "Score: 1"))
               250 30
               (place-image
                (letters->image (world-letters W-3))
                130 220
                (place-image
                 (word->image (string-append "Enter your word:\nplain"))
                 130 320
                 (place-image
                  (word->image
                   (string-append "Words:\nlane"))
                  400 250 BACKGROUND)))))


(define (world->image w)
  (place-image
   (word->image (string-append "Score: " (number->string (world-score w))))
   250 30
   (place-image
    (letters->image (world-letters w))
    130 220
    (place-image
     (word->image (string-append "Enter your word:\n" (world-input w)))
     130 320
     (place-image
      (word->image
       (string-append "Words:\n" (combine-words (world-words w))))
      400 250 BACKGROUND)))))


; combine-words : [List-of String] -> [String String -> String]
; takes a list of words and combines them all into one string
; with a "\n" separating each one

(check-expect (combine-words (list "hello" "goodbye")) "goodbye\nhello")
(check-expect (combine-words (list "a" "b")) "b\na")

(define (combine-words los)
  (if (empty? los) ""
      (foldl (λ (x y) (string-append x "\n" y)) (first los) (rest los))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Step 3: The Backspace/Delete Key
;;
;; Update your program so that when the player presses Backspace (Windows) or
;; Delete (Mac), the game clears the last letter that they entered. The special
;; string "\b" stands for delete/backspace.
;;
;; Note: Ensure your program is "well-behaved" when the player presses
;; delete/backspace and there are no characters to delete.

;; [TODO] Revise your program to support for backspace/delete.

;; available-letter? : Letters String -> Boolean
;;
;; Check if a letter is available.

(check-expect (available-letter? L-1 "t") #true)
(check-expect (available-letter? L-3 "x") #false)

(define (available-letter? l k)
  (ormap (λ (n) (string=? n k)) l))


(define DICTIONARY (read-words "words.txt"))


;; key-pressed : World KeyEvent -> World
;;
;; Produce a new World in reaction to a key-press.

(check-expect (key-pressed W-1 "\r") W-1)
(check-expect (key-pressed W-2 "a") (make-world L-2 "a" (list "lane") 1))
(check-expect (key-pressed W-2 "\b") (make-world L-2 "" (list "lane") 1))
(check-expect (key-pressed W-3 "\r") (make-world L-2 "" (list "plain" "lane") 3))
(check-expect (key-pressed W-3 "\b") (make-world L-2 "plai" (list "lane") 1))

(define (key-pressed w k)
  (cond
    [(available-letter? (world-letters w) k)
     (make-world (world-letters w)
                 (string-append (world-input w) k)
                 (world-words w)
                 (world-score w))]
    [(and (string=? k "\r")
          (string-contains? (first (world-letters w)) (world-input w))
          (ormap (λ (n) (string=? (world-input w) n)) DICTIONARY)
          (> (string-length (world-input w)) 3)
          (not (ormap (λ (n) (string=? (world-input w) n)) (world-words w))))
     (make-world (world-letters w)
                 ""
                 (cons (world-input w) (world-words w))
                 (get-new-score w))]
    [(string=? k "\r")
     (make-world (world-letters w)
                 ""
                 (world-words w)
                 (world-score w))]
    [(and (string=? k "\b")
          (not (string=? "" (world-input w))))
     (make-world (world-letters w)
                 (delete-last (world-input w))
                 (world-words w)
                 (world-score w))]
    [else w]))


; get-new-score : World -> Number
; returns the score value of a given word in a world

(check-expect (get-new-score
               (make-world L-2 "plain" (list "lane") 1)) 3)
(check-expect (get-new-score
               (make-world L-2 "lane" '() 0)) 1)
(check-expect (get-new-score
               (make-world L-2 "lane" (list "pain") 1)) 2)
(check-expect (get-new-score
               (make-world L-2 "explain" (list "pain") 1)) 15)

(define (get-new-score w)
  (local [; has-all-letters? : World -> Boolean
          ; checks to see if the input of the world contains all letters
          (define (has-all-letters? w)
            (andmap (λ (n) (string-contains? n (world-input w)))
                    (world-letters w)))]
    (if (has-all-letters? w)
        (+ (world-score w) (string-length (world-input w)) 7)
        (+ (world-score w) (- (string-length (world-input w)) 3)))))


; delete-last : String -> String
; removes the last letter of a string

(check-expect (delete-last "hello") "hell")
(check-expect (delete-last "hey") "he")

(define (delete-last s)
  (substring s 0 (- (string-length s) 1)))


;; play : World -> World
;;
;; Uses big-bang to play a game of Spelling Bee, given Letters.

(define (play w)
  (big-bang
      w
    (to-draw world->image)
    (on-key key-pressed)))

(play W-4)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Step 4: More Checks: Duplicate Words, 4+ Letter-Words, Dictionary Word
;;
;; Revise your program to ensure that the word entered by the player is:
;; 1. An available letter (already done in HW3),
;; 2. Contains the center letter (already done in HW3),
;; 3. A dictionary word,
;; 4. At least four letters long, and
;; 5. Is not a duplicate of an already entered word.
;;
;; We've given you a file called words.txt, which you can use as a dictionary.
;; It is not a comprehensive dictionary, but has roughly 50,000 words. Every
;; line of the file is a word, and they are arranged in alphabetical order
;; (technically, lexicographic order), if W1 appears before W2 in the file,
;; then (string<? W1 W2) is true.
;;
;; Suggestion #1: you can use the read-lines function in the 2htdp/batch-io
;; library to read the dictionary to a constant.
;;
;; Suggestion #2: It is very difficult to work with a list of 50,000 words.
;; So, to get started we recommend defining a small dictionary of words, and
;; then replacing it the the code that reads words from the file. E.g.,

;(define LITTLE-DICTIONARY (list "explain" "plain" "nail" "lap"))

;; [TODO] Revise your program to implement the five checks above.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Step 5: Scoring a Game
;;
;; Finally, revise your program to display the current score. The score for
;; each word is:
;;
;; 1. One point for a four-letter word,
;;
;; 2. An additional point for every additional letter beyond the first four, and
;;
;; 3. An additional seven bonus points for using all seven letters.

;; [TODO] Revise your program to support scoring.

