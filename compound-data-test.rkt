;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname compound-data-test) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define-struct name (first last))

(define N1 (make-name "Lindsey" "Handley"))

(name-first N1)
(name-last N1)

(name? N1)
(name? "Lindsey Handley")