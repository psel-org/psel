;; -*- lexical-binding: t; -*-

(defvar Test.TCO.eqInt
  (lambda (a)
    (lambda (b)
      (= a b))))

(defvar Test.TCO.succInt
  (lambda (i)
    (1+ i)))
