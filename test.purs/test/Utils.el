;; -*- lexical-binding: t; -*-

(defvar Test.Utils.assertEqual
  (lambda (label)
    (lambda (a)
      (lambda (b)
        (message label)
        (if (equal a b)
            t
          (psel/assert-error label))))))

(defvar Test.Utils.assertEqualRecord
  (lambda (label)
    (lambda (a)
      (lambda (b)
        (message label)
        (if (psel/alist-equal a b)
            t
          (psel/assert-error label))))))
