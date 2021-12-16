;; -*- lexical-binding: t; -*-

(defvar Test.Main.assertEqual
  (lambda (label)
    (lambda (a)
      (lambda (b)
        (message label)
        (if (equal a b)
            t
          (psel/assert-error label))))))

(defvar Test.Main.assertEqualRecord
  (lambda (label)
    (lambda (a)
      (lambda (b)
        (message label)
        (if (psel/alist-equal a b)
            t
          (psel/assert-error label))))))

(defvar Test.Main.mkMainLike
  (lambda (f)
    (lambda ()
      (funcall f nil))))
