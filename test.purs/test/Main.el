;; -*- lexical-binding: t; -*-

(defvar Test.Main.assertEqual
  (lambda (label)
    (lambda (a)
      (lambda (b)
        (message label)
        (if (equal a b)
            t
          (throw 'assert-equal-error nil))))))

(defvar Test.Main.assertEqualRecord
  (lambda (label)
    (lambda (a)
      (lambda (b)
        (message label)
        (if (psel/alist-equal a b)
            t
          (throw 'assert-equal-error nil))))))

(defvar Test.Main.mkMainLike
  (lambda (f)
    (lambda ()
      (funcall f nil))))
