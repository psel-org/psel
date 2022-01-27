;; -*- lexical-binding: t; -*-

(defvar Test.Main.mkMainLike
  (lambda (f)
    (lambda ()
      (funcall f nil))))
