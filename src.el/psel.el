;; psel -*- lexical-binding: t; -*-

(defun psel/alist-set (field val alist)
  "Update the first cons with car eq to field in a immutable way."
  (cond ((null alist)
         (throw 'ps nil))
        ((eq (caar alist) field)
         (cons (cons field val) (cdr alist)))
        (t
         (cons (car alist) (psel/alist-set field val (cdr alist))))))

(provide 'psel)
