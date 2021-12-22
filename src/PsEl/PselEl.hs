{-# LANGUAGE QuasiQuotes #-}

module PsEl.PselEl where

import NeatInterpolation (trimming)
import PsEl.SExp (FeatureName (FeatureName))
import PsEl.SExpPrinter (displaySymbol)
import RIO

-- 生成するelで足りない関数を定義。全てのモジュールelよりrequireされる必要あり。
-- emacs標準で同等機能が提供されている場合は乗り換える。
-- psel/alist-set はオブジェクト更新時に使う。
--
-- ???複数の output.el がload-path上にある場合どうする？
pselEl :: FeatureName -> Text
pselEl (FeatureName sym) =
    let feature = utf8BuilderToText $ displaySymbol sym
     in [trimming|
;; psel.el -*- lexical-binding: t; -*-

(require 'seq)

;; Exception

(defun psel/unrecoverable-error (&rest args)
  (signal 'psel/unrecoverable-error args))

(defun psel/assert-error (msg)
  (signal 'psel/assert-error (list msg)))

;; Alist

(defun psel/alist-get (field alist)
  (let ((c (assq field alist)))
    (if (null c)
        (psel/unrecoverable-error)
      (cdr c))))

(defun psel/alist-set (field val alist)
  "Update the first cons with car eq to field in a immutable way."
  (cond ((null alist)
         (psel/unrecoverable-error))
        ((eq (caar alist) field)
         (cons (cons field val) (cdr alist)))
        (t
         (cons (car alist) (psel/alist-set field val (cdr alist))))))

(defun psel/alist-set-or-insert (field val alist)
  "Update the first appearnce or insert a new field in an immutable way."
  (cond ((null alist)
         (cons (cons field val) nil))
        ((eq (caar alist) field)
         (cons (cons field val) (cdr alist)))
        (t
         (cons (car alist) (psel/alist-set-or-insert field val (cdr alist))))))

(defun psel/alist-delete (field alist)
  "Delete the first cons with car eq to field in a immutable way."
  (cond ((null alist)
         (psel/unrecoverable-error))
        ((eq (caar alist) field)
         (cdr alist))
        (t
         (cons (car alist) (psel/alist-delete field (cdr alist))))))

(defun psel/alist-equal (a b)
  "Alist equality. Key order doesn't matter."
  (let ((keys (mapcar 'car a)))
    (and (eq (length keys) (length b))
         (equal a (mapcar (lambda (key) (assq key b)) keys)))))

;; funcall

(defmacro psel/funcall (f &rest args)
  "funcall 1-arg function"
  (seq-reduce (lambda (b a) `(funcall ,b ,a)) args f))

(provide '$feature)
|]
