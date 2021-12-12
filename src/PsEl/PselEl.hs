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

;; Exception

(defun psel/throw-unrecoverable-error ()
  (throw 'psel/unrecoverable-error nil))

(defun psel/throw-assert-error (msg)
  (throw (cons 'psel/assert-error msg) nil))

;; Alist

(defun psel/alist-set (field val alist)
  "Update the first cons with car eq to field in a immutable way."
  (cond ((null alist)
         (psel/throw-unrecoverable-error))
        ((eq (caar alist) field)
         (cons (cons field val) (cdr alist)))
        (t
         (cons (car alist) (psel/alist-set field val (cdr alist))))))

(defun psel/alist-delete (field alist)
  "Delete the first cons with car eq to field in a immutable way."
  (cond ((null alist)
         (psel/throw-unrecoverable-error))
        ((eq (caar alist) field)
         (cdr alist))
        (t
         (cons (car alist) (psel/alist-delete field (cdr alist))))))

;; Currying

(defun psel/curry2 (fsym)
  (lambda (a)
    (lambda (b)
      (funcall (symbol-function fsym) a b))))

(defun psel/curry3 (fsym)
  (lambda (a)
    (lambda (b)
      (lambda (c)
        (funcall (symbol-function fsym) a b c)))))

(defun psel/curry3 (fsym)
  (lambda (a)
    (lambda (b)
      (lambda (c)
        (lambda (d)
          (funcall (symbol-function fsym) a b c d))))))


;; Funcall

(defun psel/funcall2 (f a b)
  (funcall (funcall f a) b))

(provide '$feature)
|]
