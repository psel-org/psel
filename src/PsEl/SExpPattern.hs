{-# LANGUAGE PatternSynonyms #-}

module PsEl.SExpPattern where

import PsEl.SExp as S

pattern Integer i = SExp (S.Integer i)
pattern Symbol s = SExp (S.Symbol s)
pattern Lambda1 s e = SExp (S.Lambda1 s e)
pattern Lambda0 e = SExp (S.Lambda0 e)
pattern FunCall1 f a = SExp (S.FunCall1 f a)
pattern FunCall0 f = SExp (S.FunCall0 f)
pattern Pcase es alts = SExp (S.Pcase es alts)
