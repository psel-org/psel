{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module PsEl.SExpTraverse where

import Control.Lens (Indexable, IndexedTraversal', indexed)
import Data.Functor.Foldable (cata)
import Data.List (mapAccumL)
import PsEl.SExp
import RIO
import RIO.Lens (_2)
import RIO.NonEmpty qualified as NE
import RIO.Set qualified as Set

-- indexはシンプルなself-recursiveなTCOを最適化するために必要な情報のみ含む。
-- その意味ではIArgとICondを分ける必要はないかな..
data Index
    = ILambda1
    | IBind Symbol
    | -- 引数部
      -- e.g. (pcase <args> ...), (alist <args>), (funcall x <arg>)
      IArg
    | -- 条件部
      -- e.g. (if <cond> ..), (cond (<cond> ..) (<cond> ...))
      ICond
    | -- 長さ一のボディ
      IBody1
    | -- funcall1 の対象
      IFunCall1

-- | `Taversal' SExp Symbol` for free variables
freeVars ::
    forall f p.
    (Applicative f, Indexable [Index] p) =>
    p Symbol (f Symbol) ->
    SExp ->
    f SExp
freeVars p s = cata go s [] mempty
  where
    go ::
        SExpF ([Index] -> Set Symbol -> f SExp) ->
        [Index] ->
        Set Symbol ->
        f SExp
    -- 自由変数のみfを適用する
    go (Symbol sym) ix vars
        | not (Set.member sym vars) = SExp . Symbol <$> indexed p ix sym
    -- 束縛(lambda)
    go (Lambda1 sym body) ix vars = do
        body' <- body (ILambda1 : ix) (Set.insert sym vars)
        pure $ SExp $ Lambda1 sym body'

    -- 束縛(let*)
    go (Let LetStar binds body) ix vars = do
        let f vars (sym, val) = (Set.insert sym vars, (sym,) <$> val (IBind sym : ix) vars)
        let (vars', binds') = mapAccumL f vars binds
        binds'' <- sequenceA binds'
        body' <- body (IBody1 : ix) vars'
        pure $ SExp $ Let LetStar binds'' body'

    -- 束縛(letrec)
    go (Let LetRec binds body) ix vars = do
        let vars' = foldr (Set.insert . fst) vars binds
        let f (sym, val) = (sym,) <$> val (IBind sym : ix) vars'
        binds' <- traverse f binds
        body' <- body (IBody1 : ix) vars
        pure $ SExp $ Let LetRec binds' body'

    -- 束縛(pcase)
    go (Pcase exps alts) ix vars = do
        exps' <- traverse (\e -> e (IArg : ix) vars) exps
        alts' <- traverse (pcaseAlt ix vars) alts
        pure $ SExp $ Pcase exps' alts'

    -- 関数呼出し(funcall)
    go (FunCall1 f arg) ix vars = do
        f' <- f (IFunCall1 : ix) vars
        arg' <- arg (IArg : ix) vars
        pure $ SExp $ FunCall1 f' arg'

    -- if
    go (If condE thenE elseE) ix vars = do
        condE' <- condE (ICond : ix) vars
        thenE' <- thenE (IBody1 : ix) vars
        elseE' <- elseE (IBody1 : ix) vars
        pure $ SExp $ If condE' thenE' elseE'

    -- cond
    go (Cond alts) ix vars = do
        let f (condE, bodyE) = do
                condE' <- condE (ICond : ix) vars
                bodyE' <- bodyE (IBody1 : ix) vars
                pure (condE', bodyE')
        alts' <- traverse f alts
        pure $ SExp $ Cond alts'

    -- list
    go (List xs) ix vars = do
        xs' <- traverse (\e -> e (IArg : ix) vars) xs
        pure $ SExp $ List xs'

    -- mkalist
    go (MkAlist xs) ix vars = do
        xs' <- traverse (\(f, e) -> (f,) <$> e (IArg : ix) vars) xs
        pure $ SExp $ MkAlist xs'

    -- その他(再帰しないもの)
    go s ix vars =
        SExp <$> sequenceA (error "unreachable" <$> s)

    pcaseAlt ::
        [Index] ->
        Set Symbol ->
        PcaseAlt ([Index] -> Set Symbol -> f SExp) ->
        f (PcaseAlt SExp)
    pcaseAlt ix vars (PcaseAlt patterns guard code) =
        let (vars', patterns') = mapAccumL (ppattern (ICond : ix)) vars patterns
         in PcaseAlt
                <$> sequenceA patterns'
                <*> traverse (\e -> e (ICond : ix) vars') guard
                <*> code (IBody1 : ix) vars'

    ppattern ::
        [Index] ->
        Set Symbol ->
        PPattern ([Index] -> Set Symbol -> f SExp) ->
        (Set Symbol, f (PPattern SExp))
    ppattern ix vars PAny =
        (vars, pure PAny)
    ppattern ix vars (PInteger i) =
        (vars, pure (PInteger i))
    ppattern ix vars (PString s) =
        (vars, pure (PString s))
    ppattern ix vars (PCharacter c) =
        (vars, pure (PCharacter c))
    ppattern ix vars (PBind sym) =
        (Set.insert sym vars, pure (PBind sym))
    ppattern ix vars (PBackquotedList es) =
        let (vars', es') =
                mapAccumL
                    ( \vars e -> case e of
                        Left sym -> (vars, pure (Left sym))
                        Right e' -> over _2 (fmap Right) $ ppattern ix vars e'
                    )
                    vars
                    es
         in (vars', PBackquotedList <$> sequenceA es')
    ppattern ix vars (PBackquotedVector es) =
        let (vars', es') =
                mapAccumL
                    ( \vars e -> case e of
                        Left sym -> (vars, pure (Left sym))
                        Right e' -> over _2 (fmap Right) $ ppattern ix vars e'
                    )
                    vars
                    es
         in (vars', PBackquotedVector <$> sequenceA es')
    ppattern ix vars (PAnd pps) =
        let (vars', pps') = mapAccumL (ppattern ix) vars pps
         in (vars', PAnd <$> sequenceA pps')
    ppattern ix vars (PPred pred) =
        (vars, PPred <$> pred ix vars)
    ppattern ix vars (PPredBool b) =
        (vars, pure (PPredBool b))
    ppattern ix vars (PApp e pp) =
        let (vars', pp') = ppattern ix vars pp
         in (vars', PApp <$> e ix vars <*> pp')
