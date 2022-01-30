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
    | ILambda0
    | IBind Symbol
    | -- 引数部
      -- progn の最後の式以外もargと見做す
      -- e.g. (pcase <args> ...), (alist <args>), (funcall x <arg>)
      IArg
    | -- 条件部
      -- e.g. (if <cond> ..), (cond (<cond> ..) (<cond> ...))
      ICond
    | -- 長さ一のボディ
      ITail
    | -- funcall1(1引数呼出し)の対象
      IFunCall1
    | IFunCall0
    | IFunCallN

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
    go (Lambda0 body) ix vars = do
        body' <- body (ILambda0 : ix) vars
        pure $ SExp $ Lambda0 body'

    -- 束縛(let*)
    go (Let LetStar binds body) ix vars = do
        let f vars (sym, val) = (Set.insert sym vars, (sym,) <$> val (IBind sym : ix) vars)
        let (vars', binds') = mapAccumL f vars binds
        binds'' <- sequenceA binds'
        body' <- body (ITail : ix) vars'
        pure $ SExp $ Let LetStar binds'' body'

    -- 束縛(letrec)
    go (Let LetRec binds body) ix vars = do
        let vars' = foldr (Set.insert . fst) vars binds
        let f (sym, val) = (sym,) <$> val (IBind sym : ix) vars'
        binds' <- traverse f binds
        body' <- body (ITail : ix) vars
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
    go (FunCall0 f) ix vars = do
        f' <- f (IFunCall0 : ix) vars
        pure $ SExp $ FunCall0 f'
    go (FunCallN f args) ix vars = do
        f' <- f (IFunCallN : ix) vars
        args' <- traverse (\arg -> arg (IArg : ix) vars) args
        pure $ SExp $ FunCallN f' args'

    -- 関数呼出し(ネイティブ)
    go (FunCallNative sym args) ix vars = do
        args' <- traverse (\a -> a (IArg : ix) vars) args
        pure $ SExp $ FunCallNative sym args'

    -- if
    go (If condE thenE elseE) ix vars = do
        condE' <- condE (ICond : ix) vars
        thenE' <- thenE (ITail : ix) vars
        elseE' <- elseE (ITail : ix) vars
        pure $ SExp $ If condE' thenE' elseE'

    -- cond
    go (Cond alts) ix vars = do
        let f (condE, bodyE) = do
                condE' <- condE (ICond : ix) vars
                bodyE' <- bodyE (ITail : ix) vars
                pure (condE', bodyE')
        alts' <- traverse f alts
        pure $ SExp $ Cond alts'

    -- mkalist
    go (MkAlist xs) ix vars = do
        xs' <- traverse (\(f, e) -> (f,) <$> e (IArg : ix) vars) xs
        pure $ SExp $ MkAlist xs'

    -- progn
    go (Progn xs) ix vars = do
        let len = length xs
        let addIndex i = if i == len then ITail else IArg
        xs' <- traverse (\(i, e) -> e (addIndex i : ix) vars) $ zip [1 ..] xs
        pure $ SExp $ Progn xs'

    -- その他(再帰しないもの)
    go s ix vars =
        SExp <$> sequenceA (error "unreachable" <$> s)

    pcaseAlt ::
        [Index] ->
        Set Symbol ->
        PcaseAlt ([Index] -> Set Symbol -> f SExp) ->
        f (PcaseAlt SExp)
    pcaseAlt ix vars (PcaseAlt patterns guard code) =
        let (vars', patterns') = mapAccumL ppattern vars $ map (fmap ($ (ICond : ix))) patterns
         in PcaseAlt
                <$> sequenceA patterns'
                <*> traverse (\e -> e (ICond : ix) vars') guard
                <*> code (ITail : ix) vars'

    -- 相互再帰のrecursion schemeを避けるため ppatternは手動での再帰関数。
    -- 構造系では先にくる要素で束縛が存在しうることに注意。
    -- PPatternの型パラメータ(Set Symbol -> f SExp)が必要なのは pred と appのみ。
    ppattern ::
        Set Symbol ->
        PPattern (Set Symbol -> f SExp) ->
        (Set Symbol, f (PPattern SExp))
    ppattern vars PAny =
        (vars, pure PAny)
    ppattern vars (PInteger i) =
        (vars, pure (PInteger i))
    ppattern vars (PString s) =
        (vars, pure (PString s))
    ppattern vars (PCharacter c) =
        (vars, pure (PCharacter c))
    ppattern vars (PBind sym) =
        (Set.insert sym vars, pure (PBind sym))
    ppattern vars (PBackquotedList es) =
        let (vars', es') =
                mapAccumL
                    ( \vars e -> case e of
                        Left sym -> (vars, pure (Left sym))
                        Right e' -> over _2 (fmap Right) $ ppattern vars e'
                    )
                    vars
                    es
         in (vars', PBackquotedList <$> sequenceA es')
    ppattern vars (PBackquotedVector es) =
        let (vars', es') =
                mapAccumL
                    ( \vars e -> case e of
                        Left sym -> (vars, pure (Left sym))
                        Right e' -> over _2 (fmap Right) $ ppattern vars e'
                    )
                    vars
                    es
         in (vars', PBackquotedVector <$> sequenceA es')
    ppattern vars0 (PBackquotedCons car cdr) =
        let (vars1, car') = ppattern vars0 car
            (vars2, cdr') = ppattern vars1 cdr
         in (vars2, PBackquotedCons <$> car' <*> cdr')
    ppattern vars (PAnd pps) =
        let (vars', pps') = mapAccumL ppattern vars pps
         in (vars', PAnd <$> sequenceA pps')
    ppattern vars (PPred pred) =
        (vars, PPred <$> pred vars)
    ppattern vars (PPredBool b) =
        (vars, pure (PPredBool b))
    ppattern vars (PApp e pp) =
        let (vars', pp') = ppattern vars pp
         in (vars', PApp <$> e vars <*> pp')
