{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module PsEl.SExpTraverse where

import Data.Functor.Foldable (cata)
import Data.List (mapAccumL)
import Lens.Micro (Traversal')
import PsEl.SExp
import RIO
import RIO.Lens (_2)
import RIO.NonEmpty qualified as NE
import RIO.Set qualified as Set

-- | `Taversal' SExp Symbol` for free variables
freeVars :: forall f. Applicative f => (Symbol -> f Symbol) -> SExp -> f SExp
freeVars f s = cata go s mempty
  where
    go ::
        SExpF (Set Symbol -> f SExp) ->
        Set Symbol ->
        f SExp
    -- 自由変数のみfを適用する
    go (Symbol sym) vars
        | not (Set.member sym vars) = SExp . Symbol <$> f sym
    -- 束縛(lambda)
    go (Lambda1 sym es) vars =
        SExp <$> (Lambda1 sym <$> traverse ($ Set.insert sym vars) es)
    -- 束縛(let*)
    go (Let LetStar binds body) vars =
        let (vars', binds') =
                mapAccumL
                    (\vars (sym, val) -> (Set.insert sym vars, (sym,) <$> val vars))
                    vars
                    binds
         in SExp
                <$> ( Let LetStar
                        <$> sequenceA binds'
                        <*> traverse ($ vars') body
                    )
    -- 束縛(letrec)
    go (Let LetRec binds body) vars =
        let vars' = foldr (Set.insert . fst) vars binds
         in SExp
                <$> ( Let LetRec
                        <$> traverse (\(sym, val) -> (sym,) <$> val vars') binds
                        <*> traverse ($ vars') body
                    )
    -- 束縛(pcase)
    go (Pcase exps alts) vars =
        SExp <$> (Pcase <$> traverse ($ vars) exps <*> traverse (pcaseAlt vars) alts)
    -- その他
    go s vars =
        SExp <$> sequenceA (($ vars) <$> s)

    pcaseAlt :: Set Symbol -> PcaseAlt (Set Symbol -> f SExp) -> f (PcaseAlt SExp)
    pcaseAlt vars (PcaseAlt patterns guard code) =
        let (vars', patterns') = mapAccumL ppattern vars patterns
         in PcaseAlt
                <$> sequenceA patterns'
                <*> traverse ($ vars') guard
                <*> code vars'

    ppattern :: Set Symbol -> PPattern (Set Symbol -> f SExp) -> (Set Symbol, f (PPattern SExp))
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
