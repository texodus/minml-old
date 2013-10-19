------------------------------------------------------------------------------

-- | Expression parser.

--   `LetExpr` and `TypExpr` are roughly identical; here they've been arranged
--   slightly differently to show the symmetry between function application
--   & combinators of the `Applicative` type class.

------------------------------------------------------------------------------

{-# LANGUAGE GADTs      #-}
{-# LANGUAGE RankNTypes #-}

module Forml.Parse.Expr (
    exprP
) where

import Control.Applicative
import Control.Arrow
import Control.Lens
import Data.Monoid
import Language.Javascript.JMacro
import Text.Parsec                hiding (many, (<|>), optional)
import Text.Parsec.Expr

import Forml.AST
import Forml.AST.Replace
import Forml.Parse.Indent
import Forml.Parse.Notation
import Forml.Parse.Patt
import Forml.Parse.Record
import Forml.Parse.Token
import Forml.Parse.Type
import Forml.Parse.Val

------------------------------------------------------------------------------

exprP :: Parser Expr Expr
exprP =

    letMacroP
        <|> try macroP
        <|> jsExprP
        <|> recExprP
        <|> absExprP absExpr
        <|> matExprP
        <|> try typExprP
        <|> appExprP

letMacroP :: Parser Expr Expr
letMacroP = do
    reservedOp "`" 
    def <- notationP
    reservedOp "`" 
    reservedOp "="
    ms  <- def <$> withCont exprP
    macros %= flip mappend (MacroList [ms])
    withSep exprP

macroP :: Parser Expr Expr
macroP = use macros >>= merge
    where
        merge (MacroList ms) = foldl (<|>) parserZero (fmap tryChild ms)

        tryChild :: Macro Expr -> Parser Expr Expr

        --tryChild (MacroTerm (Token "λ") exs) =
        --    reservedOp "λ" >> merge exs

        --tryChild (MacroTerm (Token "\\") exs) =
        --    reservedOp "\\" >> merge exs

        tryChild (MacroTerm (Token x) exs) =
            reserved x >> merge exs

        tryChild (MacroTerm (Let a) exs) =
            try (replace a <$> symP <*> merge exs) 

        tryChild (MacroTerm (Arg a) exs) =
            try (replace a <$> exprP <*> merge exs) 

        tryChild (MacroTerm Sep exs) =
            withSep (merge exs)

        tryChild (MacroLeaf x) =
            return x

jsExprP :: Parser Expr Expr
jsExprP =
    unwrap
        (reservedOp "``" >> (anyChar `manyTill` reservedOp "``"))
        <?> "Javascript"
    where
        unwrap :: Parser Expr String -> Parser Expr Expr
        unwrap f = do
            f' <- f
            case parseJME f' of
                Left x  -> parserFail (show x)
                Right x -> return (JSExpr x)

absExprP :: Parser Expr () -> Parser Expr Expr
absExprP f =
    pure (AbsExpr (Sym "_match"))
        <*  f
        <*> (pure (MatExpr (VarExpr (SymVal (Sym "_match"))) . (:[])) <*> caseP)
        <?> "Abstraction"

absExpr :: Parser Expr ()
absExpr =
    reserved "fun" <|> reservedOp "\\" <|> reservedOp "λ"

matExprP :: Parser Expr Expr
matExprP =
    pure MatExpr
        <*  reserved "match"
        <*> exprP
        <*  reserved "with"
        <*  indented
        <*> withScope (try ((,) <$> pattP <* toOp <*> exprP) `sepEndBy` sep)
        <?> "Match Expression"

caseP :: Parser Expr (Patt, Expr)
caseP = (,) <$> pattP <* toOp <*> exprP

toOp :: Parser Expr ()
toOp  = reservedOp "->" <|> reservedOp "="

recExprP :: Parser Expr Expr
recExprP =
    RecExpr <$> recordP exprP
        <?> "Record Expression"

typExprP :: Parser Expr Expr
typExprP =
    pure TypExpr
        <*  optional (reserved "data")
        <*> typSymP
        <*  reserved ":"
        <*> typAbsP
        <*> withSep exprP
        <?> "Type Kind Expression"

appExprP :: Parser Expr Expr
appExprP = buildExpressionParser opPs termP <?> "Application"

    where
        opPs =
            [ Infix ap AssocLeft ]
                : toInfixTerm opConst AssocLeft (tail ops)

        toInfixTerm optr assoc =
            fmap . fmap $
                flip Infix assoc
                <<< uncurry (*>)
                <<< reservedOp
                &&& return . optr

        ap = indented >> return AppExpr
        valExprP = VarExpr <$> valP <?> "Value"
        termP = valExprP <|> matExprP <|> macroP <|> parens exprP
        opConst = (AppExpr .) . AppExpr . VarExpr . SymVal . Sym

------------------------------------------------------------------------------
