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
import Control.Monad.Identity
import Data.Monoid
import Language.Javascript.JMacro
import Text.Parsec.Expr

import Text.Parsec hiding (many, optional, (<|>))

import Forml.AST
import Forml.AST.Replace
import Forml.Parse.Indent
import Forml.Parse.Notation
import Forml.Parse.Macro
import Forml.Parse.Patt
import Forml.Parse.Record
import Forml.Parse.Token
import Forml.Parse.Type
import Forml.Parse.Val

------------------------------------------------------------------------------

exprP :: Parser Expr
exprP =

    letMacroP
        <|> macroP
        <|> jsExprP
        <|> recExprP
        <|> matExprP
        <|> typExprP
        <|> appExprP
        <?> "Expression"

letMacroP :: Parser Expr
letMacroP = withScope $ do
    antiQuote
    def <- notationP
    antiQuote
    reservedOp "="
    ms <- def <$> withCont exprP
    macros %= mappend (MacList [ms])
    withSep exprP

macroP :: Parser Expr
macroP = ($ undefined) . fst <$> (use macros >>= macroPRec exprP . filterP)

jsExprP :: Parser Expr
jsExprP =
    unwrap
        (reservedOp "``" >> (anyChar `manyTill` reservedOp "``"))
        <?> "Javascript"
    where
    unwrap :: Parser String -> Parser Expr
    unwrap f = do
        f' <- f
        case parseJME f' of
            Left x  -> parserFail (show x)
            Right x -> return (JSExpr x)

matExprP :: Parser Expr
matExprP =
    pure MatExpr
        <*  reserved "match"
        <*> exprP
        <*  reserved "with"
        <*> withCont (try caseP `sepEndBy` sep)
        <?> "Match Expression"
    where
        caseP = (,) <$> pattP <* toOp <*> withCont exprP

toOp :: Parser ()
toOp  = reservedOp "->" <|> reservedOp "="

recExprP :: Parser Expr
recExprP =
    RecExpr <$> recordP exprP
        <?> "Record Expression"

typExprP :: Parser Expr
typExprP =
    pure TypExpr
        <*  optional (reserved "data")
        <*> try (typSymP
        <*  reserved ":")
        <*> typAbsP
        <*> capture (withSep exprP)
        <?> "Type Kind Expression"

capture :: Parser Expr -> Parser (Maybe Expr)
capture prsr = Just <$> prsr <|> do
    ms <- use macros
    tailMacros .= ms
    return Nothing

appExprP :: Parser Expr
appExprP = do
    macs <- use macros
    buildExpressionParser (opPs macs) termP <?> "Application"

    where
        opPs (MacList ms) =
            [ Infix appl AssocLeft ]
                : toInfixTerm opConst AssocLeft (tail ops)
                ++ [foldl macOps [] ms]

        toInfixTerm optr assoc =
            fmap . fmap $
                flip Infix assoc
                <<< uncurry (*>)
                <<< reservedOp
                &&& return . optr

        appl = indented >> return AppExpr
        valExprP = VarExpr <$> valP <?> "Value"
        termP = valExprP <|> matExprP <|> macroP <|> parens exprP
        opConst = (AppExpr .) . AppExpr . VarExpr . SymVal . Sym

type OpTable = [Operator String MacroState Identity Expr]

macOps :: OpTable -> Macro Expr -> OpTable
macOps opss (Term (Arg x) (MacList ys)) =
    opss ++ [Postfix $ foldl1 (<|>) (ggg x `fmap` ys)]
    where
        ggg st (Term (Token y) ms) = do
            reservedOp y
            cont <- macroPRec exprP ms
            return (\z -> replace st z . ($ undefined) . fst $ cont)

        ggg _ _ = error "PARADOX: This should not be"

macOps opss _ = opss
    
------------------------------------------------------------------------------
