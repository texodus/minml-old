{-# LANGUAGE TemplateHaskell #-}

module Unit.Source where

import Control.Monad
import Control.Applicative
import Control.Lens
import Data.Hashable
import System.IO.Unsafe
import System.Directory
import Test.Hspec
import Test.HUnit

import Minml.AST
import Minml.Compile
import Minml.Prelude
import Minml.TypeCheck
import Minml.Javascript
import Minml.RenderText

import Utils

data TestRec = TestRec {
    _source :: String,
    _ast :: Either Err Expr,
    _types :: Either Err Expr,
    _js :: Either Err String,
    _evaled :: Either Err String
} deriving (Show, Read)

makeLenses ''TestRec

golds :: [(String, TestRec)]
golds = unsafePerformIO $ do
    files <- getDirectoryContents "src/obj"
    files <- foldM filt [] files
    fmap (fmap read) $ sequence (fmap (readFile . ("src/obj/"++)) files)

    where
        filt acc file = do
            exists <- doesFileExist ("src/obj/" ++ file)
            return $ if exists then file : acc else acc

sample :: String -> String -> Spec
sample title source = do
    gold <- getGold
    case gold of
        Just gold -> do
            let parsed  = parse' source
            let checked = join . fmap typeCheck $ gold ^. ast
            let scripted = join . fmap renderText . join . fmap generateJs $ gold ^. ast
            it ("should parse " ++ title) $
                assertEqual "" (gold ^. ast) parsed
            it ("should type check " ++ title) $
                assertEqual "" (gold ^. types) checked
            it ("should generate javascript for " ++ title) $
                assertEqual "" (gold ^. js) scripted
            it ("should compile & run " ++ title) $ do
                answer <- case fmap nodejs $ gold ^. js of
                    Left x -> return $ Left x
                    Right y -> Right `fmap` y
                assertEqual "" (gold ^. evaled) answer
        Nothing ->
            it ("Writing new gold record for " ++ title) $ do
                let parsed  = parse' source
                let checked = join . fmap typeCheck $ parsed
                let scripted = join . fmap renderText . join . fmap generateJs $ parsed
                answer <- case fmap nodejs scripted of
                    Left x -> return $ Left x
                    Right y -> Right `fmap` y
                writeFile ("src/obj/" ++ show (abs $ hash source)) $
                    show (source, TestRec source parsed checked scripted answer)
                assertFailure "Generated records, rerun suite"


    where
        getGold = case source `lookup` golds of
            Nothing -> return Nothing
            Just x  -> return $ Just x

        parse' a = head . tail . fst <$> foldM parse ([], emptyState) [("Prelude", prelude), ("Test Case",  a)]


-------------------------------------------------------------------------------
