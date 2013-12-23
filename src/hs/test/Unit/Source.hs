{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}

module Unit.Source where

import Control.Monad
import Control.Applicative
import Control.Lens
import Data.Hashable
import Data.Serialize
import GHC.Generics
import System.IO.Unsafe
import System.Directory
import Test.Hspec
import Test.HUnit

import qualified Data.ByteString as B

import Minml.AST
import Minml.Compile
import Minml.Prelude
import Minml.TypeCheck
import Minml.Javascript
import Minml.RenderText
import Minml.Serialize

import Utils

data TestRec = TestRec {
    _source :: String,
    _ast :: Either Err Expr,
    _types :: Either Err Expr,
    _js :: Either Err String,
    _evaled :: Either Err String
} deriving (Show, Read, Generic)

instance Serialize TestRec

makeLenses ''TestRec

golds :: [(String, TestRec)]
golds = unsafePerformIO $ do
    files <- getDirectoryContents "src/obj"
    files <- foldM filt [] files
    fmap (fmap (conv . unpack)) $ sequence (fmap (B.readFile . ("src/obj/"++)) files)

    where
        conv (Left x) = error "Could not deserialize"
        conv (Right x) = x
        filt acc file = do
            exists <- doesFileExist ("src/obj/" ++ file)
            return $ if exists then file : acc else acc

sample :: String -> String -> Spec
sample title source = do
    gold <- getGold
    case gold of
        Just gold -> do
            let parsed  = either (error "test") id $ unpack . pack . parse' $ source
            let checked = join . fmap typeCheck $ gold ^. ast
            let scripted = join . fmap renderText . join . fmap generateJs $ gold ^. ast
            describe title $ do
                it "should parse" $
                    assertEqual "" (gold ^. ast) parsed
                it "should type check" $
                    assertEqual "" (gold ^. types) checked
                it "should generate javascript" $
                    assertEqual "" (gold ^. js) scripted
                it "should compile & run" $ do
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
                B.writeFile ("src/obj/" ++ show (abs $ hash source)) $
                    pack (source, TestRec source parsed checked scripted answer)
                assertFailure "Generated records, rerun suite"


    where
        getGold = case source `lookup` golds of
            Nothing -> return Nothing
            Just x  -> return $ Just x

        parse' a = head . tail . fst <$> 
            foldM parse ([], emptyState) [
                ("Prelude", prelude), 
                ("Test Case",  a)
            ]


-------------------------------------------------------------------------------
