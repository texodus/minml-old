{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE TemplateHaskell #-}

module Unit.Source where

import Control.Applicative
import Control.Lens
import Control.Monad
import Data.FileEmbed
import Data.Hashable
import Data.Maybe
import Data.Monoid
import Data.Serialize
import GHC.Generics
import System.Directory
import System.IO.Unsafe
import Test.Hspec
import Test.HUnit
import Text.InterpolatedString.Perl6

import qualified Data.ByteString      as B
import qualified Data.ByteString.UTF8 as BU

import Minml.AST
import Minml.Compile
import Minml.Compile.Prelude
import Minml.Compile.RenderText
import Minml.Javascript
import Minml.Serialize
import Minml.TypeCheck

import Utils

data TestRec = TestRec {
    _source  :: String,
    _ast     :: Either Err Expr,
    _types   :: Either Err Expr,
    _js      :: Either Err String,
    _evaled  :: Either Err String,
    _end2end :: Either Err String,
    _name    :: String
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
                it "should execute" $ do
                    answer <- case fmap nodejs $ gold ^. js of
                        Left x -> return $ Left x
                        Right y -> Right `fmap` y
                    assertEqual "" (gold ^. evaled) answer
                it "should run end to end" $ do
                    answer <- case com parsed of
                        Left x -> return $ Left x
                        Right y -> Right `fmap` y
                    assertEqual "" (gold ^. end2end) answer

        Nothing ->
            inProgress' title source pendingWith True


    where
        com q = do
            p <- q
            p' <- typeCheck p
            p'' <- generateJs p'
            p''' <- renderText p''
            return $ nodejs p'''

        getGold = case source `lookup` golds of
            Nothing -> return Nothing
            Just x  -> return $ Just x

parse' :: String -> Either Err Expr
parse' a = fromMaybe undefined . foldl1 mappend . fmap Just . fst <$>
    foldM parse ([], emptyState) [
        ("Prelude", prelude),
        ("Test Case",  a)
    ]

pendingSample title source =
    inProgress' title source pendingWith False

inProgress title source =
    inProgress' title source assertFailure False

inProgress' title source asert gen = do
    let parsed  = parse' source
    let checked = join . fmap typeCheck $ parsed
    let scripted = join . fmap renderText . join . fmap generateJs $ parsed
    let all = join . fmap renderText . join . fmap generateJs $ checked
    let answer = unsafePerformIO $ case fmap nodejs scripted of
            Left x -> return $ Left x
            Right y -> Right `fmap` y
    let allAnswer = unsafePerformIO $ case fmap nodejs all of
            Left x -> return $ Left x
            Right y -> Right `fmap` y

    describe title $ do
        when gen $ it ("\nGenerated new gold record for:\n\n" ++ source) $ do
            B.writeFile ("src/obj/" ++ show (abs $ hash source)) $
                pack (source, TestRec source parsed checked scripted answer allAnswer title)
            assertFailure "Please rerun test suite"
        it "should parse" $ asert $ show parsed
        it "should type check" $ asert $ show checked
        it "should generate javascript" $ asert $ show scripted
        it "should execute" $ asert $ show answer
        it "should run end to end"$ asert $ show allAnswer



-------------------------------------------------------------------------------
