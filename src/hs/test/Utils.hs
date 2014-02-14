{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Utils where

import Control.Exception as E
import Control.Concurrent
import Control.Concurrent.MVar
import Data.Maybe
import Network.HTTP
import Network.URI
import System.IO
import System.Process
import System.Exit
import Test.HUnit
import Text.Parsec
import Test.QuickCheck
import Language.Javascript.JMacro
import Data.FileEmbed
import System.IO.Unsafe
import Data.IORef

import qualified Data.ByteString.UTF8 as BU

import Minml.AST
import Minml.Parse.Token
import Minml.Compile.Prelude
import Minml.Compile.RenderText


import Control.Monad

instance Arbitrary Sym where

    arbitrary = (Sym . (:[])) `fmap` choose ('a', 'z')

instance Arbitrary Lit where

    arbitrary = oneof [ StrLit `fmap` oneof (return `fmap` [ "string1", "string2", "string3" ])
                      , NumLit `fmap` oneof (return `fmap` [ 1, 2, 3, 4, 5, 6, 7 ]) ]

instance Arbitrary Val where

    arbitrary = oneof [ SymVal `fmap` arbitrary
                      , LitVal `fmap` arbitrary ]

instance Arbitrary Expr where

    arbitrary = sized expr'
        where
            expr' 0 = VarExpr `fmap` arbitrary
            expr' n = oneof [ liftM3 LetExpr arbitrary subExprr (Just `fmap` subExprr)
                            , liftM2 AppExpr subExprr subExprr
                            , liftM2 AbsExpr arbitrary subExprr
                            , liftM  VarExpr arbitrary ]

                where subExprr = expr' (n `div` 10)

assertParse :: (Eq a, Show a) => Parser a -> String -> Either Err a -> Assertion
assertParse p a b = assertEqual "" b parseResult
    where
        parseResult = case runParser p emptyState "" a of
            Left x -> Left . Err . show $ x
            Right x -> Right x

assertGenerate :: (ToJExpr a) => a -> Either Err String -> Assertion
assertGenerate a = assertEqual "" gen
    where
        gen = renderText $ toJExpr a 

nodejs x =
  let uri = fromMaybe undefined $ parseURI "http://127.0.0.1:1337"
      args = [ mkHeader HdrContentLength (show$ length x)
             , mkHeader HdrContentType "application/x-www-form-urlencoded" ]
  in do
    result <- E.try $ simpleHTTP (Request uri POST args x) >>= getResponseBody
    case result of
        Right z -> return z
        Left (err :: SomeException) -> do
            evalServer
            nodejs x

server :: Chan ProcessHandle
server = unsafePerformIO newChan

evalServer :: IO ()
evalServer = do

    ch <- newEmptyMVar

    forkOS $ do
        
        (Just std_in', Just std_out', Just std_err', p) <-
            createProcess (proc "node" []) {
                std_in = CreatePipe, 
                std_out = CreatePipe, 
                std_err = CreatePipe,
                close_fds = True
            }

        hPutStrLn std_in' jsServer
        hFlush std_in'
        hSetBuffering std_out' NoBuffering
        status <- hGetContents std_out'
        length (takeWhile (/= '\n') status) `seq` putMVar ch ()
        errors <- hGetContents std_err'
        putStrLn errors
        z <- length status `seq` waitForProcess p
        case z of
            ExitFailure _ -> error "Don't go into an infinite loop!"
            ExitSuccess -> do
                putStrLn "Closed"
                return ()

    takeMVar ch

jsServer :: String
jsServer = BU.toString $(embedFile "src/js/test/evalServer.js")
