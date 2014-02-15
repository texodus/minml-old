{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

module Main where

import Criterion.Main

import Minml.Parse


import Control.Concurrent
import Control.Exception  as E
import Control.Lens
import Control.Monad
import Data.FileEmbed
import Data.Maybe
import Data.Serialize
import GHC.Generics
import Network.HTTP
import Network.URI
import System.Directory
import System.Exit
import System.IO
import System.IO.Unsafe
import System.Process

import qualified Data.ByteString      as B
import qualified Data.ByteString.UTF8 as BU

import Minml.AST
import Minml.Compile.Prelude
import Minml.Compile.RenderText
import Minml.Optimize
import Minml.Javascript
import Minml.Serialize
import Minml.TypeCheck


data TestRec = TestRec {
    _source  :: String,
    _ast     :: Either Err Expr,
    _types   :: Either Err Expr,
    _js      :: Either Err String,
    _evaled  :: Either Err String,
    _optd    :: Either Err Expr,
    _end2end :: Either Err String,
    _name    :: String
} deriving (Show, Read, Generic)

instance Serialize TestRec

makeLenses ''TestRec

golds :: [(String, TestRec)]
golds = take 10 $ unsafePerformIO $ do
    files <- getDirectoryContents "src/obj"
    files <- foldM filt [] files
    fmap (fmap (conv . unpack)) $ sequence (fmap (B.readFile . ("src/obj/"++)) files)

    where
        conv (Left x) = error "Could not deserialize"
        conv (Right x) = x
        filt acc file = do
            exists <- doesFileExist ("src/obj/" ++ file)
            return $ if exists then file : acc else acc

parse' = nf (show . fmap fst . flip (parseMinml "Benchmark") emptyState)

check' = nf (show . (>>= typeCheck))

optimize' = nf (show . (fmap optimize :: Either Err Expr -> Either Err Expr))

generate' = nf (show . join . fmap renderText . join . fmap generateJs)

nodejs x =
  let uri = fromMaybe undefined $ parseURI "http://127.0.0.1:1337"
      args = [ mkHeader HdrContentLength (show$ length x)
             , mkHeader HdrContentType "application/x-www-form-urlencoded" ]
  in do
    result <- E.try $ simpleHTTP (Request uri POST args x) >>= getResponseBody
    case result of
        Right z -> return z
        Left (err :: SomeException) -> do
            return $ show err

evalServer :: IO ()
evalServer = do

    ch <- newEmptyMVar

    forkIO $ do

        (Just std_in', Just std_out', _, p) <-
            createProcess (proc "node" []) {
                std_in = CreatePipe,
                std_out = CreatePipe,
                close_fds = True
            }

        hPutStrLn std_in' jsServer
        hFlush std_in'
        hSetBuffering std_out' NoBuffering
        status <- hGetContents std_out'
        length (takeWhile (/= '\n') status) `seq` putMVar ch ()
        z <- length status `seq` waitForProcess p
        case z of
            ExitFailure _ -> error "Don't go into an infinite loop!"
            ExitSuccess -> do
                putStrLn "Closed"
                return ()

    takeMVar ch

jsServer :: String
jsServer = BU.toString $(embedFile "src/js/test/evalServer.js")

main :: IO ()
main = do
    evalServer
    defaultMain [
        bgroup "Parsing" $ fmap (\rec -> bench (rec ^. name) $ parse' (rec ^. source)) (fmap snd golds),
        bgroup "Type Checking" $ fmap (\rec -> bench (rec ^. name) $ check' (rec ^. ast)) (fmap snd golds),
        bgroup "Optimizing" $ fmap (\rec -> bench (rec ^. name) $ optimize' (rec ^. ast)) (fmap snd golds),
        bgroup "Generating Javascript" $ fmap (\rec -> bench (rec ^. name) $ generate' (rec ^. optd)) (fmap snd golds),
        bgroup "Executing" $ fmap (\rec -> bench (rec ^. name) $ nfIO (case rec ^. js of Right ex -> nodejs ex; Left err -> return $ show err)) (fmap snd golds)
        ]
