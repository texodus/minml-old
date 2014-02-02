{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Criterion.Main

import Minml.Parse


import Control.Monad
import Control.Lens
import Data.Serialize
import GHC.Generics
import System.Directory
import Control.Exception as E
import Control.Concurrent
import Data.Maybe
import Network.HTTP
import Network.URI
import System.IO
import System.Process
import System.Exit
import Data.FileEmbed
import System.IO.Unsafe

import qualified Data.ByteString as B
import qualified Data.ByteString.UTF8 as BU

import Minml.AST
import Minml.Prelude
import Minml.TypeCheck
import Minml.Javascript
import Minml.RenderText
import Minml.Serialize

data TestRec = TestRec {
    _source :: String,
    _ast :: Either Err Expr,
    _types :: Either Err Expr,
    _js :: Either Err String,
    _evaled :: Either Err String,
    _name :: String
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
        bgroup "Generating Javascript" $ fmap (\rec -> bench (rec ^. name) $ generate' (rec ^. ast)) (fmap snd golds),
        bgroup "Executing" $ fmap (\rec -> bench (rec ^. name) $ nfIO (case rec ^. js of Right ex -> nodejs ex; Left err -> return $ show err)) (fmap snd golds)
        ]
