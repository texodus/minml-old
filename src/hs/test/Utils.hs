module Utils where

import System.IO
import System.Process
import System.Exit
import Test.HUnit
import Text.Parsec
import Text.Parsec.Pos
import Language.Javascript.JMacro

import Forml.AST
import Forml.Parse.Token
import Forml.RenderText

assertParse :: (Eq a, Show a) => Parser Expr a -> String -> Either Err a -> Assertion
assertParse p a b = flip (assertEqual "") parseResult b
	where
		parseResult = case runParser p (initialPos "", []) "" a of
			Left x -> Left . Err . show $ x
			Right x -> Right x

assertGenerate :: (ToJExpr a) => a -> Either Err String -> Assertion
assertGenerate a b = assertEqual "" gen b
    where
        gen = renderText $ toJExpr a 


node :: String -> IO String
node js = do

    (Just std_in', Just std_out', Just std_err', p) <-
        createProcess (proc "node" []) { std_in = CreatePipe, std_out = CreatePipe, std_err = CreatePipe }

    hPutStrLn std_in' $ js ++ ";process.exit(0);\n"

    z <- waitForProcess p
    status <- hGetContents std_out'
    errs <- hGetContents std_err'
    
    case z of
        ExitFailure n -> return $ "FATAL: error code " ++ show n ++ "\n" ++ errs
        ExitSuccess   -> return $ status
    


