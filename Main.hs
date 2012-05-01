{-# LANGUAGE DeriveDataTypeable #-}
module Main where

import Hython.Core.Interpret
import Hython.Parser.Parser
import Hython.Parser.Lexer
import System.Console.CmdArgs

data Config = Config
  { file :: FilePath
  , lexStage :: Bool
  , parseStage :: Bool
  } deriving (Data, Typeable, Show, Eq)

main = do
  op <- cmdArgs $ Config { file = def &= argPos 0 &= typ "FILE"
                         , lexStage = def &= typ "LEX-STAGE"
                         , parseStage = def &= typ "PARSE-STAGE"
                        } &= help "A little interpreter with Python-like grammer" &= details ["Specify - for FILE to read source code from stdin.", "-l\tStop after the lex stage", "-p\tStop after the parse stage"]
  c <- if file op == "-" then getContents else readFile (file op)
  if lexStage op
     then case lexer c of
       Left err -> print err
       Right ts -> mapM_ print ts
     else if parseStage op
             then case parser c of
               Left err -> print err
               Right ts -> mapM_ print ts
             else case parser c of
               Left err -> print err
               Right ts -> runInterpreter ts >> return ()
