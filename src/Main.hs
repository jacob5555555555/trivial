module Main where

import Data.List as List
import Data.Map as Map
--import System.Console.ANSI
import System.IO
import Control.Monad
import System.Environment as Env

import Lang
import Test
import Util
import Parse
import Print

main :: IO()
main = do args <- Env.getArgs
          case args of
            [] -> do
                _ <- runProg emptyNameMap emptyProgram
                return ()
            [filename] -> do
                fileRes <- parseFile filename
                case fileRes of
                  Left err -> putStrLn $ "Error parsing file " ++ filename ++ ": " ++ err
                  Right (names, prog) -> do
                      runProg names prog
                      return ()
            _ -> do
                name <- Env.getProgName
                putStrLn ("usage: " ++ name ++ " [filename]")
          return ()

parseFile :: String -> IO (Either String (NameMap, Program))
parseFile name = do
    contents <- readFile name
    let progLines = Prelude.filter (/= "") $ lines contents
    return $ foldM addLineToProg (emptyNameMap, emptyProgram) progLines


addLineToProg :: (NameMap, Program) -> String -> Either String (NameMap, Program)
addLineToProg (names, prog) input =
    case evalPrompt names prog input of
      Left err -> Left err
      Right (Right (newNames, ex)) -> Left ("Can't have plain expression in file: " ++ printEx newNames ex)
      Right (Left (newNames, eq)) -> Right (newNames, addToProg eq prog)



runProg :: NameMap -> Program -> IO Program
runProg names prog= do
    input <- prompt names prog
    case input of
      Left (newNames, eq) ->
          runProg newNames (addToProg eq prog)
      Right (newNames, ex) ->
          do putStrLn $ printEx newNames (reduceProg prog ex)
             runProg newNames prog


prompt :: NameMap -> Program -> IO (Either (NameMap, Equality) (NameMap, Expression))
prompt names prog= do
    putStr "> "
    hFlush stdout
    a <- getLine
    let res = evalPrompt names prog a
    case res of
      Left err -> do putStrLn $ "Error: " ++ err
                     prompt names prog
      Right (Right (newNames, ex)) -> return $ Right (newNames, ex)
      Right (Left (newNames, eq)) -> return $ Left (newNames, eq)

evalPrompt :: NameMap -> Program -> String -> Either String (Either (NameMap, Equality)  (NameMap, Expression))
evalPrompt names prog s =
    case splitBy '=' s of
      Nothing -> case parse names s of
                   Left err -> Left err
                   Right res -> Right (Right res)

      Just (s1, s2) -> case parse names s1 of
                         Left err -> Left err
                         Right (names1, e1) ->
                             case parse names1 s2 of
                               Left err -> Left err
                               Right (names2, e2) -> Right (Left (names2, Equal reducedE1 reducedE2)) where
                                   reducedE1 = reduceProg prog e1
                                   reducedE2 = reduceProg prog e2




splitBy :: Eq a => a -> [a] -> Maybe ([a], [a])
splitBy e l =
    let splitWithExtra = fmap (\i -> List.splitAt i l) (List.elemIndex e l)
        removeExtra :: ([a], [b]) -> ([a], [b])
        removeExtra (a, _:b) = (a, b)
    in fmap removeExtra splitWithExtra
