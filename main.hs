module Main where

import AhoCorasick
import Trie
import System.Directory  

ins :: Trie -> IO Trie
ins t = do
    word <- getLine
    if null word 
      then return t
      else ins $ addString t word

promptFile :: IO String
promptFile = do
    putStrLn "Soubor k prohledani:"
    fileName <- getLine
    fileExists <- doesFileExist fileName  
    if fileExists  
      then return fileName
      else do putStrLn "Soubor neexistuje!"
              putStrLn ""
              promptFile  

main :: IO ()
main = do
    putStrLn ""
    putStrLn "----------------------"
    putStrLn "| StringFinder v 1.0 |"
    putStrLn "----------------------"
    putStrLn ""
    putStrLn "Slova k vyhledani (po skonceni 2x odentrujte):"
    t <- ins empty
    fileName <- promptFile
    contents <- readFile fileName
    putStrLn ""
    putStrLn "Vysledek hledani ve formatu (koncovy_index, slovo):"
    mapM_ print (performSearch (toACTrie t) contents)
    _ <- getLine
    return ()
