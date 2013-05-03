module Trie (Trie(..), fromList, addString, empty) where

import qualified Data.Map as Map

-- Data structure for representing the trie
-- Node consists of ending mark (word ends here) and list of children
data Trie = Node String (Map.Map Char Trie) deriving (Show)


-- Updates the list of children during addition
-- takes map of children, character identifiing new branch, whole word and the rest of the word
updateChildren :: Map.Map Char Trie -> Char -> String -> String -> Map.Map Char Trie
updateChildren m x word xs =
    let isElem = Map.member x m
    in if (not isElem) then Map.insert x (addString' (Node [] Map.empty) word xs) m
                       -- if the character isn't in the map, we insert a new trie with the rest of the word
                       else Map.update (\trie -> Just (addString' trie word xs)) x m
                       -- if there is already a branch with that character, we just update the trie


-- Adds string into a trie
addString :: Trie -> String -> Trie
addString t s = addString' t s s

-- Function for recursive calls - remembers the whole word as well as the uprocessed part of the word
addString' :: Trie -> String -> String -> Trie
-- We've added the whole word, we just mark the Node as ending
addString' (Node _ s) word [] = Node word s
-- Just the list of children gets updated
addString' (Node final s) word (x:xs) = Node final (updateChildren s x word xs)


-- Creates a trie from a list of strings
fromList :: [String] -> Trie
fromList = foldl addString empty


-- Creates an empty trie
empty :: Trie
empty = Node [] Map.empty
