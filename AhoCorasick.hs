module AhoCorasick (ACTrie, performSearch, toACTrie) where   

import qualified Data.Map as Map
import Trie


data ACTrie = None | ACNode [String] (Map.Map Char ACTrie) ACTrie deriving (Show)

                                                        
-- Follows edge specified by character
follow :: ACTrie -> Char -> Maybe ACTrie
follow (ACNode _ s _) c = Map.lookup c s
follow None _ = Nothing


-- Follows back edge
followBack :: ACTrie -> ACTrie
-- If no back edge is present (should be just root), return myself
followBack this@(ACNode _ _ None) = this
followBack (ACNode _ _ b) = b
followBack None = error "Shouldn't happen"


-- True if the node has no back edge (should be just root)
isRoot :: ACTrie -> Bool
isRoot (ACNode _ _ None) = True
isRoot _ = False

-- BACK EDGES CONSTRUCTION --

-- Makes back edges for a trie
toACTrie :: Trie -> ACTrie
toACTrie t = nt
  where nt = construct t nt

-- Constructs back edges from an old trie pointing to the new trie
-- supposed to be called with return value as the second argument - creates recursive structure
-- due to a lazy evaluation
construct :: Trie -> ACTrie -> ACTrie
construct (Node t s) new = ACNode [] (Map.mapWithKey (dfsSet new) s) None

-- Takes a father node, son node and an according character
-- creates a back edge from the son, list of strings to report in the son
-- and calls itself on all the children of the son
dfsSet :: ACTrie -> Char -> Trie -> ACTrie
dfsSet father c (Node t m) = newNode
  where newNode = ACNode words m1 (firstEdge father)
        words = if null t then collectStrings (firstEdge father) else (t:(collectStrings (firstEdge father)))
        firstEdge father | isRoot father = father
                         | otherwise = findBackEdge (followBack father) c
        m1 = Map.mapWithKey (dfsSet newNode) m

-- Finds a back edge for a father node and a character
findBackEdge :: ACTrie -> Char -> ACTrie
findBackEdge root@(ACNode t s None) c = case follow root c of Just k -> k
                                                              Nothing -> root
findBackEdge node c = case follow node c of Just k -> k
                                            Nothing -> findBackEdge (followBack node) c

-- The node has some words to report
isFinal :: ACTrie -> Bool
isFinal (ACNode t _ _) = null t
isFinal None = False

-- Collects the words from a node
collectStrings :: ACTrie -> [String]
collectStrings (ACNode _ _ None) = []
collectStrings (ACNode t s b) = t
collectStrings None = []



-- AHO-CORASICK ALGORITHM --
-- One step of the search algorithm
ahoStep :: ACTrie -> Char -> ACTrie
ahoStep node c = case follow node c of Just k -> k     -- We can extend the match in current branch of trie
                                       Nothing -> if isRoot node then node   -- We follow the back edge to the root
                                                  else ahoStep (followBack node) c  -- We follow the back edge somewhere else

-- Main search function, accumulates the results
-- aho trie sourceText positionInText accumulatedResults  
aho :: ACTrie -> String -> Int -> [(Int, String)] -> [(Int, String)]
aho (ACNode words _ _) [] n found = found ++ (map (\word -> (n, word)) words)    -- The source text is exhausted, we return the accumulator
aho node@(ACNode words _ _) (x:xs) n found = aho newNode xs (n+1) newFound
  where newNode = ahoStep node x   -- One step in the trie
        newFound = found ++ (map (\word -> (n, word)) words)  -- Record all the matches in current position
aho None _ _ _ = error "Shouldn't happen!"

-- Searches for the patterns in trie in given string
performSearch :: ACTrie -> String -> [(Int, String)]
performSearch node s = aho node s 0 []
