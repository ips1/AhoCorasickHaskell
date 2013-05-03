module AhoCorasick (ACTrie, performSearch, toACTrie) where   

import qualified Data.Map as Map
import Trie

-- Data structure for representing the Aho-Corasick trie with back edges
-- None constructor defines no trie, it is used as a back edge of the root
-- ACNode constructor takes list of strings to report in the node, map of children and a back edge
-- the structure itself will be built using the "tying-the-knot" principle
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
followBack None = error "Function followBack called on non-existing trie!"


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
construct (Node _ s) new = ACNode [] (Map.mapWithKey (dfsSet new) s) None

-- Takes a father node, son node and an according character
-- creates a back edge from the son, list of strings to report in the son
-- and calls itself on all the children of the son
dfsSet :: ACTrie -> Char -> Trie -> ACTrie
dfsSet father c (Node t m) = newNode
  where newNode = ACNode newWords m1 (firstEdge father)
        newWords = if null t then collectStrings (firstEdge father) else (t:(collectStrings (firstEdge father)))
        firstEdge n | isRoot n = n      -- If father is root, we don't follow any forward edges (it would return us)
                    | otherwise = findBackEdge (followBack n) c     -- Otherwise, we follow back edge from the father and find first forward in chain of back edges
        m1 = Map.mapWithKey (dfsSet newNode) m      -- Recursive call on all children (with new self as father parameter)

-- Finds a back edge for a father node and a character
findBackEdge :: ACTrie -> Char -> ACTrie
findBackEdge root@(ACNode _ _ None) c = case follow root c of Just k -> k   -- Can continue from the root using the specified character, target is the back edge
                                                              Nothing -> root   -- Can't continue, no more back edges to follow, root is the new back edge
findBackEdge node c = case follow node c of Just k -> k     -- Can continue from the node using the specified character, target is the back edge
                                            Nothing -> findBackEdge (followBack node) c     -- Can't continue, follow back edge and try again


-- Collects the words from a node
collectStrings :: ACTrie -> [String]
collectStrings (ACNode _ _ None) = []
collectStrings (ACNode t _ _) = t
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
aho (ACNode endingWords _ _) [] n found = found ++ (map (\word -> (n, word)) endingWords)    -- The source text is exhausted, we return the accumulator
aho node@(ACNode endingWords _ _) (x:xs) n found = aho newNode xs (n+1) newFound
  where newNode = ahoStep node x   -- One step in the trie
        newFound = found ++ (map (\word -> (n, word)) endingWords)  -- Record all the matches in current position
aho None _ _ _ = error "Function aho called on non-existing trie!"

-- Searches for the patterns in trie in given string
performSearch :: ACTrie -> String -> [(Int, String)]
performSearch node s = aho node s 0 []
