module AhoCorasick (Trie(..), fromList, addString, empty, follow, followBack, isRoot, performSearch, makeBackEdges) where   

import qualified Data.Map as Map

-- Node consists of ending mark (word ends here), list of children and a back edge
data Trie = None | Node [String] (Map.Map Char Trie) Trie deriving (Show)


-- Updates the list of children during addition
-- takes map of children, character identifiing new branch, whole word and the rest of the word
updateChildren :: (Map.Map Char Trie) -> Char -> String -> String -> (Map.Map Char Trie)
updateChildren m x word xs = let elem = Map.member x m
                                 m1 = if (not elem) then Map.insert x (addString' (Node [] Map.empty None) word xs) m
                                                       -- if the character isn't in the map, we insert a new trie with the rest of the word
                                                    else Map.update (\trie -> Just (addString' trie word xs)) x m
                                                       -- if there is already a branch with that character, we just update the trie
                             in m1


-- Adds string into a trie
addString :: Trie -> String -> Trie
addString t s = addString' t s s

-- Function for recursive calls - remembers the whole word as well as the uprocessed part of the word
addString' :: Trie -> String -> String -> Trie
-- We've added the whole word, we just mark the Node as ending
addString' (Node _ s b) word [] = (Node [word] s b)
-- Just the list of children gets updated
addString' (Node final s b) word (x:xs) = (Node final (updateChildren s x word xs) b)

                                                        
-- Follows edge specified by character
follow :: Trie -> Char -> Maybe Trie
follow (Node _ s _) c = Map.lookup c s


-- Follows back edge
followBack :: Trie -> Trie
-- If no back edge is present (should be just root), return myself
followBack this@(Node _ _ None) = this
followBack (Node _ _ b) = b


-- True if the node has no back edge (should be just root)
isRoot :: Trie -> Bool
isRoot (Node _ _ None) = True
isRoot _ = False


-- Creates a trie from a list of strings
fromList :: [String] -> Trie
fromList = foldl addString empty


-- Creates an empty trie
empty :: Trie
empty = Node [] Map.empty None



-- BACK EDGES CONSTRUCTION --

-- Makes back edges for a trie
makeBackEdges :: Trie -> Trie
makeBackEdges t = nt
  where nt = construct t nt

-- Constructs back edges from an old trie pointing to the new trie
-- supposed to be called with return value as the second argument - creates recursive structure
-- due to a lazy evaluation
construct :: Trie -> Trie -> Trie
construct (Node t s None) new = Node t (Map.mapWithKey (dfsSet new) s) None

-- Takes a father node, son node and an according character
-- creates a back edge from the son, list of strings to report in the son
-- and calls itself on all the children of the son
dfsSet :: Trie -> Char -> Trie -> Trie
dfsSet father c (Node t m b) = newNode
  where newNode = Node (collectStrings (firstEdge father) ++ t) m1 (firstEdge father)
        firstEdge father | isRoot father = father
                         | otherwise = findBackEdge (followBack father) c
        m1 = Map.mapWithKey (dfsSet newNode) m

-- Finds a back edge for a father node and a character
findBackEdge :: Trie -> Char -> Trie
findBackEdge root@(Node t s None) c = case follow root c of Just k -> k
                                                            Nothing -> root
findBackEdge node c = case follow node c of Just k -> k
                                            Nothing -> findBackEdge (followBack node) c

-- The node has some words to report
isFinal :: Trie -> Bool
isFinal (Node t _ _) = null t

-- Collects the words from a node
collectStrings :: Trie -> [String]
collectStrings (Node _ _ None) = []
collectStrings (Node t s b) = t



-- AHO-CORASICK ALGORITHM --
-- One step of the search algorithm
ahoStep :: Trie -> Char -> Trie
ahoStep node c = case follow node c of Just k -> k     -- We can extend the match in current branch of trie
                                       Nothing -> if isRoot node then node   -- We follow the back edge to the root
                                                  else ahoStep (followBack node) c  -- We follow the back edge somewhere else

-- Main search function, accumulates the results
-- aho trie sourceText positionInText accumulatedResults  
aho :: Trie -> String -> Int -> [(Int, String)] -> [(Int, String)]
aho (Node words _ _) [] n found = found ++ (map (\word -> (n, word)) words)    -- The source text is exhausted, we return the accumulator
aho node@(Node words _ _) (x:xs) n found = aho newNode xs (n+1) newFound
  where newNode = ahoStep node x   -- One step in the trie
        newFound = found ++ (map (\word -> (n, word)) words)  -- Record all the matches in current position

-- Searches for the patterns in trie in given string
performSearch :: Trie -> String -> [(Int, String)]
performSearch node s = aho node s 0 []
