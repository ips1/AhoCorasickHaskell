module AhoCorasickGarbage where

import AhoCorasick


-- TRIE GARBAGE --

-- Returns a node where the word ends
findNode :: Trie -> String -> Maybe Trie
findNode n [] = Just n
findNode (Node _ s) (x:xs) = case (Map.lookup x s) of (Just k) -> findNode k xs
                                                      (Nothing) -> Nothing

-- Converts the trie into a list
toList :: Trie -> [String]
toList (Node [] s) = foldr (++) [] (map (dfs "") (Map.toList s))
toList (Node _ s) = []:(foldr (++) [] (map (dfs "") (Map.toList s)))

dfs :: String -> (Char, Trie) -> [String]
dfs acc (c, (Node [] s)) = (foldr (++) [] (map (dfs acc2) (Map.toList s)))
  where acc2 = c:acc
dfs acc (c, (Node _ s)) = (reverse acc2):(foldr (++) [] (map (dfs acc2) (Map.toList s)))
  where acc2 = c:acc

-- END OF TRIE GARBAGE --


setEdge :: Trie -> Trie -> Trie
setEdge n (Node t s _) = Node t s n

-- The node has some words to report
isFinal :: ACTrie -> Bool
isFinal (ACNode t _ _) = null t
isFinal None = False

-- Replaces a node with a new one
replaceNode	:: Trie -> String -> Trie -> Trie
replaceNode n [] n1 = n1
replaceNode (Node t s b) (x:xs) n1 = let node = (Map.lookup x s)
                                     in case node of 
                                          Just n -> Node t (Map.insert x (replaceNode n xs n1) s) b
                                          Nothing -> Node t s b  

-- Debug output --
printTrie :: Trie -> [(Char, String)]
printTrie (Node t m None) = foldr (++) [] (map printNode (Map.toList m))

printNode :: (Char, Trie) -> [(Char, String)]
printNode (c, (Node t m b)) = (c, printChildren b):(foldr (++) [] (map printNode (Map.toList m)))

printChildren :: Trie -> String
printChildren (Node t m b) = map fst (Map.toList m)
printChildren None = []
------------------
