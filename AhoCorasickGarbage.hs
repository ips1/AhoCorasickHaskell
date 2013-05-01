module AhoCorasickGarbage where

import AhoCorasick

setEdge :: Trie -> Trie -> Trie
setEdge n (Node t s _) = Node t s n


-- Replaces a node with a new one
replaceNode	:: Trie -> String -> Trie -> Trie
replaceNode n [] n1 = n1
replaceNode (Node t s b) (x:xs) n1 = let node = Map.lookup x s
                                     in case node of 
                                          Just n -> Node t (Map.insert x (replaceNode n xs n1) s) b
                                          Nothing -> Node t s b  

-- Debug output --
printTrie :: Trie -> [(Char, String)]
printTrie (Node t m None) = concatMap printNode (Map.toList m)

printNode :: (Char, Trie) -> [(Char, String)]
printNode (c, Node t m b) = (c, printChildren b):concatMap printNode (Map.toList m)

printChildren :: Trie -> String
printChildren (Node t m b) = map fst (Map.toList m)
printChildren None = []
------------------
