{-# LANGUAGE NoImplicitPrelude #-}
-- Всё что угодно, главное, чтобы соответствовало
-- заданию
module ITMOPrelude.Tree where

data Tree a = Nil | Cons a (Tree a) (Tree a) 

emptyTree :: Tree a
emptyTree = Nil

add :: Tree a -> a -> Tree a
add a b = Cons b a Nil

addLeft :: Tree a -> a -> Tree a
addLeft Nil a = add emptyTree a
addLeft (Cons root left right) a = Cons root (addLeft left a) right

addRight :: Tree a -> a -> Tree a
addRight Nil a = add emptyTree a
addRight (Cons root left right) a = Cons root left (addRight right a)
--------------------------------
--		p			   q      -- 
--	   / \            / \     --
--    a   q    ->    p   c    --
--       / \   <-   / \       -- 
--      b   c      a   b      --
--------------------------------
leftTurn :: Tree a -> Tree a
leftTurn (Cons p a (Cons q b c)) = Cons q (Cons p a b) c
leftTurn p = p

rightTurn :: Tree a -> Tree a
rightTurn (Cons q (Cons p a b) c) = Cons p a (Cons q b c)
rightTurn p = p

treeMap :: (a -> b) -> Tree a -> Tree b
treeMap _ Nil = Nil
treeMap f (Cons root left right) = Cons (f root) (treeMap f left) (treeMap f right)
        
treeFoldr :: (a -> b -> b) -> b -> Tree a -> b
treeFoldr f z Nil = z
treeFoldr f z (Cons root left right) = treeFoldr f (f root (treeFoldr f z right)) left
