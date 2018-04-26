module BSTree where

data BSTree a = Nil | Node a (BSTree a) (BSTree a) deriving(Eq, Ord, Show) 

build :: Ord a => [a] -> BSTree a
build src = buildTree src Nil

buildTree :: Ord a => [a] -> BSTree a -> BSTree a
buildTree src tr
  | src == [] = tr
  | otherwise = buildTree (drop 1 src) (insert (head src) tr)

insert :: Ord a => a -> BSTree a -> BSTree a
insert x Nil = Node x Nil Nil
insert x (Node a b c)
  | x >= a = Node a b (insert x c)
  | otherwise = Node a (insert x b) c

inorder :: Ord a => BSTree a -> [a]
inorder Nil = []
inorder (Node a b c) = (inorder b) ++ [a] ++ (inorder c) 

search :: Ord a => BSTree a -> a -> Maybe (BSTree a)
search Nil x = Nothing
search (Node a b c) x
  | a == x = Just (Node a b c)
  | x > a = search c x
  | otherwise = search b x

instance Functor BSTree where
  fmap func Nil = Nil
  fmap func (Node a b c) = (Node (func a) (fmap func b) (fmap func c)) 