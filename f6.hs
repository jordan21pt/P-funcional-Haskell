data BTree a = Empty | 
               Node a (BTree a) (BTree a)
             deriving Show 

--a. 
contaNodos :: BTree a -> Int
contaNodos Empty = 0
contaNodos (Node x e d) = contaNodos e + contaNodos d + 1

--b. 
folhas :: BTree a -> Int
folhas Empty = 0
folhas (Node _ Empty Empty) = 1
folhas (Node x e d) = folhas e + folhas d 

--c.
prune :: Int -> BTree a -> BTree a
prune 0 _ = Empty
prune _ Empty = Empty
prune n (Node x e d) = (Node x (prune (n-1) e) (prune (n-1) d))

--e. 
path :: [Bool] -> BTree a -> [a]
path _ Empty = []
path [] (Node y e d) = [y]  
path (x:xs) (Node y e d) 
    | x = y : path xs d
    | not x = y : path xs e

--f.
mirror :: BTree a -> BTree a
mirror Empty = Empty
mirror (Node x e d) = (Node x (mirror d) (mirror e))


--g.
zipWithBT :: (a -> b -> c) -> BTree a -> BTree b -> BTree c
zipWithBT f (Node x e1 d1) (Node y e2 d2) = 
   Node (f x y) (zipWithBT f e1 e2) (zipWithBT f d1 d2) 
zipWithBT _ _ _ = Empty

--h. 
unzipBT :: BTree (a,b,c) -> (BTree a,BTree b,BTree c)
unzipBT Empty = (Empty,Empty,Empty)
unzipBT (Node (x,y,z) e d) = 
    let (a1,b1,c1) = unzipBT e
        (a2,b2,c2) = unzipBT d
    in (Node x a1 a2, Node y b1 b2, Node z c1 c2)

--2.
--a.
minimo :: Ord a => BTree a -> a
minimo (Node x Empty d) = x
minimo (Node x e d) = minimo e 

--d. 
semMinimo :: Ord a => BTree a -> BTree a
semMinimo (Node x Empty d) = d
semMinimo (Node x e d) = Node x (semMinimo e) d 

minSmin :: Ord a => BTree a -> (a,BTree a)
minSmin (Node x Empty d) = (x,d)
minSmin (Node x e d) = 
    let (m,sm) = minSmin e in (m, Node x sm d)

