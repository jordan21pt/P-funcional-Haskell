{-
map:: (a->b) -> [a] -> [b]
map funcao [] = []
map funcao (x:xs) = (funcao x): map funcao xs
fazer alteracao a uma lista


filter :: (a -> Bool) -> [a] -> [a]
filter funcao [] = []
filter funcao (x:xs)
    | funcao x = x : filter funcao xs
    | not (funcao x) = filter funcao xs
deitar fora elementos


foldr :: (a->b->b) -> b -> [a] -> b
foldr funcao i [] = i
foldr funcao i (x:xs) = funcao x (foldr funcao i xs)


-}

--1.
--a.
any' :: (a->Bool) -> [a] -> Bool
any' _ [] = False
any' p (x:xs) 
    | p x = True
    | not (p x) = any' p xs

any'' :: (a->Bool) -> [a] -> Bool
any'' _ [] = False
any'' p (x:xs) = p x || any' p xs

-- funcao extra, nao esta na ficha 5
all' :: (a->Bool) -> [a] -> Bool
all' _ [] = True
all' p (x:xs) = p x && all' p xs
 
--b. 
zipWith' :: (a->b->c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys

--c.
takeWhile' :: (a->Bool) -> [a] -> [a]
takeWhile' _ [] = []
takeWhile' p (x:xs) 
    | p x = x : takeWhile' p xs
    | otherwise = []

--d. 
dropWhile' :: (a->Bool) -> [a] -> [a]
dropWhile' p [] = []
dropWhile' p (x:xs)
    | p x = dropWhile' p xs
    | otherwise = x: xs

--e. 
span' :: (a-> Bool) -> [a] -> ([a],[a])
span' _ [] = ([],[])
span' p (x:xs)
    | not (p x) = ([], x:xs)
    | p x     = let (le,ld) = span' p xs 
                in (x:le,ld)

--f.
deleteBy' :: (a -> a -> Bool) -> a -> [a] -> [a]
deleteBy' f _ [] = []
deleteBy' f x (y:ys) 
    | f x y = ys 
    | otherwise = y : deleteBy' f x ys 

--g. 
sortOn' :: Ord b => (a -> b) -> [a] -> [a]
sortOn' _ [] = []
sortOn' f (x:xs) = insert x (sortOn' f xs)
    where insert x [] = [x]
          insert x (y:ys) 
            | f x <= f y = x:y:ys
            | otherwise = y : insert x ys 



--2.
type Polinomio = [Monomio]
type Monomio = (Float,Int)

--a.
selgrau :: Int -> Polinomio -> Polinomio
selgrau g = filter (\(c,e)-> e==g) 

--b.
conta :: Int -> Polinomio -> Int
conta g p = sum (map (\(c,e)-> if e == g then 1 else 0) p)

conta' :: Int -> Polinomio -> Int
conta' g = foldr aux 0 
    where aux :: Monomio -> Int -> Int 
          aux (c,e) n = if e == g then 1+n else n

conta'' :: Int -> Polinomio -> Int
conta'' g = foldr (\(c,e) n -> if e==g then 1+n else n) 0 


--c.
grau :: Polinomio -> Int
grau p = maximum (map snd p)

--d.
--deriv :: Polinomio -> Polinomio
--deriv p = filter (\(c,e)-> c /= 0) (map (\(c,e)->c*(fromIntegral e),e-1)) p) 

--e.
calcula :: Float -> Polinomio -> Float
calcula v p = sum ( map (\(c,e) -> c*(v^e) ) p)

calcula' :: Float -> Polinomio -> Float
calcula' v = foldr aux 0 
    where aux :: Monomio -> Float -> Float 
          aux (c,e) resultado = c*(v^e) + resultado
          
--h. 
ordena' :: Polinomio -> Polinomio
ordena' = sortOn' snd

ordena'' :: Polinomio -> Polinomio
ordena'' [] = []
ordena'' (x:xs) = ordena'' lmenores ++ [x] ++ ordena'' lmaiores
    where lmenores = filter (\m1 -> snd m1 <= snd x) xs
          lmaiores = filter (\m1 -> snd m1 > snd x) xs


--3.

type Mat a = [[a]]
m = [[1,2,3], [0,4,5], [0,0,6]]
d12 = [[1,2,3], [4,5,6], [7,8,9]]
c12 = [[1,1,1], [2,2,2], [1,1,1]]
--a
dimOK :: Mat a -> Bool
dimOK (l:m) = let n = length l 
              in all (\l1-> length l1 == n) m


--a - a parar logo que der false
dimOK' :: Mat a -> Bool
dimOK' (l:l2:m) = length l == length l2 && dimOK' (l2:m)
dimOK' _ = True

--b
dimMat :: Mat a -> (Int,Int)
dimMat m = (length m , length (head m))

--c. 
addMat :: Num a => Mat a -> Mat a -> Mat a
addMat (l1:m1) (l2:m2) = zipWith (+) l1 l2 : addMat m1 m2
addMat [] [] = []

--c. mais compacto, a funcao zipWith faz logo tudo 
addMat' :: Num a => Mat a -> Mat a -> Mat a
addMat' = zipWith (+)

--addMat'' :: Num a => Mat a -> Mat a -> Mat a
--addMat'' (l1:m1) (l2:m2) = zipWith (\l1 l2 -> (zipWith (+) l1 l2)) m1 m2
--addMat'' [] [] = []

--d.
transpose :: Mat a -> Mat a 
transpose ([]:_) = []
transpose m = map head m : transpose (map tail m)

--f.
zipWMat :: (a -> b -> c) -> Mat a -> Mat b -> Mat c
zipWMat f (l1:m1) (l2:m2) = zipWith f l1 l2: zipWMat f m1 m2
zipWMat _ _ _= []

--f, mais compacta
zipWMat' :: (a -> b -> c) -> Mat a -> Mat b -> Mat c
zipWMat' f = zipWith (zipWith f)

--g.
triSup :: Num a => Mat a -> Bool
triSup [] = True
triSup (l:m) =  all (==0) (map head m) && triSup (map tail m)

--g. outra versao
triSup' :: Num a => Mat a -> Bool
triSup' [] = True
triSup' (l:m) = all (==0) (aux 1 m)
    where aux :: Int -> Mat a -> [a]
          aux k (l:m) = take k l ++ aux (k+1) m
          aux _ [] = []