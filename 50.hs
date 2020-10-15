-- 1. deAte 1 5 corresponde à lista [1,2,3,4,5]
deAte :: Int -> Int -> [Int]
deAte x y = if x > y then [] else x : deAte (x + 1) y

-- 2. deEmAte 1 3 10 corresponde `a lista [1,3,5,7,9].
deEmAte :: Int -> Int -> Int -> [Int]
deEmAte x y z = if x > z then [] else x : deEmAte y (y+(y-x)) z

--3. juntaL [1,2,3] [10,20,30] = [1,2,3,10,20,30]
juntaL :: [a] -> [a] -> [a]
juntaL [] [] = []
juntaL l [] = l
juntaL [] l = l
juntaL (x:xs) (y:ys) = x : juntaL xs (y:ys) 

--4. Encontra [10,20,30] 1 corresponde a 20.
encontra :: [a] -> Int -> a
encontra (h:hs) 0 = h
encontra (h:hs) x = encontra hs (x-1)

--5. inverte [10,20,30] corresponde a [30,20,10].
inverte :: [a] -> [a]
inverte [] = []
inverte (h:hs) = inverte hs ++ [h] 

-- 6. mostra 2 [10,20,30] corresponde a [10,20]
mostra :: Int -> [a] -> [a]
mostra 0 _ = []
mostra _ [] = []
mostra 1 (h:hs) = [h]
mostra x (h:hs) = h : mostra (x-1) hs

-- 7. tira 2 [10,20,30] corresponde a [30]
tira :: Int -> [a] -> [a]
tira 0 l = l
tira _ [] = []
tira 1 (h:hs) = hs
tira x (h:hs) = tira (x-1) hs

-- 8. myZip [1,2,3] [10,20,30,40] corresponde a [(1,10),(2,20),(3,30)].
myZip :: [a] -> [b] -> [(a,b)]
myZip [] _ = []
myZip _ [] = []
myZip (x:xs) (y:ys) = (x,y) : myZip xs ys

-- 9. elemento 20 [10,20,30] corresponde a True enquanto que elem 2 [10,20,30] corresponde a False.
elemento :: Eq a => a -> [a] -> Bool
elemento x [] = False
elemento x (h:hs) = if x == h then True else elemento x hs

-- 10. replica replicate 3 10 corresponde a [10,10,10]
replica :: Int -> a -> [a]
replica 0 y = []
replica x y = y : replica (x-1) y  

-- 11.  colocaEntre 1 [10,20,30] corresponde a [10,1,20,1,30]
colocaEntre :: a -> [a] -> [a]
colocaEntre x [] = []
colocaEntre x [y] = [y]
colocaEntre x (h:hs) = h : x : colocaEntre x hs

-- 12. agrupa [1,2,2,3,4,4,4,5,4] corresponde a [[1],[2,2],[3],[4,4,4],[5],[4]].
--grupa :: Eq a => [a] -> [[a]]
--grupa [] = []
--grupa (x:xs) = 


-- 13. desgrupa [[1],[2,2],[3],[4,4,4],[5],[4]] corresponde a [1,2,2,3,4,4,4,5,4]
desgrupa :: [[a]] -> [a]
desgrupa [] = []
desgrupa [h:hs] =  h : desgrupa [hs]
