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
grupa :: Eq a => [a] -> [[a]]
grupa [] = []
grupa (x:xs) = (x : takeWhile (== x) xs) : grupa (dropWhile (== x) xs)

-- 13. desgrupa [[1],[2,2],[3],[4,4,4],[5],[4]] corresponde a [1,2,2,3,4,4,4,5,4]
desgrupa :: [[a]] -> [a]
desgrupa [] = [] 
desgrupa (h:hs) = h ++ desgrupa hs

--14. inits' [11,21,13] corresponde a [[],[11],[11,21],[11,21,13]]
inits' :: [a] -> [[a]]
inits' [] = []
inits' l = inits' (init l) ++ [l] 

--15. tails' [1,2,3] corresponde a [[1,2,3],[2,3],[3],[]]
tails' :: [a] -> [[a]]
tails' [] = [[]]
tails' l = l : tails' (tail l)

--16. isPrefixOf' [10,20] [10,20,30] corresponde a True enquanto que
--isPrefixOf' [10,30] [10,20,30] corresponde a False
isPrefixOf' :: Eq a => [a] -> [a] -> Bool
isPrefixOf' [] _ = True 
isPrefixOf' _ [] = False
isPrefixOf' (x:xs) (y:ys) = if x == y then isPrefixOf' xs ys
                            else False

--17. isSuffixOf' [20,30] [10,20,30] corresponde a True enquanto 
--que isSuffixOf' [10,30] [10,20,30] corresponde a False
isSuffixOf' :: Eq a => [a] -> [a] -> Bool 
isSuffixOf' [] _ = True
isSuffixOf' _ [] = False
isSuffixOf' (x:xs) (y:ys) = if x /= y then isSuffixOf' xs (y:ys)
                            else False

--18. isSubsequenceOf' [20,40] [10,20,30,40] corresponde a True 
--enquanto que isSubsequenceOf' [40,20] [10,20,30,40] corresponde a False
isSubsequenceOf' :: Eq a => [a] -> [a] -> Bool 
isSubsequenceOf' [] _ = True
isSubsequenceOf' _ [] = False
isSubsequenceOf' (x:xs) (y:ys) = if x == y then isSubsequenceOf' xs ys
                                 else isSubsequenceOf' (x:xs) ys

--19. elemIndices' 3 [1,2,3,4,3,2,3,4,5] corresponde a [2,4,6]
elemIndices' :: Eq a => a -> [a] -> [Int]
elemIndices' _ [] = []
elemIndices' x (h:hs) = if x == h then 0 : map (+1) (elemIndices' x hs)
                        else map (+1) (elemIndices' x hs)

--20. nub' [1,2,1,2,3,1,2] corresponde a [1,2,3]
nub' :: Eq a => [a] -> [a]
nub' [] = [] 
nub' (h:hs) = h : filter (/= h) (nub' hs)

--21.delete' 2 [1,2,1,2,3,1,2] corresponde a [1,1,2,3,1,2].
-- Se n˜ao existir nenhuma ocorrˆencia a 
--fun¸c˜ao dever´a retornar a lista recebida
delete' :: Eq a => a -> [a] -> [a]
delete' _ [] = []
delete' x (h:hs) = if x == h then hs 
                    else h: delete' x hs

--22. remLfromL [1,2,3,4,5,1] [1,5] corresponde a [2,3,4,1]
remLfromL :: Eq a => [a] -> [a] -> [a]
remLfromL [] _ = []
remLfromL l [] = l
remLfromL l (y:ys) = remLfromL (delete y l) ys

--23. union' [1,1,2,3,4] [1,5] corresponde a [1,1,2,3,4,5]
union' :: Eq a => [a] -> [a] -> [a] 
union' l [] = l
union' [] l = l
union' l (x:xs) = if x `elem` l then union' l xs
                    else union' (l ++ [x]) xs

--24. intersect' [1,1,2,3,4] [1,3,5] corresponde a [1,1,3]
intersect' :: Eq a => [a] -> [a] -> [a] 
intersect' [] _ = []
intersect' _ [] = []
intersect' (x:xs) l = if x `elem` l then x : intersect' xs l
                        else intersect' xs l 

--25. insert' 25 [1,20,30,40] corresponde a [1,20,25,30,40]
insert' _ [] = []
insert' n (x:xs) 
    | n <= x = n : x : xs
    | otherwise = x : insert' n xs 

--26. unwords' ["Programacao", "Funcional"] corresponde 
--    a "Programacao Funcional".
unwords' :: [String] -> String
unwords' [] = ""
unwords' [l1] = l1
unwords' (l1:ls) = l1 ++ " " ++ unwords' ls

--27. unlines' ["Prog", "Func"] corresponde a "Prog\nFunc\n".
unlines' :: [String] -> String
unlines' [] = ""
unlines' [l1] = l1 ++ "\n"
unlines' (l1:ls) = l1 ++ "\n" ++ unlines' ls



-- 28. As posicoes da lista comecam em 0, i.e., 
--  funcao devera retornar 0 se o primeiro elemento da lista for o maior.
pMaior :: Ord a => [a] -> Int
pMaior (h:hs) = if h == maximum (h:hs) then 0 
                else 1 + pMaior hs

--29. temRepetidos' [11,21,31,21] corresponde a True enquanto 
--que temRepetidos' [11,2,31,4] corresponde a False
temRepetidos' :: Eq a => [a] -> Bool
temRepetidos' [] = False
temRepetidos' (x:xs) = x `elem` xs || temRepetidos' xs

--30. algarismos' "123xp5" corresponde a "1235".
algarismos' :: [Char] -> [Char]
algarismos' [] = []
algarismos' (x:xs)
    | isDigit x = x : algarismos' xs
    | otherwise = algarismos' xs

--31. posImpares' [10,11,7,5] corresponde a [11,5]
posImpares' :: [a] -> [a] 
posImpares' [] = []
posImpares' [x] = []
posImpares' (_:s:xs) = s : posImpares' xs

--32. posPares' [10,11,7,5] corresponde a [10,7]
posPares' :: [a] -> [a]
posPares' [] = []
posPares' [x] = [x]
posPares' (x:_:xs) = x : posPares' xs

--33. isSorted' [1,2,2,3,4,5] corresponde a True, 
--enquanto que isSorted' [1,2,4,3,4,5] corresponde a False
isSorted' :: Ord a => [a] -> Bool 
isSorted' [] = True
isSorted' [_] = True
isSorted' (x:y:xs) = x <= y && isSorted' (y:xs)

--34. que calcula o resultado de ordenar uma lista
iSort' :: Ord a => [a] -> [a]
iSort' [] = []
iSort' (x:xs) = insert' x (iSort' xs)
    where insert' :: Ord a => a -> [a] -> [a]
          insert' x [] = [x]
          insert' x (y:ys) 
            | x <= y = x : y : ys 
            | x > y = y : insert' x ys

--35. menor "sai" "saiu" corresponde a True enquanto que menor "programacao"
-- "funcional" corresponde a False
menor' :: String -> String -> Bool
menor' [] [] = False
menor' [] (_:_) = True
menor' (_:_) [] = False
menor' (x:xs) (y:ys) 
    | x > y = False
    | x == y = menor' xs ys
    | x < y = True

--36. elemMSet' 'a' [('b',2), ('a',4), ('c',1)] corresponde a True enquanto
--que elemMSet' 'd' [('b',2), ('a',4), ('c',1)] corresponde a False
elemMSet' :: Eq a => a -> [(a,Int)] -> Bool
elemMSet' _ [] = False
elemMSet' x ((h,_):hs) 
    | x == h = True
    | x /= h = elemMSet' x hs

--37. lengthMSet' [('b',2), ('a',4), ('c',1)] corresponde a 7.
lengthMSet' :: [(a,Int)] -> Int
lengthMSet' [] = 0
lengthMSet' ((_,t):hs) = t + lengthMSet' hs

--38. converteMSet' [('b',2), ('a',4), ('c',1)] corresponde a "bbaaaac"
converteMSet' :: [(a,Int)] -> [a]
converteMSet' [] = []
converteMSet' ((h,1):hs) = h : converteMSet' hs
converteMSet' ((h,t):hs) = h : converteMSet' ((h,t-1):hs)

--39. insereMSet' 'c' [('b',2), ('a',4), ('c',1)]
--  corresponde a [(’b’,2), (’a’,4), (’c’,2)]
insereMSet' :: Eq a => a -> [(a,Int)] -> [(a,Int)]
insereMSet' x [] = [(x, 1)]
insereMSet' x ((h,t):hs) 
    | x == h = (h,t+1) : hs
    | x /= h = (h,t) : insereMSet' x hs

--40. removeMSet' 'c' [('b',2), ('a',4), ('c',1)] 
-- corresponde a [('b',2), ('a',4)].
removeMSet' :: Eq a => a -> [(a,Int)] -> [(a,Int)]
removeMSet' _ [] = []
removeMSet' x ((h,t):hs) 
    | x /= h = (h,t) : removeMSet' x hs
    | x == h = removeMSet' x hs 

--41. constroiMSet "aaabccc" corresponde a [(’a’,3), (’b’,1), (’c’,3)]
constroiMSet' :: Ord a => [a] -> [(a,Int)] 
constroiMSet' [] = []
constroiMSet' (x:xs) = insereMSet' x (constroiMSet' xs)

--42. que divide uma lista de Either s em duas listas
partitionEithers' :: [Either a b] -> ([a],[b])
partitionEithers' l = (partitionLeft l, partitionRight l)
    where partitionLeft [] = []
          partitionLeft ((Left x):xs) = x : partitionLeft xs
          partitionLeft ((Right _):xs) = partitionLeft xs 
          partitionRight [] = []
          partitionRight ((Right x):xs) = x : partitionRight xs
          partitionRight ((Left _):xs) = partitionRight xs

--43. que colecciona os elementos do tipo a de uma lista
catMaybes' :: [Maybe a] -> [a]
catMaybes' [] = []
catMaybes' ((Just a): xs) = a : catMaybes' xs
catMaybes' (Nothing: xs) = catMaybes' xs

--44. que, dada umaposicao inicial (coordenadas) e uma lista de movimentos, 
-- calcula a posicao final do robot depois 
-- de efectuar essa sequencia de movimentos
data Movimento = Norte | Sul | Este | Oeste
        deriving Show

posicao :: (Int,Int) -> [Movimento] -> (Int,Int) 
posicao (x,y) [Norte] = (x,y+1)
posicao (x,y) [Sul]   = (x,y-1)
posicao (x,y) [Este]  = (x+1,y)
posicao (x,y) [Oeste] = (x-1,y)

--45. que, dadas as posi¸c˜oes
-- inicial e final (coordenadas) do robot, 
-- produz uma lista de movimentos suficientes para que o
-- robot passe de uma posi¸c˜ao para a outra.
caminho :: (Int,Int) -> (Int,Int) -> [Movimento]
caminho (xi,yi) (xf,yf) 
    | xi < xf = Este : caminho (xi+1,yi) (xf,yf)
    | xi > xf = Oeste : caminho (xi-1,yi) (xf,yf)
    | yi < yf = Norte : caminho (xi,yi+1) (xf,yf)
    | yi > yf = Sul : caminho (xi,yi-1) (xf,yf)
    | otherwise = []

--46. testa se uma lista de movimentos
-- s´o ´e composta por movimentos verticais (Norte ou Sul)
vertical :: [Movimento] -> Bool
vertical [] = True
vertical (x:xs) = case x of Este -> False
                            Oeste -> False
                            _ -> vertical xs

--47. dada uma lista nao vazia de posicoes, 
--determina a que esta mais perto da origem 
--(note que as coordenadas de cada ponto sao numeros inteiros)
data Posicao = Pos Int Int
    deriving Show   
maisCentral :: [Posicao] -> Posicao
maisCentral [Pos x y] = Pos x y
maisCentral ((Pos x1 y1): (Pos x2 y2): xs) 
    | (x1^2 + y1^2) <= (x2^2 + y2^2) = maisCentral (Pos x1 y1: xs) 
    | (x1^2 + y1^2) > (x2^2 + y2^2) = maisCentral (Pos x2 y2: xs)

--48. dada uma posicao e uma lista de posicoes, 
--selecciona da lista as posicoes adjacentes a posicao dada
vizinhos :: Posicao -> [Posicao] -> [Posicao] 
vizinhos _ [] = []
vizinhos (Pos x y) ((Pos x1 y1) :xs) = 
    if abs (x - x1) == 1 && y == y1 || abs (y - y1) == 1 && x == x1
    then Pos x1 y1 : vizinhos (Pos x y) xs
    else vizinhos (Pos x y) xs 

--49. testa se todas as posicoes de uma dada lista tem a mesma ordenada
mesmaOrdenada :: [Posicao] -> Bool
mesmaOrdenada [] = True
mesmaOrdenada [Pos _ _] = True
mesmaOrdenada ((Pos x y): (Pos _ y1): xs) = 
    y == y1 && mesmaOrdenada (Pos x y : xs)

--50. nao ha mais do que semaforo nao vermelho
data Semaforo = Verde | Amarelo | Vermelho
        deriving Show

interseccaoOK :: [Semaforo] -> Bool
interseccaoOK l = verificaNvermelhos l <= 1
    where verificaNvermelhos :: [Semaforo] -> Int 
        --funcao que conta o n de nao vermelhos
          verificaNvermelhos [] = 0 
          verificaNvermelhos (Vermelho:t) = verificaNvermelhos t
          verificaNvermelhos (_:t) = 1 + verificaNvermelhos t
