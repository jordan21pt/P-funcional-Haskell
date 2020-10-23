import Data.Char

--2.
-- a. duplica todos os elementos de uma funcao
dobros :: [Float] -> [Float]
dobros [] = []
dobros (h:hs) = 2*h : dobros hs

-- b.  calcula o nu´mero de vezes 
--    que um caracter ocorre numa string. 
numOcorre :: Char -> String -> Int
numOcorre _ [] = 0
numOcorre x (h:hs) = if x == h then 1 + numOcorre x hs
                                else numOcorre x hs

--c. que testa se uma lista so tem elementos positivos. 
positivos :: [Int] -> Bool
positivos [] = True
positivos (h:hs) = if (h > 0) then positivos hs
                    else False

--d. que retira todos os elementos 
--    nao positivos de uma lista de inteiros
soPos :: [Int] -> [Int]
soPos [] = []
soPos (h:hs) = if h > 0 then h : soPos hs
                else soPos hs


--e. que soma todos os nu´meros negativos da lista de entrada.
--eg :: [Int] -> Int
--eg [] = [] 


--f. devolve os ultimos 3 elementos de uma lista
--   se uma lista tiver menos de 3 elementos devolve a
--   a propria lista
--tresUlt :: [a] -> [a]
--tresUlt l = l 
--tresUlt (e:b:c:d:as) = tresUlt (b:c:d:as)

--g. que calcula a lista de segundas componentes dos pares
segundos :: [(a,b)] -> [b]
segundos [] = [] 
segundos ((_,b):xs) = b : segundos xs 

--h. que testa se um elemento aparece como primeiro 
    --  elem de um conj
nosPrimeiros :: (Eq a) => a -> [(a,b)] -> Bool 
nosPrimeiros _ [] = False
nosPrimeiros x ((a,_):xs) = if x == a then True 
                            else nosPrimeiros x xs

sumTriplos :: (Num a, Num b, Num c) => [(a,b,c)] -> (a,b,c)
sumTriplos [] = (0,0,0)
sumTriplos ((x,y,z):xs) = 
    let (a,b,c) = sumTriplos xs
    in (x+a, y+b, z+c)

--3.
--a. recebe de uma lista os caracteres que sao algarismos
soDigitos' :: [Char] -> [Char] 
soDigitos' [] = []
soDigitos' (h:hs) = if isDigit h then h : soDigitos' hs
                    else soDigitos' hs

--b. conta os caracteres que sao letras minusculas
minusculas :: [Char] -> Int
minusculas [] = 0 
minusculas (h:hs) = if isLower h then 1 + minusculas hs 
                    else minusculas hs

--c. devolve uma lista com os algarismos que ocorrem na 
    -- string pela mesma ordem
nums :: String -> [Int]
nums [] = []
nums (h:hs) = if isDigit h then digitToInt(h) : nums hs
            else nums hs

--4.
type Polinomio = [Monomio] 
type Monomio = (Float,Int)
--a. forma a que (conta n p) indica quantos monomios de grau n existem em p. 
conta :: Int -> Polinomio -> Int
conta _ [] = 0 
conta n ((_,e):rp) 
    | n == e = 1 + conta n rp
    | otherwise = conta n rp 