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
somaNeg :: [Int] -> Int
somaNeg [] = 0
somaNeg (h:hs) = if (h < 0) then h + somaNeg hs
                 else somaNeg hs 

--f. devolve os ultimos 3 elementos de uma lista
--   se uma lista tiver menos de 3 elementos devolve a propria lista
tresUlt :: [a] -> [a]
tresUlt [] = []
tresUlt [h] = [h]
tresUlt [h,y] = [h,y]
tresUlt l = reverse (take 3 (reverse (l)))

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
    
    
    
--c. selgrau
selgrau :: Int -> Polinomio -> Polinomio
selgrau _ [] = []
selgrau g ((c,e):p) 
        | g == e = (c,e) : selgrau g p
        | g /= e = selgrau g p

--d. deriv
deriv :: Polinomio -> Polinomio
deriv [] = []
deriv ((c,e):p) 
        | e /= 0 = (c*(fromIntegral e), e-1) : deriv p  
        | e == 0 = deriv p

--e. calcula 
calcula :: Float -> Polinomio -> Float
calcula _ [] = 0
calcula x ((c,e):p) = (c * (x ^ e)) + calcula x p

--f. simp
simp :: Polinomio -> Polinomio 
simp [] = []
simp ((c,e):p) 
        | c == 0 = simp p 
        | c /= 0 = (c,e) : simp p

--g. mult
mult :: Monomio -> Polinomio -> Polinomio
mult _ [] = []
mult (c,e) ((c1,e1):p) = (c*c1, e+e1) : mult (c,e) p 

--h. normaliza
normaliza :: Polinomio -> Polinomio
normaliza [] = []
normaliza ((c,e):p) = 
    let p1 = selgrau e ((c,e):p)
        c1 = somaCoef p1
        p2 = semgrau e p 
    in if c1 /= 0 then (c1,e) : normaliza p2
        else normaliza p2

somaCoef :: Polinomio -> Float
somaCoef [] = 0
somaCoef ((c,_): p) = c + somaCoef p

semgrau :: Int -> Polinomio -> Polinomio
semgrau _ [] = []
semgrau g ((c,e):p) 
        | g /= e = (c,e) : semgrau g p
        | g == e = semgrau g p

--i. soma
soma :: Polinomio -> Polinomio -> Polinomio
soma p1 p2 = normaliza (p1 ++ p2)

--j. produto
produto :: Polinomio -> Polinomio -> Polinomio
produto _ [] = []
produto [] _ = []
produto (m:p1) p2 = mult m p2 ++ produto p1 p2

produtoF :: Polinomio -> Polinomio -> Polinomio
produtoF p1 p2 = normaliza (produto p1 p2)

--k. ordena
ordena :: Polinomio -> Polinomio
ordena [] = []
ordena (m:p) = insere m (ordena p)
    where insere :: Monomio -> Polinomio -> Polinomio
          insere _ [] = []
          insere (c,e) ((c1,e1) : p) 
                | e < e1 = (c,e) : (c1,e1) : p 
                | e > e1 = (c1,e1) : insere (c,e) p 
                | e == e1 = (c+c1,e) : p

--l. equiv
equiv :: Polinomio -> Polinomio -> Bool
equiv p1 p2 = ordena (normaliza p1) == ordena (normaliza p2)
    
    

