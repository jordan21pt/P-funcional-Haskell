import Data.Char

--3. 
digitAlpha :: String -> (String, String)
digitAlpha [] = ([],[])
digitAlpha (x:xs) = let (ll,ld) = digitAlpha xs
                    in if isDigit x then (ll,x:ld)
                       else if isAlpha x then (x:ll,ld)
                       else (ll,ld)

--3. com acumuladores
digitAlpha' :: String -> (String, String)
digitAlpha' s = parte s ([],[]) 
    where parte :: String -> (String, String) -> (String, String)
          parte [] (ll,ld) = (ll,ld)
          parte (x:xs) (ll,ld) 
                | isDigit x = parte xs (ll,x:ld)  
                | isAlpha x = parte xs (x:ll,ld)
                | otherwise = parte xs (ll,ld)

--4.            
nzp :: [Int] -> (Int,Int,Int)
nzp [] = (0,0,0)
nzp (x:xs) 
    | x < 0 = (neg+1,zeros,pos)
    | x == 0 = (neg,zeros+1,pos)
    | x > 0 = (neg,zeros,pos+1)
    where (neg,zeros,pos) = nzp xs

--4. com acumuladores
nzp' :: [Int] -> (Int,Int,Int)
nzp' l = conta l (0,0,0)
    where conta :: [Int] -> (Int,Int,Int) -> (Int,Int,Int)
          conta [] (neg,zeros,pos) = (neg,zeros,pos)
          conta (x:xs) (neg,zeros,pos)
            | x < 0 = conta xs (neg+1,zeros,pos)
            | x == 0 = conta xs (neg,zeros+1,pos)
            | x > 0 = conta xs (neg,zeros,pos+1)

--5. 
fromDigits' :: [Int] -> Int
fromDigits' [] = 0
fromDigits' (h:t) = let n = length t
                    in calcula n (h:t)

calcula :: Int -> [Int] -> Int
calcula _ [] = 0
calcula n (h:t) = h*10^n +calcula (n-1) t 

---
fromDigits'' :: [Int] -> Int
fromDigits'' l = calc (reverse l)
    where calc :: [Int] -> Int
          calc [] = 0
          calc (h:t) = h + 10 * (calc t)

--5. com acumulador
fromDigits''' :: [Int] -> Int
fromDigits''' l = calc l 0
    where calc :: [Int] -> Int -> Int
          calc [] s = s
          calc (h:t) s = calc t (h+s*10) 
 