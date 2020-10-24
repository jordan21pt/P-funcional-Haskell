import Data.Char

--1.
--a. que calcula o per´ımetro de uma circunferˆencia, dado o comprimento do seu raio
raio :: Int -> Int
raio r = 2 * pi * r

--b. calcula a distˆancia entre dois pontos no plano Cartesiano. Cada ponto e um par de valores do tipo Double
dist :: Floating a => (a, a) -> (a, a) -> a
dist (x1,y1) (x2,y2) = sqrt ((x1-x2)^2 + (y1-y2)^2)

--c. que recebe uma lista e devolve um par com o primeiro e o ´ultimo elemento dessa lista
primULT :: [a] -> (a,a)
primULT l = (head l, last l)

--d. multiplo m n testa se o n´umero inteiro m e multiplo de n.
multiplo :: Integral a => a -> a -> Bool
multiplo m n = if mod m n == 0

--e. que recebe uma lista e, se o comprimento da lista for ımpar retiralhe o primeiro elemento, caso contrario devolve a propria lista
truncaImpar :: [a] -> [a]
truncaImpar l = if mod x 2 /= 0 then tail l else l
    where x = length l
-- usamos a variavel 'x' para representar o comprimento da lista l

--f.  calcula o maior de dois numeros inteiros.
max2 :: Int a => a -> a -> a
max2 x y = if x > y then x else y

--g. calcula o maior de tres numeros inteiros, usando a funcao max2
max3 :: Int a => a -> a -> a -> a
max3 x y z = max2 x (max2 y z)



--2.
--a. recebe os (3) coeficientes de um polinomio de 2o grau e que calcula o numero de raızes (reais) desse polinomio
nRaizes :: (Ord a, Num p, Floating a) => a -> a -> a -> p
nRaizes a b c = if delta > 0 then 2 else if delta == 0 then 1 else 0
    where delta = sqrt(b^2 -4*a*c)

--b. usando a fun¸c˜ao anterior, recebe os coeficientes do polin´omio e calcula a lista das suas ra´ızes reais
--raizes a b c = if nRaizes =
-- FALTA FAZER ESTA


--3.
type Hora = (Int,Int)

--a.  testar se um par de inteiros representa uma hora do dia valida
horaVal (x, y) = if x >= 0 && x < 24 && y >= 0 && y < 60 then True else False

-- b.  testar se uma hora ´e ou n˜ao depois de outra (comparacao)
horaComp (h1,m1) (h2,m2) | h1 < h2 = False
                         | h1 == h2 && m1 < m2 = False
                         | otherwise = True 

--c. coverter de horas para minutos
horaConv (h,m) = h * 60 + m

--d. converte de minutos para horas
horaConv2 m = (div m 60,mod m 60)

--e. calcula a diferença entre duas horas em minutos
horaDif (h1,m1) (h2,m2) = horaConv (h1-h2,m1-m2) 

--f. acrescentar minutos a uma hora
horaAdd min (h,m) = horaConv2 (horaConv (h,m) + min)


--4.
data Hora = H Int Int deriving (Show,Eq)

--a.  testar se um par de inteiros representa uma hora do dia valida
horaValv2 x y = if x >= 0 && x < 24 && y >= 0 && y < 60 then True else False

--b.  testar se uma hora ´e ou n˜ao depois de outra (comparacao)
horaCompv2 h1 m1 h2 m2 | h1 < h2 = False
                       | h1 == h2 && m1 < m2 = False
                       | otherwise = True 
                       
--c. coverter de horas para minutos
horaConvV2 h m = h * 60 + m

--d. converte de minutos para horas
horaConv2V2 m = H (div m 60) (mod m 60)

--e. calcula a diferença entre duas horas em minutos
horaDifV2 h1 m1 h2 m2 = horaConvV2 (h1-h2) (m1-m2)

--f. acrescentar minutos a uma hora

horaAddV2 min h m = horaConv2V2 (horaConvV2 h m + min)


--5.
data Semaforo = Verde | Amarelo | Vermelho deriving (Show,Eq)

--a. calcula o proximo estado so semaforo
next :: Semaforo -> Semaforo 
next Verde = Amarelo
next Amarelo = Vermelho
next Vermelho = Verde

--b. determina se e obrigatorio parar num semaforo
stop :: Semaforo -> Bool 
stop x = if x == Vermelho then True else False

--c. testa se o estado de dois semaforos num cruzamento e seguro
safe :: Semaforo -> Semaforo -> Bool 
safe x y | x == Verde && y == Verde = False
         | x == Verde && y == Amarelo = False
         | x == Amarelo && y == Verde = False
         | x == Amarelo && y == Amarelo = False
         | otherwise = True



--6.
            -- Cartesiano x y           Polar r a
data Ponto = Cartesiano Double Double | Polar Double Double 
                deriving (Show,Eq)

--a. calcula a distancia de um pontop ao eixo vertical
posx :: Ponto -> Double
posx (Cartesiano x y) = x
posx (Polar r a) = r * (cos a)

--b. calcula a distancia de um pontop ao eixo horizontal
posy :: Ponto -> Double
posy (Cartesiano x y) = x
posy (Polar r a) = r * (sin a)

--c. calcula a distancia de um ponto a origem
raio :: Ponto -> Double 
raio (Cartesiano x y) = sqrt (x^2+y^2)
raio (Polar r a) = r

--d. calcula o angulo entre o vector que liga a origem a um ponto e o eixo horizontal
angulo :: Ponto -> Double 
angulo (Polar r a) = a 
angulo (Cartesiano x y) 
    | y == 0 && x >= 0 = 0
    | y > 0 && x == 0 = pi/2
    | y == 0 && x < 0 = pi
    | y < 0 && x == 0 = pi+(pi/2)
    | y > 0 && x > 0 = atan (y/x)
    | y > 0 && x < 0 = pi - atan(y/(-x))
    | y < 0 && x < 0 = pi + atan (y/x) 
    | y < 0 && x > 0 = 2*pi - atan (-y/x)

--e. calcula a distˆancia entre dois pontos
dist :: Ponto -> Ponto -> Double 
dist p1 p2 = 
    let x1 = posx p1
        x2 = posx p2
        y1 = posy p1
        y2 = posy p2
    in sqrt( (x1-x2)^2 + (y1-y2)^2 ) 


--7.
data Figura = Circulo Ponto Double          -- Circulo p r
            | Rectangulo Ponto Ponto        -- Rectangulo p1 p2
            | Triangulo Ponto Ponto Ponto   -- Triangulo p1 p2 p3
            deriving (Show,Eq)
            
--a. testa se uma figura e um  polıgono
poligono :: Figura -> Bool 
poligono (Circulo p1 r) = r > 0
poligono (Rectangulo p1 p2) = if (posx p1 == posx p2) !! (posy p1 == posy p2) then False else True
poligono (Triangulo p1 p2 p3) =
    let d12 = dist p1 p2 
        d13 = dist p1 p3 
        d23 = dist p2 p3
    in (d12 <= d13 + d23) && (d13 <= d12 + d23) && (d23 <= d13 + d12)

--b. calcula a lista dos vertices de uma figura
vertices :: Figura -> [Ponto]
vertices (Circulo _ _) = []
vertices (Triangulo p1 p2 p3) = [p1,p2,p3]
vertices (Rectangulo p1 p2) = 
    let x1 = posx p1
        x2 = posx p2
        y1 = posy p1
        y2 = posy p2
        
    in [p1,p2,(Cartesiano x2 y1),(Cartesiano x1 y2)]

--c. calcular a area de uma figura
area :: Figura -> Double
area (Triangulo p1 p2 p3) = 
    let a = dist p1 p2 
        b = dist p2 p3 
        c = dist p3 p1 
        s = (a+b+c) / 2 -- semi-perimetro 
    in sqrt (s*(s-a)*(s-b)*(s-c)) -- formula de Heron 
area (Circulo p1 r) = (2 * pi * r )^2
area (Rectangulo p1 p2) = 
    let x1 = posx p1
        x2 = posx p2
        y1 = posy p1
        y2 = posy p2
    in abs(((x1) - (x2)) * ((y1) - (y2)))

--8.
--ord :: Char -> Int  --chr :: Int -> Char 

--a. testa se um Char e uma minuscula
isLower' :: Char -> Bool
isLower' x = x >= 'a' && x < 'z'

isLower'' x = elem x ['a'..'z']    -- Tambem funciona

--c. testa se um Char e uma letra
isAlpha' :: Char -> Bool
isAlpha' x = (x >= 'a' && x < 'z') || (x >= 'A' && x < 'Z')

isAlpha'' x = elem x (['a'..'z'] ++ ['A'++'Z'])     -- Tambem funciona

--d. converte uma letra para a respectiva maiuscula
toUpper' :: Char -> Char
toUpper' x 
        | isLower' x = chr ((ord x) - ((ord 'a') - (ord 'A' )))
        | otherwise = x

