import Data.Char

dist :: Floating a => (a, a) -> (a, a) -> a
dist (x1,y1) (x2,y2) = sqrt ((x1-x2)^2 + (y1-y2)^2)

truncaImpar :: [a] -> [a]
truncaImpar l = if mod x 2 /= 0 then tail l else l
    where x = length l
-- usamos a variavel 'x' para representar o comprimento da lista l

multiplo :: Integral a => a -> a -> Bool
multiplo m n = if mod m n == 0

--max2 :: Int -> Int -> Int
--max2 x y = if x > y then x else y

--max3 x y z = max2 x (max2 y z)

-- //////////////////////////////////////////////////////////////////////////////

nRaizes :: (Ord a, Num p, Floating a) => a -> a -> a -> p
nRaizes a b c = if delta > 0 then 2 else if delta == 0 then 1 else 0
    where delta = sqrt(b^2 -4*a*c)

--raizes a b c = if nRaizes =
-- FALTA FAZER ESTA

type Hora = (Int,Int)
data Hora = H Int Int deriving (Show,Eq)

--hora Valida
horaVal (x, y) = if x >= 0 && x < 24 && y >= 0 && y < 60 then True else False


horaValv2 x y = if x >= 0 && x < 24 && y >= 0 && y < 60 then True else False
--

-- compara se a hora1 e dps da hora2
horaComp (h1,m1) (h2,m2) | h1 < h2 = False
                         | h1 == h2 && m1 < m2 = False
                         | otherwise = True 


horaCompv2 h1 m1 h2 m2 | h1 < h2 = False
                       | h1 == h2 && m1 < m2 = False
                       | otherwise = True 

-- converte de horas para minutos
horaConv (h,m) = h * 60 + m

horaConvV2 h m = h * 60 + m
--


--converte de minutos para horas
horaConv2 m = (div m 60,mod m 60)

horaConv2V2 m = H (div m 60) (mod m 60)
--


--calcula a diferença entre duas horas em minutos
horaDif (h1,m1) (h2,m2) = horaConv (h1-h2,m1-m2) 

horaDifV2 h1 m1 h2 m2 = horaConvV2 (h1-h2) (m1-m2)
     

-- acrescentar minutos a uma hora
horaAdd min (h,m) = horaConv2 (horaConv (h,m) + min)

horaAddV2 min h m = horaConv2V2 (horaConvV2 h m + min)
--


-- //////////////////////////////////////////////////////////////////
data Semaforo = Verde | Amarelo | Vermelho deriving (Show,Eq)

next :: Semaforo -> Semaforo 
next Verde = Amarelo
next Amarelo = Vermelho
next Vermelho = Verde

stop :: Semaforo -> Bool 
stop x = if x == Vermelho then True else False

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


posx :: Ponto -> Double
posx (Cartesiano x y) = x
posx (Polar r a) = r * (cos a)

posy :: Ponto -> Double
posy (Cartesiano x y) = x
posy (Polar r a) = r * (sin a)

raio :: Ponto -> Double 
raio (Cartesiano x y) = sqrt (x^2+y^2)
raio (Polar r a) = r

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
            
poligono :: Figura -> Bool 
poligono (Circulo p1 r) = r > 0
poligono (Rectangulo p1 p2) = if (posx p1 == posx p2) !! (posy p1 == posy p2) then False else True
poligono (Triangulo p1 p2 p3) =
    let d12 = dist p1 p2 
        d13 = dist p1 p3 
        d23 = dist p2 p3
    in (d12 <= d13 + d23) && (d13 <= d12 + d23) && (d23 <= d13 + d12)

vertices :: Figura -> [Ponto]
vertices (Circulo _ _) = []
vertices (Triangulo p1 p2 p3) = [p1,p2,p3]
vertices (Rectangulo p1 p2) = 
    let x1 = posx p1
        x2 = posx p2
        y1 = posy p1
        y2 = posy p2
        
    in [p1,p2,(Cartesiano x2 y1),(Cartesiano x1 y2)]

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

isLower' :: Char -> Bool
isLower' x = x >= 'a' && x < 'z'

--isLower'' x = elem x ['a'..'z'] -> Tambem funciona

isAlpha' :: Char -> Bool
isAlpha' x = (x >= 'a' && x < 'z') || (x >= 'A' && x < 'Z')

-- isAlpha' x = elem x (['a'..'z'] ++ ['A'++'Z'])

toUpper' :: Char -> Char
toUpper' x 
        | isLower' x = chr ((ord x) - ((ord 'a') - (ord 'A' )))
        | otherwise = x

