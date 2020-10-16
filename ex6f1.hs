import Data.Char

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

