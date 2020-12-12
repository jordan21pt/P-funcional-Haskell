import Data.List

data Frac = F Integer Integer 

f1 = F 3 2
f2 = F 6 4

mdc :: Integer -> Integer -> Integer
mdc x y
    | x == y = x
    | x<y = mdc x (y-x)
    | x>y = mdc (x-y) y

normaliza :: Frac -> Frac
normaliza (F _ 0) = error "Invalida"
normaliza (F 0 _) = F 0 1
normaliza (F n d) = let n' = abs n
                        d' = abs d
                        k = mdc n' d'
                        s = if (n*d)>0 then 1 else (-1)
                    in F (s*(div n' k)) (div d' k)

instance Eq Frac where
    (==) (F n1 d1) (F n2 d2) = n1*d2 == n2*d1

instance Ord Frac where
    compare (F n1 d1) (F n2 d2)
        | n1 * d2 < n2 * d1 = LT   -- less then
        | n1 * d2 == n2 * d1 = EQ  -- equal
        | n1 * d2 > n2 * d1 = GT   -- greater then
    
instance Show Frac where
    show (F n d) = show n ++ "/" ++ d


instance Num Frac where
    (+) (F n1 d1) (F n2 d2) = normaliza (F (n1*d2+n2*d1) (d1*d2))
    (*) (F n1 d1) (F n2 d2) = normaliza (F (n1*n2) (d1*d2))
    (-) (F n1 d1) (F n2 d2) = normaliza (F (n1*d2-n2*d1) (d1*d2))
    negate (F n d) = normaliza (F (-n) d) 
    abs (F n d) = F (abs n) (abs d)
    signum (F n d) = F (signum (n*d)) 1 
    fromInteger x = F x 1

maioresDobro :: Frac -> [Frac] -> [Frac]
maioresDobro f lf = filter (>(2*f)) lf          -- filter (\f1 -> f1>(2*f)) lf



--------------------------------------------------------------------------------------
data Exp a = Const a 
              | Simetrico (Exp a)
              | Mais (Exp a) (Exp a)
              | Menos (Exp a) (Exp a)
              | Mult (Exp a) (Exp a)

infixa' :: Show a => Exp a -> String
infixa' (Const x) = show x --qq q seja o int é transformado em string
infixa' (Simetrico e) = "-" ++ (infixa' e)
infixa' (Mais e1 e2) = "(" ++ infixa' e1 ++ " + " ++ infixa' e2 ++ ")"
infixa' (Menos e1 e2) = "(" ++ infixa' e1 ++ " - " ++ infixa' e2 ++ ")"
infixa' (Mult e1 e2) = "(" ++ infixa' e1 ++ " * " ++ infixa' e2 ++ ")"

instance Show a => Show (Exp a) where
    show e = infixa' e

instance (Num a, Eq a) => (Exp a) where
    (==) e1 e2 = calcula' e1 == calcula' e2

calcula' :: Num a => Exp a -> a
calcula' (Const x) = x
calcula' (Simetrico e) = negate (calcula' e) 
calcula' (Mais e1 e2) = (calcula' e1) + (calcula' e2)
calcula' (Menos e1 e2) = (calcula' e1) - (calcula' e2)
calcula' (Mult e1 e2) = (calcula' e1) * (calcula' e2)

instance (Num a) => Num (Exp a) where 
    (+) e1 e2 = Const (calcula' e1 + calcula' e2)
    (*) e1 e2 = Const (calcula' e1 * calcula' e2)
    (-) e1 e2 = Const (calcula' e1 - calcula' e2)
    negate e = Const (negate (calcula' e))
    abs e = Const(abs(calcula' e))
    signum e = Const (signum (calcula' e))
    fromInteger x = Const (fromInteger x) 

-------------------------------------------------------
data Movimento = Credito Float | Debito Float deriving Show
data Data = D Int Int Int   -- Dia Mes Ano
data Extracto = Ext Float [(Data, String, Movimento)] deriving Show


ext = Ext 1000 [(D 1 12 2000, "A", Debito 100),
                (D 1 1 2000, "B", Credito 50),
                (D 1 1 1999, "C", Debito 50)]

instance Eq Data where
    (==) (D dia1 mes1 ano1) (D dia2 mes2 ano2) = 
            dia1 == dia2 && mes1 == mes2 && ano1 == ano2

instance Ord Data where
    compare (D dia1 mes1 ano1) (D dia2 mes2 ano2) 
        | (ano1,mes1,dia1) < (ano2,mes2,dia2) = LT
        | (ano1,mes1,dia1) == (ano2,mes2,dia2) = EQ
        | (ano1,mes1,dia1) > (ano2,mes2,dia2) = GT
        
instance Show Data where
    show (D dia mes ano) = show ano ++ "/" ++ show mes ++ "/" ++ show dia

ordena :: Extracto -> Extracto 
ordena (Ext saldoI listaMov) = Ext saldoI (sortOn (\(data,descricao,movimento) -> a) listaMov)

colunas :: String -> Int -> String
colunas s n = take n (s++ replicate n ' ')


instance Show Extracto where
    show (Ext saldoI listaMov) = 
        "Saldo anterior: " ++ show saldoI ++ "\n" ++ 
        replicate (4*12) '-' ++ "\n" ++ 
        colunas "Datas" 12 ++ colunas "Descricao" 12 ++ colunas "Credito" 12 ++ colunas "Debito" 12 ++ "\n" ++
        replicate (4*12) '-' ++ "\n" ++ 
        listaMovimentos listaMov ++ "\n" ++ 
        replicate (4*12) 'n' ++ "\n"
        "Saldo atual: " ++ show (Saldo (Ext saldoI listaMov))


listaMovimentos :: [(Data, String, Movimento)] -> String
listaMovimentos [] = []
listaMovimentos ((data, descricao, Credito x): lm) =
    col (show data) 12 ++ col descricao 12 ++ col (show x) 12 ++ "\n" ++ listaMovimentos listaMov
listaMovimentos ((data, descricao, Debito x): lm) = 
    col (show data) 12 ++ col descricao 12 ++ col (show x) 12 ++ "\n" ++ listaMovimentos listaMov


saldo :: Extracto -> Float
saldo (Ext saldoI listaMov) = saldoI + saldoC - SaldoD
            where = (saldoC, SaldoD) = somaCredDeb listaMov
                    somaCredDeb :: [(Data, String, Movimento)] -> (Float, Float)
                    somaCredDeb [] = (0,0)
                    somaCredDeb ((_,_,m):lm) = 
                        let (saldoC, SaldoD) = somaCredDeb lm
                        in case m of 
                                Credito x -> (saldoC+x, SaldoD)
                                Debito x -> (saldoC, SaldoD+x)









