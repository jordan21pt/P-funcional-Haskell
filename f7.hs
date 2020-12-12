data ExpInt = Const Int 
              | Simetrico ExpInt
              | Mais ExpInt ExpInt
              | Menos ExpInt ExpInt
              | Mult ExpInt ExpInt

e1 = Mult (Const 4) (Mais (Simetrico (Const 2)) (Const 3))

calcula :: ExpInt -> Int
calcula (Const x) = x
calcula (Simetrico e) = -(calcula e) 
calcula (Mais e1 e2) = (calcula e1) + (calcula e2)
calcula (Menos e1 e2) = (calcula e1) - (calcula e2)
calcula (Mult e1 e2) = (calcula e1) * (calcula e2)

infixa :: ExpInt -> String
infixa (Const x) = show x --qq q seja o int é transformado em string
infixa (Simetrico e) = "-" ++ (infixa e)
infixa (Mais e1 e2) = "(" ++ infixa e1 ++ " + " ++ infixa e2 ++ ")"
infixa (Menos e1 e2) = "(" ++ infixa e1 ++ " - " ++ infixa e2 ++ ")"
infixa (Mult e1 e2) = "(" ++ infixa e1 ++ " * " ++ infixa e2 ++ ")"

posfixa :: ExpInt -> String
posfixa (Const x) = show x --qq q seja o int é transformado em string
posfixa (Simetrico e) = (posfixa e) ++ " - "
posfixa (Mais e1 e2) = posfixa e1 ++ " " ++ posfixa e2 ++ " + "
posfixa (Menos e1 e2) = posfixa e1 ++ " " ++ posfixa e2 ++ " - "
posfixa (Mult e1 e2) =  posfixa e1 ++ " " ++ posfixa e2 ++ " * "