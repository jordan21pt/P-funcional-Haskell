--3.
data Contacto = Casa Integer
            | Trab Integer
            | Tlm Integer
            | Email String
            deriving Show

type Nome = String
type Agenda = [(Nome, [Contacto])]

--a.
ag = [("A", [Casa 9999, Tlm 8888]),
      ("B", [Email "bemail@gmail.com", Trab 5555])]

acrescEmail :: Nome -> String -> Agenda -> Agenda
acrescEmail nome email [] = [(nome,[Email email])]
acrescEmail nome email ((nome1, contacto1):ag) 
    | nome == nome1 = (nome,(Email email):contacto1):ag
    | nome /= nome1 = (nome1, contacto1) : acrescEmail nome email ag

verEmails :: Nome -> Agenda -> Maybe [String]
verEmails nome [] = Nothing
verEmails nome ((nome1, contacto1):ag)
    | nome == nome1 = Just (getEmails contacto1)
    | nome /= nome1 = verEmails nome ag
    where getEmails :: [Contacto] -> [String]
          getEmails [] = []
          getEmails ((Email email):contacto1) = email: getEmails contacto1
          getEmails (_:contacto1) = getEmails contacto1

--c.

consTelefs :: [Contacto] -> [Integer]
consTelefs [] = []
consTelefs ((Casa t): listaCont) = t : consTelefs listaCont
consTelefs ((Tlm t): listaCont) = t : consTelefs listaCont
consTelefs ((Trab t): listaCont) = t : consTelefs listaCont
consTelefs (_: listaCont) = consTelefs listaCont

--c. com cases

consTelefs' :: [Contacto] -> [Integer]
consTelefs' [] = []
consTelefs' (x: listaCont) 
    case x of
    (Casa t) -> t : consTelefs' listaCont
    (Trab t) -> t : consTelefs' listaCont
    (Tlm t) -> t : consTelefs' listaCont
    _ -> consTelefs' listaCont


--d.

casa :: Nome -> Agenda -> Maybe Integer
casa nome [] = Nothing
casa nome ((nome1, lc):ag) 
    | nome /= nome1 = casa nome ag
    | nome == nome1 = getCasa lc 
    where getCasa :: [Contacto] -> Maybe Integer
          getCasa [] = Nothing
          getCasa ((Casa t): lc) = Just t
          getCasa ((_):lc) = getCasa lc 

