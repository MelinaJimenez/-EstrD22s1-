--TIPOS RECURSIVOS SIMPLES

data Color = Azul| Rojo deriving Show 
data Celda = Bolita Color Celda | CeldaVacia deriving Show

nroBolitas::Color -> Celda -> Int
nroBolitas c CeldaVacia      = 0
nroBolitas c (Bolita col cel)= unoSi(esBolitaColor c col) + nroBolitas c cel

unoSi ::Bool -> Int
unoSi True  = 1
unoSi False = 0

esBolitaColor::Color -> Color -> Bool
esBolitaColor Azul Azul = True
esBolitaColor Rojo Rojo = True
esBolitaColor _ _       = False

--------------------------
poner::Color -> Celda -> Celda
poner c CeldaVacia      = Bolita c CeldaVacia
poner c (Bolita col cel)= Bolita col(poner c cel)

---------------------------
sacar::Color -> Celda -> Celda
sacar c CeldaVacia = CeldaVacia
sacar c (Bolita col cel) = if(esBolitaColor c col)
							 then cel	
							 else Bolita col (sacar c cel)
							 
							 
-----------------------------
--ponerN:: Int -> Color -> Celda -> Celda
--ponerN n col CeldaVacia