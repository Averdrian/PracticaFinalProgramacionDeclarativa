--ADRIAN ESTEVEZ GALLEGO

--definicion del tipo Prop
data Prop = P | Q | R | S deriving(Show, Eq, Read)

--definicion del tipo Formula
data Formula = PROP Prop | NOT Formula | OR Formula Formula | AND Formula Formula | PARENTESIS Formula deriving(Eq, Read)

instance Show Formula where
    show (PROP x) = show(x)
    show (NOT x) = "¬" ++ show(x)
    show (OR f1 f2) = show(f1) ++ " V " ++ show(f2)
    show (AND f1 f2) = show(f1) ++ " ^ " ++ show(f2)
    show (PARENTESIS f) = "(" ++ show(f) ++ ")"


--comprueba que una formula es una clausula
esClausula :: Formula -> Bool
esClausula (PROP _) = True
esClausula (OR f1 f2) = esClausula f1 && esClausula f2
esClausula (NOT (PROP _)) = True
esClausula (NOT _) = False
esClausula (AND _ _) = False
esClausula (PARENTESIS _) = False

--devuelve dada un formula en FNC un lista en la que sus elementos son sus clausulas
fncAlista :: Formula -> [Formula]
fncAlista (AND f1 f2) = let {x = if(esClausula f1) then [f1] else fncAlista f1; y = if(esClausula f2) then [f2] else fncAlista f2} in x++y
fncAlista (OR f1 f2) = if(esClausula f1 && esClausula f2) then [OR f1 f2] else error "NO ESTA EN FNC"
fncAlista (PROP p) = [PROP p]
fncAlista (PARENTESIS f) = fncAlista f
fncAlista (NOT f) = if(esClausula f) then [NOT f] else error "NO ESTA EN FNC"

--devuelve dada una clausula su lista de elementos atomicos, tanto ciertos como negados
clausulaLista :: Formula -> [Formula]
clausulaLista (OR f1 f2) = clausulaLista f1 ++ clausulaLista f2
clausulaLista (PROP p) = [PROP p]
clausulaLista (NOT (PROP p)) = [NOT (PROP p)]
clausulaLista _ = error "NO ES CLAUSULA"

--devuelve si es un literal es positivo
esLiteralPositivo :: Formula -> Bool
esLiteralPositivo (NOT(PROP _)) = False
esLiteralPositivo (PROP _) = True
esLiteralPositivo _ = error "NO ES UN LITERAL"

--devuelve cierto si es clausula de horn
esClausulaHorn :: Formula -> Bool
esClausulaHorn f = length (filter (esLiteralPositivo) (clausulaLista f)) < 2

--devuelve la lista de literales del resolvente de dos clausulas
resolvente :: [Formula] -> [Formula] -> [Formula]
resolvente xs ys = let r = resolventeAux xs ys  in [x | x <- (xs ++ ys), (any (==x) r) == False]


--devuelve la lista de literales, tanto positivos como negativos, que debemos eliminar para tener la lista de elementos del resolvente
resolventeAux :: [Formula] -> [Formula] -> [Formula]
resolventeAux (x:xs) ys = (resuelveUno x ys) ++ (resolventeAux xs ys)
resolventeAux [] _ = []

--develeve un literal y su negacion si debe quitarse del resolvente
resuelveUno :: Formula -> [Formula] -> [Formula]
resuelveUno (NOT x) (y:ys) = if(y == x) then [NOT x, y] else resuelveUno (NOT x) ys
resuelveUno (PROP x) (y:ys) = if(NOT(PROP (x)) == y) then [PROP x, y] else resuelveUno (PROP x) ys
resuelveUno _ [] = []
resuelveUno _ _ = error "ELEMENTO NO VALIDO"


resolucion :: [[Formula]] -> [Formula] -> [Formula]
resolucion xs h = let x = resolucionAux xs h in if any (==False) (map (esLiteralPositivo) x)  then filter (\l -> esLiteralPositivo l == False) x else if (length x) < (length xs) then [] else x

resolucionAux :: [[Formula]] -> [Formula] -> [Formula]
resolucionAux (x:xs) h = let res = resolucionUnPaso (x:xs) h in if length res == 1 && esLiteralPositivo (res!!0) then res ++ resolucionAux (x:xs) (resolvente x h) else res
resolucionAux _ [] = []
resolucionAux [] _ = []

resolucionUnPaso :: [[Formula]] -> [Formula] -> [Formula]
resolucionUnPaso (x:xs) h = let z = (resolvente x h) in if  z == (x++h) then (resolucionUnPaso xs h) else if z == [] then z else [z!!0]
resolucionUnPaso _ [] = []
resolucionUnPaso [] _ = []

menuAplicacion :: IO()
menuAplicacion = do
  putStr "Introduzca la fórmula: "
  formulaStr <- getLine
  putStr "¿Qué accion quieres realizar? \n 1. Comprobar si una fórmula es una clausula\n 2. Devolver lista de clausulas una fórmula en FNC \n 3. Comprobar si una fórmula es clausula de Horn\n"
  let formula = read formulaStr :: Formula
  opcion <- getChar
  let respuesta = if opcion == '1' then show(esClausula formula) else if opcion == '2' then show(fncAlista formula) else show(esClausulaHorn formula)
  print(respuesta)
