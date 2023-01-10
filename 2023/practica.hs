--ADRIAN ESTEVEZ GALLEGO

--definicion del tipo Prop
data Prop = P | Q | R | S deriving(Show, Eq, Read)

data Formula = PROP Prop | NOT Formula | OR Formula Formula | AND Formula Formula | PARENTESIS Formula deriving(Eq, Read)

instance Show Formula where
    show (PROP x) = show(x)
    show (NOT x) = "¬" ++ show(x)
    show (OR f1 f2) = show(f1) ++ " V " ++ show(f2)
    show (AND f1 f2) = show(f1) ++ " ^ " ++ show(f2)
    show (PARENTESIS f) = "(" ++ show(f) ++ ")"



esClausula :: Formula -> Bool
esClausula (PROP _) = True
esClausula (OR f1 f2) = esClausula f1 && esClausula f2
esClausula (NOT (PROP _)) = True
esClausula (NOT _) = False
esClausula (AND _ _) = False
esClausula (PARENTESIS _) = False


fncAlista :: Formula -> [Formula]
fncAlista (AND f1 f2) = let {x = if(esClausula f1) then [f1] else fncAlista f1; y = if(esClausula f2) then [f2] else fncAlista f2} in x++y
fncAlista (OR f1 f2) = if(esClausula f1 && esClausula f2) then [OR f1 f2] else error "NO ESTA EN FNC"
fncAlista (PROP p) = [PROP p]
fncAlista (PARENTESIS f) = fncAlista f
fncAlista (NOT f) = if(esClausula f) then [NOT f] else error "NO ESTA EN FNC"


clausulaLista :: Formula -> [Formula]
clausulaLista (OR f1 f2) = clausulaLista f1 ++ clausulaLista f2
clausulaLista (PROP p) = [PROP p]
clausulaLista (NOT (PROP p)) = [NOT (PROP p)]
clausulaLista _ = error "NO ES CLAUSULA"