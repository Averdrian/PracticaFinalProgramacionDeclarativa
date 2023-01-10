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
-- esClausula f
--     | f == (PROP _) = True
--     | f == NOT x = esClausula x
--     | f == OR f1 f2 = esClausula f1 && esClausula f2
--     | f == AND _ _ = False
--     | f == PARENTESIS x = esClausula x
--     | otherwise = False

esClausula (PROP _) = True
esClausula (OR f1 f2) = esClausula f1 && esClausula f2
esClausula (NOT f) = esClausula f
esClausula (PARENTESIS f) = esClausula f
esClausula (AND _ _) = False

fncAlista :: Formula -> [Prop]
-- fncAlista f = if f == (AND f1 f2) then fncAlista f1:fncAlista f2 else []
fncAlista (PROP x) = [x]
fncAlista (AND f1 f2) = (fncAlista f1) ++ (fncAlista f2)
fncAlista (_) = error "La formula no esta en FNC"
-- fncAlista (OR _ _) = [P]
-- fncAlista (NOT _) = [S]
-- fncAlista (PARENTESIS _) = [P]