--ADRIAN ESTEVEZ GALLEGO

--definicion del tipo Prop
data Prop = P | Q | R | S

--instanciar show en Prop 
instance Show Prop where
    show (P) = "P"
    show (Q) = "Q"
    show (R) = "R"
    show (S) = "S"

data Formula = PROP Prop | NOT Formula | OR Formula Formula | AND Formula Formula | PARENTESIS Formula

instance Show Formula where
    show (PROP x) = show(x)
    show (NOT x) = "¬" ++ show(x)
    show (OR f1 f2) = show(f1) ++ " V " ++ show(f2)
    show (AND f1 f2) = show(f1) ++ " ^ " ++ show(f2)
    show (PARENTESIS f) = "(" ++ show(f) ++ ")"

esClausula :: Formula -> Bool
esClausula f
    | f == PROP _ = True
    | f == NOT x = esClausula x
    | f == OR f1 f2 = esClausula f1 && esClausula f2
    | f == AND _ _ = False
    | f == PARENTESIS x = esClausula x
