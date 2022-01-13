--ADRIAN ESTEVEZ GALLEGO


--funcion hash, cada caracter suma un numero segun su posicion en el abecedario
--agrupado de tres en tres
hash :: [Char] -> Int
hash (x:xs)
    | x < 'd' = mod (1 + hash(xs)) 10
    | x < 'g' = mod (2 + hash(xs)) 10
    | x < 'j' = mod (3 + hash(xs)) 10
    | x < 'm' = mod (4 + hash(xs)) 10
    | x < 'o' = mod (5 + hash(xs)) 10
    | x < 'r' = mod (6 + hash(xs)) 10
    | x < 'u' = mod (7 + hash(xs)) 10
    | x < 'x' = mod (8 + hash(xs)) 10
    | otherwise = mod (9 + hash (xs)) 10
hash [] = 0

--tipo de datos hash
data Hash = Hash Int ([Char], [Char]) deriving (Show, Read)

--cargar fichero
--cargarDiccionario :: FilePath -> IO ()
--cargarDiccionario = do
--    cs <- readFile "datos.txt"
--    putStrLm cs

muestraContenidoFichero :: FilePath -> IO ()
muestraContenidoFichero f = do
  cs <- readFile f
  putStrLn cs