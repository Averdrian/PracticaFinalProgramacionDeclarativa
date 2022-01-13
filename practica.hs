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
data Hash = Hash Int ([Char], [Char])

instance Show Hash where
  show (Hash _ (a,b)) = "("++a++" = "++b++")"

--type Traduccion = ([Char], [Char])
--type Hash = (Int, Traduccion)

hashAll :: [[Char]] -> [Hash]
hashAll (x:xs) = let a = takeWhile (/= ' ') x in let b = tail(dropWhile (/= ' ') x) in Hash (hash(a)) (a, b):hashAll xs
hashAll  [] = []

--devuelve true si el id del hash concuerda con el id introducido
hashHasIdx :: Int -> Hash -> Bool
hashHasIdx i (Hash idx _) = idx == i

filterHash :: Int -> [Hash] -> [Hash]
filterHash  idx xs = filter(hashHasIdx idx) xs


--devuleve un array de hash en un string
hashArrayToStr :: [Hash] -> String
hashArrayToStr (x:xs) = show x ++  hashArrayToStrAux xs
hashArrayToStr [] = ""

hashArrayToStrAux :: [Hash] -> String
hashArrayToStrAux (x:xs) = " , " ++ show x ++ hashArrayToStrAux xs
hashArrayToStrAux [] = ""


--cargar fichero
--cargarDiccionario :: FilePath -> IO ()


cargarContenidoFichero :: IO [Hash]
cargarContenidoFichero = do
  fileContent <- readFile "datos.txt"
  let fileLines = lines fileContent in let tablaHash = (hashAll(fileLines)) in return tablaHash--putStrLn ("[0]->"++hashArrayToStr(filterHash 0 tablaHash))
  --putStrLn "[0]->"++hashHasIdx

mostrarDiccionario :: IO()
mostrarDiccionario = do
  tablaHash <- cargarContenidoFichero
  mostrarLineaDiccionario 0 tablaHash

mostrarLineaDiccionario :: Int -> [Hash] -> IO()
mostrarLineaDiccionario i tablaHash = do
  if i > 9 || i < 0 then return ()
  else do 
    putStrLn ("["++show i++"]->"++hashArrayToStr(filterHash i tablaHash))
    mostrarLineaDiccionario (i+1) tablaHash

--Hash hash(takeWhile (/= ' ') fileLines !! 0) (takeWhile(/= ' ') fileLines !! 0, tail(dropWhile (/= ' ') fileLines !! 0))

--findHashIdx :: [Hash] -> [Hash]



