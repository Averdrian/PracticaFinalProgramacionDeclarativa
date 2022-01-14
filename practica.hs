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

--sobrescritura de Show de hash
instance Show Hash where
  show (Hash _ (a,b)) = "("++a++" = "++b++")"


hashAll :: [[Char]] -> [Hash]
hashAll (x:xs) = let a = takeWhile (/= ' ') x in let b = tail (dropWhile (/= ' ') x) in Hash (hash(a)) (a, b):hashAll xs
hashAll  [] = []


--devuelve true si el id del hash concuerda con el id introducido
hashHasIdx :: Int -> Hash -> Bool
hashHasIdx i (Hash idx _) = idx == i

--devuelve la lista de hash con el indice introducido
filterHash :: Int -> [Hash] -> [Hash]
filterHash  idx xs = [x | x <- xs, hashHasIdx idx x]


--devuleve un array de hash en un string
hashArrayToStr :: [Hash] -> String
hashArrayToStr (x:xs) = show x ++  hashArrayToStrAux xs
hashArrayToStr [] = ""

hashArrayToStrAux :: [Hash] -> String
hashArrayToStrAux (x:xs) = " , " ++ show x ++ hashArrayToStrAux xs
hashArrayToStrAux [] = ""


--cargar el contenido del fichero en un array de Hash
cargarContenidoFichero :: IO [Hash]
cargarContenidoFichero = do
  fileContent <- readFile "datos.txt"
  let fileLines = lines fileContent in let tablaHash = hashAll fileLines in return tablaHash
  
--parte un string en palabras separadas por espacio 
splitLines :: [Char] -> [[Char]]
splitLines [] = []
splitLines (x:xs) = (word (x:xs)) : (splitLines (dropWhile (/= ' ') xs))

word :: [Char] -> [Char]
word [] = []
word (x:xs) = if x == ' ' then word xs else takeWhile (/= ' ') (x:xs)

--devuelve la respuesta de todas las traducciones
response :: [Hash] -> [[Char]] -> [[Char]]
response tablaHash xs  = map(traducir tablaHash) xs

--busca la traduccion de una palabra
traducir :: [Hash] -> [Char] -> [Char]
traducir tablaHash palabra = let idxHash = filterHash (hash palabra) tablaHash in traducirAux idxHash palabra

traducirAux :: [Hash] -> [Char] -> [Char]
traducirAux [] _ = "Sin traduccion"
traducirAux (Hash _ (a,b):xs) palabra = if palabra == a then b else traducirAux xs palabra

--longitud media de array de string
lengthMedia :: [[Char]] -> IO Float
lengthMedia xs = do 
  return (realToFrac (sum(map (length) xs)) / realToFrac (length xs))


--muestra el diccionario
mostrarDiccionario :: IO()
mostrarDiccionario = do
  tablaHash <- cargarContenidoFichero
  mostrarLineaDiccionario 0 tablaHash

mostrarLineaDiccionario :: Int -> [Hash] -> IO()
mostrarLineaDiccionario i tablaHash = do
  if i > 9 || i < 0 then return ()
  else do 
    putStrLn ("["++show i++"]->"++hashArrayToStr (filterHash i tablaHash))
    mostrarLineaDiccionario (i+1) tablaHash

--mostrar traducciones metiendo palabras a mano
introducirPalabras :: IO()
introducirPalabras = do
  tablaHash <- cargarContenidoFichero
  putStr "Introduce las palabras para evaluar: "
  linea <- getLine
  putStr "Traducciones: "
  let palabras = splitLines linea in print (response tablaHash $ palabras)
  longitudMedia <- lengthMedia (splitLines  linea)
  putStr "La longitud media de las palabras introducidas es: "
  print(longitudMedia)
  --let longitudMedia = lengthMedia (splitLines linea) in print longitudMedia 


