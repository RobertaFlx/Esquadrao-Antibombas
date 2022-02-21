module Start where

import System.Random
import System.Exit

type Coordenadas = (Int, Int)
type Valor = Int
type Elem = (Coordenadas,Valor)
type Matriz = [Elem]   
        
createMatriz :: Int -> Int -> Int -> Matriz
createMatriz a b c = [((x,y), c) | x <-[1,2..a], y <-[1,2..b]]

generateRandomPositions :: Int -> Int -> Int -> Int -> Int -> [(Int, Int)] -> [(Int, Int)]
generateRandomPositions quantLinhas quantColunas 0 random1 random2 lista = lista
generateRandomPositions quantLinhas quantColunas quantBombas random1 random2 lista =
    generateRandomPositions quantLinhas quantColunas (quantBombas-1) (random1 + 59) (random2 + 277) ((generateRandomTuple quantLinhas quantColunas random1 random2 lista) : lista)  
    
generateRandomTuple :: Int -> Int -> Int -> Int -> [(Int, Int)] -> (Int, Int)
generateRandomTuple quantLinhas quantColunas random1 random2 lista
    | (x, y) `elem` lista = generateRandomTuple quantLinhas quantColunas (random1+137) (random2+331) lista
    | otherwise = (x, y)
    where x = generateRandomNumber quantLinhas (random1)
          y = generateRandomNumber quantColunas (random2)      
    
generateRandomNumber :: Int -> Int -> Int
generateRandomNumber num random = 
    if (random `mod` 2 == 0) then
        (((random * 20) `mod` num) + 1)
    else
        (((random * 43) `mod` num) + 1)  
    
addBombs :: [(Int, Int)] -> Matriz -> Matriz
addBombs [] mtz = mtz
addBombs ((x, y): mtzTail) mtz = addBombs mtzTail (insertBomb x y mtz [])  

-- Funcao que insere uma bomba na posicao passada como parametro
insertBomb :: Int -> Int -> Matriz -> Matriz -> Matriz
insertBomb a b [] mtzFinal = mtzFinal
insertBomb a b (((x, y), z): mtz) mtzFinal = 
    if(a == x && b == y) then
        mtzFinal++[((x, y), -1)]++mtz 
    else
        insertBomb a b mtz (mtzFinal++[((x, y), z)])

-- Função que encontra as bombas e chama a função que soma os adjacentes a uma bomba
adjacentBombs :: Matriz -> Matriz -> Matriz
adjacentBombs [] matriz = matriz
adjacentBombs (((x, y), z): mtz) matriz = 
    if (z == -1) then 
        adjacentBombs mtz (adjacentSum x y matriz) 
        else (adjacentBombs mtz matriz)

-- Função que soma os adjacentes a uma bomba
adjacentSum :: Int -> Int -> Matriz -> Matriz
adjacentSum x y mtz = invert x (y-1) (invert x (y+1) (invert (x-1) y (invert (x-1) (y+1) (invert (x-1) (y-1) (invert (x+1) y (invert (x+1) (y+1) (invert (x+1)       (y-1) mtz [])[])[])[])[])[])[])[]

-- Funcao auxiliar
invert:: Int -> Int -> Matriz -> Matriz-> Matriz
invert a b mtz anterior = reverse (verificaSoma a b mtz anterior)

printMatriz :: Int -> Int -> Matriz -> IO()
printMatriz quantLinhas quantColunas matriz = putStrLn (getValuesMatriz 1 quantLinhas quantColunas matriz)

-- Cria uma representacao em String da matriz passada como parametro
getValuesMatriz :: Int -> Int -> Int -> Matriz -> String
getValuesMatriz linha quantLinhas quantColunas matriz
    | linha == (quantLinhas+1) = ""
    | otherwise = convertIntToString (getValuesLine linha quantColunas matriz) ++ "\n" ++ getValuesMatriz (linha+1) quantLinhas quantColunas matriz
    
convertIntToString :: [Int] -> String
convertIntToString [] = ""
convertIntToString (h:t) 
    | h == -2 = "* " ++ convertIntToString t
    | h == -1 = "B " ++ convertIntToString t
    | otherwise = show h ++ " " ++ convertIntToString t    
    
-- Retorna uma lista de inteiros com os valores da linha passada como parametro
getValuesLine :: Int -> Int -> Matriz -> [Int]
getValuesLine quantLinhas 0 matriz = []
getValuesLine quantLinhas quantColunas (((x, y), v):mtz)
    | quantLinhas == x = v:getValuesLine quantLinhas (quantColunas-1) mtz
    | otherwise = getValuesLine quantLinhas quantColunas mtz
    

startGame :: IO()
startGame = do
    let quantLinhas = 9
    let quantColunas = 9
    let quantBombas = 9
    
    putStrLn"\n"
    
    --Função para fazer o random das posições das bombas
    g <- newStdGen
    let (a,b) = randomR (1,999999 :: Int) g
    let random1 = a

    h <- newStdGen
    let (c,d) = randomR (1,999999 :: Int) h
    let random2 = c

    let matriz = createMatriz quantLinhas quantColunas 0
    
    let posicoesAletorias = generateRandomPositions quantLinhas quantColunas quantBombas random1 random2 []
    
    let matrizComBombas = (addBombs posicoesAletorias matriz)
    
    let preparaCampo = adjacentBombs matrizComBombas matrizComBombas

    let matrizInicial = (createMatriz quantLinhas quantColunas (-2))
    
    printMatriz quantLinhas quantColunas matrizInicial
    
    putStrLn "Informe a sua jogada:" 

    --Chamar alguma função relacionada a jogada do usuário  

    
main :: IO()
main = do
    startGame

        

    



