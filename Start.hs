module Start where

import System.Random
import System.Exit

type Coordenadas = (Int, Int)
type Valor = Int
type Elem = (Coordenadas,Valor)
type Matriz = [Elem]

-- Função que retorna true se a posição indicada for uma bomba
checkPosition :: (Int, Int) -> Matriz -> Bool
checkPosition tupla [] = False
checkPosition (x, y) (((a, b), c): mtzTail) = 
    if (x == a && y == b && c == -1) then 
        True 
    else 
        checkPosition (x, y) mtzTail 

-- Função que modifica a matriz que é mostrada ao usuário usando a matriz interna
revealsMatriz :: Matriz -> Matriz -> Matriz -> Matriz
revealsMatriz [] mtzInterna mtzUsuario = mtzUsuario
revealsMatriz (((a,b), c): mtzInternaTail) mtzInterna mtzUsuario = revealsMatriz mtzInternaTail mtzInterna (modifyMatriz a b mtzInterna mtzUsuario)
 
modifyMatriz :: Int -> Int -> Matriz -> Matriz -> Matriz
modifyMatriz x y (((a,b), c): mtz) mtz_usuario = 
    if (x == a && y == b) then 
        ((modifyPosition x y c mtz_usuario [])) 
        else modifyMatriz x y mtz mtz_usuario

-- Função que modifica a matriz do usuário de acordo com as coordenadas recebidas
modifyPosition :: Int -> Int -> Int -> Matriz -> Matriz -> Matriz 
modifyPosition x y z (((a,b), c): mtzUsuario) mtzFinal = 
    if (x == a && y == b) then 
        mtzFinal ++ (([((x, y), z)] ++ mtzUsuario)) 
        else modifyPosition x y z mtzUsuario (mtzFinal ++ [((a,b), c)])
        
revealing :: Int -> Int -> Matriz -> Matriz -> Matriz -> Matriz
revealing quantLinhas quantColunas [] mtzUsuario mtzInterna = mtzUsuario
revealing quantLinhas quantColunas (((x, y), z):mtzUsuarioTail) mtzUsuario mtzInterna = 
    if (z /= 0) then
        revealing quantLinhas quantColunas mtzUsuarioTail mtzUsuario mtzInterna
    else 
        if (revealedAround x y quantLinhas quantColunas mtzUsuario) then
            revealing quantLinhas quantColunas mtzUsuarioTail mtzUsuario mtzInterna
        else
            revealing quantLinhas quantColunas (revealedCross x y quantLinhas quantColunas mtzUsuario mtzInterna) (revealedCross x y quantLinhas quantColunas mtzUsuario mtzInterna) mtzInterna

revealedAround :: Int -> Int -> Int -> Int -> Matriz -> Bool
revealedAround x y quantLinhas quantColunas mtzUsuario
    | (x == 1 && y == 1) = (revealed (x) (y+1) mtzUsuario) && (revealed (x+1) (y) mtzUsuario)
    | (x == 1 && y == quantColunas) = (revealed (x) (y-1) mtzUsuario) && (revealed (x+1) (y) mtzUsuario)
    | (x == quantLinhas && y == quantColunas) = (revealed (x) (y-1) mtzUsuario) && (revealed (x-1) (y) mtzUsuario)
    | (x == quantLinhas && y == 1) = (revealed (x) (y+1) mtzUsuario) && (revealed (x-1) (y) mtzUsuario)
    | (x == 1 && y > 1 && y < quantColunas) = (revealed (x) (y+1) mtzUsuario) && (revealed (x) (y-1) mtzUsuario) && (revealed (x+1) (y) mtzUsuario)
    | (x > 1 && x < quantLinhas && y == quantColunas) = (revealed (x-1) (y) mtzUsuario) && (revealed (x+1) (y) mtzUsuario) && (revealed (x) (y-1) mtzUsuario)
    | (x == quantLinhas && y > 1 && y < quantColunas) = (revealed (x-1) (y) mtzUsuario) && (revealed (x) (y-1) mtzUsuario) && (revealed (x) (y+1) mtzUsuario)
    | (x > 1 && x < quantLinhas && y == 1) = (revealed (x) (y+1) mtzUsuario) && (revealed (x-1) (y) mtzUsuario) && (revealed (x+1) (y) mtzUsuario)
    | otherwise = (revealed (x-1) (y) mtzUsuario) && (revealed (x) (y+1) mtzUsuario) && (revealed (x+1) (y) mtzUsuario) && (revealed (x) (y-1) mtzUsuario)
 
revealed :: Int -> Int -> Matriz -> Bool
revealed x y (((a,b),v):mtzTail) = 
    if (a==x && b==y) then 
        (v/=(-2)) 
        else (revealed x y mtzTail) 
 
revealedCross :: Int -> Int -> Int -> Int -> Matriz -> Matriz -> Matriz
revealedCross x y quantLinhas quantColunas mtzUsuario mtzInterna
    | (x == 1 &&  y == 1) = modifyMatriz (x+1) y mtzInterna (modifyMatriz x (y+1) mtzInterna mtzUsuario)
    | (x == 1 && y == quantColunas) = modifyMatriz (x+1) y mtzInterna (modifyMatriz x (y-1) mtzInterna mtzUsuario)
    | (x == quantLinhas && y == quantColunas) = modifyMatriz (x-1) y mtzInterna (modifyMatriz x (y-1) mtzInterna mtzUsuario)
    | (x == quantLinhas && y==1) = modifyMatriz (x-1) y mtzInterna (modifyMatriz x (y+1) mtzInterna mtzUsuario)
    | (x==1 && y > 1 && y < quantColunas) = modifyMatriz (x+1) y mtzInterna (modifyMatriz x (y-1) mtzInterna (modifyMatriz x (y+1) mtzInterna mtzUsuario))
    | (x>1 && x < quantLinhas && y==quantColunas) = modifyMatriz (x-1) y mtzInterna (modifyMatriz (x+1) (y) mtzInterna (modifyMatriz x (y-1) mtzInterna mtzUsuario))
    | (x== quantLinhas && y > 1 && y < quantColunas) = modifyMatriz (x-1) y mtzInterna (modifyMatriz x (y-1) mtzInterna (modifyMatriz x (y+1) mtzInterna mtzUsuario))
    | (x>1 && x<quantLinhas && y==1) = modifyMatriz x (y+1) mtzInterna (modifyMatriz (x-1) y mtzInterna (modifyMatriz (x+1) y mtzInterna mtzUsuario))
    | otherwise = modifyMatriz (x-1) y mtzInterna (modifyMatriz (x) (y+1) mtzInterna (modifyMatriz (x+1) (y) mtzInterna (modifyMatriz x (y-1) mtzInterna  mtzUsuario)))            
            
-- Função para contar quantas posições ainda estão escondidas   
hiddenAccount :: Int -> Matriz -> Int
hiddenAccount num [] = num
hiddenAccount num (((x, y), v) : mtz) = 
    if (v == -2) then
        (hiddenAccount (num+1) mtz) 
        else (hiddenAccount (num) mtz)

actions :: Int -> Int -> Int -> Matriz -> Matriz -> IO()
actions quantLinhas quantColunas quantBombas mtzInterna mtzUsuario = do
    entrada <- getLine
    
    putStrLn"\n"
    
    let jogada = words entrada
    let acao = jogada !! 0
    let x = read (jogada !! 1) :: Int
    let y = read (jogada !! 2) :: Int

    if(acao == "Abrir") then do
        let matrizUsuario = if(checkPosition (x, y) mtzInterna) then revealsMatriz mtzInterna mtzInterna mtzUsuario else modifyMatriz x y mtzInterna mtzUsuario
        let matrizUsuarioReveladaRecursivamente = revealing quantLinhas quantColunas matrizUsuario matrizUsuario mtzInterna

        printMatriz quantLinhas quantColunas (matrizUsuarioReveladaRecursivamente)
        if(checkPosition (x, y) mtzInterna) then
            putStrLn "PERDEU"
                else if (hiddenAccount 0 matrizUsuarioReveladaRecursivamente == quantBombas) then 
                    putStrLn "VENCEU" 
                    else actions quantLinhas quantColunas quantBombas mtzInterna matrizUsuario 
    else do
        putStrLn "Opcão inválida"
        actions quantLinhas quantColunas quantBombas mtzInterna mtzUsuario      
    
      
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
invert a b mtz antecessor = reverse (checkAndSum a b mtz antecessor)

-- Função que verifica se tal posição não é uma bomba e soma + 1
checkAndSum :: Int -> Int -> Matriz -> Matriz-> Matriz
checkAndSum a b [] antecessor = [ ]
checkAndSum a b (((x, y), z): mtz) antecessor = 
    if(a == x && b == y && z /= -1) then 
        antecessor ++ (reverse ([((x,y), z+1)] ++ mtz)) 
        else checkAndSum a b mtz antecessor++ [((x,y), z)]

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

    --Chama função relacionada a jogada do usuário 
    actions quantLinhas quantColunas quantBombas preparaCampo matrizInicial


main :: IO()
main = do
    startGame

        

    



