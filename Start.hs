module Start where

import qualified Instructions as Instt 
import System.Random
import System.Exit
import Data.Time (getCurrentTime)
import Data.Time.Clock (diffUTCTime)
import Data.Time

type Coordenadas = (Int, Int)
type Valor = Int
type Elem = (Coordenadas,Valor)
type Matriz = [Elem]

printWin :: IO()
printWin = do
    putStrLn " __________________________________________ "
    putStrLn "|                                          |" 
    putStrLn "|         █▀▀ █▀█ █▄ █ █ █ █▀█ █ █         |" 
    putStrLn "|         █ ▄ █▀█ █ ▀█ █▀█ █ █ █ █         |"
    putStrLn "|         ▀▀▀ ▀ ▀ ▀  ▀ ▀ ▀ ▀▀▀  ▀▀         |"
    putStrLn "|__________________________________________|"
    
printLose :: IO()
printLose = do
    putStrLn " __________________________________________ "
    putStrLn "|                                          |" 
    putStrLn "|         █▀█ █▀▀ █▀█ █▀▄ █▀▀ █ █          |" 
    putStrLn "|         █▀▀ █▀  █▀▄ █ █ █▀  █ █          |"
    putStrLn "|         ▀   ▀▀▀ ▀ ▀ ▀▀  ▀▀▀  ▀▀          |"
    putStrLn "|__________________________________________|"
    
printTimesUP :: IO()
printTimesUP = do
    putStrLn " __________________________________________ "
    putStrLn "|                                          |" 
    putStrLn "|          ▀█▀ █▀▀ █▀▄▀█ █▀█ █▀█           |" 
    putStrLn "|           █  █▀  █ █ █ █▀▀ █ █           |"
    putStrLn "|           ▀  ▀▀▀ ▀ ▀ ▀ ▀   ▀▀▀           |"
    putStrLn "|                                          |"
    putStrLn "|     █▀▀ █▀▀ █▀▀ █▀█ ▀█▀ █▀█ █▀▄ █▀█      |" 
    putStrLn "|     █▀  ▀▀█ █ ▄ █ █  █  █▀█ █ █ █ █      |"
    putStrLn "|     ▀▀▀ ▀▀▀ ▀▀▀ ▀▀▀  ▀  ▀ ▀ ▀▀  ▀▀▀      |"
    putStrLn "|__________________________________________|"
    
printFullLife :: IO()
printFullLife = do
    putStrLn " __________________________________________ "
    putStrLn "|                                          |"
    putStrLn "|     ▄███▄███▄  ▄███▄███▄  ▄███▄███▄      |" 
    putStrLn "|     █████████  █████████  █████████      |"
    putStrLn "|      ▀█████▀    ▀█████▀    ▀█████▀       |"
    putStrLn "|        ▀█▀        ▀█▀        ▀█▀         |" 
    putStrLn "|__________________________________________|"
    
printOneLostLife :: IO()
printOneLostLife = do
    putStrLn " __________________________________________ "
    putStrLn "|                                          |"
    putStrLn "|     ▄███▄███▄  ▄███▄███▄                 |" 
    putStrLn "|     █████████  █████████                 |"
    putStrLn "|      ▀█████▀    ▀█████▀                  |"
    putStrLn "|        ▀█▀        ▀█▀                    |" 
    putStrLn "|__________________________________________|"
    
printTwoLostLife :: IO()
printTwoLostLife = do
    putStrLn " __________________________________________ "
    putStrLn "|                                          |"
    putStrLn "|     ▄███▄███▄                            |" 
    putStrLn "|     █████████                            |"
    putStrLn "|      ▀█████▀                             |"
    putStrLn "|        ▀█▀                               |" 
    putStrLn "|__________________________________________|"
    

-- Função que retorna true se a posição indicada já tenha sido revelada nas jogadas anteriores
checkPositionIsRevealed :: (Int, Int) -> Matriz -> Bool
checkPositionIsRevealed tupla [] = False
checkPositionIsRevealed (x, y) (((a, b), c): mtzTail) = 
    if (x == a && y == b && c /= -2) then 
        True
    else 
        checkPositionIsRevealed (x, y) mtzTail 
        
-- Função que retorna true se a posição indicada for uma bomba letal
checkPositionIsLethalBomb :: (Int, Int) -> Matriz -> Bool
checkPositionIsLethalBomb tupla [] = False
checkPositionIsLethalBomb (x, y) (((a, b), c): mtzTail) = 
    if (x == a && y == b && c == -1) then 
        True 
    else 
        checkPositionIsLethalBomb (x, y) mtzTail    
                      
-- Função que retorna true se a posição indicada for uma bomba desarmável 
checkPositionIsBomb :: (Int, Int) -> Matriz -> Bool
checkPositionIsBomb tupla [] = False
checkPositionIsBomb (x, y) (((a, b), c): mtzTail) = 
    if (x == a && y == b && c == -3) then 
        True 
    else 
        checkPositionIsBomb (x, y) mtzTail 
        

-- Função que modifica a matriz do usuário de acordo com as coordenadas recebidas
modifyPosition :: Int -> Int -> Int -> Matriz -> Matriz -> Matriz 
modifyPosition x y z (((a,b), c): mtzUsuario) mtzFinal = 
    if (x == a && y == b) then 
        mtzFinal ++ (([((x, y), z)] ++ mtzUsuario)) 
    else modifyPosition x y z mtzUsuario (mtzFinal ++ [((a,b), c)])

-- Função que modifica a matriz que é mostrada ao usuário usando a matriz interna
revealsMatriz :: Matriz -> Matriz -> Matriz -> Matriz
revealsMatriz [] mtzInterna mtzUsuario = mtzUsuario
revealsMatriz (((a,b), c): mtzInternaTail) mtzInterna mtzUsuario = revealsMatriz mtzInternaTail mtzInterna (modifyMatriz a b mtzInterna mtzUsuario)
 
modifyMatriz :: Int -> Int -> Matriz -> Matriz -> Matriz
modifyMatriz x y (((a,b), c): mtz) mtz_usuario = 
    if (x == a && y == b) then 
        ((modifyPosition x y c mtz_usuario [])) 
    else modifyMatriz x y mtz mtz_usuario
        
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
            
-- Função para contar quantas posições ainda estão escondidas   
hiddenAccount :: Int -> Matriz -> Int
hiddenAccount num [] = num
hiddenAccount num (((x, y), v) : mtz) = 
    if (v == -2) then
        (hiddenAccount (num+1) mtz) 
    else (hiddenAccount (num) mtz)

-- Função para contar quantas bombas ainda estão armadas  
bombsAccount :: Int -> Matriz -> Int
bombsAccount num [] = num
bombsAccount num (((x, y), v) : mtz) = 
    if (v == -3) then
        (bombsAccount (num+1) mtz) 
    else (bombsAccount (num) mtz)

-- Função para contar quantas bombas letais o código contém 
alertsAccount :: Int -> Matriz -> Int
alertsAccount num [] = num
alertsAccount num (((x, y), v) : mtz) = 
    if (v == -5) then
        (alertsAccount (num+1) mtz) 
    else (alertsAccount (num) mtz) 
             
actions :: Int -> Int -> Int -> Matriz -> Matriz -> Matriz ->  Matriz -> Matriz -> UTCTime ->Int -> Int -> IO()
actions quantLinhas quantColunas quantBombasLetais mtzInterna mtzUsuario mtzAnteriorRevelada mtzDesativada mtzAlerta time life quantAlertasNaMatriz = do

    putStrLn "\nInforme a sua jogada:"
    
    entrada <- getLine
    
    -- Pega o tempo atual de cada entrada do usuário
    timeAtual <- getCurrentTime
    
    -- Verifica a diferença do tempo em segundos
    let diferenca = realToFrac (diffUTCTime timeAtual time)
    
    putStrLn "\n"
    let jogada = words entrada
    let acao = jogada !! 0
    let x = read (jogada !! 1) :: Int
    let y = read (jogada !! 2) :: Int
    
    
    let matrizUsuario = modifyMatriz x y mtzInterna mtzUsuario
    let matrizUsuarioRevelada = revealing quantLinhas quantColunas matrizUsuario matrizUsuario mtzInterna
    let matrizInternaRevelada = revealsMatriz mtzInterna mtzInterna mtzUsuario
    let mtzUsuarioDesativada = modifyMatriz x y mtzDesativada matrizUsuarioRevelada
    let mtzUsuarioAlerta =  modifyMatriz x y mtzAlerta matrizUsuarioRevelada
    
    -- A subtração da quantidade de bombas letais pela quantidade de alertas colocados na matriz
    let subtracaoLetaisPorAlertas =  quantBombasLetais + quantAlertasNaMatriz -- Quantidade de alertas sempre será o númeral zero ou um número negativo
    
    -- Verifica quantas bombas precisam ser desarmadas
    let quantBombasAtivadas = bombsAccount 0 mtzUsuarioDesativada
    
    -- Se for maior que 540 segundos de diferença o jogador perde, por causa do tempo esgotado
    if(diferenca >= 540.00) then do
        putStrLn "\nO tempo de jogo expirou."
    	printTimesUP
    	exitSuccess
    	
    -- Quando o usuário selecionar a opção de Abrir caminho
    else if(acao == "Abrir") then do
        printMatriz quantLinhas quantColunas (matrizUsuarioRevelada)
        if(checkPositionIsLethalBomb (x, y) mtzInterna)  then do         -- O usuário perde o jogo imediatamente após abrir caminho em uma bomba letal.
            printMatriz quantLinhas quantColunas (matrizInternaRevelada) 
            printLose
            exitSuccess
        
        -- Condição e função de retirar vida caso o usuário tente abrir uma posição já revelada
        else if (checkPositionIsRevealed(x,y) mtzAnteriorRevelada) then do
            let lifeActual = life - 1 
            
            -- Verifica se o usuário ainda possui vidas para prosseguir jogando
            if (lifeActual == 0) then do
               putStrLn "\nVocê perdeu uma vida por tentar abrir uma posição já revelada.\nVocê não possui vidas suficientes."
               printLose
               exitSuccess
               
            -- Informa ao usuário que perdeu uma vida, e retorna quantas ainda tem.
            else if (lifeActual == 1) then do
               printTwoLostLife
               putStrLn "\nVocê perdeu uma vida por tentar abrir uma posição já revelada, uma restante."
               actions quantLinhas quantColunas quantBombasLetais mtzInterna mtzUsuario matrizUsuarioRevelada mtzDesativada mtzAlerta time lifeActual quantAlertasNaMatriz
            else do
               printOneLostLife
               putStrLn "\nVocê perdeu uma vida por tentar abrir uma posição já revelada, duas restantes."
               actions quantLinhas quantColunas quantBombasLetais mtzInterna mtzUsuario matrizUsuarioRevelada mtzDesativada mtzAlerta time lifeActual quantAlertasNaMatriz
        
        -- Se a quantidade de posições não reveladas for igual à subtração da quantidade de bombas letais pela quantidade de alertas colocados na matriz e não tiver mais nenhuma bomba desarmável ativada, o usuário ganha.
        else if (hiddenAccount 0 matrizUsuarioRevelada == subtracaoLetaisPorAlertas) then do 
            if (quantBombasAtivadas == 0) then do
               printWin 
               exitSuccess
            else do
               actions quantLinhas quantColunas quantBombasLetais mtzInterna matrizUsuario matrizUsuarioRevelada mtzDesativada mtzAlerta time life quantAlertasNaMatriz
        	
        else do
            actions quantLinhas quantColunas quantBombasLetais mtzInterna matrizUsuario matrizUsuarioRevelada mtzDesativada mtzAlerta time life quantAlertasNaMatriz
    
    -- Quando o usuário selecionar a opção de Alertar Caminho
    else if(acao == "Alerta") then do
        if (checkPositionIsRevealed(x,y) mtzAnteriorRevelada) then do   -- A opção de tentar marcar um alerta em uma posição que já foi revelada, é inválida.
            putStrLn "Opcão inválida\n"
            printMatriz quantLinhas quantColunas (matrizUsuarioRevelada)
            actions quantLinhas quantColunas quantBombasLetais mtzInterna mtzUsuario matrizUsuarioRevelada mtzDesativada mtzAlerta time life quantAlertasNaMatriz
            
        else if (checkPositionIsLethalBomb (x, y) mtzInterna) then do -- Verifica se o campo alertado é uma bomba letal, se sim diminui em quantAlertasNaMatrizAtual para servir de controle em condições para ganhar.
            
            printMatriz quantLinhas quantColunas (mtzUsuarioAlerta)
            
            -- Quantidade de alertas na matriz sempre vai ser um número negativo, que vai ser somado a quantidade de bombas letais
            let quantAlertasNaMatrizAtual = quantAlertasNaMatriz - 1     
            actions quantLinhas quantColunas quantBombasLetais mtzInterna mtzUsuarioAlerta matrizUsuarioRevelada mtzDesativada mtzAlerta time life quantAlertasNaMatrizAtual
            
        else do
            printMatriz quantLinhas quantColunas (mtzUsuarioAlerta)
            actions quantLinhas quantColunas quantBombasLetais mtzInterna mtzUsuarioAlerta matrizUsuarioRevelada mtzDesativada mtzAlerta time life quantAlertasNaMatriz 
            
                
    -- Quando o usuário selecionar a opção de Desativar bomba          
    else if(acao == "Desativar") then do
        
        if(checkPositionIsLethalBomb (x, y) mtzInterna) then do -- O usuário perde o jogo imediatamente após desativar uma bomba letal.
            printMatriz quantLinhas quantColunas (matrizInternaRevelada) 
            printLose
            exitSuccess
        
        else if(checkPositionIsBomb (x, y) mtzInterna) then do
            printMatriz quantLinhas quantColunas (mtzUsuarioDesativada) 
            
            -- Se a quantidade de posições não reveladas for igual à subtração da quantidade de bombas letais pela quantidade de alertas colocados na matriz e não tiver mais nenhuma bomba desarmável ativada, o usuário ganha.
            if (hiddenAccount 0 matrizUsuarioRevelada == subtracaoLetaisPorAlertas) then do 
                if (quantBombasAtivadas == 0) then do
                    printWin 
                    exitSuccess
                else do
                    actions quantLinhas quantColunas quantBombasLetais mtzInterna mtzUsuarioDesativada matrizUsuarioRevelada mtzDesativada mtzAlerta time life quantAlertasNaMatriz 
                    
            else do
                actions quantLinhas quantColunas quantBombasLetais mtzInterna mtzUsuarioDesativada matrizUsuarioRevelada mtzDesativada mtzAlerta time life quantAlertasNaMatriz            
      
        else do
            printMatriz quantLinhas quantColunas (matrizUsuarioRevelada)
            let lifeActual = life - 1 
            
            -- Verifica se o usuário ainda possui vidas para prosseguir jogando
            if (lifeActual == 0) then do
               putStrLn "\nVocê perdeu uma vida por tentar desarmar uma posição sem bomba.\nVocê não possui vidas suficientes."
               printLose
               exitSuccess
            
            -- Informa ao usuário que perdeu uma vida, e retorna quantas ainda tem.
            else if (lifeActual == 1) then do
               printTwoLostLife
               putStrLn "\nVocê perdeu uma vida por tentar desarmar uma posição sem bomba, uma restante."
               actions quantLinhas quantColunas quantBombasLetais mtzInterna mtzUsuario matrizUsuarioRevelada mtzDesativada mtzAlerta time lifeActual quantAlertasNaMatriz 
        
            else do
               printOneLostLife
               putStrLn "\nVocê perdeu uma vida por tentar desarmar uma posição sem bomba, duas restantes."
               actions quantLinhas quantColunas quantBombasLetais mtzInterna mtzUsuario matrizUsuarioRevelada mtzDesativada mtzAlerta time lifeActual quantAlertasNaMatriz
    
    -- Quando o usuário selecionar a opção de Sair do jogo               
    else if(acao == "Sair") then do
        exitSuccess
        
    -- Uma entrada inválida vai resultar no término do jogo
    else do
        exitSuccess
        
        
createMatriz :: Int -> Int -> Int -> Matriz
createMatriz a b c = [((x,y), c) | x <-[1,2..a], y <-[1,2..b]]

generateRandomPositions :: Int -> Int -> Int -> Int -> Int -> [(Int, Int)] -> [(Int, Int)]
generateRandomPositions quantLinhas quantColunas 0 random1 random2 lista = lista
generateRandomPositions quantLinhas quantColunas quantBombasLetais random1 random2 lista =
    generateRandomPositions quantLinhas quantColunas (quantBombasLetais-1) (random1 + 59) (random2 + 277) ((generateRandomTuple quantLinhas quantColunas random1 random2 lista) : lista)  
    
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
 
-- Adiciona bombas desarmáveis 
addBombs :: [(Int, Int)] -> Matriz -> Matriz
addBombs [] mtz = mtz
addBombs ((x, y): mtzTail) mtz = addBombs mtzTail (insertBomb x y mtz [])  

-- Adiciona bombas letais
addBombsLetais :: [(Int, Int)] -> Matriz -> Matriz
addBombsLetais [] mtz = mtz
addBombsLetais ((x, y): mtzTail) mtz = addBombsLetais mtzTail (insertBombLetal x y mtz [])  

-- Funcao que insere uma bomba na posicao passada como parametro
insertBombLetal :: Int -> Int -> Matriz -> Matriz -> Matriz
insertBombLetal a b [] mtzFinal = mtzFinal
insertBombLetal a b (((x, y), z): mtz) mtzFinal = 
    if(a == x && b == y) then
        mtzFinal++[((x, y), -1)]++mtz 
    else
        insertBombLetal a b mtz (mtzFinal++[((x, y), z)])
        
-- Funcao que insere uma bomba na posicao passada como parametro
insertBomb :: Int -> Int -> Matriz -> Matriz -> Matriz
insertBomb a b [] mtzFinal = mtzFinal
insertBomb a b (((x, y), z): mtz) mtzFinal = 
    if(a == x && b == y) then
        mtzFinal++[((x, y), -3)]++mtz 
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

-- Função para converter a matriz interna de números inteiros para uma saída em String
convertIntToString :: [Int] -> String
convertIntToString [] = ""
convertIntToString (h:t) 
    | h == -2 = "* " ++ convertIntToString t
    | h == -3 = "\ESC[92mB\ESC[0m " ++ convertIntToString t
    | h == -1 = "\ESC[31mL\ESC[0m " ++ convertIntToString t
    | h == -4 = "\ESC[32mD\ESC[0m " ++ convertIntToString t
    | h == -5 = "\ESC[33mA\ESC[0m " ++ convertIntToString t
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
    
    -- Bombas desarmáveis
    let quantBombas = 4
    
    -- Bombas Letais
    let quantBombasLetais = 8
    
    --Função para fazer o random das posições das bombas
    g <- newStdGen
    let (a,b) = randomR (1,999999 :: Int) g
    let random1 = a

    h <- newStdGen
    let (c,d) = randomR (1,999999 :: Int) h
    let random2 = c
    
    --Função para fazer o random das posições das bombas Letais
    s <- newStdGen
    let (e,f) = randomR (1,999999 :: Int) s
    let random3 = e

    t <- newStdGen
    let (o,p) = randomR (1,999999 :: Int) t
    let random4 = o

    let matriz = createMatriz quantLinhas quantColunas 0
    
    -- Funções para gerar posições aleatórias para cada tipo de bomba
    
    let posicoesAletoriasL = generateRandomPositions quantLinhas quantColunas quantBombasLetais random1 random2 []
    
    let posicoesAletorias = generateRandomPositions quantLinhas quantColunas quantBombas random3 random4 []
    
    let matrizComBombasLetais = (addBombsLetais posicoesAletoriasL matriz)
    
    let preparaCampo = adjacentBombs matrizComBombasLetais matrizComBombasLetais
    
    let matrizCompleta = (addBombs posicoesAletorias preparaCampo)

    let matrizInicial = (createMatriz quantLinhas quantColunas (-2))
    
    let matrizDesativada = (createMatriz quantLinhas quantColunas (-4))
    
    let mtzAlerta = (createMatriz quantLinhas quantColunas (-5))
    
    putStrLn "\n"
    
    --Imprime na tela a vida inicial
    printFullLife
    
    putStrLn "\n"
    
    printMatriz quantLinhas quantColunas matrizInicial
    
    -- Pega o tempo do usuário assim que ele inicia o jogo
    time <- getCurrentTime
    
    -- Define a quantidade de vidas disponíveis para o usuário
    let life = 3
    
    -- Define a quantidade de alertas 
    let quantAlertasNaMatriz = 0
    
    --Chama função relacionada a jogada do usuário com o time e a vida
    actions quantLinhas quantColunas quantBombasLetais matrizCompleta matrizInicial matrizInicial matrizDesativada mtzAlerta time life quantAlertasNaMatriz

main :: IO()
main = do
    Instt.main
    startGame

        

    



