import qualified Screen as Scr
import qualified Credits as Crdt
import qualified Rules as Rls
import qualified Start as Stt
import System.IO
import System.IO.Unsafe (unsafeDupablePerformIO)
import System.Exit
import Control.Concurrent

title :: [[Char]]
title = [
    " ____________________________________________________ ",
    "|                                                    |", 
    "|                                                    |", 
    "|        █▀▀ █▀▀ █▀█ █ █ █▀█ █▀▄ █▀█ █▀█ █▀█         |", 
    "|        █▀  ▀▀█ █ █ █ █ █▀█ █ █ █▀▄ █▀█ █ █         |",
    "|        ▀▀▀ ▀▀▀ ▀▀▀▀ ▀▀ ▀ ▀ ▀▀  ▀ ▀ ▀ ▀ ▀▀▀         |",
    "|                                                    |",
    "|     █▀█ █▄ █ ▀█▀ █ █▀▀▄ █▀█ █▀▄▀█ █▀▀▄ █▀█ █▀▀     |", 
    "|     █▀█ █ ▀█  █  █ █▀▀▄ █ █ █ █ █ █▀▀▄ █▀█ ▀▀█     |",
    "|     ▀ ▀ ▀  ▀  ▀  ▀ ▀▀▀▀ ▀▀▀ ▀ ▀ ▀ ▀▀▀▀ ▀ ▀ ▀▀▀     |",
    "|                                                    |", 
    "|____________________________________________________|"] 
    
menu :: [[Char]]
menu = [
    "Iniciar   ", 
    "Regras    ",
    "Creditos  ", 
    "Sair      "]

commandsTable :: [[Char]]
commandsTable = [
    " s / w - mover cursor  ", 
    " f - selecionar        "]

-- Cria o menu e imprime ele na tela
printMenu :: [[Char]] -> IO()
printMenu menuTab = do
    let initialBuffer = Scr.createScreenBuffer Scr.width Scr.height Scr.emptyPxl
    let titleBuf = Scr.createBufferFromStringMatrix title
    let menuBuf = Scr.createBufferFromStringMatrix menuTab
    let tableBuf = Scr.createBufferFromStringMatrix commandsTable

    let tmp2 = Scr.renderCentralized initialBuffer menuBuf 0 8
    let tmp3 = Scr.renderInBuffer tmp2 tableBuf 1 3
    let tmp4 = Scr.renderCentralized tmp3 titleBuf 0 (-4)

    Scr.printScreen tmp4


printArrow :: [[Char]] -> Int -> [[Char]]
printArrow [] _ = []
printArrow (row:rest) 0     = (" " ++ row ++ " <<<") : rest
printArrow (row:rest) i     = row : printArrow rest (pred i)


mainLoop :: Int -> IO()
mainLoop index = do
    let maxIndex = 4

    printMenu $ printArrow menu index
    
    -- Pega um unico caracter da entrada
    hSetBuffering stdin NoBuffering
    command <- getChar
    hSetBuffering stdin LineBuffering

    -- Recalcula posicao do cursor
    let newIndex = case command of 'w' -> ((pred index) + maxIndex) `mod` maxIndex
                                   's' -> ((succ index) + maxIndex) `mod` maxIndex
                                   cmd -> index


    if command == 'f' && index == 3 then exitSuccess  -- Sair do Jogo
    else if command =='f' && index == 2 then Crdt.main
    else if command == 'f' && index == 1 then Rls.main
    else if command == 'f' && index == 0 then Stt.main -- Inicia Jogo
    else putStrLn ""       -- Continua no menu

    mainLoop newIndex


main :: IO()
main = mainLoop 0
