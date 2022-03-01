module Instructions where

import qualified Screen as Scr
import System.IO
import System.IO.Unsafe (unsafeDupablePerformIO)
import Control.Concurrent
 

texto:: [[Char]]
texto = [
    " _______________________________________________________________________________________________________________________________",
    "|                                                                                                                               |",
    "|                                                                                                                               |",
    "|                                        █▀▀ █▀█ █▀▄▀█ █▀█    █ █▀█ █▀▀ █▀█ █▀█                                                 |",                                                           
    "|                                        █   █ █ █ █ █ █ █  ▄ █ █ █ █ ▄ █▀█ █▀▄                                                 |",                                                         
    "|                                        ▀▀▀ ▀▀▀ ▀ ▀ ▀ ▀▀▀  ▀▀▀ ▀▀▀ ▀▀▀ ▀ ▀ ▀ ▀                                                 |",                                                            
    "|                                                                                                                               |",
    "|                                                                                                                               |",
    "|   >>> Bombas desármáveis podem ser identificadas pela letra B, e bombas letais pela letra L.                                  |",
    "|                                                                                                                               |",
    "|   >>> Por padrão, existem 4 bombas desármáveis e 8 bombas letais escondidas pelo campo.                                       |",
    "|                                                                                                                               |",
    "|   >>> O jogador possui três opções de jogada, sendo elas: Abrir caminho, Desativar Bomba e Adicionar alerta.                  |",                                                                                                                          
    "|                                                                                                                               |",
    "|   >>> O numeral 0 indicará que a posição escolhida não possui nenhuma bomba, outros numerais indicam que ao redor deles       |",
    "|       existe uma quantidade de bombas Letais igual ao numeral. Logo, o númeral 1, por exemplo, indica que existe uma bomba    |",
    "|       letal ao seu redor.                                                                                                     |",
    "|                                                                                                                               |",
    "|   >>> Bombas que já foram desativadas podem ser identificadas pela letra D, e Alertas marcados podem ser identificados        |",
    "|       pela letra A. Após um Alerta ser marcado ele não pode ser desmarcado.                                                   |",                                                                                                   
    "|                                                                                                                               |",
    "|   >>> O jogador deve escrever a ação da jogada que quer fazer(Abrir, Desativar, Alerta), e em seguida indicar a posição       |",
    "|       da coluna e da linha, respectivamente.                                                                                  |",  
    "|                                                                                                                               |",
    "|   >>> Para ganhar, o jogador deve desativar as Bombas B e revelar todos as posições sem bomba.                                |",                                                                                                          
    "|_______________________________________________________________________________________________________________________________|"]

menu :: [[Char]]
menu = [
    "Começar a partida <<<"]

commandsTable :: [[Char]]
commandsTable = [ 
    " f - selecionar        "]

-- Cria o menu e imprime ele na tela
printMenu :: [[Char]] -> IO()
printMenu menuTab = do

    let initialBuffer = Scr.createScreenBuffer Scr.width Scr.height Scr.emptyPxl
    let textoBuf = Scr.createBufferFromStringMatrix texto
    let menuBuf = Scr.createBufferFromStringMatrix menuTab
    let tableBuf = Scr.createBufferFromStringMatrix commandsTable
    
    let tmp1 = Scr.renderCentralized initialBuffer textoBuf 0 0
    let tmp2 = Scr.renderCentralized tmp1 menuBuf 0 14
    let tmp3 = Scr.renderInBuffer tmp2 tableBuf 1 3
    
    Scr.printScreen tmp3 


mainLoop :: Int -> IO()
mainLoop index = do

    printMenu $ menu 

    -- Pega um unico caracter da entrada
    hSetBuffering stdin NoBuffering
    command <- getChar
    hSetBuffering stdin LineBuffering

    -- Processa o comando recebido
    case command of 'f' -> putStrLn " "
                    cmd -> mainLoop 0
 

main :: IO()
main = mainLoop 0
