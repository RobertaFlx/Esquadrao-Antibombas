module Rules where

import qualified Screen as Scr
import System.IO
import System.IO.Unsafe (unsafeDupablePerformIO)
import Control.Concurrent
 

texto:: [[Char]]
texto = [
    " _______________________________________________________________________________________________________________________________",
    "|                                                                                                                               |",
    "|   Você está sendo treinado para ser um especialista em desarmamento de bombas e foi chamado para um teste prático em campo.   |",
    "|                                                                                                                               |",
    "|   Ao chegar no lugar indicado, é informado que foram implantadas bombas comuns, que podem ser desarmadas, e bombas letais,    |",
    "|   que você ainda não tem treinamento necessário para desativar. O objetivo do jogo é conseguir andar pelo campos seguros,     |",
    "|   descobrindo onde estão as bombas e as desarmando quando possível.                                                           |",
    "|                                                                                                                               |",
    "|   >>> O jogador inicia o teste com nota máxima, conforme o passar do jogo, sua nota poderá diminuir de acordo com os erros    |",
    "|       cometidos no teste.                                                                                                     |",
    "|                                                                                                                               |",
    "|   >>> Se o jogador errar mais de três vezes, ficará abaixo da média e reprovará no teste, desta forma, o jogo termina.        |",
    "|                                                                                                                               |",
    "|   >>> Erros são contados apenas quando o jogador tentar desativar uma bomba em um campo que não possui bomba ou abrir um      |",
    "|       caminho já revelado.                                                                                                    |",
    "|                                                                                                                               |",
    "|   >>> Se o jogador tentar desarmar ou abrir caminho em uma bomba letal, automaticamente, ele é reprovado no teste e o         |",
    "|       jogo termina.                                                                                                           |",
    "|                                                                                                                               |",
    "|   >>> Se o tempo acabar ou a quantidade de jogadas zerar, o jogador é reprovado e o jogo termina.                             |",
    "|_______________________________________________________________________________________________________________________________|"]

menu :: [[Char]]
menu = [
    "Voltar para o menu <<<"]

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
