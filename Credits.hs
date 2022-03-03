module Credits where

import qualified Screen as Scr
import System.IO
import System.IO.Unsafe (unsafeDupablePerformIO)
import Control.Concurrent

texto :: [[Char]]
texto = [
    " ________________________________________________________________________________",
    "|                                                                                |",
    "|   Projeto em Haskell para disciplina Paradigmas de Linguagens de Programação   |", 
    "|                                                                                |", 
    "|   Integrantes do Grupo:                                                        |", 
    "|                                                                                |",
    "|   >>> Adísio Pereira Fialho Júnior                                             |",
    "|   >>> Sonaly Katly Garcia Nunes                                                |",
    "|   >>> Roberta Felix da Silva                                                   |",
    "|________________________________________________________________________________|"]

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

    let tmp1 = Scr.renderCentralized initialBuffer textoBuf 0 2
    let tmp2 = Scr.renderCentralized tmp1 menuBuf 0 10
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
