module Screen where

import Text.Printf
import System.IO
import qualified Terminal
import System.IO
import System.IO.Unsafe (unsafeDupablePerformIO)

-- um pixel representa uma string que sera tratada
-- como uma unidade na tela
data IPixel = Pixel [Char]

pixelWidth = 2 -- tamanho do pixel padrao da tela

termSize = unsafeDupablePerformIO Terminal.getTermSize
termHeight = fst termSize
termWidth = snd termSize

-- tamanho da tela composta por pixels
width = termWidth `div` pixelWidth
height = termHeight

-- alguns pixels padroes
emptyPxl = replicate pixelWidth ' '

-- cria uma matrix de strings que serve como buffer para imprimir na tela
createScreenBuffer :: Int -> Int -> [Char] -> [[IPixel]]
createScreenBuffer w h c = createScreenBufferColored w h c

-- cria uma matrix de strings colorida
createScreenBufferColored :: Int -> Int -> [Char] -> [[IPixel]]
createScreenBufferColored w h c = replicate h $ replicate w $ (Pixel c)

-- largura de um buffer, maior largura entre todas as linhas
bufferWidth :: [[IPixel]] -> Int
bufferWidth buffer = maximum $ map length buffer

bufferHeight :: [[IPixel]] -> Int
bufferHeight buffer = length buffer

-- imprime um buffer no centro de outro com um deslocamento
renderCentralized :: [[IPixel]] -> [[IPixel]] -> Int -> Int -> [[IPixel]]
renderCentralized buffer source offsetX offsetY = do
    let x = (bufferWidth buffer) `div` 2 - (bufferWidth source) `div` 2 + offsetX
    let y = (bufferHeight buffer) `div` 2 - (bufferHeight source) `div` 2 + offsetY
    renderInBuffer buffer source x y

-- imprime buffer no buffer
renderInBuffer :: [[IPixel]] -> [[IPixel]] -> Int -> Int -> [[IPixel]]
renderInBuffer buffer source x y
    | length buffer == 0    = buffer
    | length source == 0    = buffer
    | y <= 0                = renderInBufferRow bufferFirst sourceFirst x : renderInBuffer bufferTail sourceTail x y
    | otherwise             = bufferFirst : renderInBuffer bufferTail source x (pred y)
    where bufferFirst = head buffer
          bufferTail = tail buffer 
          sourceFirst = head source
          sourceTail = tail source

-- imprime string em outra string
renderInBufferRow :: [IPixel] -> [IPixel] -> Int -> [IPixel]
renderInBufferRow target source i 
    | length source == 0= target
    | i >= length target= target
    | i <= 0            = sourceFirst : renderInBufferRow targetTail sourceTail 0
    | otherwise         = targetFirst : renderInBufferRow targetTail source (pred i) 
    where targetFirst = head target
          targetTail = tail target
          sourceFirst = head source
          sourceTail = tail source

-- converte string em buffer
stringToBuffer :: [Char] -> Int -> [IPixel]
stringToBuffer "" _ = []
stringToBuffer source step = do
    let rest = drop step source
    let content = take step $ source ++ (repeat ' ')
    (Pixel content) : stringToBuffer rest step

-- cria uma matrix de pixels a partir de uma lista de strings
createBufferFromStringMatrix :: [[Char]] -> [[IPixel]]
createBufferFromStringMatrix [] = []
createBufferFromStringMatrix (row:source) = do
    stringToBuffer row pixelWidth : createBufferFromStringMatrix source

bufferToString :: [[IPixel]] -> [Char]
bufferToString [] = ""
bufferToString (row:buffer) = bufferRowToString row ++ bufferToString buffer

bufferRowToString :: [IPixel] -> [Char]
bufferRowToString [] = "\n"
bufferRowToString ((Pixel content):row)= do
    content ++ bufferRowToString row

-- renderiza um buffer na tela utilizando buffer na saída do terminal
printScreen :: [[IPixel]] -> IO()
printScreen buffer = do
    hSetBuffering stdout (BlockBuffering Nothing)
    putStrLn $ bufferToString buffer
    hFlush stdout
    hSetBuffering stdout LineBuffering

    
