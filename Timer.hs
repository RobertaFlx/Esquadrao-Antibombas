module Timer where

import qualified Screen as Scr
import qualified Rules as Rls
import System.IO
import System.IO.Unsafe (unsafeDupablePerformIO)
import System.Exit
import Control.Concurrent
import Control.Concurrent.STM

main = do
    timer1 <- newTimer (300 * 1000000)
    waitTimer timer1
    putStrLn "Timer 1 expired"


data State = Start 
type Timer = (TVar State, TMVar ())

waitTimer :: Timer -> IO ()
waitTimer (_, timer) = atomically $ readTMVar timer


newTimer :: Int -> IO Timer
newTimer n = do
    state <- atomically $ newTVar Start
    timer <- atomically $ newEmptyTMVar
    forkIO $ do
        threadDelay n
        atomically $ do
            runState <- readTVar state
            case runState of
                Start -> putTMVar timer ()
    return (state, timer)
