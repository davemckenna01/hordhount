import System.Environment
import Control.Monad.State
import Report

-- A simple word count "wc" clone

-- count 'dave is cool'
-- > (12, 3, 0, 'x') 12 characters, 3 words, 0 lines, last char = 'x'

tick :: Char -> State (Report, Char) ()
tick x = state $ \s -> ((), addToReport s x) 

count :: [Char] -> State (Report, Char) ()
count [] = return ()
count (x:xs) = do
    tick x
    count xs
    return ()

-- this is equiv and actually works:
--count (x:xs) = tick x >>= \s -> count xs >>= \s -> return ()

stateMonadStyle :: IO ()
stateMonadStyle = do
    args <- getArgs
    let filename = head args
    file <- readFile filename
    -- there's runState, execState, and evalState, all similar only they
    -- differ in what parts of the tuple they return (fst, snd, or both)
    let finalState = execState (count file) (Report 0 0 1,'x')
    print $ fst finalState 

main = stateMonadStyle
