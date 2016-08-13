import System.Environment
import Control.Monad.State
import Report

-- A simple word count "wc" clone

-- count 'dave is cool'
-- > (12, 3, 0, 'x') 12 characters, 3 words, 0 lines, last char = 'x'

count :: [Char] -> State (Report, Char) ()
count [] = return ()
count (x:xs) = do
    report <- get
    put $ addToReport report x
    count xs
    return ()

stateMonadStyle :: IO ()
stateMonadStyle = do
    args <- getArgs
    let filename = head args
    file <- readFile filename
    -- there's runState, execState, and evalState, all similar only they
    -- differ in what parts of the tuple they return (fst, snd, or both)
    let finalState = execState (count file) (Report 0 0 1, 'x') -- 'x' could be anything
    print $ fst finalState 

main = stateMonadStyle
