import System.Environment
import Control.Monad.State

-- A simple word count "wc" clone

-- count 'dave is cool'
-- > (12, 3, 0, 'x') 12 characters, 3 words, 0 lines, last char = 'x'

-- this could be called "categorize"... it advances the state of the counters
-- based on how it categorizes a character
advanceState :: (Int, Int, Int, Char) -> Char -> (Int, Int, Int, Char)
advanceState (c, w, ln, last) x 
    | x == ' '  && last /= ' '                 = (c + 1, w + 1, ln,     x)
    | x == '\n' && last /= ' ' && last /= '\n' = (c,     w + 1, ln + 1, x)
    | x == '\n'                                = (c,     w,     ln + 1, x)
    | otherwise                                = (c + 1, w,     ln,     x)

count :: [Char] -> State (Int, Int, Int, Char) ()
count [] = return ()
count (x:xs) = do
    s <- get
    put $ advanceState s x
    count xs
    return ()

stateMonadStyle :: IO ()
stateMonadStyle = do
    args <- getArgs
    let filename = head args
    file <- readFile filename
    let finalState = runState (count file) (0,0,1,'x')
    print $ snd finalState 

main = stateMonadStyle
