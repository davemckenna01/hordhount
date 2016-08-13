import System.Environment
import Control.Monad.State

-- A simple word count "wc" clone

-- count 'dave is cool'
-- > (12, 3, 0, 'x') 12 characters, 3 words, 0 lines, last char = 'x'

data Report = Report {
    characters :: Int,
    words      :: Int,
    lines      :: Int
} deriving Show

-- this could be called "categorize"... it advances the state of the counters
-- based on how it categorizes a character
advanceState :: (Report, Char) -> Char -> (Report, Char)
advanceState (Report c w ln, last) x 
    | x == ' '  && last /= ' '                 = (Report (c + 1) (w + 1)  ln,      x)
    | x == '\n' && last /= ' ' && last /= '\n' = (Report  c      (w + 1) (ln + 1), x)
    | x == '\n'                                = (Report  c       w      (ln + 1), x)
    | otherwise                                = (Report (c + 1)  w       ln,      x)

count :: [Char] -> State (Report, Char) ()
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
    -- there's runState, execState, and evalState, all similar only they
    -- differ in what parts of the tuple they return (fst, snd, or both)
    let finalState = execState (count file) (Report 0 0 1,'x')
    print $ fst finalState 

main = stateMonadStyle
