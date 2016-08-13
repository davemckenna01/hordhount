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

tick :: Char -> State (Report, Char) ()
tick x = state $ \s -> ((), advanceState s x) 

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
    let finalState = runState (count file) (Report 0 0 1,'x')
    print $ snd finalState 

main = stateMonadStyle
